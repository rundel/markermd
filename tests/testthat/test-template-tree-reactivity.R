# The template editor's tree is drawn once per document and patched in place
# (markermd_tree_state) only when the active question's selection or filtered
# set changes, so rule edits and renames cost nothing on the tree.

test_that("rule edits and renames neither redraw nor patch the tree", {
  qmd = withr::local_tempfile(fileext = ".qmd")
  writeLines(c("# Q1", "", "Some text.", "", "```{r}", "1 + 1", "```", "", "# Q2", "", "More."), qmd)
  doc_ast = parse_assignment_document(qmd)
  app = template_editor_app(shiny::reactiveVal(doc_ast), NULL, source_path = qmd)

  counts = new.env()
  counts$renders = 0
  original_render = render_ast_tree
  testthat::local_mocked_bindings(render_ast_tree = function(...) {
    counts$renders = counts$renders + 1
    original_render(...)
  })

  shiny::testServer(app$server, {
    patches = list()
    send = session$sendCustomMessage
    session$sendCustomMessage = function(type, message) {
      if (identical(type, "markermd_tree_state")) patches[[length(patches) + 1]] <<- message
      send(type, message)
    }
    tree = function() invisible(output[["ast_panel-ast_tree_ui"]])

    session$flushReact()
    tree()
    expect_equal(counts$renders, 1)

    # A heading click arrives through the tree's one delegated input and
    # patches the highlight without a redraw
    n_patches = length(patches)
    session$setInputs(`ast_panel-tree_event` = list(index = 1L, action = "toggle", nonce = 1))
    session$flushReact()
    tree()
    expect_equal(counts$renders, 1)
    expect_equal(length(patches), n_patches + 1)
    expect_equal(unlist(patches[[length(patches)]]$direct), 1L)
    expect_equal(question_modules()[[1]]$server$question()@selected_nodes@node_ids, "q1")

    # Adding a rule, editing its values and renaming the question change
    # neither the selection nor the filtered set
    n_patches = length(patches)
    session$setInputs(`question_1-add_rule` = 1)
    session$flushReact()
    tree()
    rule_inputs = grep("^question_1-rule_.*-verb$", names(shiny::reactiveValuesToList(input)), value = TRUE)
    rule = if (length(rule_inputs) > 0) sub("-verb$", "", rule_inputs[1]) else "question_1-rule_1"
    do.call(session$setInputs, stats::setNames(list("has content"), paste0(rule, "-verb")))
    for (value in c("a", "ab", "abc")) {
      do.call(session$setInputs, stats::setNames(list(value), paste0(rule, "-values")))
      tree()
    }
    session$setInputs(`question_1-question_name` = "Renamed")
    session$flushReact()
    tree()

    q = question_modules()[[1]]$server$question()
    expect_equal(q@name, "Renamed")
    expect_length(q@rules, 1)
    expect_equal(counts$renders, 1)
    expect_equal(length(patches), n_patches)

    # Toggling again deselects; a toggle on a non-selectable node or an index
    # outside the tree is ignored
    session$setInputs(`ast_panel-tree_event` = list(index = 2L, action = "toggle", nonce = 2))
    session$setInputs(`ast_panel-tree_event` = list(index = 99L, action = "toggle", nonce = 3))
    session$flushReact()
    expect_equal(question_modules()[[1]]$server$question()@selected_nodes@node_ids, "q1")
    session$setInputs(`ast_panel-tree_event` = list(index = 1L, action = "toggle", nonce = 4))
    session$flushReact()
    expect_equal(question_modules()[[1]]$server$question()@selected_nodes@node_ids, character(0))
    expect_equal(unlist(patches[[length(patches)]]$direct), NULL)
  })
})
