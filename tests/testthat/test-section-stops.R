# A question's heading section ends at the next heading another question of
# the same template selects, in grading and in every view (grading, the
# template editor's badges and tree, the mark-side cards and Source view, and
# assignment_outline())

ss_parse = function(...) {
  q2r::parse_qmd(paste(c(...), collapse = "\n"), quiet = TRUE)
}

ss_question = function(id, ids, rules = list(), scope = "sections") {
  markermd_question(
    id = id, name = paste0("Q", id),
    selected_nodes = markermd_node_selection(node_ids = if (scope == "document") character(0) else ids),
    rules = rules, scope = scope
  )
}

ss_kinds = function(ast, question, stop_ids = NULL) {
  blocks = get_question_ast(ast, question, stop_ids)@blocks@content
  vapply(blocks, q2r_node_kind, character(1))
}

# A student promoted "## Q1" to "#": its section would otherwise swallow Q2
# and Q3
ss_promoted = function() {
  ss_parse(
    "# Q1 {#q1}", "", "Answer one.", "",
    "## Q2 {#q2}", "", "Answer two.", "", "```{r}", "2", "```", "",
    "## Q3 {#q3}", "", "Answer three."
  )
}

ss_questions = function() {
  list(ss_question(1L, "q1"), ss_question(2L, "q2"), ss_question(3L, "q3"))
}

test_that("other_question_heading_ids() lists the other questions' selections", {
  qs = ss_questions()
  expect_equal(other_question_heading_ids(qs, qs[[1]]), c("q2", "q3"))
  expect_equal(other_question_heading_ids(qs, qs[[2]]), c("q1", "q3"))

  template = markermd_template(original_ast = ss_promoted(), questions = qs)
  expect_equal(other_question_heading_ids(template, qs[[3]]), c("q1", "q2"))

  # Ids a question shares with another never stop its own sections, and a
  # document-scope question neither stops nor is stopped
  shared = list(ss_question(1L, c("q1", "q2")), ss_question(2L, "q2"))
  expect_equal(other_question_heading_ids(shared, shared[[1]]), character(0))
  whole = ss_question(4L, character(0), scope = "document")
  expect_equal(other_question_heading_ids(c(qs, list(whole)), whole), character(0))
  expect_equal(other_question_heading_ids(c(qs, list(whole)), qs[[1]]), c("q2", "q3"))
})

test_that("a promoted heading no longer swallows later questions' sections", {
  ast = ss_promoted()
  qs = ss_questions()

  expect_equal(ss_kinds(ast, qs[[1]]), c("Heading", "Markdown", "Heading", "Markdown", "Chunk", "Heading", "Markdown"))
  expect_equal(ss_kinds(ast, qs[[1]], other_question_heading_ids(qs, qs[[1]])), c("Heading", "Markdown"))
  expect_equal(ss_kinds(ast, qs[[2]], other_question_heading_ids(qs, qs[[2]])), c("Heading", "Markdown", "Chunk"))

  # Grading applies the stops: a "no chunks" rule on Q1 passes once Q2's chunk
  # is no longer counted as Q1's
  qs[[1]] = ss_question(1L, "q1", list(markermd_rule(node_type = "Chunk", verb = "has at most", values = 0L)))
  template = markermd_template(original_ast = ss_promoted(), questions = qs)
  results = validate_repo_against_rules(ast, template)
  expect_equal(results$Q1$status, "pass")
  expect_equal(validate_question_rules(ast, qs[[1]])$status, "fail")
})

test_that("questions on adjacent sibling headings are unchanged by the stops", {
  ast = ss_parse(
    "# Q1 {#q1}", "", "a1", "",
    "# Q2 {#q2}", "", "a2", "", "```{r}", "x", "```", "",
    "# Q3 {#q3}", "", "a3"
  )
  qs = ss_questions()
  for (q in qs) {
    expect_identical(
      get_question_ast(ast, q, other_question_heading_ids(qs, q)),
      get_question_ast(ast, q)
    )
  }
})

test_that("a parent heading's section ends at a child heading another question selects", {
  # The chosen semantics: the section ends at the stop; it does not resume
  # after the other question's part, so "Part 1b" belongs to neither question
  ast = ss_parse(
    "# Part 1 {#part-1}", "", "Intro.", "",
    "## Part 1a {#part-1a}", "", "Answer a.", "",
    "## Part 1b {#part-1b}", "", "Answer b."
  )
  parent = ss_question(1L, "part-1")
  child = ss_question(2L, "part-1a")
  qs = list(parent, child)

  expect_equal(ss_kinds(ast, parent, other_question_heading_ids(qs, parent)), c("Heading", "Markdown"))
  expect_equal(ss_kinds(ast, child, other_question_heading_ids(qs, child)), c("Heading", "Markdown"))
})

test_that("stops end sections inside a div too", {
  ast = ss_parse(
    "::: {#box}", "", "# Q1 {#q1}", "", "a1", "", "## Q2 {#q2}", "", "a2", "", ":::"
  )
  qs = list(ss_question(1L, "q1"), ss_question(2L, "q2"))
  expect_equal(ss_kinds(ast, qs[[1]], other_question_heading_ids(qs, qs[[1]])), c("Heading", "Markdown"))
})

test_that("the editor's tree highlight covers exactly the blocks grading takes", {
  ast = ss_promoted()
  qs = ss_questions()
  stops = other_question_heading_ids(qs, qs[[1]])
  items = build_ast_tree_structure(ast)
  direct = node_ids_to_indices(ast, "q1")

  covered = question_covered_indices(ast, "q1", stops)
  state = tree_selection_state(items, direct, integer(0), covered = covered)

  records = q2r_flatten(ast)
  blocks = question_block_indices(records, "q1", character(0), stops)
  expect_equal(state$selected, sort(unique(c(direct, blocks))))
  expect_false(any(node_ids_to_indices(ast, c("q2", "q3")) %in% state$selected))

  # The blocks behind the highlight are the nodes grading counts
  graded = get_question_ast(ast, qs[[1]], stops)@blocks@content
  expect_identical(lapply(blocks, function(i) records[[i]]$node), graded)

  # Without stops the covered set is the tree's own descendant walk
  for (id in c("q1", "q2", "q3")) {
    expect_equal(
      question_covered_indices(ast, id),
      as.integer(compute_all_selected_nodes(items, node_ids_to_indices(ast, id)))
    )
  }
})

test_that("question_covered_indices() matches the tree walk on the bundled key", {
  key = system.file("examples/test_assignment/hw3-key/assignment.qmd", package = "markermd")
  skip_if(!nzchar(key))
  ast = parse_assignment_document(key)
  items = build_ast_tree_structure(ast)
  ids = unlist(lapply(q2r_flatten(ast), function(r) {
    node = r$node
    if ((S7::S7_inherits(node, q2r::pandoc_header) || S7::S7_inherits(node, q2r::pandoc_div)) && nzchar(node@attr@id)) node@attr@id
  }))
  expect_gt(length(ids), 0)
  for (id in ids) {
    expect_equal(
      question_covered_indices(ast, id),
      as.integer(compute_all_selected_nodes(items, node_ids_to_indices(ast, id))),
      info = id
    )
  }
})

test_that("the editor's question server stops its sections at other questions' headings", {
  doc = ss_promoted()
  q1 = ss_question(1L, "q1", list(markermd_rule(node_type = "Chunk", verb = "has at most", values = 0L)))
  others = list(ss_question(2L, "q2"), ss_question(3L, "q3"))
  all = shiny::reactiveVal(c(list(q1), others))

  shiny::testServer(
    question_server,
    args = list(id = "q1", ast = shiny::reactive(doc), initial_question = q1, all_questions = all),
    {
      session$flushReact()
      covered = session$returned$covered_indices()
      expect_equal(covered, question_covered_indices(doc, "q1", c("q2", "q3")))
      expect_false(any(node_ids_to_indices(doc, c("q2", "q3")) %in% covered))

      # Without the other questions the section runs on and the tree walks
      # its own descendants
      all(list(q1))
      session$flushReact()
      expect_null(session$returned$covered_indices())
    }
  )
})

test_that("the editor's rule badges apply the stops", {
  ast = ss_promoted()
  q1 = ss_question(1L, "q1", list(markermd_rule(node_type = "Chunk", verb = "has at most", values = 0L)))
  stops = shiny::reactiveVal(c("q2", "q3"))

  shiny::testServer(
    function(id) {
      shiny::moduleServer(id, function(input, output, session) {
        state = shiny::reactiveVal(q1)
        machine = question_rules_server(input, output, session, state, shiny::reactive(ast), stops)
        session$userData$machine = machine
      })
    },
    args = list(id = "rules"),
    {
      session$flushReact()
      status = session$userData$machine$rule_status()
      expect_true(isTRUE(status[[1]]$passed))
      stops(character(0))
      session$flushReact()
      status = session$userData$machine$rule_status()
      expect_false(isTRUE(status[[1]]$passed))
    }
  )
})

test_that("the Source view highlight ends where the section does", {
  lines = c(
    "# Q1 {#q1}", "", "Answer one.", "",
    "## Q2 {#q2}", "", "Answer two.", "",
    "## Q3 {#q3}", "", "Answer three."
  )
  ast = ss_parse(lines)
  ranges = map_content_to_lines(lines, "q1", ast, c("q2", "q3"))
  covered_lines = unlist(lapply(ranges, function(r) seq(r$start, r$end)))
  expect_true(all(1:3 %in% covered_lines))
  expect_false(any(5:11 %in% covered_lines))

  # Without stops the whole promoted section is highlighted
  ranges = map_content_to_lines(lines, "q1", ast)
  expect_true(all(c(1, 7, 11) %in% unlist(lapply(ranges, function(r) seq(r$start, r$end)))))
})

test_that("assignment_outline() counts against a template's stops", {
  dir = tempfile()
  dir.create(dir)
  file = file.path(dir, "hw.qmd")
  writeLines(c(
    "# Q1 {#q1}", "", "a1", "", "```{r}", "1", "```", "",
    "## Q2 {#q2}", "", "a2", "", "```{r}", "2", "```"
  ), file)

  plain = assignment_outline(file)
  expect_equal(plain$n_chunk[plain$id == "q1"], 2)

  template = markermd_template(original_ast = ss_promoted(), questions = ss_questions()[1:2])
  stopped = assignment_outline(file, template = template)
  expect_equal(stopped$n_chunk[stopped$id == "q1"], 1)
  expect_equal(stopped$n_chunk[stopped$id == "q2"], 1)

  expect_error(assignment_outline(file, template = "x.yaml"), "markermd_template")
})
