div_fixture_ast = function() {
  file = system.file("examples/div_assignment/assignment.qmd", package = "markermd")
  parse_assignment_document(file)
}

parse_qmd_lines = function(...) {
  q2r::parse_qmd(paste(c(...), collapse = "\n"), quiet = TRUE)
}

test_that("node_id_for_index returns ids only for headings and id'd divs", {
  ast = div_fixture_ast()

  expect_equal(node_id_for_index(ast, 1), "question-1") # heading
  expect_equal(node_id_for_index(ast, 2), "")           # paragraph
  expect_equal(node_id_for_index(ast, 3), "q1-answer")  # id'd div
  expect_equal(node_id_for_index(ast, 5), "")           # chunk inside the div
  expect_equal(node_id_for_index(ast, 6), "")           # class-only div
})

test_that("node_ids_to_indices resolves headings and id'd divs, never class-only divs", {
  ast = div_fixture_ast()

  expect_equal(node_ids_to_indices(ast, "q1-answer"), 3L)
  expect_equal(node_ids_to_indices(ast, "question-1"), 1L)
  expect_equal(sort(node_ids_to_indices(ast, c("question-1", "q1-answer"))), c(1L, 3L))
  expect_equal(node_ids_to_indices(ast, "callout-note"), integer(0))
  expect_equal(node_ids_to_indices(ast, character(0)), integer(0))
})

test_that("node_is_selectable: headings and id'd divs only", {
  ast = div_fixture_ast()
  recs = q2r_flatten(ast)

  expect_true(node_is_selectable(recs[[1]]$node))   # heading
  expect_false(node_is_selectable(recs[[2]]$node))  # paragraph
  expect_true(node_is_selectable(recs[[3]]$node))   # id'd div
  expect_false(node_is_selectable(recs[[6]]$node))  # class-only div
})

test_that("q2r_flatten records separate display parent, real container, and block position", {
  ast = div_fixture_ast()
  recs = q2r_flatten(ast)

  container = vapply(recs, function(r) r$container, integer(1))
  block_pos = vapply(recs, function(r) r$block_pos, integer(1))

  # container tracks real AST nesting only: div children point at their div,
  # everything else (including section members under headings) is top-level
  expect_equal(container, c(0L, 0L, 0L, 3L, 3L, 0L, 6L, 0L, 0L))

  # block_pos bridges top-level records to ast@blocks@content positions, in
  # document order, and is NA for nested records
  expect_equal(block_pos, c(1L, 2L, 3L, NA, NA, 4L, NA, 5L, 6L))
  top = which(!is.na(block_pos))
  for (i in top) {
    expect_identical(recs[[i]]$node, ast@blocks@content[[block_pos[i]]])
  }

  # a section member's display parent is its heading even though it is a
  # top-level block (synthetic vs real nesting)
  expect_equal(recs[[9]]$parent, 8)
  expect_equal(recs[[9]]$container, 0L)

  # nested divs: the inner div's children point at the inner div
  nested = parse_qmd_lines(
    "::: {.outer}", "::: {#inner}", "inside", ":::", ":::"
  )
  nrecs = q2r_flatten(nested)
  expect_equal(vapply(nrecs, function(r) r$container, integer(1)), c(0L, 1L, 2L))
  expect_equal(vapply(nrecs, function(r) r$block_pos, integer(1)), c(1L, NA, NA))
})

test_that("classify_selected_ids splits heading vs div ids", {
  ast = div_fixture_ast()

  split = classify_selected_ids(ast, c("question-1", "q1-answer"))
  expect_equal(split$heading_ids, "question-1")
  expect_equal(split$div_ids, "q1-answer")
})

test_that("find_div_by_id finds top-level and nested divs, NULL when missing", {
  ast = div_fixture_ast()
  div = find_div_by_id(ast@blocks@content, "q1-answer")
  expect_true(S7::S7_inherits(div, q2r::pandoc_div))
  expect_equal(length(div@content@content), 2L)
  expect_null(find_div_by_id(ast@blocks@content, "does-not-exist"))

  nested = parse_qmd_lines(
    "::: {.outer}", "::: {#inner}", "inside", ":::", ":::"
  )
  hit = find_div_by_id(nested@blocks@content, "inner")
  expect_true(S7::S7_inherits(hit, q2r::pandoc_div))
})

test_that("get_question_ast drills into a selected div's contents", {
  ast = div_fixture_ast()
  q = markermd_question(
    1L, "Q1", markermd_node_selection(node_ids = "q1-answer"),
    list(markermd_rule(node_type = "Chunk", verb = "has at least", values = 1))
  )

  qast = get_question_ast(ast, q)
  kinds = vapply(qast@blocks@content, q2r_node_kind, character(1))
  expect_equal(kinds, c("Markdown", "Chunk")) # the div's children, not the wrapper

  res = validate_question_rules(ast, q)
  expect_equal(res$status, "pass") # nested chunk is now visible to the rule
})

test_that("heading + nested div selection counts nested content once", {
  ast = div_fixture_ast()
  q = markermd_question(
    1L, "Q1", markermd_node_selection(node_ids = c("question-1", "q1-answer")),
    list(markermd_rule(node_type = "Chunk", verb = "has between", values = c(1, 1)))
  )

  res = validate_question_rules(ast, q)
  expect_equal(res$status, "pass") # exactly one chunk, no double count
})

test_that("an id'd but empty div yields zero blocks and fails a count rule cleanly", {
  ast = parse_qmd_lines("# H", "", "::: {#empty}", ":::")
  q = markermd_question(
    1L, "Q", markermd_node_selection(node_ids = "empty"),
    list(markermd_rule(node_type = "Any node", verb = "has at least", values = 1))
  )

  qast = get_question_ast(ast, q)
  expect_equal(length(qast@blocks@content), 0L)

  res = validate_question_rules(ast, q)
  expect_equal(res$status, "fail")
})

test_that("multi-type rules count node kinds as a logical OR", {
  ast = parse_qmd_lines(
    "# H", "", "Some markdown paragraph.", "", "```{=html}", "<p>raw html</p>", "```"
  )
  kinds = vapply(ast@blocks@content, q2r_node_kind, character(1))
  expect_equal(kinds, c("Heading", "Markdown", "Raw Block"))

  # union of Markdown + Raw Block is 2 nodes
  expect_true(evaluate_rule(ast, markermd_rule(c("Markdown", "Raw Block"), "has at least", 2L))$passed)
  # Raw Block alone is only 1 node
  expect_false(evaluate_rule(ast, markermd_rule("Raw Block", "has at least", 2L))$passed)
  # "Any node" anywhere in the selection disables type filtering
  expect_true(evaluate_rule(ast, markermd_rule(c("Any node", "Heading"), "has at least", 3L))$passed)
})

test_that("q2r_node_label surfaces an id'd div's id", {
  ast = div_fixture_ast()
  label = q2r_node_label(q2r_flatten(ast)[[3]]$node)
  expect_true(startsWith(label, "Div"))
  expect_match(label, "#q1-answer", fixed = TRUE)
})
