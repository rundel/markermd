# Regression tests for resolving a heading nested inside a fenced div. Selecting
# such a heading used to yield an empty question AST (every rule failed); it now
# resolves the heading's section within the div.

hid_parse = function(...) q2r::parse_qmd(paste(c(...), collapse = "\n"), quiet = TRUE)

hid_question = function(id, name, node_ids) {
  markermd_question(
    id = as.integer(id), name = name,
    selected_nodes = markermd_node_selection(node_ids = node_ids)
  )
}

hid_doc = function() {
  hid_parse(
    "## Q1 {#q1}", "", "Intro text.", "",
    "::: {#panel .callout}", "",
    "### Inner {#inner}", "", "Some prose here.", "",
    "```{r}", "x <- 1", "```", "",
    "more prose", "",
    "```{r}", "y <- 2", "```", "",
    ":::", "",
    "## Q2 {#q2}", "", "After the div.", ""
  )
}

test_that("get_question_ast resolves a heading nested inside a div to its in-div section", {
  ast = hid_doc()
  qa = get_question_ast(ast, hid_question(1, "Q", "inner"))

  kinds = vapply(qa@blocks@content, q2r_node_kind, character(1))
  expect_true("Heading" %in% kinds)
  # The in-div section is the heading plus its following siblings up to the
  # closing fence: 2 chunks and 2 markdown paragraphs.
  expect_equal(sum(kinds == "Chunk"), 2L)
  expect_equal(sum(kinds == "Markdown"), 2L)
})

test_that("top-level heading sections still resolve unchanged", {
  ast = hid_doc()

  q1 = get_question_ast(ast, hid_question(1, "Q1", "q1"))
  k1 = vapply(q1@blocks@content, q2r_node_kind, character(1))
  # Q1 owns its intro paragraph and the whole div, and stops at Q2.
  expect_true(all(c("Heading", "Markdown", "Div") %in% k1))
  expect_false("Heading" %in% k1[-1])  # no second heading leaked in

  q2 = get_question_ast(ast, hid_question(2, "Q2", "q2"))
  expect_equal(length(q2@blocks@content), 2L)  # heading + one paragraph
})

test_that("an unresolved heading id yields an empty section rather than erroring", {
  ast = hid_doc()
  qa = get_question_ast(ast, hid_question(1, "Q", "does-not-exist"))
  expect_equal(length(qa@blocks@content), 0L)
})

test_that("heading_section_indices does not double-count when outer and inner headings both selected", {
  ast = hid_doc()
  records = q2r_flatten(ast)
  # Selecting both q1 (which contains the div) and inner: the div is included
  # whole under q1 and is not also descended into for inner.
  idx_q1 = heading_section_indices(records, "q1")
  idx_both = heading_section_indices(records, c("q1", "inner"))
  expect_identical(idx_q1, idx_both)
})

test_that("line_node_id_chains does not leak an in-div heading id past the closing fence", {
  lines = c(
    "## Q1 {#q1}", "Intro.",
    "::: {#panel}",
    "### Inner {#inner}", "In div line.",
    ":::",
    "After div line.",
    "## Q2 {#q2}", "Q2 body."
  )
  ast = hid_parse(lines)
  chains = line_node_id_chains(lines, ast)

  in_div = which(lines == "In div line.")
  after = which(lines == "After div line.")
  expect_true("inner" %in% chains[[in_div]])
  expect_false("inner" %in% chains[[after]])
  expect_true("q1" %in% chains[[after]])
})
