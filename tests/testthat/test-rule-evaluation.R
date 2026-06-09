# Fixture document parsed once per file: 2 headings, 1 labelled chunk (via a
# knitr-style header so normalization is exercised end to end), 2 markdown
# paragraphs, 1 bullet list
rule_eval_fixture_ast = local({
  qmd = tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: Rule evaluation fixture",
    "---",
    "",
    "# Question 1",
    "",
    "Some text mentioning quantiles here.",
    "",
    "```{r q1-plot, echo=FALSE}",
    "plot(1)",
    "```",
    "",
    "More text after the chunk.",
    "",
    "## Summary",
    "",
    "- item a",
    "- item b"
  ), qmd)
  markermd::parse_assignment_document(qmd)
})

rule = function(node_type, verb, values) {
  markermd::markermd_rule(node_type = node_type, verb = verb, values = values)
}


test_that("count verbs respect their boundaries (one Chunk, two Headings in fixture)", {
  ast = rule_eval_fixture_ast

  expect_true(markermd:::evaluate_rule(ast, rule("Chunk", "has at least", 1L))$passed)
  expect_false(markermd:::evaluate_rule(ast, rule("Chunk", "has at least", 2L))$passed)

  expect_true(markermd:::evaluate_rule(ast, rule("Chunk", "has at most", 1L))$passed)
  expect_false(markermd:::evaluate_rule(ast, rule("Chunk", "has at most", 0L))$passed)

  expect_true(markermd:::evaluate_rule(ast, rule("Chunk", "has between", c(1, 1)))$passed)
  expect_false(markermd:::evaluate_rule(ast, rule("Chunk", "has between", c(2, 3)))$passed)
  expect_false(markermd:::evaluate_rule(ast, rule("Chunk", "has between", c(0, 0)))$passed)

  expect_true(markermd:::evaluate_rule(ast, rule("Heading", "has between", c(2, 2)))$passed)
})


test_that("node types combine as OR and 'Any node' skips filtering", {
  ast = rule_eval_fixture_ast

  # 2 headings + 1 chunk
  expect_true(markermd:::evaluate_rule(ast, rule(c("Chunk", "Heading"), "has between", c(3, 3)))$passed)
  expect_true(markermd:::evaluate_rule(ast, rule("Any node", "has at least", 5L))$passed)

  # absent node kinds: zero matches fails 'at least' but passes 'at most 0'
  expect_false(markermd:::evaluate_rule(ast, rule("Table", "has at least", 1L))$passed)
  expect_true(markermd:::evaluate_rule(ast, rule("Table", "has at most", 0L))$passed)
})


test_that("content verbs glob-match node text case-insensitively", {
  ast = rule_eval_fixture_ast

  expect_true(markermd:::evaluate_rule(ast, rule("Markdown", "has content", "*quantile*"))$passed)
  expect_true(markermd:::evaluate_rule(ast, rule("Markdown", "has content", "*QUANTILE*"))$passed)
  expect_false(markermd:::evaluate_rule(ast, rule("Markdown", "has content", "*zebra*"))$passed)

  # chunks match on their code
  expect_true(markermd:::evaluate_rule(ast, rule("Chunk", "has content", "*plot(1)*"))$passed)

  expect_false(markermd:::evaluate_rule(ast, rule("Markdown", "lacks content", "*quantile*"))$passed)
  expect_true(markermd:::evaluate_rule(ast, rule("Markdown", "lacks content", "*zebra*"))$passed)

  # the empty pattern passes trivially for both polarity verbs
  expect_true(markermd:::evaluate_rule(ast, rule("Markdown", "has content", ""))$passed)
  expect_true(markermd:::evaluate_rule(ast, rule("Markdown", "lacks content", ""))$passed)
})


test_that("'has name' matches heading titles and chunk labels", {
  ast = rule_eval_fixture_ast

  expect_true(markermd:::evaluate_rule(ast, rule("Heading", "has name", "Question*"))$passed)
  expect_true(markermd:::evaluate_rule(ast, rule("Heading", "has name", "Summary"))$passed)
  expect_true(markermd:::evaluate_rule(ast, rule("Chunk", "has name", "q1-plot"))$passed)
  expect_true(markermd:::evaluate_rule(ast, rule("Chunk", "has name", "*plot*"))$passed)
  expect_false(markermd:::evaluate_rule(ast, rule("Chunk", "has name", "missing-label"))$passed)

  # nodes without names cannot satisfy a non-empty pattern
  expect_false(markermd:::evaluate_rule(ast, rule("Markdown", "has name", "*anything*"))$passed)
})


test_that("evaluate_rule accepts a pandoc AST or a plain node list and reports messages", {
  ast = rule_eval_fixture_ast
  nodes = ast@blocks@content

  via_ast = markermd:::evaluate_rule(ast, rule("Chunk", "has at least", 1L))
  via_list = markermd:::evaluate_rule(nodes, rule("Chunk", "has at least", 1L))
  expect_equal(via_ast$passed, via_list$passed)

  expect_named(via_ast, c("passed", "message"))
  expect_match(as.character(via_ast$message), "count check passed")
  failed = markermd:::evaluate_rule(ast, rule("Chunk", "has at least", 5L))
  expect_match(as.character(failed$message), "count check failed")
})


test_that("extract_node_name reads heading titles and chunk labels, else empty", {
  nodes = rule_eval_fixture_ast@blocks@content
  kinds = vapply(nodes, markermd:::q2r_node_kind, character(1))

  headings = nodes[kinds == "Heading"]
  expect_equal(markermd:::extract_node_name(headings[[1]]), "Question 1")
  expect_equal(markermd:::extract_node_name(headings[[2]]), "Summary")

  chunk = nodes[kinds == "Chunk"][[1]]
  expect_equal(markermd:::extract_node_name(chunk), "q1-plot")

  markdown = nodes[kinds == "Markdown"][[1]]
  expect_equal(markermd:::extract_node_name(markdown), "")
})
