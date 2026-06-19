# Regression tests for the zero-rule question path. A question with selected
# nodes but no rules is a valid template; it used to crash mark()'s validation
# card because create_rule_details() indexed an empty rules list.

vqr_ast = function() {
  q2r::parse_qmd(paste(c("## Q1 {#q1}", "", "Some prose.", "", "```{r}", "1", "```", ""), collapse = "\n"), quiet = TRUE)
}

test_that("validate_question_rules passes a question with selected nodes but no rules", {
  q = markermd_question(id = 1L, name = "Q1", selected_nodes = markermd_node_selection(node_ids = "q1"))
  res = validate_question_rules(vqr_ast(), q)

  expect_equal(res$status, "pass")
  expect_length(res$messages, 1L)
  expect_true(isTRUE(res$passed))
})

test_that("create_rule_details renders a zero-rule question without indexing the empty rules list", {
  q = markermd_question(id = 1L, name = "Q1", selected_nodes = markermd_node_selection(node_ids = "q1"))
  res = validate_question_rules(vqr_ast(), q)

  ui = create_rule_details(q, res)
  expect_s3_class(ui, "shiny.tag")
  expect_match(as.character(ui), "No rules defined", fixed = TRUE)
})

test_that("create_rule_details still renders one row per rule for a rule-bearing question", {
  rule = markermd_rule(node_type = "Chunk", verb = "has at least", values = 1)
  q = markermd_question(
    id = 1L, name = "Q1",
    selected_nodes = markermd_node_selection(node_ids = "q1"),
    rules = list(rule)
  )
  res = validate_question_rules(vqr_ast(), q)

  expect_length(res$messages, 1L)
  expect_length(res$passed, 1L)
  expect_s3_class(create_rule_details(q, res), "shiny.tag")
})
