# Content / name rules match their pattern as a regular expression (the rule UI
# labels these fields "regex"); they previously ran it through glob2rx().

rr_ast = function() {
  q2r::parse_qmd(paste(c(
    "## Q {#q}", "", "```{r}", "library(ggplot2)", "```", ""
  ), collapse = "\n"), quiet = TRUE)
}

rr_question = function(rule) {
  markermd_question(
    id = 1L, name = "Q",
    selected_nodes = markermd_node_selection(node_ids = "q"),
    rules = list(rule)
  )
}

test_that("a 'has content' rule matches its value as an unanchored regex", {
  # "library" must match "library(ggplot2)" (it would not under glob2rx anchoring)
  rule = markermd_rule(node_type = "Chunk", verb = "has content", values = "library")
  res = validate_question_rules(rr_ast(), rr_question(rule))
  expect_equal(res$status, "pass")

  rule2 = markermd_rule(node_type = "Chunk", verb = "has content", values = "gg.*plot")
  expect_equal(validate_question_rules(rr_ast(), rr_question(rule2))$status, "pass")
})

test_that("a 'lacks content' rule fails when the regex matches", {
  rule = markermd_rule(node_type = "Chunk", verb = "lacks content", values = "library")
  expect_equal(validate_question_rules(rr_ast(), rr_question(rule))$status, "fail")
})

test_that("an uncompilable pattern matches nothing instead of erroring mid-grading", {
  rule = markermd_rule(node_type = "Chunk", verb = "has content", values = "library(")
  res = validate_question_rules(rr_ast(), rr_question(rule))
  expect_equal(res$status, "fail")  # no match, no error
})
