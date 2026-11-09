# Rule patterns that do not compile, question names that are blank once
# trimmed, document-scope filters in the template tree, and template()'s
# handling of a mistyped local path.

rp_ast = function() {
  q2r::parse_qmd(paste(c(
    "## Q {#q}", "", "```{r}", "library(ggplot2)", "```", ""
  ), collapse = "\n"), quiet = TRUE)
}

rp_question = function(rule) {
  markermd_question(
    id = 1L, name = "Q",
    selected_nodes = markermd_node_selection(node_ids = "q"),
    rules = list(rule)
  )
}

test_that("an invalid pattern fails both content verbs with an explicit message", {
  # "lacks content" used to pass vacuously because the pattern could not be
  # tried; a rule the grader typed wrong must not pass every submission
  lacks = markermd_rule(node_type = "Chunk", verb = "lacks content", values = "library(")
  res = validate_question_rules(rp_ast(), rp_question(lacks))
  expect_equal(res$status, "fail")
  expect_match(res$messages, "invalid regular expression")

  has = markermd_rule(node_type = "Chunk", verb = "has content", values = "library(")
  expect_match(validate_question_rules(rp_ast(), rp_question(has))$messages, "invalid regular expression")

  name = markermd_rule(node_type = "Heading", verb = "has name", values = "Q(")
  expect_match(validate_question_rules(rp_ast(), rp_question(name))$messages, "invalid regular expression")
})

test_that("evaluate_question_rules is the one implementation behind validate_question_rules", {
  rule = markermd_rule(node_type = "Chunk", verb = "has at least", values = 1L)
  q = rp_question(rule)
  expect_equal(
    vapply(markermd:::evaluate_question_rules(rp_ast(), q), function(r) r$passed, logical(1)),
    validate_question_rules(rp_ast(), q)$passed
  )

  # a sections question with rules and no nodes targets nothing
  nothing = markermd_question(1L, "Q", rules = list(rule))
  res = markermd:::evaluate_question_rules(rp_ast(), nothing)
  expect_false(res[[1]]$passed)
  expect_equal(res[[1]]$message, "No nodes selected")
  expect_equal(validate_question_rules(rp_ast(), nothing)$status, "fail")
  expect_match(validate_question_rules(rp_ast(), nothing)$details, "\\(none\\)")
})

test_that("question names must have content once trimmed and are unique once trimmed", {
  expect_error(markermd_question(id = 1L, name = NA_character_), "cannot be empty")
  expect_error(markermd_question(id = 1L, name = "   "), "cannot be empty")
  ast = q2r::parse_qmd("# x", quiet = TRUE)
  padded = list(
    markermd_question(id = 1L, name = "Q1"),
    markermd_question(id = 2L, name = "Q1 ")
  )
  expect_error(markermd_template(original_ast = ast, questions = padded), "names must be unique")
})

test_that("a document question's filters color the excluded nodes in the template tree", {
  # question_filtered_indices() feeds the tree's red "excluded" markers; a
  # document-scope question selects no nodes but its filters still apply to
  # the whole document in grading, so the tree must show the same exclusions
  ast = q2r::parse_qmd(paste(c(
    "# A", "", "text", "", "```{r}", "1", "```", "", "# B", "", "```{r}", "2", "```", ""
  ), collapse = "\n"), quiet = TRUE)
  filtered = markermd_question(
    1L, "Document",
    filters = list(markermd_filter_group(
      conditions = list(markermd:::new_markermd_filter_condition(type = "node type", value = "Chunk", negate = TRUE))
    )),
    scope = "document"
  )
  dropped = markermd:::question_filtered_indices(ast, filtered)
  expect_length(dropped, 2)
  records = markermd:::q2r_flatten(ast)
  kinds = vapply(dropped, function(i) markermd:::q2r_node_kind(records[[i]]$node), character(1))
  expect_true(all(kinds == "Chunk"))

  plain = markermd_question(1L, "Document", scope = "document")
  expect_length(markermd:::question_filtered_indices(ast, plain), 0)
})

test_that("a mistyped local path is reported as not found, not as a GitHub repository", {
  expect_error(template("hw1/assignment.qmd"), "not found locally")
  expect_error(template("hw1/assignment.qmd", local_dir = tempdir()), "not found locally")
  expect_error(template("owner/notes.Rmd"), "not found locally")
})
