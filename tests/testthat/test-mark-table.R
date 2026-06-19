# Coverage for the repository-overview cell renderers, including the tooltip
# escaping fix and the pass/fail/parse-error branches.

mt_template = function(names) {
  qs = lapply(seq_along(names), function(i) markermd_question(id = as.integer(i), name = names[i]))
  markermd_template(original_ast = q2r::parse_qmd("# x", quiet = TRUE), questions = qs)
}

test_that("validation_status_cell shows the pass count and a check when all pass", {
  tmpl = mt_template(c("Q1", "Q2"))
  vr = list(repoA = list(Q1 = list(status = "pass"), Q2 = list(status = "pass")))
  html = markermd:::validation_status_cell("repoA", vr, tmpl)
  expect_match(html, "2/2")
  expect_match(html, "text-success")
})

test_that("validation_status_cell escapes question names in the tooltip", {
  tmpl = mt_template(c('Q"1', "Q2"))
  vr = list(repoA = list(`Q"1` = list(status = "fail"), Q2 = list(status = "pass")))
  html = markermd:::validation_status_cell("repoA", vr, tmpl)
  expect_match(html, "1/2")
  expect_match(html, "Q&quot;1")          # quote escaped in the title attribute
  expect_false(grepl('Q"1"', html, fixed = TRUE))  # never the raw quote
})

test_that("validation_status_cell renders a warning marker for a parse error", {
  html = markermd:::validation_status_cell(
    "repoA", list(), NULL, repo_errors = list(repoA = "boom\nsecond line")
  )
  expect_match(html, "triangle-exclamation")
  expect_match(html, "boom")
  expect_false(grepl("second line", html, fixed = TRUE))  # only the first line
})

test_that("grading_progress_cell renders a progress bar with the graded/total count", {
  tmpl = mt_template(c("Q1", "Q2"))
  graded_pairs = data.frame(
    assignment_repo = "repoA", question_name = "Q1", stringsAsFactors = FALSE
  )
  html = markermd:::grading_progress_cell(
    "repoA", tmpl, list(repoA = 1L), graded_pairs, c("Q1", "Q2")
  )
  expect_match(html, "progress-bar")
  expect_match(html, "1/2")
})
