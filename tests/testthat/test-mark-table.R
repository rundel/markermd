# Coverage for the repository-overview table builders and cell renderers,
# including the tooltip escaping fix and the pass/fail/parse-error branches.

mt_template = function(names) {
  qs = lapply(seq_along(names), function(i) markermd_question(id = as.integer(i), name = names[i]))
  markermd_template(original_ast = q2r::parse_qmd("# x", quiet = TRUE), questions = qs)
}

# A three-repo fixture for the filter/build tests: repoB has a validation
# failure, repoC has a parse error, and only repoA is fully graded.
mt_fixture = function() {
  list(
    repo_list = c("repoA", "repoB", "repoC"),
    validation_results = list(
      repoA = list(Q1 = list(status = "pass"), Q2 = list(status = "pass")),
      repoB = list(Q1 = list(status = "fail"), Q2 = list(status = "pass"))
    ),
    repo_errors = list(repoC = "could not parse"),
    all_progress = c(repoA = 2L, repoB = 1L, repoC = 0L),
    question_names = c("Q1", "Q2"),
    graded_pairs = data.frame(
      assignment_repo = c("repoA", "repoA", "repoB"),
      question_name = c("Q1", "Q2", "Q1"),
      stringsAsFactors = FALSE
    )
  )
}

test_that("filter_repo_table_rows applies the name filter case-insensitively", {
  fx = mt_fixture()
  visible = markermd:::filter_repo_table_rows(
    fx$repo_list, name_filter = " REPOB ", status_filter = "all",
    validation_results = fx$validation_results, repo_errors = fx$repo_errors,
    all_progress = fx$all_progress, question_names = fx$question_names
  )
  expect_equal(visible, "repoB")
})

test_that("filter_repo_table_rows counts validation failures and parse errors as failed", {
  fx = mt_fixture()
  visible = markermd:::filter_repo_table_rows(
    fx$repo_list, name_filter = NULL, status_filter = "failed",
    validation_results = fx$validation_results, repo_errors = fx$repo_errors,
    all_progress = fx$all_progress, question_names = fx$question_names
  )
  expect_setequal(visible, c("repoB", "repoC"))
})

test_that("filter_repo_table_rows splits graded from ungraded by progress", {
  fx = mt_fixture()
  graded = markermd:::filter_repo_table_rows(
    fx$repo_list, name_filter = NULL, status_filter = "graded",
    validation_results = fx$validation_results, repo_errors = fx$repo_errors,
    all_progress = fx$all_progress, question_names = fx$question_names
  )
  expect_equal(graded, "repoA")
  ungraded = markermd:::filter_repo_table_rows(
    fx$repo_list, name_filter = NULL, status_filter = "ungraded",
    validation_results = fx$validation_results, repo_errors = fx$repo_errors,
    all_progress = fx$all_progress, question_names = fx$question_names
  )
  expect_setequal(ungraded, c("repoB", "repoC"))
})

test_that("filter_repo_table_rows status filters no-op without progress data", {
  fx = mt_fixture()
  for (status in c("graded", "ungraded", "all")) {
    visible = markermd:::filter_repo_table_rows(
      fx$repo_list, name_filter = NULL, status_filter = status,
      validation_results = fx$validation_results, repo_errors = fx$repo_errors,
      all_progress = NULL, question_names = NULL
    )
    expect_equal(visible, fx$repo_list)
  }
})

test_that("build_repo_table_data keys buttons to the full repo_list positions", {
  fx = mt_fixture()
  tmpl = mt_template(fx$question_names)
  # Only repoB and repoC visible: their button indices must stay 2 and 3
  visible = c("repoB", "repoC")
  df = markermd:::build_repo_table_data(
    visible = visible,
    visible_idx = match(visible, fx$repo_list),
    repo_list = fx$repo_list,
    collection = list(repo = c("repoA", "repoB")),
    artifact_paths = list(repoA = "a.html", repoB = NA, repoC = NA),
    repo_to_github = list(repoB = "org/repoB"),
    validation_results = fx$validation_results,
    template_obj = tmpl,
    repo_errors = fx$repo_errors,
    all_progress = fx$all_progress,
    graded_pairs = fx$graded_pairs,
    question_names = fx$question_names,
    active_row = 3L
  )

  # htmltools escapes the onclick quotes structurally (&#39;), so the
  # payloads are matched in their escaped form
  expect_equal(nrow(df), 2)
  expect_match(df$Repository[1], "repo_select_clicked&#39;, 2,")
  expect_match(df$Repository[2], "repo_select_clicked&#39;, 3,")
  expect_match(df$Repository[1], 'data-row="2"')
  expect_match(df$Repository[2], 'data-row="3"')
  expect_match(df$Folder[2], "folder_clicked&#39;, 3,")

  # The active row (repoC, position 3) carries the baked-in active class
  expect_match(df$Repository[2], "repo-select-btn active")
  expect_match(df$Repository[1], "repo-select-btn\"")

  # repoB links to GitHub; repoC falls back to the greyed span
  expect_match(df$GitHub[1], "https://github.com/org/repoB")
  expect_match(df$GitHub[2], "opacity-25")

  # repoC has no document, so its Source cell is a greyed span, not a button
  expect_match(df$Source[1], "source_clicked&#39;, 2,")
  expect_match(df$Source[2], "No document found")
})

test_that("repo_table_gt renders both repos' cells to raw html", {
  fx = mt_fixture()
  tmpl = mt_template(fx$question_names)
  visible = c("repoA", "repoB")
  df = markermd:::build_repo_table_data(
    visible = visible,
    visible_idx = match(visible, fx$repo_list),
    repo_list = fx$repo_list,
    collection = list(repo = c("repoA", "repoB")),
    artifact_paths = list(repoA = "a.html", repoB = NA, repoC = NA),
    repo_to_github = list(),
    validation_results = fx$validation_results,
    template_obj = tmpl,
    repo_errors = fx$repo_errors,
    all_progress = fx$all_progress,
    graded_pairs = fx$graded_pairs,
    question_names = fx$question_names,
    active_row = 1L
  )
  html = gt::as_raw_html(markermd:::repo_table_gt(df), inline_css = FALSE)
  expect_match(html, "repoA")
  expect_match(html, "repoB")
  expect_match(html, "progress-bar")
})

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
