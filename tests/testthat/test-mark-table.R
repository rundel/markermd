# Coverage for the Assignments table builders and cell renderers, including
# the tooltip escaping fix, the pass/fail/parse-error branches, the score
# columns folded in from the scores table, and the reactable they feed.

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

# The fixture's scores: repoA fully graded, repoB graded on Q1 only
mt_scores = function(fx) {
  markermd:::build_scores_table_data(
    data.frame(
      question_name = c("Q1", "Q1", "Q2"),
      assignment_repo = c("repoA", "repoB", "repoA"),
      score = c(8, 6, 4),
      stringsAsFactors = FALSE
    ),
    fx$question_names, fx$repo_list
  )
}

mt_build = function(fx, visible, scores_table = NULL, repo_to_github = list()) {
  markermd:::build_repo_table_data(
    visible = visible,
    visible_idx = match(visible, fx$repo_list),
    repo_list = fx$repo_list,
    collection = list(repo = c("repoA", "repoB")),
    artifact_paths = list(repoA = "a.html", repoB = NA, repoC = NA),
    repo_to_github = repo_to_github,
    validation_results = fx$validation_results,
    template_obj = mt_template(fx$question_names),
    repo_errors = fx$repo_errors,
    all_progress = fx$all_progress,
    graded_pairs = fx$graded_pairs,
    question_names = fx$question_names,
    scores_table = scores_table
  )
}

mt_render = function(widget) {
  as.character(htmltools::renderTags(htmltools::as.tags(widget))$html)
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
  # Only repoB and repoC visible: their button indices must stay 2 and 3
  df = mt_build(fx, c("repoB", "repoC"), repo_to_github = list(repoB = "org/repoB"))

  # Repository names are plain text; the row's buttons are keyed to the
  # repo's position in the full repo_list, not the visible subset. htmltools
  # escapes the onclick quotes structurally (&#39;), so the payloads are
  # matched in their escaped form
  expect_equal(nrow(df), 2)
  expect_equal(df$Repository, c("repoB", "repoC"))
  expect_match(df$Folder[2], "folder_clicked&#39;, 3,")
  expect_match(df$Validation_html[1], "validation_clicked&#39;, 2,")

  # repoB links to GitHub; repoC falls back to the greyed span
  expect_match(df$GitHub[1], "https://github.com/org/repoB")
  expect_match(df$GitHub[2], "opacity-25")

  # repoC has no document, so its Source cell is a greyed span, not a button
  expect_match(df$Source[1], "source_clicked&#39;, 2,")
  expect_match(df$Source[2], "No document found")

  # Validation and Progress sort on numbers: the passed fraction (a parse
  # error sorting below every result) and the graded count, with their
  # rendered cells alongside
  expect_equal(df$Validation, c(0.5, -1))
  expect_match(df$Validation_html[2], "triangle-exclamation")
  expect_equal(df$Progress, c(1L, 0L))
  expect_match(df$Progress_html[1], "1/2")

  # without a scores table there are no score columns
  expect_false(any(c("score_Q1", "score_Q2", "Total") %in% names(df)))
})

test_that("build_repo_table_data folds in the score columns and total by repository", {
  fx = mt_fixture()
  df = mt_build(fx, c("repoB", "repoA"), scores_table = mt_scores(fx))

  # the score columns are named by question id (score_column_id()), so a
  # question named "Total" or "Source" cannot collide with a fixed column
  expect_equal(df$score_Q1, c(6, 8))
  expect_equal(df$score_Q2, c(NA, 4))
  expect_equal(df$Total, c(NA, 12))
  expect_equal(df$Total_partial, c(6, 12))
  # the scores table's own graded count is not carried over (Progress has it)
  expect_false("Graded" %in% names(df))
  expect_equal(df$Progress, c(1L, 2L))
})

test_that("build_repo_table_data copes with no visible rows", {
  fx = mt_fixture()
  df = mt_build(fx, character(0), scores_table = mt_scores(fx))
  expect_equal(nrow(df), 0)
  expect_true(all(c("Repository", "Validation", "Progress", "score_Q1", "score_Q2", "Total") %in% names(df)))
})

test_that("validation_sort_key ranks parse errors below every result and passes highest", {
  fx = mt_fixture()
  key = function(repo) markermd:::validation_sort_key(repo, fx$validation_results, fx$repo_errors)
  expect_equal(key("repoA"), 1)
  expect_equal(key("repoB"), 0.5)
  expect_equal(key("repoC"), -1)
  expect_true(is.na(markermd:::validation_sort_key("repoA", list(), list())))
})

test_that("repo_table_reactable renders the rows, score headers, footer and click handler", {
  fx = mt_fixture()
  scores = mt_scores(fx)
  df = mt_build(fx, c("repoA", "repoB"), scores_table = scores)
  summary = markermd:::build_scores_summary_rows(scores, fx$question_names)
  totals = c(Q1 = 10, Q2 = 5)

  widget = markermd:::repo_table_reactable(df, summary, totals, click_input = "score_clicked")
  expect_s3_class(widget, "reactable")
  html = mt_render(widget)
  expect_match(html, "repoA")
  expect_match(html, "repoB")
  # the link cells and the progress bar render as HTML
  expect_match(html, "folder_clicked")
  expect_match(html, "progress-bar")
  # the points sit on their own muted line under the column name
  expect_match(html, "10 pts")
  expect_match(html, "5 pts")
  expect_match(html, "15 pts")
  expect_no_match(html, "Q1 (10 pts)", fixed = TRUE)
  # the footer carries the statistic labels and values, padded with a
  # character rather than an entity (reactable escapes footers)
  expect_match(html, "Median")
  expect_match(html, "12")
  expect_no_match(html, "&nbsp;", fixed = TRUE)
  # score cells are clickable and report to the click input
  expect_match(html, "score-link")
  expect_match(html, "score_clicked", fixed = TRUE)

  js = as.character(markermd:::repo_table_click_js("score_clicked", c("Q1", "Q2")))
  expect_match(js, '{"score_Q1":"Q1","score_Q2":"Q2"}', fixed = TRUE)
  expect_match(js, "priority: 'event'", fixed = TRUE)
  expect_no_match(js, "Repository'", fixed = TRUE)
})

test_that("repo_table_reactable has no score columns, footer or handler without questions", {
  fx = mt_fixture()
  df = mt_build(fx, c("repoA", "repoB"))
  widget = markermd:::repo_table_reactable(df, NULL, numeric(0), click_input = "score_clicked")
  html = mt_render(widget)
  expect_match(html, "repoA")
  expect_no_match(html, "score-link")
  expect_no_match(html, "setInputValue('score_clicked'", fixed = TRUE)
  expect_no_match(html, "Median")
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

test_that("validation_status_cell becomes a Validation-tab button when given its row", {
  tmpl = mt_template(c("Q1", "Q2"))
  vr = list(repoA = list(Q1 = list(status = "pass"), Q2 = list(status = "fail")))

  # htmltools writes the quotes in the onclick attribute as entities
  html = markermd:::validation_status_cell("repoA", vr, tmpl, row_index = 3L)
  expect_match(html, "<button", fixed = TRUE)
  expect_match(html, "Shiny.setInputValue(&#39;validation_clicked&#39;, 3, {priority: &#39;event&#39;})", fixed = TRUE)
  expect_match(html, "1/2")

  # the parse-error marker is a button too, so a broken repo is one click away
  err = markermd:::validation_status_cell(
    "repoA", list(), NULL, repo_errors = list(repoA = "boom"), row_index = 2L
  )
  expect_match(err, "validation_clicked&#39;, 2,", fixed = TRUE)
  expect_match(err, "triangle-exclamation")

  # without a row the bare marker is returned
  expect_no_match(markermd:::validation_status_cell("repoA", vr, tmpl), "<button", fixed = TRUE)
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

test_that("repo_table_reactable tags optional questions in their headers", {
  fx = mt_fixture()
  scores = mt_scores(fx)
  df = mt_build(fx, c("repoA", "repoB"), scores_table = scores)
  summary = markermd:::build_scores_summary_rows(scores, fx$question_names)
  totals = c(Q1 = 10, Q2 = 5)

  plain = mt_render(markermd:::repo_table_reactable(df, summary, totals))
  expect_no_match(plain, "markermd-optional-tag")

  tagged = mt_render(markermd:::repo_table_reactable(df, summary, totals, optional = "Q2"))
  expect_equal(lengths(regmatches(tagged, gregexpr("markermd-optional-tag", tagged))), 1)
  expect_match(tagged, "nothing selected still counts as graded")
})

test_that("optional_question_names reads the optional flag from the settings", {
  d = tempfile("optnames_"); dir.create(d)
  markermd:::save_grade_state(d, "Q1", markermd::markermd_grade_state(total_score = 10))
  markermd:::save_grade_state(d, "Doc", markermd::markermd_grade_state(total_score = 0, optional = TRUE))
  expect_equal(markermd:::optional_question_names(d, c("Q1", "Doc", "Q3")), "Doc")
  expect_equal(markermd:::optional_question_names(d, "Q1"), character(0))
})
