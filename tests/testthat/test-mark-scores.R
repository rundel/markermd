# Coverage for the Scores tab builders (R/utils_mark_scores.R): the wide
# per-repository table, the summary rows pinned beneath it, the caption, and
# the interactive reactable they feed.

ms_scores = function() {
  data.frame(
    question_name = c("Q1", "Q1", "Q1", "Q2", "Q2", "Q2"),
    assignment_repo = rep(c("repoA", "repoB", "repoC"), 2),
    score = c(8, 10, NA, 4.5, NA, NA),
    stringsAsFactors = FALSE
  )
}

ms_render = function(widget) {
  as.character(htmltools::renderTags(htmltools::as.tags(widget))$html)
}

test_that("build_scores_table_data lays out one column per question with totals over graded questions", {
  tbl = build_scores_table_data(ms_scores(), c("Q1", "Q2"), c("repoA", "repoB", "repoC"))
  expect_equal(names(tbl), c("Repository", "Q1", "Q2", "Total", "Graded"))
  expect_equal(tbl$Q1, c(8, 10, NA))
  expect_equal(tbl$Q2, c(4.5, NA, NA))
  # a partially graded repo totals what is graded; an ungraded one has no total
  expect_equal(tbl$Total, c(12.5, 10, NA))
  expect_equal(tbl$Graded, c(2L, 1L, 0L))
})

test_that("build_scores_table_data keeps repository order and copes with no scores", {
  tbl = build_scores_table_data(ms_scores()[0, ], "Q1", c("repoB", "repoA"))
  expect_equal(tbl$Repository, c("repoB", "repoA"))
  expect_true(all(is.na(tbl$Q1)))
  expect_true(all(is.na(tbl$Total)))
  expect_equal(tbl$Graded, c(0L, 0L))
})

test_that("build_scores_summary_rows summarises graded scores per question in the table's layout", {
  tbl = build_scores_table_data(ms_scores(), c("Q1", "Q2"), c("repoA", "repoB", "repoC"))
  s = build_scores_summary_rows(tbl, c("Q1", "Q2"))
  expect_equal(names(s), names(tbl))
  expect_equal(s$Repository, c("Graded", "Mean", "Median", "SD"))
  expect_equal(s$Q1, c(2, 9, 9, stats::sd(c(8, 10))))
  # one graded score has a mean and median but no standard deviation
  expect_equal(s$Q2, c(1, 4.5, 4.5, NA))
  # the Total column covers only the repository with every question graded
  expect_equal(s$Total, c(1, 12.5, 12.5, NA))
  expect_true(all(is.na(s$Graded)))
})

test_that("build_scores_summary_rows handles nothing graded and no questions", {
  tbl = build_scores_table_data(ms_scores()[0, ], "Q1", c("repoA", "repoB"))
  s = build_scores_summary_rows(tbl, "Q1")
  expect_equal(s$Q1, c(0, NA, NA, NA))
  expect_equal(s$Total, c(0, NA, NA, NA))

  none = build_scores_summary_rows(build_scores_table_data(ms_scores()[0, ], character(0), "repoA"), character(0))
  expect_equal(names(none), c("Repository", "Total", "Graded"))
  expect_equal(none$Total, c(0, NA, NA, NA))
})

test_that("scores_caption reports how many repositories the Total statistics cover", {
  tbl = build_scores_table_data(ms_scores(), c("Q1", "Q2"), c("repoA", "repoB", "repoC"))
  s = build_scores_summary_rows(tbl, c("Q1", "Q2"))
  expect_match(
    as.character(scores_caption(s, 3)),
    "the Total column uses the 1 of 3 repositories with every question graded",
    fixed = TRUE
  )
})

test_that("format_score and format_points render compactly", {
  expect_equal(format_score(c(10, 7.5, 9.333333, NA)), c("10", "7.5", "9.33", ""))
  expect_equal(format_points(1), "1 pt")
  expect_equal(format_points(10), "10 pts")
  expect_equal(format_points(7.5), "7.5 pts")
})

test_that("score_tint leaves full marks and ungraded cells alone and deepens with the shortfall", {
  expect_null(score_tint(10, 10))
  expect_null(score_tint(12, 10))
  expect_null(score_tint(NA, 10))
  expect_null(score_tint(5, NA))
  expect_null(score_tint(5, 0))

  half = score_tint(5, 10)
  zero = score_tint(0, 10)
  expect_match(half$background, "^rgba\\(220, 53, 69, 0\\.11\\)$")
  expect_match(zero$background, "^rgba\\(220, 53, 69, 0\\.22\\)$")
  # scores below zero are clamped to the deepest tint
  expect_equal(score_tint(-3, 10), zero)
})

test_that("graded_progress_cell mirrors the Validation tab's bar with the count overlaid", {
  html = as.character(graded_progress_cell(3L, 4L))
  expect_match(html, "progress-bar bg-success")
  expect_match(html, "width: 75%;", fixed = TRUE)
  expect_match(html, ">3/4<", fixed = TRUE)
  expect_match(html, "3 of 4 questions graded", fixed = TRUE)

  expect_match(as.character(graded_progress_cell(0L, 0L)), "width: 0%;", fixed = TRUE)
})

test_that("scores_reactable renders the table with two-line headers and footer rows", {
  tbl = build_scores_table_data(ms_scores(), c("Q1", "Q2"), c("repoA", "repoB", "repoC"))
  totals = c(Q1 = 10, Q2 = 5)
  widget = scores_reactable(tbl, build_scores_summary_rows(tbl, c("Q1", "Q2")), totals)
  expect_s3_class(widget, "reactable")

  html = ms_render(widget)
  # the points sit on their own muted line under the column name
  expect_match(html, "10 pts")
  expect_match(html, "5 pts")
  expect_match(html, "15 pts")
  expect_no_match(html, "Q1 (10 pts)", fixed = TRUE)
  expect_match(html, "repoC")
  # the footer carries the statistic labels and values, and blank lines are
  # padded with a character rather than an entity (reactable escapes footers)
  expect_match(html, "Median")
  expect_match(html, "12.5")
  expect_no_match(html, "&nbsp;", fixed = TRUE)
  # the graded count renders as the progress bar
  expect_match(html, "progress-bar bg-success")
  # without a click input the cells carry no link styling or handler
  expect_no_match(html, "score-link")
  expect_no_match(html, "setInputValue")
})

test_that("scores_reactable wires question cells and repository names to the click input", {
  tbl = build_scores_table_data(ms_scores(), c("Q1", "Q2"), c("repoA", "repoB", "repoC"))
  totals = c(Q1 = 10, Q2 = 5)
  widget = scores_reactable(
    tbl, build_scores_summary_rows(tbl, c("Q1", "Q2")), totals,
    click_input = "scores-score_clicked"
  )
  html = ms_render(widget)
  expect_match(html, "score-link")
  expect_match(html, "scores-score_clicked", fixed = TRUE)

  js = as.character(scores_click_js("scores-score_clicked", c("Q1", "Q2")))
  expect_match(js, '["Q1","Q2"]', fixed = TRUE)
  expect_match(js, "column.id === 'Repository'", fixed = TRUE)
  expect_match(js, "priority: 'event'", fixed = TRUE)
})
