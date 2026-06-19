# Builds an initialized project whose database stores a two-question template
# (Q1, Q2) and a seeded rubric, with two student repo directories. Q1 is
# configured for negative-mode grading out of 10; Q2 has no settings row, so
# scoring falls back to the app default (positive mode out of 10).
make_export_project = function() {
  d = tempfile("markproj_")
  dir.create(file.path(d, "repos", "hw01-team01"), recursive = TRUE)
  dir.create(file.path(d, "repos", "hw01-team02"), recursive = TRUE)
  d = normalizePath(d, winslash = "/")

  template_path = file.path(d, "template.yaml")
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - id: 1",
    "    name: Q1",
    "  - id: 2",
    "    name: Q2"
  ), template_path)

  suppressMessages(init_project(d))
  suppressMessages(template_import(template_path, project = d))

  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "Issue A"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -1, "Issue B"))
  markermd:::save_rubric_item(d, "Q2", "item_2", markermd_rubric_item(1L, -3, "Q2 issue"))
  markermd:::save_grade_state(
    d, "Q1",
    markermd_grade_state(current_score = 0, total_score = 10, grading_mode = "negative")
  )
  d
}

read_scores_csv = function(d) {
  utils::read.csv(file.path(d, "scores.csv"), check.names = FALSE)
}


test_that("export_scores writes per-question and total scores, NA for ungraded", {
  d = make_export_project()
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)
  markermd:::save_comment(d, "Q2", "hw01-team01", "Well done.")

  path = suppressMessages(export_scores(d))
  expect_identical(as.character(path), file.path(d, "scores.csv"))

  scores = read_scores_csv(d)
  expect_identical(names(scores), c("repo", "Q1", "Q2", "total"))
  expect_identical(scores$repo, c("hw01-team01", "hw01-team02"))

  team01 = scores[scores$repo == "hw01-team01", ]
  expect_equal(team01$Q1, 8)
  expect_equal(team01$Q2, 0)
  expect_equal(team01$total, 8)

  team02 = scores[scores$repo == "hw01-team02", ]
  expect_true(is.na(team02$Q1))
  expect_true(is.na(team02$Q2))
  expect_true(is.na(team02$total))
})


test_that("export_scores clamps positive-mode deductions at zero", {
  d = make_export_project()
  markermd:::save_grade_selection(d, "Q2", "hw01-team01", "item_2", TRUE)

  suppressMessages(export_scores(d))

  scores = read_scores_csv(d)
  team01 = scores[scores$repo == "hw01-team01", ]
  expect_equal(team01$Q2, 0)
  expect_true(is.na(team01$Q1))
  expect_true(is.na(team01$total))
})


test_that("export_scores treats a deselected pair as ungraded", {
  d = make_export_project()
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", FALSE)
  markermd:::save_comment(d, "Q2", "hw01-team01", "Well done.")

  suppressMessages(export_scores(d))

  scores = read_scores_csv(d)
  expect_true(is.na(scores[scores$repo == "hw01-team01", "Q1"]))
})


test_that("export_scores aborts when nothing has been graded", {
  d = make_export_project()
  markermd:::save_private_comment(d, "Q1", "hw01-team01", "machine note")

  expect_error(export_scores(d), "No scores to export")
  expect_false(file.exists(file.path(d, "scores.csv")))
})


test_that("export_comments writes bulleted public feedback and skips repos without any", {
  d = make_export_project()
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_1", TRUE)
  markermd:::save_comment(d, "Q1", "hw01-team01", "Watch the axis labels.\nSecond line.")
  markermd:::save_comment(d, "Q2", "hw01-team01", "Well done.")
  markermd:::save_private_comment(d, "Q2", "hw01-team02", "never student facing")

  written = suppressMessages(export_comments(d))
  expect_identical(as.character(written), file.path(d, "comments", "hw01-team01.md"))
  expect_false(file.exists(file.path(d, "comments", "hw01-team02.md")))

  lines = readLines(file.path(d, "comments", "hw01-team01.md"))
  expect_identical(lines, c(
    "## Q1",
    "",
    "- Issue A",
    "- Issue B",
    "- Watch the axis labels.",
    "  Second line.",
    "",
    "## Q2",
    "",
    "- Well done."
  ))
  expect_false(any(grepl("never student facing", lines, fixed = TRUE)))
})


test_that("export_comments aborts when no repository has public feedback", {
  d = make_export_project()
  markermd:::save_private_comment(d, "Q1", "hw01-team01", "machine note")

  expect_error(export_comments(d), "No feedback to export")
  expect_false(file.exists(file.path(d, "comments", "hw01-team01.md")))
})


test_that("export_marks runs both exports and returns their paths", {
  d = make_export_project()
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_1", TRUE)

  result = suppressMessages(export_marks(d))

  expect_named(result, c("scores", "comments"))
  expect_identical(as.character(result$scores), file.path(d, "scores.csv"))
  expect_identical(as.character(result$comments), file.path(d, "comments", "hw01-team01.md"))

  scores = read_scores_csv(d)
  expect_equal(scores[scores$repo == "hw01-team01", "Q1"], 9)
  expect_identical(
    readLines(file.path(d, "comments", "hw01-team01.md")),
    c("## Q1", "", "- Issue B")
  )
})


test_that("export_scores clamps a negative-mode score at zero when deductions exceed the total", {
  d = make_export_project()
  # Q2 negative mode out of 2 with a -3 deduction selected: raw score -1.
  markermd:::save_grade_state(
    d, "Q2",
    markermd_grade_state(current_score = 2, total_score = 2, grading_mode = "negative",
                         bound_above_zero = TRUE, bound_below_max = TRUE)
  )
  markermd:::save_grade_selection(d, "Q2", "hw01-team01", "item_2", TRUE)

  suppressMessages(export_scores(d))
  scores = read_scores_csv(d)
  expect_equal(scores[scores$repo == "hw01-team01", "Q2"], 0)
})


test_that("export_scores leaves a negative-mode score below zero when bound_above_zero is off", {
  d = make_export_project()
  markermd:::save_grade_state(
    d, "Q2",
    markermd_grade_state(current_score = 2, total_score = 2, grading_mode = "negative",
                         bound_above_zero = FALSE, bound_below_max = FALSE)
  )
  markermd:::save_grade_selection(d, "Q2", "hw01-team01", "item_2", TRUE)

  suppressMessages(export_scores(d))
  scores = read_scores_csv(d)
  expect_equal(scores[scores$repo == "hw01-team01", "Q2"], -1)
})


test_that("export_scores clamps a positive-mode score at total_score when bound_below_max is on", {
  d = make_export_project()
  # A +20 item on Q2 (positive mode out of 10): raw score 20, clamped to 10.
  markermd:::save_rubric_item(d, "Q2", "item_bonus", markermd_rubric_item(2L, 20, "Bonus"))
  markermd:::save_grade_state(
    d, "Q2",
    markermd_grade_state(current_score = 0, total_score = 10, grading_mode = "positive",
                         bound_above_zero = TRUE, bound_below_max = TRUE)
  )
  markermd:::save_grade_selection(d, "Q2", "hw01-team01", "item_bonus", TRUE)

  suppressMessages(export_scores(d))
  scores = read_scores_csv(d)
  expect_equal(scores[scores$repo == "hw01-team01", "Q2"], 10)
})


test_that("export_scores leaves a positive-mode score above total_score when bound_below_max is off", {
  d = make_export_project()
  markermd:::save_rubric_item(d, "Q2", "item_bonus", markermd_rubric_item(2L, 20, "Bonus"))
  markermd:::save_grade_state(
    d, "Q2",
    markermd_grade_state(current_score = 0, total_score = 10, grading_mode = "positive",
                         bound_above_zero = TRUE, bound_below_max = FALSE)
  )
  markermd:::save_grade_selection(d, "Q2", "hw01-team01", "item_bonus", TRUE)

  suppressMessages(export_scores(d))
  scores = read_scores_csv(d)
  expect_equal(scores[scores$repo == "hw01-team01", "Q2"], 20)
})


test_that("export_scores aborts when a question name collides with a reserved column", {
  d = tempfile("markproj_")
  dir.create(file.path(d, "repos", "hw01-team01"), recursive = TRUE)
  d = normalizePath(d, winslash = "/")
  template_path = file.path(d, "template.yaml")
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - id: 1",
    "    name: total"
  ), template_path)
  suppressMessages(init_project(d))
  suppressMessages(template_import(template_path, project = d))
  markermd:::save_rubric_item(d, "total", "item_0", markermd_rubric_item(1L, 5, "ok"))
  markermd:::save_grade_selection(d, "total", "hw01-team01", "item_0", TRUE)

  expect_error(suppressMessages(export_scores(d)), "cannot be exported")
})
