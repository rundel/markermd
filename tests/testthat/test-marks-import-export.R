# Builds an initialized project whose database stores a two-question template
# (Q1, Q2) and a seeded rubric, with two student repo directories.
make_marks_project = function() {
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
  d
}

# Writes a single-pair marks YAML; items is a character vector of description
# lines (length 0 writes items: []).
write_marks_fixture_yaml = function(repo, question, items, comment = NULL, private_comment = NULL) {
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'",
    "repos:",
    sprintf("- name: %s", repo),
    "  questions:",
    sprintf("  - name: %s", question),
    if (length(items) == 0) "    items: []" else c("    items:", sprintf("    - %s", items)),
    if (!is.null(comment)) sprintf("    comment: %s", comment),
    if (!is.null(private_comment)) sprintf("    private_comment: %s", private_comment)
  ), path)
  path
}

count_rows = function(d, table) {
  markermd:::with_database(d, function(conn) {
    DBI::dbGetQuery(conn, glue::glue("SELECT COUNT(*) AS n FROM {table}"))$n
  })
}


test_that("marks_import declaratively selects listed items and deselects the rest", {
  d = make_marks_project()
  path = write_marks_fixture_yaml("hw01-team01", "Q1", "Issue B", private_comment = "machine note")

  result = suppressMessages(marks_import(path, project = d))

  expect_equal(result$action, "written")
  expect_equal(result$n_selected, 1L)

  sel = markermd:::load_grade_selections(d, "Q1", "hw01-team01")
  expect_false(sel$item_0)
  expect_true(sel$item_1)

  expect_equal(markermd:::load_private_comment(d, "Q1", "hw01-team01"), "machine note")
  expect_null(markermd:::load_comment(d, "Q1", "hw01-team01"))
})


test_that("marks_import skips pairs with existing activity unless overwrite", {
  d = make_marks_project()
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)

  path = write_marks_fixture_yaml("hw01-team01", "Q1", "Issue B")
  result = suppressMessages(marks_import(path, project = d))

  expect_equal(result$action, "skipped")
  sel = markermd:::load_grade_selections(d, "Q1", "hw01-team01")
  expect_true(sel$item_0)
  expect_null(sel$item_1)

  result = suppressMessages(marks_import(path, project = d, overwrite = TRUE))
  expect_equal(result$action, "written")

  sel = markermd:::load_grade_selections(d, "Q1", "hw01-team01")
  expect_false(sel$item_0)
  expect_true(sel$item_1)
})


test_that("a private comment alone protects a pair from re-import", {
  d = make_marks_project()
  markermd:::save_private_comment(d, "Q2", "hw01-team01", "looked at this")

  path = write_marks_fixture_yaml("hw01-team01", "Q2", "Q2 issue")
  result = suppressMessages(marks_import(path, project = d))
  expect_equal(result$action, "skipped")
})


test_that("items: [] marks the pair without grading it", {
  d = make_marks_project()
  path = write_marks_fixture_yaml("hw01-team01", "Q1", character(0), private_comment = "no deductions")

  suppressMessages(marks_import(path, project = d))

  sel = markermd:::load_grade_selections(d, "Q1", "hw01-team01")
  expect_false(sel$item_0)
  expect_false(sel$item_1)

  marked = markermd:::marked_question_pairs(d)
  expect_true(any(marked$question_name == "Q1" & marked$assignment_repo == "hw01-team01"))

  graded = markermd:::graded_question_pairs(d)
  expect_false(any(graded$question_name == "Q1" & graded$assignment_repo == "hw01-team01"))
})


test_that("marks_import validates everything before writing anything", {
  d = make_marks_project()

  expect_error(
    marks_import(write_marks_fixture_yaml("nonexistent-repo", "Q1", "Issue A"), project = d),
    "not in this project's repos directory"
  )
  expect_error(
    marks_import(write_marks_fixture_yaml("hw01-team01", "Q9", "Issue A"), project = d),
    "not in this project's template"
  )
  expect_error(
    marks_import(write_marks_fixture_yaml("hw01-team01", "Q1", "Issue C"), project = d),
    "not in the question's rubric"
  )

  # A bad pair later in the file aborts the valid pair before it too
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'",
    "repos:",
    "- name: hw01-team01",
    "  questions:",
    "  - name: Q1",
    "    items:",
    "    - Issue A",
    "- name: hw01-team02",
    "  questions:",
    "  - name: Q1",
    "    items:",
    "    - Not a real item"
  ), path)
  expect_error(marks_import(path, project = d), "not in the question's rubric")

  expect_equal(count_rows(d, "grades"), 0)
  expect_equal(count_rows(d, "comments"), 0)
  expect_equal(count_rows(d, "private_comments"), 0)
})


test_that("marks_import rejects ambiguous duplicate descriptions in the rubric", {
  d = make_marks_project()
  markermd:::save_rubric_item(d, "Q2", "item_3", markermd_rubric_item(2L, -1, "Q2 issue"))

  expect_error(
    marks_import(write_marks_fixture_yaml("hw01-team01", "Q2", "Q2 issue"), project = d),
    "duplicate descriptions"
  )
  expect_equal(count_rows(d, "grades"), 0)
})


test_that("marks_import repo and question arguments subset the file", {
  d = make_marks_project()
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'",
    "repos:",
    "- name: hw01-team01",
    "  questions:",
    "  - name: Q1",
    "    items:",
    "    - Issue A",
    "  - name: Q2",
    "    items:",
    "    - Q2 issue",
    "- name: hw01-team02",
    "  questions:",
    "  - name: Q1",
    "    items: []"
  ), path)

  result = suppressMessages(marks_import(path, project = d, repo = "hw01-team01", question = "Q1"))
  expect_equal(nrow(result), 1L)
  expect_equal(result$repo, "hw01-team01")
  expect_equal(result$question, "Q1")
  expect_length(markermd:::load_grade_selections(d, "Q2", "hw01-team01"), 0)
  expect_length(markermd:::load_grade_selections(d, "Q1", "hw01-team02"), 0)

  expect_error(marks_import(path, project = d, repo = "hw01-team09"), "not found in the marks file")
  expect_error(marks_import(path, project = d, question = "Q9"), "not found in the marks file")
})


test_that("comments land in their own channels with the current username", {
  d = make_marks_project()
  path = write_marks_fixture_yaml(
    "hw01-team01", "Q1", "Issue A",
    comment = "shared feedback", private_comment = "internal note"
  )
  suppressMessages(marks_import(path, project = d))

  expect_equal(markermd:::load_comment(d, "Q1", "hw01-team01"), "shared feedback")
  expect_equal(markermd:::load_private_comment(d, "Q1", "hw01-team01"), "internal note")

  usernames = markermd:::with_database(d, function(conn) {
    DBI::dbGetQuery(conn, "SELECT username FROM private_comments")$username
  })
  expect_equal(usernames, markermd:::get_current_username())
})


test_that("marks export -> import round trips", {
  d = make_marks_project()
  suppressMessages(marks_import(
    write_marks_fixture_yaml("hw01-team01", "Q1", "Issue B", private_comment = "note"),
    project = d
  ))
  suppressMessages(marks_import(
    write_marks_fixture_yaml("hw01-team02", "Q2", character(0), comment = "all good"),
    project = d
  ))

  out = file.path(d, "marks.yaml")
  expect_identical(marks_export(out, project = d), out)

  marks = read_marks_yaml(out)
  expect_equal(vapply(marks$repos, function(r) r$name, character(1)), c("hw01-team01", "hw01-team02"))
  expect_equal(marks$repos[[1]]$questions[[1]]$items, "Issue B")
  expect_equal(marks$repos[[1]]$questions[[1]]$private_comment, "note")
  expect_identical(marks$repos[[2]]$questions[[1]]$items, character(0))
  expect_equal(marks$repos[[2]]$questions[[1]]$comment, "all good")

  result = suppressMessages(marks_import(out, project = d, overwrite = TRUE))
  expect_equal(result$action, c("written", "written"))

  sel = markermd:::load_grade_selections(d, "Q1", "hw01-team01")
  expect_false(sel$item_0)
  expect_true(sel$item_1)
})


test_that("marks_export validates filters and errors with nothing to export", {
  d = make_marks_project()

  expect_error(marks_export(file.path(d, "x.yaml"), project = d), "No marks to export")

  suppressMessages(marks_import(
    write_marks_fixture_yaml("hw01-team01", "Q1", "Issue A"),
    project = d
  ))
  expect_error(
    marks_export(file.path(d, "x.yaml"), project = d, repo = "hw01-team09"),
    "No marks recorded for"
  )
  expect_error(
    marks_export(file.path(d, "x.yaml"), project = d, question = "Q2"),
    "No marks recorded for"
  )
})


test_that("marks_set marks a pair and errors on existing activity without overwrite", {
  d = make_marks_project()

  result = suppressMessages(
    marks_set("hw01-team01", "Q1", items = "Issue A", private_comment = "note", project = d)
  )
  expect_equal(result$action, "written")
  expect_equal(result$n_selected, 1L)

  sel = markermd:::load_grade_selections(d, "Q1", "hw01-team01")
  expect_true(sel$item_0)
  expect_false(sel$item_1)

  expect_error(
    marks_set("hw01-team01", "Q1", items = "Issue B", project = d),
    "already has grading activity"
  )

  suppressMessages(marks_set("hw01-team01", "Q1", items = "Issue B", project = d, overwrite = TRUE))
  sel = markermd:::load_grade_selections(d, "Q1", "hw01-team01")
  expect_false(sel$item_0)
  expect_true(sel$item_1)
})


test_that("marks_set with items = NULL writes comments without touching selections", {
  d = make_marks_project()

  suppressMessages(marks_set("hw01-team01", "Q1", private_comment = "flagged for review", project = d))

  expect_length(markermd:::load_grade_selections(d, "Q1", "hw01-team01"), 0)
  expect_equal(markermd:::load_private_comment(d, "Q1", "hw01-team01"), "flagged for review")

  expect_error(marks_set("hw01-team01", "Q1", project = d), "Supply at least one")
  expect_error(
    marks_set("hw01-team01", "Q1", items = "Not real", project = d, overwrite = TRUE),
    "not in the question's rubric"
  )
})
