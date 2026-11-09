# Regression tests for the issues found in the 2026-11 package review (see
# notes/review.md): restores that work by item id, descriptions as identity,
# question renames, the document each repository is graded on, and the
# smaller helpers the fixes introduced.

# --- Snapshot restore ------------------------------------------------------------

test_that("a restore narrowed to one repository leaves every other repository as it is", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "Before"))
  suppressMessages({
    marks_set("hw01-team01", "Q1", items = "Major", project = d, overwrite = TRUE)
    marks_set("hw01-team02", "Q1", items = "Issue A", comment = "Changed", project = d, overwrite = TRUE)
  })
  before = snapshot_scores(project = d)

  suppressMessages(snapshot_restore("Before", project = d, repo = "hw01-team01"))

  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), c("Issue A", "Minor"))
  # team02 keeps the grading it had just before the restore
  expect_equal(sn_selected(d, "Q1", "hw01-team02"), "Issue A")
  expect_equal(markermd:::load_comment(d, "Q1", "hw01-team02"), "Changed")
  after = snapshot_scores(project = d)
  team02 = function(x) x$score[x$assignment_repo == "hw01-team02" & x$question_name == "Q1"]
  expect_equal(team02(after), team02(before))
  # the items kept their ids, so no repository's selections were lost
  expect_named(markermd:::load_rubric_items(d, "Q1"), c("item_0", "item_1", "item_2", "item_3"))
})

test_that("a full restore clears the pairs graded after the snapshot, comments included", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "Before"))
  suppressMessages(marks_set("hw01-team02", "Q2", items = "Half credit", comment = "Later comment", project = d))
  expect_equal(snapshot_scores(project = d)$score[
    snapshot_scores(project = d)$assignment_repo == "hw01-team02" & snapshot_scores(project = d)$question_name == "Q2"
  ], 5)

  suppressMessages(snapshot_restore("Before", project = d))

  expect_equal(sn_selected(d, "Q2", "hw01-team02"), character(0))
  expect_equal(markermd:::load_comment(d, "Q2", "hw01-team02"), "")
  scores = snapshot_scores(project = d)
  expect_true(is.na(scores$score[scores$assignment_repo == "hw01-team02" & scores$question_name == "Q2"]))
  expect_equal(scores, snapshot_scores("Before", project = d), ignore_attr = TRUE)
})

test_that("restoring the rubric alone keeps every selection of the items the snapshot has", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "Before"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -5, "Issue A (reworded)"))

  expect_warning(suppressMessages(snapshot_restore("Before", project = d, part = "rubric")), "out of step")

  expect_equal(markermd:::load_rubric_items(d, "Q1")$item_1@description, "Issue A")
  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), c("Issue A", "Minor"))
  expect_equal(sn_selected(d, "Q1", "hw01-team02"), "Looks good")
})

test_that("snapshots of a rubric with blank or duplicate descriptions restore and score by id", {
  d = sn_graded_project()
  # The app refuses these now, but a database may still hold them
  markermd:::save_rubric_item(d, "Q2", "item_6", markermd_rubric_item(3L, 0, ""))
  markermd:::save_rubric_item(d, "Q2", "item_7", markermd_rubric_item(4L, 7, "Full credit"))
  markermd:::save_grade_selection(d, "Q2", "hw01-team02", "item_7", TRUE)

  id = suppressMessages(snapshot_create(d, label = "Dup"))
  live = collect_score_data(d, "Q2", "hw01-team02")$score
  cached = snapshot_scores(id, project = d)
  expect_equal(cached$score[cached$assignment_repo == "hw01-team02" & cached$question_name == "Q2"], live)
  expect_equal(live, 7)

  # The description-keyed API refuses the duplicate, but the app's toggles
  # and the restore work by id
  expect_error(suppressMessages(marks_set("hw01-team02", "Q2", items = character(0), project = d, overwrite = TRUE)), "duplicate")
  markermd:::save_grade_selection(d, "Q2", "hw01-team02", "item_7", FALSE)
  expect_true(is.na(collect_score_data(d, "Q2", "hw01-team02")$score))
  expect_no_error(suppressMessages(snapshot_restore("Dup", project = d)))
  sel = markermd:::load_grade_selections(d, "Q2", "hw01-team02")
  expect_true(isTRUE(sel$item_7))
})

test_that("snapshot marks carry item ids that the public exports drop", {
  d = sn_graded_project()
  payload = markermd:::with_database(d, markermd:::snapshot_payload)
  q = payload$marks$repos[[1]]$questions[[1]]
  expect_true(!is.null(q$item_ids))
  expect_length(q$item_ids, length(q$items))
  expect_null(markermd:::marks_without_ids(payload$marks)$repos[[1]]$questions[[1]]$item_ids)

  id = suppressMessages(snapshot_create(d))
  dir = file.path(d, "out")
  suppressMessages(snapshot_export(id, dir, project = d))
  expect_no_match(paste(readLines(file.path(dir, "markermd-marks.yaml")), collapse = "\n"), "item_ids")
  out = tempfile(fileext = ".yaml")
  marks_export(out, project = d, snapshot = id)
  expect_no_match(paste(readLines(out), collapse = "\n"), "item_ids")
  # the content hash ignores the ids, as it does the rubric's
  expect_true(snapshot_scores(project = d) |> nrow() > 0)
})

test_that("with_action keeps the code's visibility", {
  d = sn_project()
  path = file.path(d, "template.yaml")
  expect_false(withVisible(suppressMessages(template_import(path, project = d)))$visible)
})

# --- Question renames -------------------------------------------------------------

test_that("renaming a question in the template carries its rubric and marks over", {
  d = sn_graded_project()
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:", "  path: hw1.qmd", "questions:",
    "  - id: 1", "    name: Question One", "  - id: 2", "    name: Q2"
  ), file.path(d, "renamed.yaml"))

  expect_warning(suppressMessages(template_import("renamed.yaml", project = d)), "Carried the grading")
  expect_equal(markermd:::project_question_names(d)$names, c("Question One", "Q2"))
  expect_named(markermd:::load_rubric_items(d, "Question One"), c("item_0", "item_1", "item_2", "item_3"))
  expect_length(markermd:::load_rubric_items(d, "Q1"), 0)
  expect_setequal(sn_selected(d, "Question One", "hw01-team01"), c("Issue A", "Minor"))
  expect_equal(markermd:::load_comment(d, "Question One", "hw01-team02"), "Nice")
  scores = collect_score_data(d, "Question One", "hw01-team01")
  expect_equal(scores$score, 6)
})

test_that("removing a question that holds grading warns and leaves its rows in place", {
  d = sn_graded_project()
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:", "  path: hw1.qmd", "questions:", "  - id: 2", "    name: Q2"
  ), file.path(d, "one.yaml"))
  expect_warning(suppressMessages(template_import("one.yaml", project = d)), "no longer in the template")
  expect_length(markermd:::load_rubric_items(d, "Q1"), 4)
})

# --- The document each repository is graded on -------------------------------------

test_that("parse_repo_documents picks the template's document, not the first qmd found", {
  src = system.file("examples/test_assignment/repos", package = "markermd")
  root = tempfile("docs_")
  repos = file.path(root, "repos")
  dir.create(repos, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), repos, recursive = TRUE)
  repo_list = sort(basename(list.dirs(repos, recursive = FALSE)))
  dir.create(file.path(repos, repo_list[1], "_drafts"))
  writeLines(c("# Scratch", "", "nothing here"), file.path(repos, repo_list[1], "_drafts", "scratch.qmd"))

  coll = markermd:::parse_repo_documents(repos, repo_list, "assignment.qmd", TRUE)
  expect_equal(nrow(coll), length(repo_list))
  expect_equal(basename(coll$path[coll$repo == repo_list[1]]), "assignment.qmd")
  expect_false(any(grepl("scratch", coll$path)))
  expect_true(all(!is.na(coll$ast) | !is.na(coll$error)))
})

# --- Parsing ------------------------------------------------------------------------

test_that("normalize_knitr_chunks maps the rewritten lines back to the file's lines", {
  lines = c("# A", "", "```{r setup, echo=FALSE, message=FALSE}", "x", "```", "", "text")
  out = markermd:::normalize_knitr_chunks(lines)
  map = attr(out, "line_map")
  expect_length(map, length(out))
  expect_equal(map[1:2], 1:2)
  # the header became ```{r} plus three #| lines, all from line 3
  expect_equal(sum(map == 3), 4)
  expect_equal(map[length(map)], 7L)
  ranges = markermd:::map_ranges_to_original(list(list(start = 8, end = 10)), map)
  expect_equal(ranges, list(list(start = 5, end = 7)))
})

test_that("a parse error after a rewritten chunk header names the file's line", {
  path = tempfile(fileext = ".qmd")
  writeLines(c(
    "# A", "", "```{r setup, echo=FALSE, message=FALSE}", "x", "```", "",
    "::: {.note", "text", ":::"
  ), path)
  err = tryCatch(parse_assignment_document(path), error = function(e) e)
  expect_s3_class(err, "q2r_parse_error")
  expect_match(conditionMessage(err), basename(path), fixed = TRUE)
  # the header became four lines (three options), so the parser's line 11 is
  # the file's line 8
  expect_match(conditionMessage(err), "Line 11 here is line 8 of", fixed = TRUE)
})

# --- Helpers -------------------------------------------------------------------------

test_that("question_dom_id keeps punctuation-only differences apart and plain names unchanged", {
  expect_equal(markermd:::question_dom_id("Q2"), "Q2")
  ids = markermd:::question_dom_id(c("Q1 (a)", "Q1 [a]", "Q1_a"))
  expect_false(any(duplicated(ids)))
  expect_equal(ids[3], "Q1_a")
  expect_match(ids[1], "^Q1__a__[0-9a-f]{6}$")
})

test_that("event timestamps are UTC ISO 8601 and legacy rows migrate and parse", {
  d = sn_graded_project()
  rows = sn_query(d, "SELECT timestamp FROM grades LIMIT 3")
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}Z$", rows$timestamp)))

  parsed = markermd:::parse_event_timestamp(c("2026-01-02T03:04:05.123Z", "2026-01-02 03:04:05"))
  expect_false(anyNA(parsed))
  expect_equal(format(parsed[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-01-02 03:04:05")

  # A schema 6 database's local timestamps are rewritten in UTC on first touch
  db = file.path(d, ".markermd", "markermd.sqlite")
  conn = DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(conn, "UPDATE grades SET timestamp = '2026-01-02 03:04:05'")
  DBI::dbExecute(conn, "UPDATE metadata SET value = '6' WHERE key = 'schema_version'")
  DBI::dbDisconnect(conn)
  rm(list = ls(markermd:::.initialized_dbs), envir = markermd:::.initialized_dbs)
  markermd:::with_database(d, function(conn) {
    expect_equal(markermd:::get_metadata(conn, "schema_version"), "7")
    ts = DBI::dbGetQuery(conn, "SELECT timestamp FROM grades LIMIT 1")$timestamp
    expect_match(ts, "T")
    expect_equal(
      format(markermd:::parse_event_timestamp(ts), "%Y-%m-%d %H:%M:%S"),
      "2026-01-02 03:04:05"
    )
  })
})

test_that("a newer database is refused before any migration touches it", {
  d = sn_project()
  db = file.path(d, ".markermd", "markermd.sqlite")
  conn = DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(conn, "UPDATE metadata SET value = '99' WHERE key = 'schema_version'")
  DBI::dbDisconnect(conn)
  rm(list = ls(markermd:::.initialized_dbs), envir = markermd:::.initialized_dbs)
  expect_error(markermd:::with_database(d, function(conn) TRUE), "newer version")
})

test_that("graded pairs ignore grade rows for items that no longer exist", {
  d = sn_graded_project()
  markermd:::save_grade_selection(d, "Q2", "hw01-team02", "item_gone", TRUE)
  pairs = markermd:::graded_question_pairs(d, c("hw01-team01", "hw01-team02"))
  expect_false(any(pairs$question_name == "Q2" & pairs$assignment_repo == "hw01-team02"))
})

test_that("the vectorized score builder agrees with export_scores and marks the running total", {
  d = sn_graded_project()
  scores = collect_score_data(d, c("Q1", "Q2"), c("hw01-team01", "hw01-team02"))
  expect_equal(scores$question_name, rep(c("Q1", "Q2"), each = 2))
  tbl = build_scores_table_data(scores, c("Q1", "Q2"), c("hw01-team01", "hw01-team02"))
  expect_equal(tbl$Total, c(16, NA))
  expect_equal(tbl$Total_partial, c(16, 10))
  csv = suppressMessages(export_scores(d))
  out = utils::read.csv(csv)
  expect_equal(out$total, c(16, NA))
})

test_that("the app's own actions are recorded with the actor app", {
  d = sn_graded_project()
  res = markermd:::with_action(
    d, "edit rubric item", call = "mark(): edit", parts = "rubric", actor = "app",
    markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -3, "Issue A (edited)"))
  )
  expect_match(markermd:::last_recorded_action_id(), "^act-")
  rows = project_activity(d, app = FALSE)
  expect_equal(rows$actor[1], "app")
  expect_equal(rows$fn[1], "edit rubric item")
  expect_equal(rows$parts[[1]], "rubric")
})
