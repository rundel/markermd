# An initialized project with a two-question template (Q1, Q2), a seeded
# rubric and two student repo directories, built through the exported
# functions where they exist so the action log sees them
make_action_project = function() {
  d = tempfile("actproj_")
  dir.create(file.path(d, "repos", "hw01-team01"), recursive = TRUE)
  dir.create(file.path(d, "repos", "hw01-team02"), recursive = TRUE)
  d = normalizePath(d, winslash = "/")

  template_path = file.path(d, "template.yaml")
  writeLines(c(
    paste0("format_version: '", markermd:::markermd_template_version(), "'"),
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - id: 1",
    "    name: Q1",
    "  - id: 2",
    "    name: Q2"
  ), template_path)

  suppressMessages(init_project(d))
  withr::with_envvar(c(MARKERMD_ACTOR = "setup"), {
    suppressMessages(template_import(template_path, project = d))
  })

  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "Issue A"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -1, "Issue B"))
  markermd:::save_rubric_item(d, "Q2", "item_2", markermd_rubric_item(1L, -3, "Q2 issue"))
  d
}

test_that("markermd_actor() reads the explicit actor and the agent markers", {
  expect_equal(markermd:::markermd_actor(c(MARKERMD_ACTOR = "grader-bot", CLAUDECODE = "1")), "grader-bot")
  expect_equal(markermd:::markermd_actor(c(CLAUDECODE = "1")), "claude")
  expect_equal(markermd:::markermd_actor(c(CODEX_SANDBOX = "seatbelt")), "codex")
  expect_equal(markermd:::markermd_actor(c(HOME = "/tmp")), "R")
})

test_that("a fresh database is at schema 6 with action columns", {
  d = make_action_project()
  markermd:::with_database(d, function(conn) {
    expect_equal(markermd:::get_metadata(conn, "schema_version"), "6")
    for (table in c("grades", "comments", "private_comments")) {
      expect_true("action_id" %in% DBI::dbListFields(conn, table))
    }
    expect_true(DBI::dbExistsTable(conn, "actions"))
  })
})

test_that("marks_set records an action whose events carry its id", {
  d = make_action_project()
  withr::local_envvar(c(MARKERMD_ACTOR = "claude"))
  suppressMessages(marks_set(
    "hw01-team01", "Q1", items = "Issue B", private_comment = "note",
    project = d, summary = "first pass"
  ))

  activity = project_activity(d, app = FALSE)
  row = activity[activity$fn == "marks_set", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$actor, "claude")
  expect_equal(row$summary, "first pass")
  expect_equal(row$n_pairs, 1L)
  expect_equal(row$repos[[1]], "hw01-team01")

  ids = markermd:::with_database(d, function(conn) {
    DBI::dbGetQuery(conn, "SELECT DISTINCT action_id FROM grades")$action_id
  })
  expect_equal(ids, row$id)

  detail = activity_detail(row$id, d)
  sel = detail@marks[detail@marks$change == "selection", ]
  expect_equal(sel$after, "Issue B")
  expect_equal(sel$before, "")
  expect_equal(detail@marks$after[detail@marks$change == "private_comment"], "note")
})

test_that("a call that changes nothing records no action", {
  d = make_action_project()
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'",
    "repos:",
    "- name: hw01-team01",
    "  questions:",
    "  - name: Q1",
    "    items:",
    "    - Issue A"
  ), path)
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)

  before = nrow(project_activity(d, app = FALSE))
  result = suppressMessages(marks_import(path, project = d))
  expect_equal(result$action, "skipped")
  expect_equal(nrow(project_activity(d, app = FALSE)), before)
})

test_that("rubric_import keeps the rubric before and after for its diff", {
  d = make_action_project()
  rubric_path = tempfile(fileext = ".yaml")
  rubric_export(rubric_path, project = d)
  rubric = yaml::read_yaml(rubric_path)
  rubric$questions[[2]]$items = c(rubric$questions[[2]]$items, list(list(points = -1, description = "Late")))
  yaml::write_yaml(rubric, rubric_path)

  suppressMessages(rubric_import(rubric_path, project = d, summary = "add late item"))

  row = project_activity(d, app = FALSE)
  row = row[row$fn == "rubric_import", ]
  expect_equal(row$parts[[1]], "rubric")
  expect_false(is.na(row$snapshot_id))

  detail = activity_detail(row$id, d)
  expect_true(any(detail@rubric$question == "Q2" & detail@rubric$item == "Late"))

  # The kept parts survive blob garbage collection
  markermd:::with_database(d, function(conn) markermd:::gc_snapshot_blobs(conn))
  expect_no_error(activity_detail(row$id, d))
})

test_that("app grading is grouped by idle gaps and diffed from its events", {
  d = make_action_project()
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)
  markermd:::save_comment(d, "Q1", "hw01-team01", "Well done")
  markermd:::with_database(d, function(conn) {
    DBI::dbExecute(conn, "INSERT INTO grades (question_name, assignment_repo, item_id, selected, timestamp, username)
                          VALUES ('Q2', 'hw01-team02', 'item_2', 1, '2020-01-01 10:00:00', 'grader')")
  })

  app = project_activity(d, actor = "app")
  expect_equal(nrow(app), 2)
  expect_equal(app$n_pairs, c(1L, 1L))

  recent = activity_detail(app$id[1], d)
  expect_equal(recent@marks$after[recent@marks$change == "selection"], "Issue A")
  expect_equal(recent@marks$after[recent@marks$change == "comment"], "Well done")
})

test_that("a schema 5 database migrates to 6 keeping its events", {
  d = make_action_project()
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)
  db = file.path(d, ".markermd", "markermd.sqlite")
  conn = DBI::dbConnect(RSQLite::SQLite(), db)
  for (table in c("grades", "comments", "private_comments")) {
    DBI::dbExecute(conn, glue::glue("DROP INDEX IF EXISTS idx_{table}_action"))
    DBI::dbExecute(conn, glue::glue("ALTER TABLE {table} DROP COLUMN action_id"))
  }
  DBI::dbExecute(conn, "UPDATE metadata SET value = '5' WHERE key = 'schema_version'")
  DBI::dbDisconnect(conn)
  rm(list = ls(markermd:::.initialized_dbs), envir = markermd:::.initialized_dbs)

  markermd:::with_database(d, function(conn) {
    expect_equal(markermd:::get_metadata(conn, "schema_version"), "6")
    expect_true("action_id" %in% DBI::dbListFields(conn, "grades"))
    expect_equal(nrow(DBI::dbGetQuery(conn, "SELECT * FROM grades")), 1)
  })
})
