# Grading keyed by question id (schema version 8): the questions registry,
# the migration from name-keyed databases, and the rename, removal and
# restore behavior that follows from keying on the template's question ids

qid_template_file = function(d, file, questions) {
  lines = c(
    paste0("format_version: '", markermd:::markermd_template_version(), "'"),
    "source:", "  path: hw1.qmd", "questions:"
  )
  for (q in questions) {
    lines = c(lines, paste0("  - id: ", q$id), paste0("    name: ", q$name))
  }
  path = file.path(d, file)
  writeLines(lines, path)
  path
}

# Turn a project's database back into its schema version 7 shape: the
# grading tables keyed by question name, with no question_id column and no
# registry
#
# d: Project root
# keep_template: Whether the stored template stays

qid_downgrade_to_v7 = function(d, keep_template = TRUE) {
  db = file.path(d, ".markermd", "markermd.sqlite")
  conn = DBI::dbConnect(RSQLite::SQLite(), db)
  for (table in markermd:::question_keyed_tables()) {
    cols = setdiff(DBI::dbListFields(conn, table), "question_id")
    if (table == "settings") cols = setdiff(cols, "id")
    DBI::dbExecute(conn, glue::glue("CREATE TABLE {table}_old AS SELECT {paste(cols, collapse = ', ')} FROM {table}"))
    DBI::dbExecute(conn, glue::glue("DROP TABLE {table}"))
    DBI::dbExecute(conn, glue::glue("ALTER TABLE {table}_old RENAME TO {table}"))
  }
  DBI::dbExecute(conn, "DROP TABLE questions")
  if (!keep_template) {
    DBI::dbExecute(conn, "DELETE FROM metadata WHERE key IN ('template', 'question_id_max')")
  }
  DBI::dbExecute(conn, "UPDATE metadata SET value = '7' WHERE key = 'schema_version'")
  rm(list = ls(markermd:::.initialized_dbs), envir = markermd:::.initialized_dbs)
  invisible(conn)
}

qid_query = function(d, sql) {
  markermd:::with_database(d, function(conn) DBI::dbGetQuery(conn, sql))
}

test_that("a version 7 database migrates to question ids, keeping its rows and orphaning unknown names", {
  d = sn_graded_project()
  before = qid_query(d, "SELECT id, question_name, item_id, selected FROM grades ORDER BY id")
  conn = qid_downgrade_to_v7(d)
  # A question an earlier template removed left its rows behind under its name
  DBI::dbExecute(conn, "
    INSERT INTO items (question_name, item_id, hotkey, position, points, description, created_at, updated_at)
    VALUES ('Old', 'item_old', 1, 1, -1, 'Old item', '2026-01-01', '2026-01-01')
  ")
  DBI::dbDisconnect(conn)

  markermd:::with_database(d, function(conn) {
    expect_equal(markermd:::get_metadata(conn, "schema_version"), markermd:::markermd_db_version())
    for (table in markermd:::question_keyed_tables()) {
      expect_true("question_id" %in% DBI::dbListFields(conn, table))
    }
  })
  registry = qid_query(d, "SELECT question_id, name, live, templated FROM questions ORDER BY question_id")
  expect_equal(registry$name, c("Q1", "Q2", "Old"))
  expect_equal(registry$question_id[1:2], c(1L, 2L))
  expect_gt(registry$question_id[3], 2L)
  expect_equal(registry$live, c(1L, 1L, 0L))

  after = qid_query(d, "SELECT id, question_name, item_id, selected FROM grades ORDER BY id")
  expect_equal(after, before)
  expect_length(markermd:::load_rubric_items(d, "Q1"), 4)
  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), c("Issue A", "Minor"))
  expect_length(markermd:::load_rubric_items(d, "Old"), 0)
  expect_equal(qid_query(d, "SELECT COUNT(*) AS n FROM items WHERE question_name = 'Old'")$n, 1)

  # The migrated database takes new writes under the same ids
  markermd:::save_grade_selection(d, "Q2", "hw01-team02", "item_4", TRUE)
  expect_equal(qid_query(d, "SELECT question_id FROM grades ORDER BY id DESC LIMIT 1")$question_id, 2L)
})

test_that("a version 7 database without a template keeps its names live until a template adopts them", {
  d = sn_graded_project()
  DBI::dbDisconnect(qid_downgrade_to_v7(d, keep_template = FALSE))
  registry = qid_query(d, "SELECT name, live, templated FROM questions ORDER BY name")
  expect_equal(registry$name, c("Q1", "Q2"))
  expect_equal(registry$live, c(1L, 1L))
  expect_equal(registry$templated, c(0L, 0L))
  expect_length(markermd:::load_rubric_items(d, "Q1"), 4)

  # A template naming them adopts their rows under its own ids, here swapped
  # relative to the ids the migration minted
  qid_template_file(d, "t.yaml", list(list(id = 7, name = "Q2"), list(id = 8, name = "Q1")))
  suppressWarnings(suppressMessages(template_import("t.yaml", project = d)))
  expect_equal(qid_query(d, "SELECT DISTINCT question_id FROM items WHERE question_name = 'Q1'")$question_id, 8L)
  expect_length(markermd:::load_rubric_items(d, "Q1"), 4)
  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), c("Issue A", "Minor"))
  expect_setequal(sn_selected(d, "Q2", "hw01-team01"), "Full credit")
})

test_that("rows written by hand without a question id land under their name's question", {
  d = sn_graded_project()
  markermd:::with_database(d, function(conn) {
    DBI::dbExecute(conn, "
      INSERT INTO grades (question_name, assignment_repo, item_id, selected, timestamp, username)
      VALUES ('Q1', 'hw01-team02', 'item_0', 1, '2026-01-01T00:00:00.000Z', 'me')
    ")
  })
  expect_equal(qid_query(d, "SELECT question_id FROM grades ORDER BY id DESC LIMIT 1")$question_id, 1L)
})

test_that("a renamed question keeps its grading under its id and only the display name changes", {
  d = sn_graded_project()
  qid_template_file(d, "r.yaml", list(list(id = 1, name = "Question one"), list(id = 2, name = "Q2")))
  expect_warning(suppressMessages(template_import("r.yaml", project = d)), "Carried the grading")
  expect_equal(qid_query(d, "SELECT DISTINCT question_name FROM grades WHERE question_id = 1")$question_name, "Question one")
  expect_setequal(sn_selected(d, "Question one", "hw01-team01"), c("Issue A", "Minor"))
  expect_equal(collect_score_data(d, "Question one", "hw01-team01")$score, 6)
})

test_that("removing a question and adding one with the same name starts the new one empty", {
  d = sn_graded_project()
  qid_template_file(d, "drop.yaml", list(list(id = 2, name = "Q2")))
  expect_warning(suppressMessages(template_import("drop.yaml", project = d)), "no longer in the template")
  qid_template_file(d, "readd.yaml", list(list(id = 2, name = "Q2"), list(id = 9, name = "Q1")))
  suppressWarnings(suppressMessages(template_import("readd.yaml", project = d)))

  expect_length(markermd:::load_rubric_items(d, "Q1"), 0)
  expect_length(sn_selected(d, "Q1", "hw01-team01"), 0)
  # The removed question's rows are still there under its own id
  expect_equal(qid_query(d, "SELECT COUNT(*) AS n FROM items WHERE question_id = 1")$n, 4)
})

test_that("restoring the old template brings a removed question's grading back", {
  d = sn_graded_project()
  qid_template_file(d, "drop.yaml", list(list(id = 2, name = "Q2")))
  suppressWarnings(suppressMessages(template_import("drop.yaml", project = d)))
  expect_length(markermd:::load_rubric_items(d, "Q1"), 0)

  # The import's automatic snapshot holds the template from before it
  suppressWarnings(suppressMessages(snapshot_restore("latest", project = d, part = "template")))
  expect_equal(markermd:::project_question_names(d)$names, c("Q1", "Q2"))
  expect_length(markermd:::load_rubric_items(d, "Q1"), 4)
  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), c("Issue A", "Minor"))
})

test_that("a snapshot diff lines a renamed question up with itself", {
  d = sn_graded_project()
  snap = suppressMessages(snapshot_create(project = d, label = "before rename"))
  qid_template_file(d, "r.yaml", list(list(id = 1, name = "Question one"), list(id = 2, name = "Q2")))
  suppressWarnings(suppressMessages(template_import("r.yaml", project = d)))

  diff = snapshot_diff(snap, project = d)
  expect_equal(nrow(diff@rubric), 0)
  expect_equal(nrow(diff@marks), 0)
  expect_equal(nrow(diff@scores), 0)
  expect_gt(nrow(diff@template), 0)
})

test_that("restoring a snapshot's marks after a rename restores them under the current name", {
  d = sn_graded_project()
  snap = suppressMessages(snapshot_create(project = d, label = "graded"))
  qid_template_file(d, "r.yaml", list(list(id = 1, name = "Question one"), list(id = 2, name = "Q2")))
  suppressWarnings(suppressMessages(template_import("r.yaml", project = d)))
  marks_set("hw01-team01", "Question one", items = "Looks good", overwrite = TRUE, project = d)
  expect_setequal(sn_selected(d, "Question one", "hw01-team01"), "Looks good")

  suppressMessages(snapshot_restore(snap, project = d))
  expect_setequal(sn_selected(d, "Question one", "hw01-team01"), c("Issue A", "Minor"))
  expect_equal(markermd:::project_question_names(d)$names, c("Question one", "Q2"))
})
