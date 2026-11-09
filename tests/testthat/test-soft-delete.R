soft_item_id = function(d, question, description) {
  items = markermd:::load_rubric_items(d, question)
  names(items)[vapply(items, function(item) item@description, character(1)) == description]
}

soft_q1_score = function(d, repo = "hw01-team01") {
  scores = collect_score_data(d, "Q1", repo)
  scores$score[scores$question_name == "Q1" & scores$assignment_repo == repo]
}

test_that("deleting an item keeps its grade rows but drops it from scores, and a restore brings it back", {
  d = sn_graded_project()
  snap = suppressMessages(snapshot_create(d, label = "before delete"))
  expect_equal(soft_q1_score(d), 6)
  n_grades = sn_query(d, "SELECT COUNT(*) AS n FROM grades")$n

  issue_a = soft_item_id(d, "Q1", "Issue A")
  markermd:::delete_rubric_item(d, "Q1", issue_a)

  expect_equal(sn_query(d, "SELECT COUNT(*) AS n FROM grades")$n, n_grades)
  expect_false(issue_a %in% names(markermd:::load_rubric_items(d, "Q1")))
  expect_equal(sn_selected(d, "Q1", "hw01-team01"), "Minor")
  expect_equal(soft_q1_score(d), 9)
  row = sn_query(d, paste0("SELECT deleted_at FROM items WHERE item_id = '", issue_a, "'"))
  expect_false(is.na(row$deleted_at))

  suppressMessages(snapshot_restore(snap, project = d))
  expect_true(issue_a %in% names(markermd:::load_rubric_items(d, "Q1")))
  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), c("Issue A", "Minor"))
  expect_equal(soft_q1_score(d), 6)
  # The item came back with its kept rows, so the restore wrote no grade rows
  expect_equal(sn_query(d, "SELECT COUNT(*) AS n FROM grades")$n, n_grades)
})

test_that("a deleted item's description can be used again and its id is never reissued", {
  d = sn_graded_project()
  issue_a = soft_item_id(d, "Q1", "Issue A")
  markermd:::delete_rubric_item(d, "Q1", issue_a)

  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'", "questions:", "- name: Q1", "  items:",
    "  - points: -2", "    description: Issue A"
  ), path)
  suppressMessages(rubric_import(path, project = d, question = "Q1"))

  new_id = soft_item_id(d, "Q1", "Issue A")
  expect_length(new_id, 1)
  expect_false(new_id == issue_a)
  # The old item's selections do not carry over to the new one
  expect_equal(sn_selected(d, "Q1", "hw01-team01"), "Minor")

  used = markermd:::with_database(d, markermd:::used_item_ids)
  expect_true(issue_a %in% used)
})

test_that("a replace import soft-deletes the old items and keeps their events in History", {
  d = sn_graded_project()
  markermd:::save_grade_selection(d, "Q2", "hw01-team02", soft_item_id(d, "Q2", "Half credit"), TRUE)
  n_grades = sn_query(d, "SELECT COUNT(*) AS n FROM grades")$n

  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'", "questions:", "- name: Q2", "  items:",
    "  - points: 7", "    description: Most credit"
  ), path)
  suppressMessages(rubric_import(path, project = d, mode = "replace", question = "Q2"))

  expect_equal(sn_query(d, "SELECT COUNT(*) AS n FROM grades")$n, n_grades)
  expect_equal(
    unname(vapply(markermd:::load_rubric_items(d, "Q2"), function(item) item@description, character(1))),
    "Most credit"
  )
  expect_length(sn_selected(d, "Q2", "hw01-team02"), 0)
  expect_length(sn_selected(d, "Q2", "hw01-team01"), 0)

  # The app's selection of the now-deleted item is still in the activity log,
  # labeled with the item's last description
  app = project_activity(d, actor = "app")
  expect_gte(nrow(app), 1)
  detail = activity_detail(app$id[1], d)
  expect_true("Half credit" %in% detail@marks$after[detail@marks$change == "selection"])
})

test_that("deleting a group soft-deletes it, frees its label and a restore brings it back", {
  d = sn_graded_project()
  snap = suppressMessages(snapshot_create(d, label = "with group"))
  groups = markermd:::load_item_groups(d, "Q1")
  expect_length(groups, 1)
  group_id = names(groups)

  markermd:::delete_item_group(d, "Q1", group_id)
  expect_length(markermd:::load_item_groups(d, "Q1"), 0)
  expect_equal(sn_query(d, paste0("SELECT COUNT(*) AS n FROM item_groups WHERE group_id = '", group_id, "'"))$n, 1)

  # The label is free for a new live group
  markermd:::save_item_group(d, "Q1", "group_99", markermd::markermd_rubric_group(label = "Tiers"))
  expect_equal(unname(vapply(markermd:::load_item_groups(d, "Q1"), function(g) g@label, character(1))), "Tiers")
  markermd:::delete_item_group(d, "Q1", "group_99")

  suppressMessages(snapshot_restore(snap, project = d))
  restored = markermd:::load_item_groups(d, "Q1")
  expect_named(restored, group_id)
  expect_true(restored[[1]]@exclusive)
  members = vapply(markermd:::load_rubric_items(d, "Q1"), function(item) item@group, character(1))
  expect_equal(sum(members == group_id, na.rm = TRUE), 2)
})

test_that("a schema 8 database migrates to soft deletes keeping its rows", {
  d = sn_graded_project()
  db = file.path(d, ".markermd", "markermd.sqlite")
  conn = DBI::dbConnect(RSQLite::SQLite(), db)
  # Rebuild the two tables as schema 8 wrote them: no deleted columns, and a
  # table-level label constraint on item_groups
  DBI::dbExecute(conn, "DROP INDEX IF EXISTS idx_item_groups_live_label")
  for (table in c("items", "item_groups")) {
    cols = setdiff(DBI::dbListFields(conn, table), c("deleted_at", "deleted_action_id"))
    DBI::dbExecute(conn, glue::glue("CREATE TABLE {table}_old AS SELECT {paste(cols, collapse = ', ')} FROM {table}"))
    DBI::dbExecute(conn, glue::glue("DROP TABLE {table}"))
  }
  DBI::dbExecute(conn, "ALTER TABLE items_old RENAME TO items")
  DBI::dbExecute(conn, "
    CREATE TABLE item_groups (
      id INTEGER PRIMARY KEY AUTOINCREMENT, question_id INTEGER, question_name TEXT NOT NULL,
      group_id TEXT NOT NULL, label TEXT NOT NULL, floor REAL, ceiling REAL,
      exclusive INTEGER NOT NULL DEFAULT 0, created_at TEXT NOT NULL, updated_at TEXT NOT NULL,
      UNIQUE (question_id, group_id), UNIQUE (question_id, label)
    )
  ")
  DBI::dbExecute(conn, "INSERT INTO item_groups SELECT * FROM item_groups_old")
  DBI::dbExecute(conn, "DROP TABLE item_groups_old")
  DBI::dbExecute(conn, "UPDATE metadata SET value = '8' WHERE key = 'schema_version'")
  DBI::dbDisconnect(conn)
  rm(list = ls(markermd:::.initialized_dbs), envir = markermd:::.initialized_dbs)

  expect_length(markermd:::load_rubric_items(d, "Q1"), 4)
  expect_equal(sn_query(d, "SELECT value FROM metadata WHERE key = 'schema_version'")$value, markermd:::markermd_db_version())
  expect_true("deleted_at" %in% names(sn_query(d, "SELECT * FROM items LIMIT 1")))
  expect_true("deleted_at" %in% names(sn_query(d, "SELECT * FROM item_groups LIMIT 1")))
  expect_length(markermd:::load_item_groups(d, "Q1"), 1)
  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), c("Issue A", "Minor"))

  group_id = names(markermd:::load_item_groups(d, "Q1"))
  markermd:::delete_item_group(d, "Q1", group_id)
  expect_no_error(markermd:::save_item_group(d, "Q1", "group_99", markermd::markermd_rubric_group(label = "Tiers")))
})
