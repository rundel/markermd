# Snapshots of the grading state: the writer, the automatic snapshots the
# imports take, listing and resolving, diffing, restoring, exporting and
# retention (snapshots.R, snapshot-diff.R).

# An initialized project with a two-question template, a rubric (Q1 with an
# exclusive group, Q2 plain) and marks for two repositories
sn_project = function() {
  d = tempfile("snapproj_")
  dir.create(file.path(d, "repos", "hw01-team01"), recursive = TRUE)
  dir.create(file.path(d, "repos", "hw01-team02"))
  d = normalizePath(d, winslash = "/")
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:", "  path: hw1.qmd", "questions:",
    "  - id: 1", "    name: Q1", "  - id: 2", "    name: Q2"
  ), file.path(d, "template.yaml"))
  suppressMessages(init_project(d))
  suppressMessages(template_import("template.yaml", project = d))
  d
}

sn_rubric_yaml = function() {
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.1'", "questions:",
    "- name: Q1", "  scoring:", "    total_score: 10", "    grading_mode: negative",
    "  items:", "  - points: 0", "    description: Looks good", "  - points: -3", "    description: Issue A",
    "  - group: Tiers", "    exclusive: true", "    items:",
    "    - points: -1", "      description: Minor", "    - points: -4", "      description: Major",
    "- name: Q2", "  items:", "  - points: 5", "    description: Half credit", "  - points: 10", "    description: Full credit"
  ), path)
  path
}

# The project above with the rubric imported and three pairs marked
sn_graded_project = function() {
  d = sn_project()
  suppressMessages(rubric_import(sn_rubric_yaml(), project = d, summary = "Initial rubric"))
  suppressMessages({
    marks_set("hw01-team01", "Q1", items = c("Issue A", "Minor"), private_comment = "note", project = d)
    marks_set("hw01-team02", "Q1", items = "Looks good", comment = "Nice", project = d)
    marks_set("hw01-team01", "Q2", items = "Full credit", project = d)
  })
  d
}

sn_query = function(d, sql) {
  markermd:::with_database(d, function(conn) DBI::dbGetQuery(conn, sql))
}

# Descriptions of the items currently selected for a pair (a restore mints
# fresh item ids, so selections are compared by description)
sn_selected = function(d, question, repo) {
  items = markermd:::load_rubric_items(d, question)
  sel = markermd:::load_grade_selections(d, question, repo)
  chosen = names(sel)[unlist(sel)]
  unname(vapply(items[intersect(names(items), chosen)], function(item) item@description, character(1)))
}


# --- Payload, hashing and the writer ------------------------------------------

test_that("canonical_json sorts keys recursively so construction order does not matter", {
  a = markermd:::canonical_json(list(b = 1, a = list(z = "x", y = list(2, 1))))
  b = markermd:::canonical_json(list(a = list(y = list(2, 1), z = "x"), b = 1))
  expect_identical(a, b)
  expect_identical(markermd:::snapshot_hash(a), markermd:::snapshot_hash(b))
  expect_equal(markermd:::parse_json(a)$a$y, list(2L, 1L))
})

test_that("a snapshot blob round trips through gzip and plain encodings", {
  long = paste(rep("abcdefgh", 100), collapse = "")
  gz = markermd:::encode_snapshot_blob(long)
  expect_equal(gz$encoding, "json+gzip")
  expect_equal(gz$size, nchar(long))
  expect_identical(markermd:::decode_snapshot_blob(data.frame(encoding = "json+gzip", content = I(list(gz$content)))), long)

  plain = markermd:::encode_snapshot_blob("{}")
  expect_equal(plain$encoding, "json")
  expect_identical(markermd:::decode_snapshot_blob(data.frame(encoding = "json", content = I(list(plain$content)))), "{}")
})

test_that("the payload carries the template without metadata, the rubric with ids and the marks", {
  d = sn_graded_project()
  payload = markermd:::with_database(d, markermd:::snapshot_payload)

  expect_null(payload$template$metadata)
  expect_equal(markermd:::template_list_question_names(payload$template), c("Q1", "Q2"))

  q1 = payload$rubric$questions[[1]]
  expect_equal(q1$items[[1]]$id, "item_0")
  expect_equal(q1$items[[3]]$group, "Tiers")
  expect_equal(q1$items[[3]]$id, "group_0")
  expect_equal(q1$items[[3]]$items[[1]]$id, "item_2")

  expect_equal(vapply(payload$marks$repos, function(r) r$name, character(1)), c("hw01-team01", "hw01-team02"))
  expect_equal(payload$marks$repos[[1]]$questions[[1]]$items, list("Issue A", "Minor"))
})

test_that("the rubric exchange list reads ids back and drops them from the public YAML", {
  d = sn_graded_project()
  payload = markermd:::with_database(d, markermd:::snapshot_payload)
  rubric = markermd:::rubric_from_list(payload$rubric)
  expect_named(rubric$questions[[1]]$items, c("item_0", "item_1", "item_2", "item_3"))
  expect_named(rubric$questions[[1]]$groups, "group_0")
  expect_equal(rubric$questions[[1]]$items$item_2@group, "group_0")

  out = tempfile(fileext = ".yaml")
  write_rubric_yaml(rubric, out)
  expect_no_match(paste(readLines(out), collapse = "\n"), "id:")
  testthat::skip_if_not_installed("jsonvalidate")
  expect_true(validate_rubric_file(out))
})

test_that("score_snapshot_parts agrees with collect_score_data on the live database", {
  d = sn_graded_project()
  payload = markermd:::with_database(d, markermd:::snapshot_payload)
  from_parts = markermd:::score_snapshot_parts(payload$rubric, payload$marks, c("hw01-team01", "hw01-team02"))
  live = markermd:::collect_score_data(d, c("Q1", "Q2"), c("hw01-team01", "hw01-team02"))
  key = function(df) paste(df$question_name, df$assignment_repo)
  expect_equal(from_parts$score[match(key(live), key(from_parts))], live$score)
  # team01: Q1 10 - 3 - 1 = 6, Q2 10; team02: Q1 10 (looks good), Q2 ungraded
  expect_equal(from_parts$score[from_parts$assignment_repo == "hw01-team01" & from_parts$question_name == "Q1"], 6)
  expect_true(is.na(from_parts$score[from_parts$assignment_repo == "hw01-team02" & from_parts$question_name == "Q2"]))

  totals = markermd:::snapshot_totals(from_parts, c("Q1", "Q2"))
  expect_equal(totals$total[totals$assignment_repo == "hw01-team01"], 16)
  expect_true(is.na(totals$total[totals$assignment_repo == "hw01-team02"]))
})

test_that("score_snapshot_parts treats optional questions and comments as graded", {
  rubric = list(questions = list(
    list(name = "Doc", scoring = list(total_score = 0, grading_mode = "negative", bound_above_zero = FALSE, optional = TRUE),
         items = list(list(id = "item_0", points = -2, description = "Does not render"))),
    list(name = "Q1", items = list(list(id = "item_1", points = 5, description = "Half")))
  ))
  marks = list(repos = list(
    list(name = "a", questions = list(list(name = "Doc", items = list("Does not render")))),
    list(name = "b", questions = list(list(name = "Q1", items = list(), comment = "Reviewed")))
  ))
  scores = markermd:::score_snapshot_parts(rubric, marks, c("a", "b", "c"))
  get = function(repo, q) scores$score[scores$assignment_repo == repo & scores$question_name == q]
  expect_equal(get("a", "Doc"), -2)
  expect_equal(get("b", "Doc"), 0)
  expect_equal(get("c", "Doc"), 0)
  expect_equal(get("b", "Q1"), 0)
  expect_true(is.na(get("a", "Q1")))
  expect_true(is.na(get("c", "Q1")))
})


# --- Automatic snapshots -------------------------------------------------------

test_that("an import of an empty project takes no snapshot, and the first bulk change does", {
  d = sn_project()
  expect_equal(nrow(snapshot_list(d)), 0)

  suppressMessages(rubric_import(sn_rubric_yaml(), project = d, summary = "Initial rubric"))
  rows = snapshot_list(d)
  expect_equal(nrow(rows), 1)
  expect_equal(rows$kind, "auto")
  expect_equal(rows$trigger, "rubric_import(mode = 'append')")
  expect_equal(rows$summary, "Initial rubric")
  expect_equal(rows$username, markermd:::get_current_username())
  expect_true(inherits(rows$created_at, "POSIXct"))
  expect_true(inherits(rows, "markermd_snapshot_list"))

  # The snapshot holds the state before the import: a template but no items
  parts = markermd:::with_database(d, function(conn) markermd:::read_snapshot_parts(conn, rows$snapshot_id))
  expect_equal(markermd:::template_list_question_names(parts$template), c("Q1", "Q2"))
  expect_length(parts$rubric$questions[[1]]$items, 0)
  expect_length(parts$marks$repos, 0)
})

test_that("an unchanged state is recorded as a trigger note instead of a new snapshot", {
  d = sn_project()
  suppressMessages(rubric_import(sn_rubric_yaml(), project = d))
  suppressMessages(rubric_import(sn_rubric_yaml(), project = d, mode = "replace", summary = "Same again"))
  # The second import's pre-state (rubric present, no marks) differs from the
  # first's (no rubric), so two snapshots; a third identical replace finds the
  # state equal to the second snapshot's (the replace re-minted the item ids,
  # which do not count as a change) and only adds a note
  expect_equal(nrow(snapshot_list(d)), 2)
  suppressMessages(rubric_import(sn_rubric_yaml(), project = d, mode = "replace", summary = "And again"))
  rows = snapshot_list(d)
  expect_equal(nrow(rows), 2)
  expect_equal(rows$n_notes[1], 1L)
  info = snapshot_info("latest", project = d)
  expect_equal(info@notes$key, "trigger")
  expect_match(info@notes$value, "rubric_import\\(mode = 'replace'\\): And again")
  status = markermd:::with_database(d, markermd:::snapshot_state_status)
  expect_false(status$changed)
})

test_that("snapshot = FALSE skips the automatic snapshot and summary is validated", {
  d = sn_project()
  suppressMessages(rubric_import(sn_rubric_yaml(), project = d, snapshot = FALSE))
  expect_equal(nrow(snapshot_list(d)), 0)
  expect_error(rubric_import(sn_rubric_yaml(), project = d, snapshot = "yes"), "snapshot")
  expect_error(rubric_import(sn_rubric_yaml(), project = d, summary = c("a", "b")), "summary")
})

test_that("marks_import snapshots only when it overwrites existing grading", {
  d = sn_graded_project()
  n_before = nrow(snapshot_list(d))
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'", "repos:", "- name: hw01-team02", "  questions:",
    "  - name: Q2", "    items:", "    - Half credit"
  ), path)
  suppressMessages(marks_import(path, project = d))
  expect_equal(nrow(snapshot_list(d)), n_before)

  writeLines(c(
    "format_version: '1.0'", "repos:", "- name: hw01-team01", "  questions:",
    "  - name: Q1", "    items:", "    - Major"
  ), path)
  suppressMessages(marks_import(path, project = d, overwrite = TRUE, summary = "Re-check"))
  rows = snapshot_list(d)
  expect_equal(nrow(rows), n_before + 1)
  expect_equal(rows$trigger[1], "marks_import(overwrite = TRUE)")
  expect_equal(rows$summary[1], "Re-check")
  # The snapshot holds the selections the import replaced
  parts = markermd:::with_database(d, function(conn) markermd:::read_snapshot_parts(conn, rows$snapshot_id[1]))
  team01 = parts$marks$repos[[1]]$questions[[1]]
  expect_equal(unlist(team01$items), c("Issue A", "Minor"))
})

test_that("a failing import rolls back its snapshot with the change", {
  d = sn_graded_project()
  n_before = nrow(snapshot_list(d))
  rubric = read_rubric_yaml(sn_rubric_yaml())
  # A write failing inside the transaction rolls back the import and the
  # snapshot taken before it
  testthat::local_mocked_bindings(upsert_items = function(...) stop("disk full"), .package = "markermd")
  expect_error(
    markermd:::apply_rubric_import(d, rubric, mode = "replace", snapshot = list(trigger = "test", summary = "boom")),
    "disk full"
  )
  expect_equal(nrow(snapshot_list(d)), n_before)
  expect_length(markermd:::load_rubric_items(d, "Q1"), 4)
  expect_true(markermd:::load_grade_selections(d, "Q1", "hw01-team01")$item_1)
})

test_that("template saves, project_set and item deletion take snapshots when state exists", {
  d = sn_graded_project()
  n_before = nrow(snapshot_list(d))
  suppressMessages(project_set(d, template = file.path(d, "template.yaml")))
  rows = snapshot_list(d)
  expect_equal(nrow(rows), n_before + 1)
  expect_equal(rows$trigger[1], "project_set(template = ...)")

  # The app deletes an item with selections after a snapshot; here the state
  # moved on since the last one, so a new snapshot appears, holding the
  # selections the deletion discards
  suppressMessages(marks_set("hw01-team02", "Q2", items = "Half credit", project = d))
  expect_equal(markermd:::item_selection_count(d, "Q1", "item_1"), 1)
  expect_equal(markermd:::item_selection_count(d, "Q1", "item_3"), 0)
  markermd:::delete_rubric_item(d, "Q1", "item_1", snapshot = list(trigger = "mark(): delete item", summary = "Deleted Issue A"))
  rows = snapshot_list(d)
  expect_equal(nrow(rows), n_before + 2)
  expect_equal(rows$trigger[1], "mark(): delete item")
  parts = markermd:::with_database(d, function(conn) markermd:::read_snapshot_parts(conn, rows$snapshot_id[1]))
  expect_true("Issue A" %in% unlist(parts$marks$repos[[1]]$questions[[1]]$items))
  expect_length(markermd:::load_rubric_items(d, "Q1"), 3)
})

test_that("a database created before snapshots gains a baseline on first touch", {
  d = tempfile("legacy_")
  dir.create(file.path(d, ".markermd"), recursive = TRUE)
  conn = DBI::dbConnect(RSQLite::SQLite(), file.path(d, ".markermd", "markermd.sqlite"))
  DBI::dbExecute(conn, "
    CREATE TABLE items (
      id INTEGER PRIMARY KEY AUTOINCREMENT, question_name TEXT NOT NULL, item_id TEXT NOT NULL,
      hotkey INTEGER, position INTEGER, group_id TEXT, points REAL NOT NULL, description TEXT NOT NULL,
      created_at TEXT NOT NULL, updated_at TEXT NOT NULL, UNIQUE(question_name, item_id))
  ")
  DBI::dbExecute(conn, "CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL, updated_at TEXT NOT NULL)")
  DBI::dbExecute(conn, "INSERT INTO metadata VALUES ('schema_version', '4', '2024-01-01 00:00:00')")
  DBI::dbAppendTable(conn, "items", data.frame(
    question_name = "Q1", item_id = "item_0", hotkey = 1L, position = 1L, group_id = NA_character_,
    points = -1, description = "Legacy", created_at = "2024-01-01 00:00:00", updated_at = "2024-01-01 00:00:00"
  ))
  DBI::dbDisconnect(conn)

  items = markermd:::load_rubric_items(d, "Q1")
  expect_named(items, "item_0")
  rows = markermd:::with_database(d, function(conn) {
    expect_equal(markermd:::get_metadata(conn, "schema_version"), markermd:::markermd_db_version())
    markermd:::snapshot_rows(conn)
  })
  expect_equal(nrow(rows), 1)
  expect_equal(rows$trigger, "migrate to schema 5")
  parts = markermd:::with_database(d, function(conn) markermd:::read_snapshot_parts(conn, rows$snapshot_id))
  expect_null(parts$template)
  expect_equal(parts$rubric$questions[[1]]$items[[1]]$description, "Legacy")

  # A fresh database has nothing to snapshot and gets no baseline
  fresh = tempfile("fresh_"); dir.create(fresh)
  markermd:::with_database(fresh, function(conn) {
    expect_equal(nrow(markermd:::snapshot_rows(conn)), 0)
  })
})


# --- Manual snapshots, listing and resolving ------------------------------------

test_that("snapshot_create records label, summary, metadata and cached scores", {
  d = sn_graded_project()
  id = suppressMessages(snapshot_create(d, label = "Before review", summary = "About to re-check", metadata = list(ticket = 42)))
  expect_match(id, "^snap-[0-9]{8}T[0-9]{6}Z-[0-9a-f]{4}$")

  info = snapshot_info(id, project = d)
  expect_true(S7::S7_inherits(info, markermd_snapshot))
  expect_equal(info@label, "Before review")
  expect_equal(info@summary, "About to re-check")
  expect_equal(info@kind, "manual")
  expect_true(is.na(info@trigger))
  expect_equal(info@notes$key, "ticket")
  expect_equal(info@notes$value, "42")
  expect_equal(info@parts$part, c("template", "rubric", "marks"))
  expect_equal(info@parent_snapshot_id, snapshot_list(d)$snapshot_id[2])
  totals = info@stats[info@stats$question_name == "(total)", ]
  expect_equal(totals$score[totals$assignment_repo == "hw01-team01"], 16)
  expect_true(is.na(totals$score[totals$assignment_repo == "hw01-team02"]))

  rows = snapshot_list(d)
  expect_equal(rows$kind, c("manual", "auto"))
  expect_equal(rows$n_complete[1], 1L)
  expect_equal(rows$mean_total[1], 16)
  expect_equal(nrow(snapshot_list(d, kind = "manual")), 1)
  expect_equal(nrow(snapshot_list(d, n = 1)), 1)
  expect_output(print(rows), "Before review")

  # A manual snapshot of an unchanged state is still created, sharing blobs
  id2 = suppressMessages(snapshot_create(d, label = "Again"))
  expect_equal(nrow(snapshot_list(d)), 3)
  expect_equal(sn_query(d, "SELECT COUNT(*) AS n FROM snapshot_blobs")$n, 5)
  expect_error(snapshot_create(d, label = 1), "label")
  expect_error(snapshot_create(d, metadata = list(1)), "metadata")
})

test_that("snapshot references resolve by id, prefix, label, latest and offset", {
  d = sn_graded_project()
  a = suppressMessages(snapshot_create(d, label = "Alpha"))
  b = suppressMessages(snapshot_create(d, label = "Beta"))
  markermd:::with_database(d, function(conn) {
    expect_equal(markermd:::resolve_snapshot_id(conn, "latest"), b)
    expect_equal(markermd:::resolve_snapshot_id(conn, 0), b)
    expect_equal(markermd:::resolve_snapshot_id(conn, -1), a)
    expect_equal(markermd:::resolve_snapshot_id(conn, a), a)
    expect_equal(markermd:::resolve_snapshot_id(conn, "Alpha"), a)
    expect_equal(markermd:::resolve_snapshot_id(conn, substr(a, 1, nchar(a) - 1)), a)
    expect_error(markermd:::resolve_snapshot_id(conn, "snap-"), "ambiguous")
    expect_error(markermd:::resolve_snapshot_id(conn, "nope"), "No snapshot matches")
    expect_error(markermd:::resolve_snapshot_id(conn, 1), "counts back")
    expect_error(markermd:::resolve_snapshot_id(conn, -9), "does not exist")
  })
  suppressMessages(snapshot_create(d, label = "Alpha"))
  markermd:::with_database(d, function(conn) {
    expect_error(markermd:::resolve_snapshot_id(conn, "Alpha"), "Several snapshots")
  })
  empty = sn_project()
  expect_error(snapshot_info("latest", project = empty), "no snapshots")
})

test_that("snapshot_annotate appends notes and records label and summary changes", {
  d = sn_graded_project()
  id = suppressMessages(snapshot_create(d))
  suppressMessages(snapshot_annotate(id, note = "Reviewed with the TA", project = d))
  suppressMessages(snapshot_annotate(id, metadata = list(review_doc = "notes/review.md"), label = "Named", summary = "Why", project = d))
  info = snapshot_info(id, project = d)
  expect_equal(info@label, "Named")
  expect_equal(info@summary, "Why")
  expect_equal(info@notes$key, c(NA, "review_doc", "label", "summary"))
  expect_equal(info@notes$value, c("Reviewed with the TA", "notes/review.md", "Named", "Why"))
  expect_equal(snapshot_list(d)$n_notes[1], 4L)
  expect_error(snapshot_annotate(id, project = d), "at least one")
})


# --- Diff ---------------------------------------------------------------------

test_that("snapshot_diff reports rewords, point changes, selection changes and score deltas", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "Before"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -2, "Issue A (reworded)"))
  suppressMessages({
    marks_set("hw01-team01", "Q1", items = c("Issue A (reworded)", "Major"), comment = "Careful", project = d, overwrite = TRUE)
    marks_set("hw01-team02", "Q2", items = "Half credit", project = d)
  })

  diff = snapshot_diff("Before", project = d)
  expect_true(S7::S7_inherits(diff, markermd_snapshot_diff))
  expect_equal(diff@to, "current")
  expect_setequal(diff@rubric$change, c("item reworded", "item points"))
  expect_equal(diff@rubric$item, rep("Issue A (reworded)", 2))
  expect_equal(diff@rubric$detail[diff@rubric$change == "item reworded"], "was: Issue A")
  expect_equal(diff@rubric$detail[diff@rubric$change == "item points"], "-3 pts to -2 pts")

  sel = diff@marks[diff@marks$change == "selection", ]
  expect_equal(sel$repo, c("hw01-team01", "hw01-team02"))
  # the rename is not a selection change; only the tier moved
  expect_equal(sel$before[1], "Minor")
  expect_equal(sel$after[1], "Major")
  expect_equal(sel$after[2], "Half credit")
  comment = diff@marks[diff@marks$change == "comment", ]
  expect_equal(comment$after, "Careful")

  expect_equal(nrow(diff@scores), 2)
  team01 = diff@totals[diff@totals$repo == "hw01-team01", ]
  expect_equal(team01$before, 16)
  expect_equal(team01$after, 14)
  expect_equal(team01$change, -2)
  team02 = diff@totals[diff@totals$repo == "hw01-team02", ]
  expect_true(is.na(team02$before))
  expect_equal(team02$after, 15)
  printed = paste(cli::cli_fmt(print(diff)), collapse = "\n")
  expect_match(printed, "item reworded")
  expect_match(printed, "hw01-team01 / Q1")
})

test_that("snapshot_diff matches re-minted items by points and selectors, and sees group and scoring changes", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "Before"))
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.1'", "questions:",
    "- name: Q1", "  scoring:", "    total_score: 12", "    grading_mode: negative",
    "  items:", "  - points: 0", "    description: Looks good", "  - points: -3", "    description: Issue A, restated",
    "  - group: Tiers", "    floor: -3", "    items:",
    "    - points: -1", "      description: Minor", "    - points: -4", "      description: Major"
  ), path)
  suppressMessages(rubric_import(path, project = d, mode = "replace", question = "Q1"))
  # the replace deleted the selections; put them back by description
  suppressMessages(marks_set("hw01-team01", "Q1", items = c("Issue A, restated", "Minor"), project = d, overwrite = TRUE))
  suppressMessages(marks_set("hw01-team02", "Q1", items = "Looks good", project = d, overwrite = TRUE))

  diff = snapshot_diff("Before", project = d)
  changes = diff@rubric[diff@rubric$question == "Q1", ]
  expect_true("item likely reworded" %in% changes$change)
  expect_equal(changes$item[changes$change == "item likely reworded"], "Issue A, restated")
  expect_false("item removed" %in% changes$change)
  expect_false("item added" %in% changes$change)
  bounds = changes[changes$change == "group bounds", ]
  expect_equal(bounds$item, "Tiers")
  expect_match(bounds$detail, "floor none to -3")
  expect_match(bounds$detail, "exclusive TRUE to FALSE")
  scoring = changes[changes$change == "scoring", ]
  expect_match(scoring$detail, "total_score 10 to 12")
  # no selection changes once the rename is mapped
  expect_equal(nrow(diff@marks[diff@marks$change == "selection", ]), 0)
  # the totals moved with the new total_score
  expect_equal(diff@totals$change[diff@totals$repo == "hw01-team01"], 2)
})

test_that("snapshot_diff between two snapshots and with parts restricted", {
  d = sn_graded_project()
  a = suppressMessages(snapshot_create(d, label = "A"))
  suppressMessages(marks_set("hw01-team02", "Q2", items = "Half credit", project = d))
  b = suppressMessages(snapshot_create(d, label = "B"))
  diff = snapshot_diff("A", "B", project = d)
  expect_equal(diff@from, a)
  expect_equal(diff@to, b)
  expect_equal(nrow(diff@marks), 1)
  only_rubric = snapshot_diff("A", "B", project = d, part = "rubric")
  expect_equal(nrow(only_rubric@marks), 0)
  expect_equal(nrow(only_rubric@rubric), 0)
  expect_equal(nrow(only_rubric@scores), 1)
  expect_match(paste(cli::cli_fmt(print(snapshot_diff("B", project = d))), collapse = "\n"), "No rubric changes")
})

test_that("diff_template reports added, removed, renamed and changed questions", {
  from = list(questions = list(list(id = 1, name = "Q1", node_ids = list("a")), list(id = 2, name = "Q2", node_ids = list("b"))))
  to = list(questions = list(list(id = 1, name = "Q1 renamed", node_ids = list("a", "c")), list(id = 3, name = "Q3", scope = "document")))
  out = markermd:::diff_template(from, to)
  expect_setequal(out$change, c("question removed", "question added", "question renamed", "question changed"))
  expect_equal(out$detail[out$change == "question changed"], "nodes")
  expect_equal(nrow(markermd:::diff_template(NULL, NULL)), 0)
  expect_equal(markermd:::diff_template(NULL, to)$change, "template added")
})


# --- Restore ------------------------------------------------------------------

test_that("snapshot_restore brings the rubric, marks and scores back and snapshots the state before", {
  d = sn_graded_project()
  rubric_before = tempfile(fileext = ".yaml"); marks_before = tempfile(fileext = ".yaml")
  rubric_export(rubric_before, project = d); marks_export(marks_before, project = d)
  id = suppressMessages(snapshot_create(d, label = "Before"))
  before = snapshot_scores(id, project = d)

  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -2, "Issue A (reworded)"))
  suppressMessages({
    marks_set("hw01-team01", "Q1", items = c("Issue A (reworded)", "Major"), comment = "Careful", project = d, overwrite = TRUE)
    marks_set("hw01-team02", "Q2", items = "Half credit", private_comment = "later", project = d)
    marks_set("hw01-team02", "Q1", items = "Looks good", comment = "", project = d, overwrite = TRUE)
  })
  n_before = nrow(snapshot_list(d))

  res = suppressMessages(snapshot_restore("Before", project = d, summary = "undo"))
  expect_equal(res$snapshot_id, id)
  expect_match(res$pre_restore, "^snap-")
  rows = snapshot_list(d)
  expect_equal(nrow(rows), n_before + 1)
  expect_equal(rows$trigger[1], glue::glue("snapshot_restore('{id}')"))
  expect_equal(rows$summary[1], "undo")

  after = snapshot_scores(project = d)
  expect_equal(after, before[order(before$assignment_repo, before$question_name), ], ignore_attr = TRUE)

  # The exports equal the exports taken at snapshot time: the rubric replace
  # dropped the grade events of the pair the snapshot did not know, and its
  # private note was cleared, so it no longer counts as marked
  rubric_after = tempfile(fileext = ".yaml"); marks_after = tempfile(fileext = ".yaml")
  rubric_export(rubric_after, project = d); marks_export(marks_after, project = d)
  expect_identical(readLines(rubric_after), readLines(rubric_before))
  expect_identical(readLines(marks_after), readLines(marks_before))
  expect_null(markermd:::load_private_comment(d, "Q2", "hw01-team02") |> (\(x) if (identical(x, "")) NULL else x)())

  # Nothing changed between the snapshot and now
  diff = snapshot_diff("Before", project = d)
  expect_equal(nrow(diff@rubric), 0)
  expect_equal(nrow(diff@scores), 0)
  expect_equal(nrow(diff@marks[diff@marks$change == "selection", ]), 0)

  # The pre-restore snapshot can be restored in turn
  suppressMessages(snapshot_restore(res$pre_restore, project = d))
  expect_equal(markermd:::load_comment(d, "Q1", "hw01-team01"), "Careful")
})

test_that("snapshot_restore narrows to questions and repositories and validates them", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "Before"))
  suppressMessages({
    marks_set("hw01-team01", "Q1", items = "Major", project = d, overwrite = TRUE)
    marks_set("hw01-team01", "Q2", items = "Half credit", project = d, overwrite = TRUE)
  })
  suppressMessages(snapshot_restore("Before", project = d, question = "Q1"))
  expect_setequal(sn_selected(d, "Q1", "hw01-team01"), c("Issue A", "Minor"))
  expect_equal(sn_selected(d, "Q2", "hw01-team01"), "Half credit")
  # Q2's items were left alone (same ids), Q1's were re-created
  expect_named(markermd:::load_rubric_items(d, "Q2"), c("item_4", "item_5"))
  expect_false(any(c("item_0", "item_1") %in% names(markermd:::load_rubric_items(d, "Q1"))))

  expect_error(snapshot_restore("Before", project = d, question = "Q9"), "not in this snapshot")
  expect_error(snapshot_restore("Before", project = d, repo = "nobody"), "not in this snapshot")
  expect_warning(suppressMessages(snapshot_restore("Before", project = d, part = "marks")), "out of step")

  # A repository that vanished is skipped with a warning
  unlink(file.path(d, "repos", "hw01-team02"), recursive = TRUE)
  expect_warning(suppressMessages(snapshot_restore("Before", project = d)), "no longer under")
})

test_that("snapshot_restore restores the template and refuses unknown questions without it", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "Two questions"))
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:", "  path: hw1.qmd", "questions:", "  - id: 1", "    name: Q1"
  ), file.path(d, "one.yaml"))
  suppressMessages(template_import("one.yaml", project = d, summary = "Dropped Q2"))
  expect_equal(markermd:::project_question_names(d)$names, "Q1")

  expect_error(snapshot_restore("Two questions", project = d), "not in the current template")
  suppressMessages(snapshot_restore("Two questions", project = d, part = c("template", "rubric", "marks")))
  expect_equal(markermd:::project_question_names(d)$names, c("Q1", "Q2"))
  expect_equal(nrow(snapshot_diff("Two questions", project = d)@template), 0)
})

test_that("snapshot_restore resets a question's scoring when the snapshot had none", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "Before"))
  markermd:::save_grade_state(d, "Q2", markermd_grade_state(current_score = 0, total_score = 20, grading_mode = "positive"))
  expect_equal(markermd:::load_grade_state(d, "Q2")@total_score, 20)
  suppressMessages(snapshot_restore("Before", project = d))
  expect_null(markermd:::load_grade_state(d, "Q2"))
})


# --- Export, scores, retention, backup -----------------------------------------

test_that("snapshot_export writes valid YAML files and a sidecar", {
  d = sn_graded_project()
  id = suppressMessages(snapshot_create(d, label = "Archive", summary = "Keep", metadata = list(ticket = "7")))
  dir = file.path(d, "archive")
  files = suppressMessages(snapshot_export("Archive", dir, project = d))
  expect_setequal(basename(files), c("markermd-template.yaml", "markermd-rubric.yaml", "markermd-marks.yaml", "snapshot.yaml"))

  rubric = read_rubric_yaml(file.path(dir, "markermd-rubric.yaml"))
  expect_equal(vapply(rubric$questions, function(q) q$name, character(1)), c("Q1", "Q2"))
  marks = read_marks_yaml(file.path(dir, "markermd-marks.yaml"))
  expect_length(marks$repos, 2)
  template = read_template_yaml(file.path(dir, "markermd-template.yaml"))
  expect_equal(markermd:::template_question_names(template), c("Q1", "Q2"))

  sidecar = yaml::read_yaml(file.path(dir, "snapshot.yaml"))
  expect_equal(sidecar$snapshot_id, id)
  expect_equal(sidecar$label, "Archive")
  expect_equal(sidecar$notes[[1]]$key, "ticket")
  expect_setequal(unlist(sidecar$files), basename(files)[-4])

  testthat::skip_if_not_installed("jsonvalidate")
  expect_true(validate_rubric_file(file.path(dir, "markermd-rubric.yaml")))
  expect_true(validate_marks_file(file.path(dir, "markermd-marks.yaml")))
  expect_true(validate_template_file(file.path(dir, "markermd-template.yaml")))
})

test_that("the export functions can write a past snapshot's part", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "Before"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -2, "Changed"))
  suppressMessages(marks_set("hw01-team02", "Q2", items = "Half credit", project = d))

  out = tempfile(fileext = ".yaml")
  rubric_export(out, project = d, snapshot = "Before", question = "Q1")
  rubric = read_rubric_yaml(out)
  expect_equal(vapply(rubric$questions, function(q) q$name, character(1)), "Q1")
  expect_equal(rubric$questions[[1]]$items[[2]]@description, "Issue A")

  marks_export(out, project = d, snapshot = "Before", repo = "hw01-team02")
  marks = read_marks_yaml(out)
  expect_equal(marks$repos[[1]]$name, "hw01-team02")
  expect_length(marks$repos[[1]]$questions, 1)

  template_export(out, project = d, snapshot = "Before")
  expect_equal(markermd:::template_question_names(read_template_yaml(out)), c("Q1", "Q2"))
})

test_that("snapshot_scores returns cached or live scores in the same layout", {
  d = sn_graded_project()
  id = suppressMessages(snapshot_create(d))
  cached = snapshot_scores(id, project = d)
  live = snapshot_scores(project = d)
  expect_equal(cached, live, ignore_attr = TRUE)
  expect_true("(total)" %in% cached$question_name)
  wide = markermd:::snapshot_scores_wide(cached, c("Q1", "Q2"))
  expect_equal(wide$Repository, c("hw01-team01", "hw01-team02"))
  expect_equal(wide$Total, c(16, 10))
  expect_equal(wide$Graded, c(2L, 1L))
})

test_that("exports record a note on the snapshot of the exported state", {
  d = sn_graded_project()
  n_before = nrow(snapshot_list(d))
  suppressMessages(export_scores(d))
  rows = snapshot_list(d)
  expect_equal(nrow(rows), n_before + 1)
  expect_equal(rows$trigger[1], "export_scores()")
  info = snapshot_info("latest", project = d)
  expect_equal(info@notes$key, "export")
  expect_equal(info@notes$value, "scores: scores.csv")
  # A second export of the unchanged state adds a note, not a snapshot
  suppressMessages(export_comments(d))
  expect_equal(nrow(snapshot_list(d)), n_before + 1)
  info = snapshot_info("latest", project = d)
  expect_equal(info@notes$key, c("export", "trigger", "export"))
  sitrep = paste(cli::cli_fmt(project_sitrep(d)), collapse = "\n")
  expect_match(sitrep, "scores exported")
  expect_match(sitrep, "state unchanged since")
})

test_that("snapshot_state_status reports changes and events since the latest snapshot", {
  d = sn_graded_project()
  status = markermd:::with_database(d, markermd:::snapshot_state_status)
  expect_true(status$changed)
  suppressMessages(snapshot_create(d))
  status = markermd:::with_database(d, markermd:::snapshot_state_status)
  expect_false(status$changed)
  expect_equal(status$events_since, 0L)
  markermd:::save_grade_selection(d, "Q1", "hw01-team02", "item_1", TRUE)
  status = markermd:::with_database(d, markermd:::snapshot_state_status)
  expect_true(status$changed)
  expect_equal(status$events_since, 1L)
  expect_match(paste(cli::cli_fmt(project_sitrep(d)), collapse = "\n"), "state changed since the latest snapshot")
  expect_equal(markermd:::format_age(Sys.time() - 5), "just now")
  expect_equal(markermd:::format_age(Sys.time() - 3 * 3600), "3 h ago")
})

test_that("pin, delete and prune respect manual and pinned snapshots", {
  d = sn_graded_project()
  auto_id = snapshot_list(d)$snapshot_id[1]
  manual = suppressMessages(snapshot_create(d, label = "Keep"))
  expect_error(snapshot_delete("Keep", project = d), "manual")
  suppressMessages(snapshot_pin(auto_id, project = d))
  expect_true(snapshot_info(auto_id, project = d)@pinned)
  expect_error(snapshot_delete(auto_id, project = d), "pinned")
  suppressMessages(snapshot_pin(auto_id, pinned = FALSE, project = d))

  # Three more automatic snapshots, spaced by edits
  for (i in 1:3) {
    markermd:::save_rubric_item(d, "Q2", "item_5", markermd_rubric_item(2L, 10, paste("Full credit", i)))
    suppressMessages(rubric_import(sn_rubric_yaml(), project = d, mode = "replace", question = "Q2"))
  }
  rows = snapshot_list(d)
  n = nrow(rows)
  # Nothing is younger than 30 days, so the default prunes nothing
  expect_length(suppressMessages(snapshot_prune(d)), 0)
  # Keep none by age or count: the latest, the oldest baseline and the manual survive
  pruned = suppressMessages(snapshot_prune(d, keep_last = 0, keep_days = 0))
  remaining = snapshot_list(d)
  expect_equal(nrow(remaining), n - length(pruned))
  expect_true(rows$snapshot_id[1] %in% remaining$snapshot_id)
  expect_true(auto_id %in% remaining$snapshot_id)
  expect_true(manual %in% remaining$snapshot_id)
  expect_false(any(pruned %in% remaining$snapshot_id))
  # Blobs no snapshot (or activity log entry) references were collected
  blobs = sn_query(d, "SELECT COUNT(*) AS n FROM snapshot_blobs")$n
  referenced = sn_query(d, "SELECT COUNT(*) AS n FROM (SELECT hash FROM snapshot_parts UNION SELECT hash FROM action_parts)")$n
  expect_equal(blobs, referenced)

  suppressMessages(snapshot_delete("Keep", project = d, force = TRUE))
  expect_false(manual %in% snapshot_list(d)$snapshot_id)
})

test_that("project_backup copies the database and config and notes the latest snapshot", {
  d = sn_graded_project()
  path = suppressMessages(project_backup(d))
  expect_true(file.exists(path))
  expect_match(basename(path), "^markermd-[0-9]{8}T[0-9]{6}Z\\.sqlite$")
  expect_length(list.files(file.path(d, "backups"), pattern = "^config-.*\\.yml$"), 1)
  copy = DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(copy))
  expect_equal(DBI::dbGetQuery(copy, "SELECT COUNT(*) AS n FROM items")$n, 6)
  info = snapshot_info("latest", project = d)
  expect_equal(info@notes$key[nrow(info@notes)], "backup")
  expect_match(info@notes$value[nrow(info@notes)], "^backups/markermd-")
})

test_that("template_snapshot_choices lists distinct template versions newest first", {
  d = sn_graded_project()
  a = suppressMessages(snapshot_create(d, label = "A"))
  b = suppressMessages(snapshot_create(d, label = "B"))
  choices = markermd:::template_snapshot_choices(d)
  # A and B share the template with the import's automatic snapshot, so only
  # the oldest of the run is offered
  expect_length(choices, 1)
  expect_false(b %in% choices)
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:", "  path: hw1.qmd", "questions:", "  - id: 1", "    name: Q1"
  ), file.path(d, "one.yaml"))
  suppressMessages(template_import("one.yaml", project = d))
  c_id = suppressMessages(snapshot_create(d, label = "C"))
  choices = markermd:::template_snapshot_choices(d)
  expect_length(choices, 2)
  expect_equal(unname(choices[1]), c_id)
  expect_match(names(choices)[1], "C$")
})

test_that("the history table helpers derive deltas, search text and metadata", {
  d = sn_graded_project()
  suppressMessages(snapshot_create(d, label = "One", summary = "first"))
  suppressMessages(marks_set("hw01-team02", "Q2", items = "Half credit", project = d))
  suppressMessages(snapshot_create(d, label = "Two"))
  rows = markermd:::with_database(d, function(conn) markermd:::as_snapshot_list(markermd:::snapshot_rows(conn)))
  data = markermd:::history_table_data(rows, c(x = "unused"))
  expect_equal(data$delta_mean[1], 15.5 - 16)
  expect_true(is.na(data$delta_mean[nrow(data)]))
  expect_match(data$search_text[2], "one first")
  expect_s3_class(markermd:::history_reactable(data), "reactable")
  expect_length(markermd:::snapshot_select_choices(data[0, ]), 0)
  expect_equal(unname(markermd:::snapshot_select_choices(data)[1]), data$snapshot_id[1])
  expect_match(names(markermd:::snapshot_select_choices(data))[2], "One$")
  expect_equal(markermd:::parse_metadata_lines("a: 1\nb:two\n\nc"), list(a = "1", b = "two", c = ""))
  expect_equal(markermd:::parse_metadata_lines(""), list())
  html = as.character(htmltools::renderTags(markermd:::snapshot_diff_ui(snapshot_diff("One", project = d)))$html)
  expect_match(html, "Half credit")
  expect_match(html, "hw01-team02")
})
