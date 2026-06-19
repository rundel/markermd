# Coverage tests for the SQLite grading database layer
# (R/database_integration.R, R/utils_database.R)

# Insert a grades row directly with a caller-supplied id and timestamp so we can
# force same-second ties and control which row holds the larger autoincrement id.
#
# dir: project root
# id: explicit grades.id
# question/repo/item: pair + item identifiers
# selected: 0 or 1
# ts: fixed timestamp string

cdb_insert_grade_row = function(dir, id, question, repo, item, selected, ts) {
  markermd:::with_database(dir, function(conn) {
    DBI::dbExecute(
      conn,
      "INSERT INTO grades (id, question_name, assignment_repo, item_id, selected, timestamp, username)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(id, question, repo, item, as.integer(selected), ts, "tester")
    )
    invisible()
  })
}

# Insert an items row directly, bypassing the markermd_rubric_item S7 validator
# so we can seed hotkeys the validator would reject (non-contiguous, >10).
#
# dir: project root
# question/item: identifiers
# hotkey: raw hotkey integer (NA allowed)
# points/description: item payload

cdb_insert_item_row = function(dir, question, item, hotkey, points, description) {
  markermd:::with_database(dir, function(conn) {
    DBI::dbExecute(
      conn,
      "INSERT INTO items (question_name, item_id, hotkey, points, description, created_at, updated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(
        question, item,
        if (is.na(hotkey)) NA_integer_ else as.integer(hotkey),
        points, description, "2024-01-01 00:00:00", "2024-01-01 00:00:00"
      )
    )
    invisible()
  })
}

# Build a minimal rubric exchange list (one question, given items) for
# apply_rubric_import(). Mirrors read_rubric_yaml()'s output shape: each item is
# a markermd_rubric_item, scoring is NULL.
#
# question_name: question to attach items to
# descriptions: character vector of item descriptions to add

cdb_rubric = function(question_name, descriptions) {
  items = lapply(seq_along(descriptions), function(i) {
    markermd::markermd_rubric_item(
      hotkey = if (i <= 10) as.integer(i) else NA_integer_,
      points = -1,
      description = descriptions[[i]]
    )
  })
  list(
    format_version = markermd:::markermd_rubric_version(),
    questions = list(list(name = question_name, scoring = NULL, items = items))
  )
}


test_that("same-second grade tie-break: latest id wins (selected then deselected)", {
  dir = tempfile("cdb_tie1")
  dir.create(dir)
  ts = "2024-03-01 12:00:00"

  # Two events for the same pair/item at the IDENTICAL timestamp; the larger id
  # (the deselection) must win every most-recent query.
  cdb_insert_grade_row(dir, 1L, "Q1", "repoA", "item_0", 1L, ts)
  cdb_insert_grade_row(dir, 2L, "Q1", "repoA", "item_0", 0L, ts)

  sel = markermd:::load_grade_selections(dir, "Q1", "repoA")
  expect_equal(sel[["item_0"]], FALSE)

  recent = markermd:::with_database(dir, function(conn) markermd:::load_most_recent_grades(conn))
  expect_equal(nrow(recent), 1L)
  expect_equal(recent$selected, 0L)
  expect_equal(recent$id, 2L)

  # deselection is the winner, so the pair is NOT graded
  pairs = markermd:::graded_question_pairs(dir)
  expect_equal(nrow(pairs), 0L)
})


test_that("same-second grade tie-break flips when insertion order reverses", {
  dir = tempfile("cdb_tie2")
  dir.create(dir)
  ts = "2024-03-01 12:00:00"

  # Reverse order: deselect (id 1) then select (id 2). Latest id wins -> selected.
  cdb_insert_grade_row(dir, 1L, "Q1", "repoA", "item_0", 0L, ts)
  cdb_insert_grade_row(dir, 2L, "Q1", "repoA", "item_0", 1L, ts)

  sel = markermd:::load_grade_selections(dir, "Q1", "repoA")
  expect_equal(sel[["item_0"]], TRUE)

  recent = markermd:::with_database(dir, function(conn) markermd:::load_most_recent_grades(conn))
  expect_equal(nrow(recent), 1L)
  expect_equal(recent$selected, 1L)
  expect_equal(recent$id, 2L)

  pairs = markermd:::graded_question_pairs(dir)
  expect_equal(nrow(pairs), 1L)
  expect_equal(pairs$question_name, "Q1")
  expect_equal(pairs$assignment_repo, "repoA")
})


test_that("apply_rubric_import renumbers kept items to 1..10 then NA in append mode", {
  dir = tempfile("cdb_renumber")
  dir.create(dir)

  # Seed 11 kept items with hotkeys the S7 validator would reject: a
  # non-contiguous gap (5,9) at the head and hotkeys clamped at 10 / dropped to
  # NA past ten. Display order is ORDER BY (hotkey IS NULL), hotkey, id, so:
  #   keep_a(5), keep_b(9), keep_c(10), keep_d..keep_j (NA, insertion order)
  cdb_insert_item_row(dir, "Q1", "keep_a", 5L, -1, "a")
  cdb_insert_item_row(dir, "Q1", "keep_b", 9L, -1, "b")
  cdb_insert_item_row(dir, "Q1", "keep_c", 10L, -1, "c")
  for (k in letters[4:11]) {
    cdb_insert_item_row(dir, "Q1", paste0("keep_", k), NA, -1, k)
  }

  rubric = cdb_rubric("Q1", c("new one", "new two"))
  summaries = markermd:::apply_rubric_import(dir, rubric, mode = "append")

  items = markermd:::load_rubric_items(dir, "Q1")

  # 11 kept + 2 new = 13 items, in display order
  expect_length(items, 13L)

  hotkeys = vapply(items, function(it) {
    if (is.na(it@hotkey)) NA_integer_ else as.integer(it@hotkey)
  }, integer(1))
  expect_equal(unname(hotkeys), c(1:10, NA, NA, NA))

  # display order: kept items first (by their old hotkey ordering), then the two
  # imported items at the tail
  expect_equal(
    names(items)[1:3],
    c("keep_a", "keep_b", "keep_c")
  )
  new_ids = summaries[["Q1"]]$new_ids
  expect_equal(tail(names(items), 2L), new_ids)

  # hotkey_changes names every kept item whose stored hotkey did not already
  # equal its display position. keep_a was 5 -> 1, keep_b 9 -> 2, keep_c 10 -> 3,
  # keep_d..keep_j NA -> 4..10, keep_k NA -> NA (position 11, past ten). keep_k
  # was already NA at a >10 position, so it is unchanged and absent from changes.
  changes = summaries[["Q1"]]$hotkey_changes
  expect_true("keep_a" %in% names(changes))
  expect_true("keep_b" %in% names(changes))
  expect_equal(changes[["keep_a"]], 1L)
  expect_equal(changes[["keep_b"]], 2L)
  expect_equal(changes[["keep_c"]], 3L)
  expect_equal(changes[["keep_j"]], 10L)
  expect_false("keep_k" %in% names(changes))

  # imported items take positions after the kept ones (11, 12 -> no hotkey)
  expect_equal(summaries[["Q1"]]$n_existing, 11L)
  expect_true(all(is.na(vapply(summaries[["Q1"]]$new_items, function(it) it@hotkey, integer(1)))))
})


test_that("apply_rubric_import leaves an already-correct kept hotkey out of hotkey_changes", {
  dir = tempfile("cdb_nochange")
  dir.create(dir)

  # Two kept items already at their display positions (1, 2): renumbering is a
  # no-op for them, so neither appears in hotkey_changes.
  cdb_insert_item_row(dir, "Q1", "keep_a", 1L, -1, "a")
  cdb_insert_item_row(dir, "Q1", "keep_b", 2L, -1, "b")

  summaries = markermd:::apply_rubric_import(dir, cdb_rubric("Q1", "new"), mode = "append")

  expect_length(summaries[["Q1"]]$hotkey_changes, 0L)

  items = markermd:::load_rubric_items(dir, "Q1")
  expect_equal(names(items)[1:2], c("keep_a", "keep_b"))
  expect_equal(unname(vapply(items, function(it) as.integer(it@hotkey), integer(1))), c(1L, 2L, 3L))
})


test_that("upsert_settings UPDATE branch overwrites the row in place", {
  dir = tempfile("cdb_settings")
  dir.create(dir)

  # First write (INSERT branch)
  markermd:::save_grade_state(
    dir, "Q1",
    markermd::markermd_grade_state(
      current_score = 3, total_score = 10, grading_mode = "positive",
      bound_above_zero = TRUE, bound_below_max = TRUE
    )
  )

  first = markermd:::load_grade_state(dir, "Q1")
  expect_equal(first@grading_mode, "positive")
  expect_equal(first@total_score, 10)

  # Second write for the same question (UPDATE branch): flip mode and bounds
  markermd:::save_grade_state(
    dir, "Q1",
    markermd::markermd_grade_state(
      current_score = 7, total_score = 20, grading_mode = "negative",
      bound_above_zero = FALSE, bound_below_max = FALSE
    )
  )

  second = markermd:::load_grade_state(dir, "Q1")
  expect_equal(second@grading_mode, "negative")
  expect_equal(second@total_score, 20)
  expect_equal(second@current_score, 7)
  expect_false(second@bound_above_zero)
  expect_false(second@bound_below_max)

  # UPDATE must not create a second row for Q1
  all_settings = markermd:::with_database(dir, function(conn) markermd:::load_all_settings(conn))
  expect_equal(nrow(all_settings), 1L)
  expect_equal(all_settings$question_name, "Q1")
})
