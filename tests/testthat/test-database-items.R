test_that("delete_rubric_item removes the item row and its grade events", {
  dir = tempfile("itemdel")
  dir.create(dir)

  item = markermd::markermd_rubric_item(hotkey = 1L, points = 2, description = "off by one")
  markermd:::save_rubric_item(dir, "Q1", "item_0", item)
  markermd:::save_rubric_item(dir, "Q1", "item_1", item)
  markermd:::save_rubric_item(dir, "Q2", "item_2", item)

  markermd:::with_database(dir, function(conn) {
    DBI::dbExecute(
      conn,
      "INSERT INTO grades (question_name, assignment_repo, item_id, selected, timestamp, username) VALUES (?,?,?,?,?,?)",
      params = list("Q1", "repoA", "item_0", 1L, "2024-01-01 00:00:01", "tester")
    )
    invisible()
  })

  expect_named(markermd:::load_rubric_items(dir, "Q1"), c("item_0", "item_1"))
  expect_equal(unname(markermd:::calculate_grading_progress(dir, "Q1", "repoA")), 1L)

  markermd:::delete_rubric_item(dir, "Q1", "item_0")

  # the row is gone, same-named items elsewhere are untouched
  expect_named(markermd:::load_rubric_items(dir, "Q1"), "item_1")
  expect_named(markermd:::load_rubric_items(dir, "Q2"), "item_2")

  # the deleted item's grade events no longer count the repo as graded
  expect_equal(unname(markermd:::calculate_grading_progress(dir, "Q1", "repoA")), 0L)
})


test_that("load_rubric_items orders by stored position so reorders survive a restart", {
  dir = tempfile("itemorder")
  dir.create(dir)

  a = markermd::markermd_rubric_item(hotkey = 1L, points = 1, description = "first")
  b = markermd::markermd_rubric_item(hotkey = 2L, points = 1, description = "second")
  c = markermd::markermd_rubric_item(hotkey = 3L, points = 1, description = "third")

  # saved one at a time (as Add Item does) each new item is appended
  markermd:::save_rubric_item(dir, "Q1", "item_a", a)
  markermd:::save_rubric_item(dir, "Q1", "item_b", b)
  markermd:::save_rubric_item(dir, "Q1", "item_c", c)
  expect_named(markermd:::load_rubric_items(dir, "Q1"), c("item_a", "item_b", "item_c"))

  # a whole-question write (what a move persists) sets the display order, and
  # the hotkeys follow from it regardless of what the objects carried
  markermd:::save_rubric_items(dir, "Q1", list(item_b = b, item_c = c, item_a = a))
  items = markermd:::load_rubric_items(dir, "Q1")
  expect_named(items, c("item_b", "item_c", "item_a"))
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), 1:3)

  # editing one item's points keeps its position
  markermd:::save_rubric_item(dir, "Q1", "item_c", markermd::markermd_rubric_item(hotkey = 2L, points = 5, description = "third"))
  items = markermd:::load_rubric_items(dir, "Q1")
  expect_named(items, c("item_b", "item_c", "item_a"))
  expect_equal(items$item_c@points, 5)
})


test_that("reordering items past the tenth survives a reload and an export", {
  dir = tempfile("itemorder12")
  dir.create(dir)

  ids = paste0("item_", LETTERS[1:12])
  items = setNames(lapply(1:12, function(i) {
    markermd::markermd_rubric_item(
      hotkey = markermd:::position_hotkey(i), points = -1, description = LETTERS[i]
    )
  }), ids)
  markermd:::save_rubric_items(dir, "Q1", items)
  expect_named(markermd:::load_rubric_items(dir, "Q1"), ids)

  # L moved above K: both are past the tenth and have no hotkey, so before
  # positions were stored this came back in insertion order (K, L)
  moved = items[c(1:10, 12, 11)]
  markermd:::save_rubric_items(dir, "Q1", moved)
  reloaded = markermd:::load_rubric_items(dir, "Q1")
  expect_named(reloaded, names(moved))
  expect_equal(unname(purrr::map_int(reloaded, ~ .x@hotkey)), c(1:10, NA, NA))

  # A moved up from the top wraps to the end
  wrapped = items[c(2:12, 1)]
  markermd:::save_rubric_items(dir, "Q1", wrapped)
  expect_named(markermd:::load_rubric_items(dir, "Q1"), names(wrapped))

  exported = markermd:::collect_rubric_data(dir, "Q1")
  expect_equal(
    unname(purrr::map_chr(exported$questions[[1]]$items, ~ .x@description)),
    LETTERS[c(2:12, 1)]
  )
})


test_that("an items row without a position loads in its legacy place until renumbered", {
  dir = tempfile("itemnopos")
  dir.create(dir)

  markermd:::save_rubric_items(dir, "Q1", list(
    item_a = markermd::markermd_rubric_item(1L, 1, "a"),
    item_b = markermd::markermd_rubric_item(2L, 1, "b")
  ))
  # a row written without a position (by hand, or by a path that predates
  # the column) must not sort first as a bare NULL
  markermd:::with_database(dir, function(conn) {
    DBI::dbExecute(conn, "
      INSERT INTO items (question_name, item_id, hotkey, points, description, created_at, updated_at)
      VALUES ('Q1', 'item_z', NULL, 1, 'z', '2024-01-01 00:00:00', '2024-01-01 00:00:00')
    ")
  })

  items = markermd:::load_rubric_items(dir, "Q1")
  expect_named(items, c("item_a", "item_b", "item_z"))
  expect_true(is.na(items$item_z@hotkey))

  # the next whole-question write gives it a position and hotkey
  markermd:::save_rubric_items(dir, "Q1", items)
  items = markermd:::load_rubric_items(dir, "Q1")
  expect_named(items, c("item_a", "item_b", "item_z"))
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), 1:3)
})


test_that("a schema-version-1 database gains item positions in its legacy display order", {
  dir = tempfile("itemmigrate")
  dir.create(file.path(dir, ".markermd"), recursive = TRUE)

  # Build the pre-position items table by hand: hotkeys 1-10 carried the order
  # and items past the tenth had none (so they displayed in insertion order)
  conn = DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".markermd", "markermd.sqlite"))
  DBI::dbExecute(conn, "
    CREATE TABLE items (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      question_name TEXT NOT NULL,
      item_id TEXT NOT NULL,
      hotkey INTEGER,
      points REAL NOT NULL,
      description TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      UNIQUE(question_name, item_id)
    )
  ")
  DBI::dbExecute(conn, "CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL, updated_at TEXT NOT NULL)")
  DBI::dbExecute(conn, "INSERT INTO metadata VALUES ('schema_version', '1', '2024-01-01 00:00:00')")
  DBI::dbAppendTable(conn, "items", data.frame(
    question_name = c(rep("Q1", 12), "Q2", "Q2"),
    item_id = c(paste0("item_", 1:12), "item_x", "item_y"),
    # inserted out of display order: hotkey 2 before hotkey 1, then the
    # un-hotkeyed tail; Q2 has its own numbering
    hotkey = c(2L, 1L, 3:10, NA, NA, 1L, 2L),
    points = -1,
    description = c(paste("Q1", 1:12), "x", "y"),
    created_at = "2024-01-01 00:00:00",
    updated_at = "2024-01-01 00:00:00"
  ))
  DBI::dbDisconnect(conn)

  # any markermd access migrates the database in place
  items = markermd:::load_rubric_items(dir, "Q1")
  expect_named(items, paste0("item_", c(2, 1, 3:12)))
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), c(1:10, NA, NA))

  markermd:::with_database(dir, function(conn) {
    # the position migration stamps 2 and the group migration that follows
    # it (test-rubric-groups.R) stamps the current version
    expect_equal(markermd:::get_metadata(conn, "schema_version"), markermd:::markermd_db_version())

    q1 = DBI::dbGetQuery(conn, "SELECT item_id, position FROM items WHERE question_name = 'Q1' ORDER BY position")
    expect_equal(q1$item_id, paste0("item_", c(2, 1, 3:12)))
    expect_equal(q1$position, 1:12)

    q2 = DBI::dbGetQuery(conn, "SELECT item_id, position FROM items WHERE question_name = 'Q2' ORDER BY position")
    expect_equal(q2$item_id, c("item_x", "item_y"))
    expect_equal(q2$position, 1:2)
  })

  # from then on a reorder past the tenth is kept
  reordered = items[c(1:10, 12, 11)]
  markermd:::save_rubric_items(dir, "Q1", reordered)
  expect_named(markermd:::load_rubric_items(dir, "Q1"), names(reordered))
})
