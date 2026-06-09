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
