test_that("private comments round trip with latest-row-wins semantics", {
  dir = tempfile("privcomment")
  dir.create(dir)

  expect_null(markermd:::load_private_comment(dir, "Q1", "repoA"))

  markermd:::save_private_comment(dir, "Q1", "repoA", "first note")
  expect_equal(markermd:::load_private_comment(dir, "Q1", "repoA"), "first note")

  markermd:::save_private_comment(dir, "Q1", "repoA", "revised note")
  expect_equal(markermd:::load_private_comment(dir, "Q1", "repoA"), "revised note")

  recent = markermd:::with_database(dir, markermd:::load_most_recent_private_comments)
  expect_equal(nrow(recent), 1L)
  expect_equal(recent$comment_text, "revised note")
})


test_that("private_comments table is added to an existing database on reopen", {
  dir = tempfile("privcomment")
  dir.create(dir)

  markermd:::with_database(dir, function(conn) {
    DBI::dbExecute(conn, "DROP TABLE private_comments")
    expect_false(DBI::dbExistsTable(conn, "private_comments"))
    invisible()
  })

  markermd:::with_database(dir, function(conn) {
    expect_true(DBI::dbExistsTable(conn, "private_comments"))
    invisible()
  })
})


test_that("public and private comment channels are independent", {
  dir = tempfile("privcomment")
  dir.create(dir)

  markermd:::save_comment(dir, "Q1", "repoA", "public feedback")
  markermd:::save_private_comment(dir, "Q1", "repoA", "internal note")

  expect_equal(markermd:::load_comment(dir, "Q1", "repoA"), "public feedback")
  expect_equal(markermd:::load_private_comment(dir, "Q1", "repoA"), "internal note")

  markermd:::save_private_comment(dir, "Q2", "repoA", "private only")
  expect_null(markermd:::load_comment(dir, "Q2", "repoA"))

  markermd:::save_comment(dir, "Q3", "repoA", "public only")
  expect_null(markermd:::load_private_comment(dir, "Q3", "repoA"))
})
