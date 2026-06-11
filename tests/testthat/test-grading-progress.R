insert_grade = function(conn, question, repo, item, selected, ts) {
  DBI::dbExecute(
    conn,
    "INSERT INTO grades (question_name, assignment_repo, item_id, selected, timestamp, username) VALUES (?,?,?,?,?,?)",
    params = list(question, repo, item, as.integer(selected), ts, "tester")
  )
}

insert_comment = function(conn, question, repo, text, ts) {
  DBI::dbExecute(
    conn,
    "INSERT INTO comments (question_name, assignment_repo, comment_text, timestamp, username) VALUES (?,?,?,?,?)",
    params = list(question, repo, text, ts, "tester")
  )
}

insert_private_comment = function(conn, question, repo, text, ts) {
  DBI::dbExecute(
    conn,
    "INSERT INTO private_comments (question_name, assignment_repo, comment_text, timestamp, username) VALUES (?,?,?,?,?)",
    params = list(question, repo, text, ts, "tester")
  )
}


test_that("calculate_grading_progress counts graded questions by most-recent state", {
  dir = tempfile("gradeprog")
  dir.create(dir)

  markermd:::with_database(dir, function(conn) {
    # repoA / Q1: a selected rubric item -> graded
    insert_grade(conn, "Q1", "repoA", "i1", 1, "2024-01-01 00:00:01")
    # repoA / Q2: selected then deselected (latest grade is 0) -> not graded
    insert_grade(conn, "Q2", "repoA", "i1", 1, "2024-01-01 00:00:01")
    insert_grade(conn, "Q2", "repoA", "i1", 0, "2024-01-01 00:00:02")
    # repoB / Q1: a non-empty most-recent comment -> graded
    insert_comment(conn, "Q1", "repoB", "good work", "2024-01-01 00:00:05")
    # repoB / Q2: most-recent comment is blank (older one was not) -> not graded
    insert_comment(conn, "Q2", "repoB", "draft", "2024-01-01 00:00:01")
    insert_comment(conn, "Q2", "repoB", "   ", "2024-01-01 00:00:09")
    invisible()
  })

  progress = markermd:::calculate_grading_progress(dir, c("Q1", "Q2"), c("repoA", "repoB", "repoC"))

  expect_named(progress, c("repoA", "repoB", "repoC"))
  expect_equal(unname(progress[["repoA"]]), 1L)
  expect_equal(unname(progress[["repoB"]]), 1L)
  expect_equal(unname(progress[["repoC"]]), 0L)
})


test_that("calculate_grading_progress restricts to requested questions and repos", {
  dir = tempfile("gradeprog")
  dir.create(dir)

  markermd:::with_database(dir, function(conn) {
    insert_grade(conn, "Q1", "repoA", "i1", 1, "2024-01-01 00:00:01")
    insert_grade(conn, "Q3", "repoA", "i1", 1, "2024-01-01 00:00:01")
    invisible()
  })

  # Q3 and repoB are excluded from the request
  progress = markermd:::calculate_grading_progress(dir, c("Q1", "Q2"), "repoA")
  expect_equal(unname(progress[["repoA"]]), 1L)

  expect_length(markermd:::calculate_grading_progress(dir, character(0), "repoA"), 0)
})


test_that("marked_question_pairs counts activity that graded_question_pairs ignores", {
  dir = tempfile("gradeprog")
  dir.create(dir)

  markermd:::with_database(dir, function(conn) {
    # repoA / Q1: selected then deselected -> marked, not graded
    insert_grade(conn, "Q1", "repoA", "i1", 1, "2024-01-01 00:00:01")
    insert_grade(conn, "Q1", "repoA", "i1", 0, "2024-01-01 00:00:02")
    # repoA / Q2: only a private comment -> marked, not graded
    insert_private_comment(conn, "Q2", "repoA", "machine note", "2024-01-01 00:00:03")
    # repoB / Q1: selected item -> marked and graded
    insert_grade(conn, "Q1", "repoB", "i1", 1, "2024-01-01 00:00:04")
    # repoB / Q2: latest private comment is blank -> neither
    insert_private_comment(conn, "Q2", "repoB", "draft", "2024-01-01 00:00:05")
    insert_private_comment(conn, "Q2", "repoB", "   ", "2024-01-01 00:00:06")
    # repoC / Q1: non-empty public comment -> marked and graded
    insert_comment(conn, "Q1", "repoC", "good work", "2024-01-01 00:00:07")
    invisible()
  })

  marked = markermd:::marked_question_pairs(dir)
  marked_keys = sort(paste(marked$assignment_repo, marked$question_name))
  expect_equal(marked_keys, sort(c("repoA Q1", "repoA Q2", "repoB Q1", "repoC Q1")))

  graded = markermd:::graded_question_pairs(dir)
  graded_keys = sort(paste(graded$assignment_repo, graded$question_name))
  expect_equal(graded_keys, sort(c("repoB Q1", "repoC Q1")))

  progress = markermd:::calculate_grading_progress(dir, c("Q1", "Q2"), c("repoA", "repoB", "repoC"))
  expect_equal(unname(progress[["repoA"]]), 0L)
  expect_equal(unname(progress[["repoB"]]), 1L)
  expect_equal(unname(progress[["repoC"]]), 1L)
})
