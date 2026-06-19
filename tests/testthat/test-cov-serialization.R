# Coverage tests for the serialization layer:
#   R/template-serialization.R  (metadata_to_list / metadata_from_list)
#   R/marks-serialization.R     (marks_question_from_list)

cs_make_metadata = function(created_by, created_at) {
  markermd:::markermd_metadata(created_by = created_by, created_at = created_at)
}

test_that("metadata round-trips created_by and created_at", {
  created_at = as.POSIXct("2024-01-02 03:04:05", tz = "America/New_York")
  meta = cs_make_metadata(created_by = "alice", created_at = created_at)

  lst = markermd:::metadata_to_list(meta)
  back = markermd:::metadata_from_list(lst, version = "1.0", total_nodes = 5)

  expect_equal(back@created_by, "alice")
  # The same instant survives the list round-trip even though the emitted
  # string carries the numeric offset rather than the original tz name.
  expect_equal(as.numeric(back@created_at), as.numeric(created_at))
  expect_equal(back@version, "1.0")
  expect_equal(back@total_nodes, 5L)
})

test_that("metadata_from_list parses an offsetless created_at as a UTC instant", {
  back = markermd:::metadata_from_list(
    list(created_at = "2024-01-02T03:04:05"),
    version = "1.0",
    total_nodes = 0
  )

  expected = as.POSIXct("2024-01-02T03:04:05", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  # The offsetless timestamp must not be silently dropped to the current time.
  expect_equal(as.numeric(back@created_at), as.numeric(expected))
  expect_false(is.na(back@created_at))
})

test_that("marks_question_from_list rejects a non-scalar comment", {
  expect_error(
    markermd:::marks_question_from_list(
      list(name = "Q1", items = list("a"), comment = list("a", "b"))
    ),
    "non-scalar 'comment'"
  )
})

test_that("marks_question_from_list rejects a non-scalar private_comment", {
  expect_error(
    markermd:::marks_question_from_list(
      list(name = "Q1", items = list("a"), private_comment = list("a", "b"))
    ),
    "non-scalar 'private_comment'"
  )
})

test_that("marks_question_from_list requires name and items", {
  expect_error(
    markermd:::marks_question_from_list(list(items = list("a"))),
    "non-empty 'name'"
  )
  expect_error(
    markermd:::marks_question_from_list(list(name = "Q1")),
    "missing the required 'items' field"
  )

  ok = markermd:::marks_question_from_list(
    list(name = "Q1", items = list("a", "b"), comment = "hi")
  )
  expect_equal(ok$name, "Q1")
  expect_equal(ok$items, c("a", "b"))
  expect_equal(ok$comment, "hi")
  expect_null(ok$private_comment)
})
