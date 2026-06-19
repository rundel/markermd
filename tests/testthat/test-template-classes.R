# Direct coverage for the S7 class validators. These ran only indirectly before;
# the NA guards in particular regressed a base "missing value" error.

test_that("markermd_question@id rejects NA and non-positive ids", {
  expect_error(markermd_question(id = NA_integer_, name = "Q"), "positive integer")
  expect_error(markermd_question(id = 0L, name = "Q"), "positive integer")
  expect_s7_class(markermd_question(id = 1L, name = "Q"), markermd_question)
})

test_that("markermd_node_selection rejects NA, empty, and duplicate ids", {
  expect_error(markermd_node_selection(node_ids = c("a", NA)), "must not be NA")
  expect_error(markermd_node_selection(node_ids = c("a", "")), "non-empty")
  expect_error(markermd_node_selection(node_ids = c("a", "a")), "unique")
  expect_s7_class(markermd_node_selection(node_ids = c("a", "b")), markermd_node_selection)
})

test_that("markermd_metadata@total_nodes rejects NA and negatives", {
  expect_error(markermd_metadata(total_nodes = NA_integer_), "non-negative")
  expect_error(markermd_metadata(total_nodes = -1L), "non-negative")
  expect_s7_class(markermd_metadata(total_nodes = 0L), markermd_metadata)
})

test_that("markermd_grade_state@total_score rejects NA and negatives", {
  expect_error(markermd_grade_state(total_score = NA_real_), "non-negative")
  expect_error(markermd_grade_state(total_score = -5), "non-negative")
  expect_s7_class(markermd_grade_state(total_score = 10), markermd_grade_state)
})

test_that("markermd_rubric_item@hotkey permits NA and 1-10 but nothing else", {
  expect_s7_class(markermd_rubric_item(hotkey = NA_integer_, points = 0, description = "x"), markermd_rubric_item)
  expect_s7_class(markermd_rubric_item(hotkey = 1L, points = 0, description = "x"), markermd_rubric_item)
  expect_s7_class(markermd_rubric_item(hotkey = 10L, points = 0, description = "x"), markermd_rubric_item)
  expect_error(markermd_rubric_item(hotkey = 0L, points = 0, description = "x"), "between 1 and 10")
  expect_error(markermd_rubric_item(hotkey = 11L, points = 0, description = "x"), "between 1 and 10")
})

test_that("markermd_template rejects duplicate question ids and names", {
  ast = q2r::parse_qmd("# x", quiet = TRUE)
  dup_id = list(
    markermd_question(id = 1L, name = "A"),
    markermd_question(id = 1L, name = "B")
  )
  dup_name = list(
    markermd_question(id = 1L, name = "A"),
    markermd_question(id = 2L, name = "A")
  )
  expect_error(markermd_template(original_ast = ast, questions = dup_id), "IDs must be unique")
  expect_error(markermd_template(original_ast = ast, questions = dup_name), "names must be unique")
  expect_s7_class(
    markermd_template(original_ast = ast, questions = list(markermd_question(id = 1L, name = "A"))),
    markermd_template
  )
})
