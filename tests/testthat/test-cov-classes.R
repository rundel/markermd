# Pure constructor/validator coverage for the S7 classes in
# R/project-classes.R and R/template-classes.R.

cc_valid_ast = function() {
  q2r::parse_qmd("# x", quiet = TRUE)
}

test_that("markermd_project validator rejects bad inputs and accepts a valid project", {
  expect_error(
    markermd_project(root = "/tmp/p", artifacts = c("html", NA)),
    "artifacts must not contain NA"
  )
  expect_error(
    markermd_project(root = "/tmp/p", artifacts = c("html", "html")),
    "artifacts must be unique"
  )
  expect_error(
    markermd_project(root = ""),
    "root must be a non-empty path"
  )
  expect_error(
    markermd_project(root = "/tmp/p", repos = c("a", "b")),
    "repos must be a single character value"
  )

  proj = markermd_project(root = "/tmp/p", artifacts = c("html"))
  expect_s7_class(proj, markermd_project)
})

test_that("markermd_grade_state grading_mode enum is validated", {
  expect_error(
    markermd_grade_state(grading_mode = "bogus"),
    "must be 'positive' or 'negative'"
  )
  expect_s7_class(markermd_grade_state(grading_mode = "positive"), markermd_grade_state)
  expect_s7_class(markermd_grade_state(grading_mode = "negative"), markermd_grade_state)
})

test_that("markermd_template original_ast must be a pandoc object", {
  expect_error(
    markermd_template(original_ast = list()),
    "must be a pandoc object"
  )
  expect_error(
    markermd_template(original_ast = NULL),
    "must be a pandoc object"
  )
  expect_s7_class(
    markermd_template(original_ast = cc_valid_ast()),
    markermd_template
  )
})
