# Build a small exchange-list rubric covering scoring, fractional points and
# an empty question.
build_rubric_fixture = function() {
  list(
    format_version = markermd:::markermd_rubric_version(),
    questions = list(
      list(
        name = "Q1",
        scoring = markermd::markermd_grade_state(
          current_score = 5,
          total_score = 5,
          grading_mode = "negative",
          bound_above_zero = FALSE,
          bound_below_max = TRUE
        ),
        items = list(
          markermd::markermd_rubric_item(1L, -2, "Off by one in the loop bounds"),
          markermd::markermd_rubric_item(2L, -1.5, "Missing axis labels"),
          markermd::markermd_rubric_item(3L, 1, "Particularly clear write-up")
        )
      ),
      list(name = "Q2", scoring = NULL, items = list())
    )
  )
}

# Write YAML lines to a temp file and return its path.
write_yaml_lines = function(lines) {
  path = tempfile(fileext = ".yaml")
  writeLines(lines, path)
  path
}


test_that("rubric YAML round-trips items, order and scoring", {
  rubric = build_rubric_fixture()
  path = tempfile(fileext = ".yaml")

  expect_identical(write_rubric_yaml(rubric, path), path)
  back = read_rubric_yaml(path)

  expect_equal(back$format_version, markermd:::markermd_rubric_version())
  expect_length(back$questions, 2)

  q1 = back$questions[[1]]
  expect_equal(q1$name, "Q1")
  expect_equal(
    purrr::map_dbl(q1$items, ~ .x@points),
    c(-2, -1.5, 1)
  )
  expect_equal(
    purrr::map_chr(q1$items, ~ .x@description),
    c("Off by one in the loop bounds", "Missing axis labels", "Particularly clear write-up")
  )
  expect_equal(purrr::map_int(q1$items, ~ .x@hotkey), 1:3)

  expect_equal(q1$scoring@total_score, 5)
  expect_equal(q1$scoring@grading_mode, "negative")
  expect_false(q1$scoring@bound_above_zero)
  expect_true(q1$scoring@bound_below_max)

  q2 = back$questions[[2]]
  expect_equal(q2$name, "Q2")
  expect_length(q2$items, 0)
  expect_null(q2$scoring)
})


test_that("the written YAML omits hotkeys, ids, selection and current_score", {
  rubric = build_rubric_fixture()
  path = tempfile(fileext = ".yaml")
  write_rubric_yaml(rubric, path)

  text = readLines(path)
  expect_false(any(grepl("hotkey", text)))
  expect_false(any(grepl("item_id|selected", text)))
  expect_false(any(grepl("current_score", text)))
  expect_true(any(grepl("^format_version:", text)))
  expect_true(any(grepl("bound_above_zero: false", text)))
})


test_that("hand-authored LLM-style YAML parses, with NA hotkeys past ten items", {
  item_lines = unlist(lapply(1:12, function(i) {
    c(sprintf("  - points: -%d", i), sprintf("    description: Issue %d", i))
  }))
  path = write_yaml_lines(c(
    "format_version: '1.0'",
    "questions:",
    "- name: Big question",
    "  items:",
    item_lines,
    "- name: Other question",
    "  scoring:",
    "    total_score: 4",
    "  items:",
    "  - points: 2",
    "    description: Half credit"
  ))

  rubric = read_rubric_yaml(path)

  big = rubric$questions[[1]]
  expect_length(big$items, 12)
  expect_equal(purrr::map_int(big$items[1:10], ~ .x@hotkey), 1:10)
  expect_true(all(is.na(purrr::map_int(big$items[11:12], ~ .x@hotkey))))
  expect_null(big$scoring)

  other = rubric$questions[[2]]
  expect_equal(other$scoring@total_score, 4)
  expect_equal(other$scoring@grading_mode, "positive")
  expect_equal(other$scoring@current_score, 0)
})


test_that("scoring import synthesizes current_score for negative mode", {
  scoring = markermd:::scoring_from_list(list(total_score = 8, grading_mode = "negative"))
  expect_equal(scoring@current_score, 8)
  expect_equal(scoring@grading_mode, "negative")
  expect_true(scoring@bound_above_zero)
})


test_that("missing or too-new format_version errors, as do duplicate names", {
  no_version = write_yaml_lines(c("questions: []"))
  expect_error(read_rubric_yaml(no_version), "format_version")

  too_new = write_yaml_lines(c("format_version: '99.0'", "questions: []"))
  expect_error(read_rubric_yaml(too_new), "newer")

  dupes = write_yaml_lines(c(
    "format_version: '1.0'",
    "questions:",
    "- name: Q1",
    "  items: []",
    "- name: Q1",
    "  items: []"
  ))
  expect_error(read_rubric_yaml(dupes), "duplicate question names")

  unnamed = write_yaml_lines(c(
    "format_version: '1.0'",
    "questions:",
    "- items: []"
  ))
  expect_error(read_rubric_yaml(unnamed), "non-empty 'name'")
})


test_that("write_rubric_yaml rejects malformed rubrics", {
  expect_error(write_rubric_yaml(list(), tempfile()), "questions")
  expect_error(
    write_rubric_yaml(
      list(questions = list(list(name = "Q1", items = list("not an item")))),
      tempfile()
    ),
    "markermd_rubric_item"
  )
})


test_that("validate_rubric_file accepts exported files and rejects malformed ones", {
  skip_if_not_installed("jsonvalidate")

  good = tempfile(fileext = ".yaml")
  write_rubric_yaml(build_rubric_fixture(), good)
  expect_true(validate_rubric_file(good))

  missing_name = write_yaml_lines(c(
    "format_version: '1.0'",
    "questions:",
    "- items: []"
  ))
  expect_false(validate_rubric_file(missing_name))

  string_points = write_yaml_lines(c(
    "format_version: '1.0'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - points: lots",
    "    description: Bad"
  ))
  expect_false(validate_rubric_file(string_points))

  extra_key = write_yaml_lines(c(
    "format_version: '1.0'",
    "questions:",
    "- name: Q1",
    "  hotkeys: [1, 2]",
    "  items: []"
  ))
  expect_false(validate_rubric_file(extra_key))
})
