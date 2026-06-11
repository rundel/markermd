# Builds an initialized project whose database stores a two-question template
# (Q1, Q2), mirroring the fake ghclass layout used in test-project.R.
make_rubric_project = function() {
  d = tempfile("markproj_")
  dir.create(file.path(d, "repos", "hw01-team01"), recursive = TRUE)
  d = normalizePath(d, winslash = "/")

  template_path = file.path(d, "template.yaml")
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - id: 1",
    "    name: Q1",
    "  - id: 2",
    "    name: Q2"
  ), template_path)

  suppressMessages(init_project(d))
  suppressMessages(template_import(template_path, project = d))
  d
}

# Writes a one-question rubric YAML for the given question name.
write_rubric_fixture_yaml = function(question_name, with_scoring = TRUE) {
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'",
    "questions:",
    sprintf("- name: %s", question_name),
    if (with_scoring) c(
      "  scoring:",
      "    total_score: 20",
      "    grading_mode: negative"
    ),
    "  items:",
    "  - points: -3",
    "    description: Imported issue A",
    "  - points: -1",
    "    description: Imported issue B"
  ), path)
  path
}


test_that("next_item_ids continues past the max suffix and skips collisions", {
  expect_equal(markermd:::next_item_ids(character(0), 2), c("item_0", "item_1"))
  expect_equal(markermd:::next_item_ids(c("item_0", "item_4"), 2), c("item_5", "item_6"))
  expect_equal(markermd:::next_item_ids(c("item_1", "custom-id"), 1), "item_2")
  expect_equal(markermd:::next_item_ids("item_0", 0), character(0))
})


test_that("rubric_export writes every template question, scoring only when stored", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "Seeded"))
  markermd:::save_grade_state(
    d, "Q1",
    markermd_grade_state(current_score = 0, total_score = 10, grading_mode = "positive")
  )

  out = file.path(d, "rubric.yaml")
  expect_identical(rubric_export(out, project = d), out)

  rubric = read_rubric_yaml(out)
  expect_equal(purrr::map_chr(rubric$questions, "name"), c("Q1", "Q2"))
  expect_equal(rubric$questions[[1]]$scoring@total_score, 10)
  expect_equal(purrr::map_chr(rubric$questions[[1]]$items, ~ .x@description), "Seeded")
  expect_null(rubric$questions[[2]]$scoring)
  expect_length(rubric$questions[[2]]$items, 0)
})


test_that("rubric_export filters by question and rejects unknown names", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "Seeded"))

  out = file.path(d, "q2.yaml")
  rubric_export(out, project = d, question = "Q2")
  rubric = read_rubric_yaml(out)
  expect_equal(purrr::map_chr(rubric$questions, "name"), "Q2")

  expect_error(rubric_export(out, project = d, question = "Q9"), "Unknown question")
})


test_that("rubric_export errors when the project has no questions at all", {
  d = tempfile("markproj_")
  dir.create(file.path(d, "repos"), recursive = TRUE)
  suppressMessages(init_project(d))

  expect_error(
    suppressWarnings(rubric_export(file.path(d, "x.yaml"), project = d)),
    "No rubric data"
  )
})


test_that("rubric_import append keeps existing items and mints globally fresh ids", {
  d = make_rubric_project()
  # Seed item_0/item_1 under the OTHER question to prove ids are unique across
  # all questions, not per question
  markermd:::save_rubric_item(d, "Q2", "item_0", markermd_rubric_item(1L, -1, "Q2 first"))
  markermd:::save_rubric_item(d, "Q2", "item_1", markermd_rubric_item(2L, -1, "Q2 second"))
  markermd:::save_rubric_item(d, "Q1", "item_2", markermd_rubric_item(1L, -2, "Existing"))

  suppressMessages(rubric_import(write_rubric_fixture_yaml("Q1"), project = d, mode = "append"))

  items = markermd:::load_rubric_items(d, "Q1")
  expect_equal(
    purrr::map_chr(items, ~ .x@description),
    c(item_2 = "Existing", item_3 = "Imported issue A", item_4 = "Imported issue B")
  )
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), 1:3)

  # The untouched question keeps its items
  expect_length(markermd:::load_rubric_items(d, "Q2"), 2)
})


test_that("rubric_import replace removes old items and their recorded selections", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "Old"))
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)

  progress = markermd:::calculate_grading_progress(d, c("Q1", "Q2"), "hw01-team01")
  expect_equal(unname(progress["hw01-team01"]), 1)

  suppressMessages(rubric_import(write_rubric_fixture_yaml("Q1"), project = d, mode = "replace"))

  items = markermd:::load_rubric_items(d, "Q1")
  expect_false("item_0" %in% names(items))
  expect_equal(
    unname(purrr::map_chr(items, ~ .x@description)),
    c("Imported issue A", "Imported issue B")
  )
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), 1:2)

  # The cascaded grade rows no longer count toward progress
  progress = markermd:::calculate_grading_progress(d, c("Q1", "Q2"), "hw01-team01")
  expect_equal(unname(progress["hw01-team01"]), 0)
})


test_that("rubric_import applies scoring and synthesizes current_score", {
  d = make_rubric_project()
  suppressMessages(rubric_import(write_rubric_fixture_yaml("Q1"), project = d))

  state = markermd:::load_grade_state(d, "Q1")
  expect_equal(state@total_score, 20)
  expect_equal(state@grading_mode, "negative")
  expect_equal(state@current_score, 20)

  # Scoring-less files leave settings untouched
  suppressMessages(
    rubric_import(write_rubric_fixture_yaml("Q2", with_scoring = FALSE), project = d)
  )
  expect_null(markermd:::load_grade_state(d, "Q2"))
})


test_that("rubric_import rejects question names missing from the template, writing nothing", {
  d = make_rubric_project()

  expect_error(
    rubric_import(write_rubric_fixture_yaml("Q9"), project = d),
    "not in this project's template"
  )
  expect_length(markermd:::load_rubric_items(d, "Q9"), 0)
  expect_length(markermd:::with_database(d, markermd:::load_all_items)$item_id, 0)
})


test_that("rubric_import's question argument subsets the file", {
  d = make_rubric_project()
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'",
    "questions:",
    "- name: Q1",
    "  items:",
    "  - points: -1",
    "    description: For Q1",
    "- name: Q2",
    "  items:",
    "  - points: -1",
    "    description: For Q2"
  ), path)

  suppressMessages(rubric_import(path, project = d, question = "Q2"))
  expect_length(markermd:::load_rubric_items(d, "Q1"), 0)
  expect_length(markermd:::load_rubric_items(d, "Q2"), 1)

  expect_error(rubric_import(path, project = d, question = "Q9"), "not found in")
})


test_that("rubric_import warns but proceeds when no template is stored", {
  d = tempfile("markproj_")
  dir.create(file.path(d, "repos"), recursive = TRUE)
  suppressMessages(init_project(d))

  expect_warning(
    suppressMessages(rubric_import(write_rubric_fixture_yaml("Q1"), project = d)),
    "No template is stored"
  )
  expect_length(markermd:::load_rubric_items(d, "Q1"), 2)
})


test_that("export -> import round trip is faithful", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2.5, "Fractional"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, 3, "Bonus"))

  out = file.path(d, "rubric.yaml")
  rubric_export(out, project = d)
  suppressMessages(rubric_import(out, project = d, mode = "replace"))

  items = markermd:::load_rubric_items(d, "Q1")
  expect_equal(unname(purrr::map_dbl(items, ~ .x@points)), c(-2.5, 3))
  expect_equal(unname(purrr::map_chr(items, ~ .x@description)), c("Fractional", "Bonus"))
})
