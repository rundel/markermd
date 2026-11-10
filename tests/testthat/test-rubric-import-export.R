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


test_that("rubric_item_set rewords an item in place, keeping its selections", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "Old wording"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -1, "Other item"))
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)
  n_snapshots = nrow(snapshot_list(d))

  suppressMessages(rubric_item_set(
    "Q1", "Old wording", description = "New wording", project = d, summary = "clearer wording"
  ))

  items = markermd:::load_rubric_items(d, "Q1")
  expect_equal(names(items), c("item_0", "item_1"))
  expect_equal(items$item_0@description, "New wording")
  expect_equal(items$item_0@points, -2)
  expect_equal(items$item_0@hotkey, 1L)
  expect_equal(markermd:::load_grade_selections(d, "Q1", "hw01-team01"), list(item_0 = TRUE))

  expect_equal(nrow(snapshot_list(d)), n_snapshots + 1)
  activity = project_activity(d, app = FALSE)
  row = activity[activity$fn == "rubric_item_set", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$summary, "clearer wording")

  suppressMessages(rubric_item_set("Q1", "New wording", points = -3, project = d, snapshot = FALSE))
  expect_equal(markermd:::load_rubric_items(d, "Q1")$item_0@points, -3)
  expect_equal(nrow(snapshot_list(d)), n_snapshots + 1)
})


test_that("rubric_item_set refuses unknown, ambiguous and clashing items", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "A"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -1, "B"))
  markermd:::save_rubric_item(d, "Q2", "item_2", markermd_rubric_item(1L, -1, "Dup"))
  markermd:::save_rubric_item(d, "Q2", "item_3", markermd_rubric_item(2L, -1, "Dup"))

  expect_error(rubric_item_set("Q9", "A", description = "x", project = d), "not in this project's template")
  expect_error(rubric_item_set("Q1", "Z", description = "x", project = d), "no rubric item described")
  expect_error(rubric_item_set("Q2", "Dup", description = "x", project = d), "cannot tell which")
  expect_error(rubric_item_set("Q1", "A", description = "B", project = d), "already has another")
  expect_error(rubric_item_set("Q1", "A", project = d), "Supply")
  expect_error(rubric_item_set("Q1", "A", description = "", project = d), "non-empty")
  expect_error(rubric_item_set("Q1", "A", points = NA_real_, project = d), "finite")

  n_actions = nrow(project_activity(d, app = FALSE))
  expect_message(rubric_item_set("Q1", "A", description = "A", project = d), "No change")
  expect_equal(nrow(project_activity(d, app = FALSE)), n_actions)
})

test_that("rubric_import append refuses a description the question already stores", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -3, "Imported issue A"))
  expect_error(
    suppressMessages(rubric_import(write_rubric_fixture_yaml("Q1"), project = d, mode = "append")),
    "already stored"
  )
  items = markermd:::load_rubric_items(d, "Q1")
  expect_equal(unname(purrr::map_chr(items, ~ .x@description)), "Imported issue A")
  # a replace import is the way to re-import the same descriptions
  suppressMessages(rubric_import(write_rubric_fixture_yaml("Q1"), project = d, mode = "replace"))
  expect_length(markermd:::load_rubric_items(d, "Q1"), 2)
})


test_that("rubric_items_set applies several changes as one action and one snapshot", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "A"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -1, "B"))
  markermd:::save_rubric_item(d, "Q2", "item_2", markermd_rubric_item(1L, -1, "C"))
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)
  n_snapshots = nrow(snapshot_list(d))

  changes = data.frame(
    question = c("Q1", "Q1", "Q2"),
    item = c("A", "B", "C"),
    description = c("B", "A", NA),
    points = c(NA, NA, -3)
  )
  out = suppressMessages(rubric_items_set(changes, project = d, summary = "bulk"))
  expect_equal(out$item, c("A", "B", "C"))
  expect_equal(out$description, c("B", "A", "C"))

  q1 = markermd:::load_rubric_items(d, "Q1")
  expect_equal(unname(purrr::map_chr(q1, ~ .x@description)), c("B", "A"))
  expect_equal(unname(purrr::map_dbl(q1, ~ .x@points)), c(-2, -1))
  expect_equal(markermd:::load_rubric_items(d, "Q2")$item_2@points, -3)
  expect_equal(markermd:::load_grade_selections(d, "Q1", "hw01-team01"), list(item_0 = TRUE))

  expect_equal(nrow(snapshot_list(d)), n_snapshots + 1)
  activity = project_activity(d, app = FALSE)
  expect_equal(activity$summary[activity$fn == "rubric_items_set"], "bulk")

  n_actions = nrow(activity)
  expect_message(rubric_items_set(out[c("question", "description", "points")] |>
    stats::setNames(c("question", "item", "points")), project = d), "No change")
  expect_equal(nrow(project_activity(d, app = FALSE)), n_actions)
})


test_that("rubric_items_set reads a YAML file and writes nothing when a row is bad", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "A"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -1, "B"))

  path = file.path(d, "changes.yaml")
  writeLines(c(
    "- question: Q1", "  item: A", "  description: 'A: reworded'",
    "- question: Q1", "  item: B", "  points: -4"
  ), path)
  suppressMessages(rubric_items_set(path, project = d))
  items = markermd:::load_rubric_items(d, "Q1")
  expect_equal(items$item_0@description, "A: reworded")
  expect_equal(items$item_1@points, -4)

  bad = function(...) rubric_items_set(data.frame(...), project = d)
  expect_error(bad(question = "Q1", item = c("A: reworded", "Z"), description = c("x", "y")), "no rubric item described")
  expect_error(bad(question = "Q1", item = "A: reworded", description = "B"), "more than one rubric item")
  expect_error(bad(question = "Q9", item = "B", description = "x"), "not in this project's template")
  expect_error(bad(question = "Q1", item = c("B", "B"), description = c("x", "y")), "more than once")
  expect_error(bad(question = "Q1", item = "B"), "columns")
  expect_error(bad(question = "Q1", item = "B", description = NA), "description")
  expect_error(bad(question = "Q1", item = "B", description = " "), "non-empty")
  expect_error(rubric_items_set(list(), project = d), "data frame")
  expect_equal(markermd:::load_rubric_items(d, "Q1")$item_0@description, "A: reworded")
})


test_that("rubric_item_delete removes an unselected item and renumbers the rest", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "A"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -1, "B"))
  markermd:::save_rubric_item(d, "Q1", "item_2", markermd_rubric_item(3L, -1, "C"))
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_2", TRUE)

  deleted = suppressMessages(rubric_item_delete("Q1", "A", project = d, summary = "unused"))
  expect_equal(deleted@description, "A")

  items = markermd:::load_rubric_items(d, "Q1")
  expect_equal(names(items), c("item_1", "item_2"))
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), 1:2)
  expect_equal(markermd:::load_grade_selections(d, "Q1", "hw01-team01"), list(item_2 = TRUE))
  activity = project_activity(d, app = FALSE)
  expect_equal(activity$summary[activity$fn == "rubric_item_delete"], "unused")

  expect_error(rubric_item_delete("Q1", "A", project = d), "no rubric item described")
  expect_error(rubric_item_delete("Q9", "B", project = d), "not in this project's template")
  expect_error(rubric_item_delete("Q1", "B", project = d, force = NA), "force")
})


test_that("rubric_item_delete asks before deleting a selected item", {
  d = make_rubric_project()
  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "A"))
  markermd:::save_rubric_item(d, "Q1", "item_1", markermd_rubric_item(2L, -1, "B"))
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_0", TRUE)
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_1", TRUE)

  rlang::with_interactive(value = FALSE, {
    expect_error(rubric_item_delete("Q1", "A", project = d), "selected by 1 repository")
  })
  expect_length(markermd:::load_rubric_items(d, "Q1"), 2)

  asked = 0
  answer = FALSE
  testthat::local_mocked_bindings(ask_yes_no = function(question) {
    asked <<- asked + 1
    answer
  })
  rlang::with_interactive(value = TRUE, {
    expect_null(suppressMessages(rubric_item_delete("Q1", "A", project = d)))
    expect_length(markermd:::load_rubric_items(d, "Q1"), 2)

    answer = TRUE
    suppressMessages(rubric_item_delete("Q1", "A", project = d))
  })
  expect_equal(asked, 2)
  expect_equal(names(markermd:::load_rubric_items(d, "Q1")), "item_1")
  expect_equal(markermd:::load_grade_selections(d, "Q1", "hw01-team01"), list(item_1 = TRUE))

  n_snapshots = nrow(snapshot_list(d))
  rlang::with_interactive(value = FALSE, {
    suppressMessages(rubric_item_delete("Q1", "B", project = d, force = TRUE))
  })
  expect_equal(asked, 2)
  expect_length(markermd:::load_rubric_items(d, "Q1"), 0)
  expect_equal(nrow(snapshot_list(d)), n_snapshots + 1)

  suppressMessages(snapshot_restore("latest", project = d))
  expect_equal(names(markermd:::load_rubric_items(d, "Q1")), "item_1")
  expect_equal(markermd:::load_grade_selections(d, "Q1", "hw01-team01"), list(item_1 = TRUE))
})
