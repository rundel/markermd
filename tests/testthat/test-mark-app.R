library(shinytest2)

# Build a temporary markermd project (repos/ populated from the bundled
# fixtures, a rendered report under html/, and a configured template that
# grades Question 2 on "quantile" content and Question 3 on "ggplot").
make_mark_fixture = function() {
  src = system.file("examples/test_assignment/repos", package = "markermd")
  root = tempfile("markproj_")
  repos = file.path(root, "repos")
  dir.create(repos, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), repos, recursive = TRUE)

  html_dir = file.path(root, "html")
  dir.create(html_dir)
  writeLines(
    "<html><body>student1 report</body></html>",
    file.path(html_dir, "student1-excellent.html")
  )

  qmd = file.path(repos, "student1-excellent", "assignment.qmd")
  ast = markermd:::parse_assignment_document(qmd)

  template = markermd::markermd_template(
    original_ast = ast,
    questions = list(
      markermd::markermd_question(
        1L, "Q2", markermd::markermd_node_selection(node_ids = "question-2-basic-programming"),
        list(markermd::markermd_rule(node_type = "Any node", verb = "has content", values = "*quantile*"))
      ),
      markermd::markermd_question(
        2L, "Q3", markermd::markermd_node_selection(node_ids = "question-3-data-visualization"),
        list(markermd::markermd_rule(node_type = "Any node", verb = "has content", values = "*ggplot*"))
      )
    ),
    metadata = markermd::markermd_metadata()
  )
  markermd::write_template_yaml(template, file.path(root, "template.yaml"), source_path = qmd)

  suppressMessages(markermd::init_project(root))
  suppressMessages(markermd::project_set(root, template = "template.yaml"))

  list(project = root)
}


test_that("mark app launches and validates repositories by section", {
  fixture = make_mark_fixture()

  app = shinytest2::AppDriver$new(
    markermd:::mark_app(fixture$project),
    name = "mark_validate"
  )

  values = app$get_values(export = TRUE)$export

  expect_setequal(
    values$repo_names,
    c("student1-excellent", "student2-average", "student3-poor",
      "student4-incomplete", "student5-messy")
  )
  expect_equal(values$n_questions, 2L)

  status = values$validation_status

  # The strong answer satisfies both section-scoped content rules
  expect_equal(unname(status[["student1-excellent"]][["Q2"]]), "pass")
  expect_equal(unname(status[["student1-excellent"]][["Q3"]]), "pass")

  # The weak answer skipped the quartiles in its Question 2 section
  expect_equal(unname(status[["student3-poor"]][["Q2"]]), "fail")

  # The unfinished submission completes Question 2 but never produced a plot
  expect_equal(unname(status[["student4-incomplete"]][["Q2"]]), "pass")
  expect_equal(unname(status[["student4-incomplete"]][["Q3"]]), "fail")

  # The messy submission retyped the Question 3 heading ("Visualisation"),
  # so its section anchor no longer resolves even though the work is there
  expect_equal(unname(status[["student5-messy"]][["Q3"]]), "fail")
})


test_that("grading interactions patch the score display in place", {
  fixture = make_mark_fixture()

  app = shinytest2::AppDriver$new(
    markermd:::mark_app(fixture$project),
    name = "mark_grading"
  )

  score_display = function() {
    trimws(app$get_js(
      "document.getElementById('rubric_module-grade_Q2-score_display').innerHTML"
    ))
  }

  # The grading pane's outputs are suspended until its tab is shown
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  expect_equal(score_display(), "0 / 10 pts")

  # Editing the total goes total_score_input -> server observer ->
  # update_score_display() custom message -> DOM patch
  app$run_js("Shiny.setInputValue('rubric_module-grade_Q2-total_score_input', 25)")
  app$wait_for_idle()
  expect_equal(score_display(), "0 / 25 pts")

  # The user-facing path: clicking the score display opens the Total points
  # popover, whose input (bound once the popover first opens) drives the same
  # observer. A second click closes the popover again.
  app$run_js("document.getElementById('rubric_module-grade_Q2-score_popover_trigger').click();")
  app$wait_for_idle()
  app$set_inputs(`rubric_module-grade_Q2-total_score_input` = 30, wait_ = FALSE)
  app$wait_for_idle()
  expect_equal(score_display(), "0 / 30 pts")
  app$run_js("document.getElementById('rubric_module-grade_Q2-score_popover_trigger').click();")
  app$wait_for_idle()

  # Selecting a rubric item recomputes the score through the same path
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  app$run_js("Shiny.setInputValue('rubric_module-item_0-points_text', 4)")
  app$wait_for_idle()

  app$click("rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(score_display(), "4 / 30 pts")

  # Deselecting returns the score to zero
  app$click("rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(score_display(), "0 / 30 pts")
})


test_that("a selection loaded from the database recomputes the score on question switch", {
  fixture = make_mark_fixture()
  markermd:::save_rubric_item(
    fixture$project, "Q3", "item_0",
    markermd::markermd_rubric_item(1L, 4, "Seeded bonus")
  )
  markermd:::save_grade_selection(fixture$project, "Q3", "student1-excellent", "item_0", TRUE)

  app = shinytest2::AppDriver$new(
    markermd:::mark_app(fixture$project),
    name = "mark_loaded_selection_score"
  )

  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  # Switching questions re-renders the grade widget in the same flush that
  # loads the recorded selection; the recomputed score must survive that
  # re-render (the score patch is deferred to onFlushed for this reason)
  app$set_inputs(`rubric_module-question_select` = "Q3")
  app$wait_for_idle()

  expect_equal(
    trimws(app$get_js(
      "document.getElementById('rubric_module-grade_Q3-score_display').innerHTML"
    )),
    "4 / 10 pts"
  )
})


test_that("public and private comments autosave to their own channels", {
  fixture = make_mark_fixture()

  app = shinytest2::AppDriver$new(
    markermd:::mark_app(fixture$project),
    name = "mark_comments"
  )

  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  # Sleeps outlast the 1-second autosave debounce
  app$set_inputs(`rubric_module-question_private_comment` = "internal note")
  Sys.sleep(1.5)
  app$wait_for_idle()

  expect_equal(
    markermd:::load_private_comment(fixture$project, "Q2", "student1-excellent"),
    "internal note"
  )
  expect_null(markermd:::load_comment(fixture$project, "Q2", "student1-excellent"))

  # A private note alone marks the pair as touched but not graded
  marked = markermd:::marked_question_pairs(fixture$project)
  expect_true(any(marked$question_name == "Q2" & marked$assignment_repo == "student1-excellent"))
  graded = markermd:::graded_question_pairs(fixture$project)
  expect_false(any(graded$question_name == "Q2" & graded$assignment_repo == "student1-excellent"))

  app$set_inputs(`rubric_module-question_comment` = "public feedback")
  Sys.sleep(1.5)
  app$wait_for_idle()

  expect_equal(
    markermd:::load_comment(fixture$project, "Q2", "student1-excellent"),
    "public feedback"
  )
  graded = markermd:::graded_question_pairs(fixture$project)
  expect_true(any(graded$question_name == "Q2" & graded$assignment_repo == "student1-excellent"))
})


test_that("rubric YAML export and import work through the mark app", {
  fixture = make_mark_fixture()
  markermd:::save_rubric_item(
    fixture$project, "Q2", "item_0",
    markermd::markermd_rubric_item(1L, -2, "Seeded")
  )

  app = shinytest2::AppDriver$new(
    markermd:::mark_app(fixture$project),
    name = "mark_rubric_io"
  )

  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  # Open the io popover so its download buttons exist in the DOM, then export
  # both scopes and re-read them through the public reader
  app$run_js("document.getElementById('rubric_module-rubric_io_menu').click();")
  app$wait_for_idle()

  all_path = app$get_download("rubric_module-export_all")
  exported = markermd::read_rubric_yaml(all_path)
  expect_equal(purrr::map_chr(exported$questions, "name"), c("Q2", "Q3"))
  expect_equal(
    purrr::map_chr(exported$questions[[1]]$items, ~ .x@description),
    "Seeded"
  )

  question_path = app$get_download("rubric_module-export_question")
  expect_equal(
    purrr::map_chr(markermd::read_rubric_yaml(question_path)$questions, "name"),
    "Q2"
  )

  rubric_yaml = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'",
    "questions:",
    "- name: Q2",
    "  scoring:",
    "    total_score: 15",
    "  items:",
    "  - points: -3",
    "    description: Imported A",
    "  - points: -1",
    "    description: Imported B"
  ), rubric_yaml)

  # Append: the seeded item stays first, imported items follow in order
  app$upload_file(`rubric_module-rubric_import_file` = rubric_yaml)
  app$wait_for_idle()
  app$click("rubric_module-confirm_rubric_import")
  app$wait_for_idle()

  values = app$get_values(export = TRUE)$export
  expect_equal(
    values[["rubric_module-rubric_item_descriptions"]],
    c("Seeded", "Imported A", "Imported B")
  )

  items = markermd:::load_rubric_items(fixture$project, "Q2")
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), 1:3)

  # The imported scoring rebuilds the grade widget with the new total
  expect_equal(
    trimws(app$get_js(
      "document.getElementById('rubric_module-grade_Q2-score_display').innerHTML"
    )),
    "0 / 15 pts"
  )

  # Replace: the previous items are dropped for the fresh set
  app$upload_file(`rubric_module-rubric_import_file` = rubric_yaml)
  app$wait_for_idle()
  app$set_inputs(`rubric_module-rubric_import_mode` = "replace")
  app$click("rubric_module-confirm_rubric_import")
  app$wait_for_idle()

  values = app$get_values(export = TRUE)$export
  expect_equal(
    values[["rubric_module-rubric_item_descriptions"]],
    c("Imported A", "Imported B")
  )
  expect_false("item_0" %in% values[["rubric_module-rubric_item_ids"]])
  expect_length(markermd:::load_rubric_items(fixture$project, "Q2"), 2)
})
