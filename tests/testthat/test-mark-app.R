library(shinytest2)

# Build a temporary markermd project (repos/ populated from the bundled
# fixtures, a rendered report under html/, and a configured template that
# grades Question 2 on "quantile" content and Question 3 on "ggplot").
make_mark_fixture = function() {
  src = system.file("examples/test_assignment", package = "markermd")
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
    c("student1-excellent", "student2-average", "student3-poor")
  )
  expect_equal(values$n_questions, 2L)

  status = values$validation_status

  # The strong answer satisfies both section-scoped content rules
  expect_equal(unname(status[["student1-excellent"]][["Q2"]]), "pass")
  expect_equal(unname(status[["student1-excellent"]][["Q3"]]), "pass")

  # The weak answer lacks the quantile content in its Question 2 section
  expect_equal(unname(status[["student3-poor"]][["Q2"]]), "fail")
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

  # Selecting a rubric item recomputes the score through the same path
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  app$run_js("Shiny.setInputValue('rubric_module-item_0-points_text', 4)")
  app$wait_for_idle()

  app$click("rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(score_display(), "4 / 25 pts")

  # Deselecting returns the score to zero
  app$click("rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(score_display(), "0 / 25 pts")
})
