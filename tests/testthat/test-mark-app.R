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
