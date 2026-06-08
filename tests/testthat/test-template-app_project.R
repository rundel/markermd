library(shinytest2)

# Build a temporary markermd project whose key (solution) repo holds the bundled
# example assignment. When with_template = TRUE a two-question template is
# written and recorded in the config so it can be preloaded for editing.
make_template_project = function(with_template = TRUE) {
  src = system.file("examples/test_assignment/student1-excellent", package = "markermd")
  root = tempfile("tmplproj_")
  key = file.path(root, "key")
  dir.create(key, recursive = TRUE)
  dir.create(file.path(root, "repos"))
  file.copy(list.files(src, full.names = TRUE), key)

  qmd = file.path(key, "assignment.qmd")

  if (with_template) {
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
  }

  suppressMessages(markermd::init_project(root))
  suppressMessages(markermd::project_set(root, key = "key"))
  if (with_template) {
    suppressMessages(markermd::project_set(root, template = "template.yaml"))
  }

  list(root = root)
}


test_that("template() preloads a project's configured template", {
  proj = make_template_project(with_template = TRUE)

  app = shinytest2::AppDriver$new(
    template(proj$root),
    name = "template_project_preload"
  )

  expect_equal(
    app$get_values(export = "n_questions") |> unlist(use.names = FALSE),
    2
  )
})


test_that("template() saves into the project and records it in the config", {
  proj = make_template_project(with_template = FALSE)

  expect_true(is.na(markermd::project_config(proj$root)@template))

  app = shinytest2::AppDriver$new(
    template(proj$root),
    name = "template_project_save"
  )

  app$click("add_question")
  app$click("save_to_project")
  Sys.sleep(1)

  expect_true(file.exists(file.path(proj$root, "template.yaml")))
  expect_equal(markermd::project_config(proj$root)@template, "template.yaml")
})
