library(shinytest2)

# Build a temporary markermd project whose key (solution) repo holds the bundled
# example assignment. When with_template = TRUE a two-question template is
# written and imported into the project database so it can be preloaded for
# editing.
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


test_that("template() preloads a project's stored template", {
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


test_that("template() saves into the project database", {
  proj = make_template_project(with_template = FALSE)

  expect_null(markermd:::load_template_from_db(proj$root))

  app = shinytest2::AppDriver$new(
    template(proj$root),
    name = "template_project_save"
  )

  app$click("add_question")
  app$click("save_to_project")
  app$wait_for_idle()

  expect_false(is.null(markermd:::load_template_from_db(proj$root)))
})


test_that("template() imports a YAML file into the editor", {
  proj = make_template_project(with_template = FALSE)
  qmd = file.path(proj$root, "key", "assignment.qmd")

  import_yaml = tempfile(fileext = ".yaml")
  writeLines(c(
    'format_version: "3.0"',
    sprintf('source: {path: "%s"}', qmd),
    "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: [{node_type: Chunk, verb: has at least, count: 1}]}",
    "- {id: 2, name: Q3, node_ids: [question-3-data-visualization], rules: []}"
  ), import_yaml)

  app = shinytest2::AppDriver$new(
    template(proj$root),
    name = "template_project_import"
  )

  expect_equal(app$get_values(export = "n_questions") |> unlist(use.names = FALSE), 0)

  # The Import button triggers the hidden #import_file picker via onclick;
  # shinytest2 sets that file input directly.
  app$upload_file(import_file = import_yaml)
  app$wait_for_idle()

  expect_equal(app$get_values(export = "n_questions") |> unlist(use.names = FALSE), 2)
  # Imported names are shown verbatim, not renumbered to "Question <id>".
  expect_setequal(
    app$get_values(export = "question_names") |> unlist(use.names = FALSE),
    c("Q2", "Q3")
  )
})


test_that("template() confirms before an import replaces existing questions", {
  proj = make_template_project(with_template = TRUE)
  qmd = file.path(proj$root, "key", "assignment.qmd")

  import_yaml = tempfile(fileext = ".yaml")
  writeLines(c(
    'format_version: "3.0"',
    sprintf('source: {path: "%s"}', qmd),
    "questions:",
    "- {id: 1, name: Imported, node_ids: [question-2-basic-programming], rules: []}"
  ), import_yaml)

  app = shinytest2::AppDriver$new(
    template(proj$root),
    name = "template_project_import_confirm"
  )

  expect_equal(app$get_values(export = "n_questions") |> unlist(use.names = FALSE), 2)

  app$upload_file(import_file = import_yaml)
  app$wait_for_idle()

  # The editor's questions survive until the replace modal is confirmed
  expect_equal(app$get_values(export = "n_questions") |> unlist(use.names = FALSE), 2)

  app$click("confirm_import")
  app$wait_for_idle()

  expect_equal(app$get_values(export = "n_questions") |> unlist(use.names = FALSE), 1)
  expect_setequal(
    app$get_values(export = "question_names") |> unlist(use.names = FALSE),
    "Imported"
  )
})


test_that("template() exports the current template to YAML", {
  proj = make_template_project(with_template = TRUE)

  app = shinytest2::AppDriver$new(
    template(proj$root),
    name = "template_project_export"
  )

  # Export lives in a popover; open it so the download link is in the live DOM.
  app$run_js("document.getElementById('io_menu').click();")
  app$wait_for_idle()

  out = app$get_download("export_template")
  tmpl = markermd::read_template_yaml(out, require_ast = FALSE)
  expect_length(tmpl@questions, 2)
})
