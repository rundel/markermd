library(shinytest2)

# Build a temporary collection (copy of the bundled fixtures) plus a template
# that grades Question 2 on "quantile" content and Question 3 on "ggplot".
make_mark_fixture = function() {
  src = system.file("examples/test_assignment", package = "markermd")
  collection = tempfile("markcoll_")
  dir.create(collection)
  file.copy(list.files(src, full.names = TRUE), collection, recursive = TRUE)

  qmd = file.path(collection, "student1-excellent", "assignment.qmd")
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
  template_path = tempfile(fileext = ".yaml")
  markermd::write_template_yaml(template, template_path, source_path = qmd)

  list(collection = collection, template = template_path)
}


test_that("mark app launches and validates repositories by section", {
  fixture = make_mark_fixture()

  app = shinytest2::AppDriver$new(
    markermd:::mark_app(fixture$collection, template = fixture$template, download_archives = FALSE),
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
