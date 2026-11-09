file = system.file("examples/test_assignment/repos/student1-excellent/", package = "markermd")

get_n_questions = function(app) {
  app$get_values(export = "n_questions") |> unlist(use.names = FALSE)
}


test_that("Add 1 question", {
  announce_app_snapshots("template-app_questions")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name  = "add_1"
  )


  expect_equal(get_n_questions(app), 0)

  add_entry(app)

  expect_equal(get_n_questions(app), 1)

  app$expect_values(export = TRUE)
})

test_that("Add 2 questions", {
  announce_app_snapshots("template-app_questions")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name  = "add_2"
  )
  
  expect_equal(get_n_questions(app), 0)

  add_entry(app)
  add_entry(app)

  expect_equal(get_n_questions(app), 2)

  app$expect_values(export = TRUE)
})

test_that("Add 2 questions, delete 1", {
  announce_app_snapshots("template-app_questions")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name  = "add_2-delete_1"
  )
  
  expect_equal(get_n_questions(app), 0)

  add_entry(app)
  add_entry(app)

  expect_equal(get_n_questions(app), 2)

  app$wait_for_idle()
  app$click("question_2-delete_question")
  # Deleting now requires confirming the modal; wait for it to be shown
  app$wait_for_js("!!document.querySelector('#shiny-modal.show #question_2-confirm_delete_question')")
  app$click("question_2-confirm_delete_question")
  app$wait_for_idle()

  expect_equal(get_n_questions(app), 1)

  app$expect_values(export = TRUE)
})


test_that("Add 2 questions, delete 1, add 1", {
  announce_app_snapshots("template-app_questions")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name  = "add_2-delete_1-add_1"
  )

  expect_equal(get_n_questions(app), 0)
  
  add_entry(app)
  add_entry(app)

  expect_equal(get_n_questions(app), 2)

  app$wait_for_idle()
  app$click("question_2-delete_question")
  # Deleting now requires confirming the modal; wait for it to be shown
  app$wait_for_js("!!document.querySelector('#shiny-modal.show #question_2-confirm_delete_question')")
  app$click("question_2-confirm_delete_question")
  app$wait_for_idle()

  expect_equal(get_n_questions(app), 1)

  app$wait_for_idle()
  add_entry(app)

  expect_equal(get_n_questions(app), 2)

  app$expect_values(export = TRUE)
})


test_that("Save Template downloads a readable YAML template", {
  announce_app_snapshots("template-app_questions")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name = "save_yaml"
  )

  add_entry(app)
  expect_equal(get_n_questions(app), 1)

  path = app$get_download("save_template")
  expect_match(path, "\\.yaml$")

  back = markermd::read_template_yaml(path, require_ast = TRUE)
  expect_s3_class(back, "markermd::markermd_template")
  expect_length(back@questions, 1)
  expect_gt(back@metadata@total_nodes, 0L)
})

test_that("Add a document question", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name  = "add_document"
  )

  add_entry(app, coverage = "document")

  expect_equal(get_n_questions(app), 1)
  expect_equal(unname(unlist(app$get_values(export = "question_names"))), "Document")
  expect_match(app$get_value(output = "question_1-selected_nodes_display")$html, "Entire document")
})

test_that("an active document entry highlights the entire tree", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name  = "document_tree"
  )
  n_selected = function() {
    html = tree_dom_html(app)
    sum(gregexpr(" selected\"", html)[[1]] > 0)
  }

  add_entry(app, coverage = "document")
  expect_gt(n_selected(), 0)

  # every non-root node of the document is highlighted
  ast = markermd:::parse_assignment_document(fs::dir_ls(file, glob = "*.qmd")[[1]])
  expect_equal(n_selected(), 2 * length(markermd:::q2r_flatten(ast)))

  # a sections question with nothing selected highlights nothing
  add_entry(app)
  expect_equal(n_selected(), 0)
})
