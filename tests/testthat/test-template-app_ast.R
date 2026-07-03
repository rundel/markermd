file = system.file("examples/test_assignment/repos/student1-excellent/", package = "markermd")

get_n_questions = function(app) {
  app$get_values(export = "n_questions") |> unlist(use.names = FALSE)
}


test_that("Select Question 1 heading", {
  announce_app_snapshots("template-app_ast")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = shinytest2::AppDriver$new(
    template(file),
    name  = "select_3"
  )

  app$click("ast_panel-select_children_3")
  app$expect_values(export = TRUE)
})

test_that("Select Question 2 heading", {
  announce_app_snapshots("template-app_ast")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = shinytest2::AppDriver$new(
    template(file),
    name  = "select_8"
  )

  app$click("ast_panel-select_children_8")
  app$expect_values(export = TRUE)
})

test_that("Select Question 1 & 2 headings", {
  announce_app_snapshots("template-app_ast")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = shinytest2::AppDriver$new(
    template(file),
    name  = "select_3_8"
  )

  app$click("ast_panel-select_children_3")
  app$click("ast_panel-select_children_8")
  app$expect_values(export = TRUE)
})
