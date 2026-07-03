file = system.file("examples/div_assignment/", package = "markermd")

test_that("Select an id'd div", {
  announce_app_snapshots("template-app_divs")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = shinytest2::AppDriver$new(
    template(file),
    name = "select_div_3"
  )

  # Flatten index 3 is the ::: {#q1-answer .answer} div (heading 1, intro para,
  # then the div); clicking its circle selects the div and stores its id.
  app$click("ast_panel-select_children_3")
  app$expect_values(export = TRUE)
})
