library(shinytest2)

file = system.file("examples/test_assignment/repos/student1-excellent/", package = "markermd")

test_that("A rule shows a live pass/fail status against the document", {
  app = shinytest2::AppDriver$new(
    template(file),
    name = "rule_status"
  )

  # Create a question, scope it to the Question 1 section, and add a default
  # rule ("Any node" / "has at least" / 1), which passes on that section.
  app$click("add_question")
  app$click("ast_panel-select_children_3")
  app$click("question_1-add_rule")

  app$wait_for_idle()

  app$expect_values(export = TRUE)
})
