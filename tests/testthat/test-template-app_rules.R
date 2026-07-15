file = system.file("examples/test_assignment/repos/student1-excellent/", package = "markermd")

test_that("A rule shows a live pass/fail status against the document", {
  announce_app_snapshots("template-app_rules")
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
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


test_that("Rule edits are captured into state before an add re-renders the rows", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name = "rule_crud"
  )

  rules_export = function() {
    app$get_values(export = "rules_per_question")$export$rules_per_question[[1]]
  }

  app$click("add_question")
  app$click("ast_panel-select_children_3")
  app$click("question_1-add_rule")
  app$wait_for_idle()

  # Edit rule 1 to a content check, then add a second rule: the pending edits
  # must be captured into state before the structural re-render
  app$set_inputs(`question_1-rule_1-verb` = "has content")
  app$wait_for_idle()
  app$set_inputs(`question_1-rule_1-values` = "quantile")
  app$wait_for_idle()
  app$click("question_1-add_rule")
  app$wait_for_idle()

  rules = rules_export()
  expect_length(rules, 2)
  expect_equal(rules[[1]]$verb, "has content")
  expect_equal(rules[[1]]$values, "quantile")
  expect_equal(rules[[2]]$verb, "has at least")
})


test_that("Deleting a rule preserves the re-indexed survivor's own values", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name = "rule_delete"
  )

  rules_export = function() {
    app$get_values(export = "rules_per_question")$export$rules_per_question[[1]]
  }

  app$click("add_question")
  app$click("ast_panel-select_children_3")
  app$click("question_1-add_rule")
  app$wait_for_idle()

  app$set_inputs(`question_1-rule_1-verb` = "has content")
  app$wait_for_idle()
  app$set_inputs(`question_1-rule_1-values` = "quantile")
  app$wait_for_idle()
  app$click("question_1-add_rule")
  app$wait_for_idle()

  # Delete rule 1: the default rule survives, re-indexed into slot 1 with its
  # own values intact
  app$click("question_1-rule_1-delete")
  app$wait_for_idle()

  rules = rules_export()
  expect_length(rules, 1)
  expect_equal(rules[[1]]$verb, "has at least")
  expect_equal(as.numeric(rules[[1]]$values), 1)

  # The re-indexed survivor's re-rendered inputs still capture edits
  app$set_inputs(`question_1-rule_1-values` = 3)
  app$wait_for_idle()

  rules = rules_export()
  expect_equal(as.numeric(rules[[1]]$values), 3)
})
