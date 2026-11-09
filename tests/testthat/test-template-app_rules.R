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
  add_entry(app)
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

  add_entry(app)
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


test_that("Deleting a rule keeps the survivor under its own id with its values", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name = "rule_delete"
  )

  rules_export = function() {
    app$get_values(export = "rules_per_question")$export$rules_per_question[[1]]
  }

  add_entry(app)
  app$click("ast_panel-select_children_3")
  app$click("question_1-add_rule")
  app$wait_for_idle()

  app$set_inputs(`question_1-rule_1-verb` = "has content")
  app$wait_for_idle()
  app$set_inputs(`question_1-rule_1-values` = "quantile")
  app$wait_for_idle()
  app$click("question_1-add_rule")
  app$wait_for_idle()

  # Delete rule 1: the default rule survives as rule_2 with its own values
  # intact, and the vacated id is gone from the page
  app$click("question_1-rule_1-delete")
  app$wait_for_idle()

  rules = rules_export()
  expect_length(rules, 1)
  expect_equal(rules[[1]]$verb, "has at least")
  expect_equal(as.numeric(rules[[1]]$values), 1)
  expect_equal(app$get_js("document.querySelectorAll('[id^=\"question_1-rule_1-\"]').length"), 0)

  # The survivor's inputs still edit it
  app$set_inputs(`question_1-rule_2-values` = 3)
  app$wait_for_idle()

  rules = rules_export()
  expect_equal(as.numeric(rules[[1]]$values), 3)

  # A new rule takes a fresh id, never the vacated one
  app$click("question_1-add_rule")
  app$wait_for_idle()
  expect_length(rules_export(), 2)
  expect_equal(app$get_js("document.querySelectorAll('[id^=\"question_1-rule_3-\"]').length > 0"), TRUE)
  expect_equal(app$get_js("document.querySelectorAll('[id^=\"question_1-rule_1-\"]').length"), 0)
})


test_that("A double-click on Add Rule keeps every rule's node types", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  app = new_app_driver(
    template(file),
    name = "rule_double_add"
  )

  rules_export = function() {
    app$get_values(export = "rules_per_question")$export$rules_per_question[[1]]
  }

  add_entry(app)
  app$click("ast_panel-select_children_3")
  app$click("question_1-add_rule")
  app$wait_for_idle()

  app$set_inputs(`question_1-rule_1-node_types` = "Chunk")
  app$wait_for_idle()
  expect_equal(rules_export()[[1]]$node_type, "Chunk")

  # Two clicks inside one round trip: the second lands before the client has
  # re-reported the re-rendered row inputs
  app$run_js("
    var btn = document.getElementById('question_1-add_rule');
    btn.click(); btn.click();
  ")
  app$wait_for_idle()

  rules = rules_export()
  expect_length(rules, 3)
  expect_equal(rules[[1]]$node_type, "Chunk")
})
