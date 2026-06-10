library(shinytest2)

file = system.file("examples/test_assignment/student1-excellent/", package = "markermd")

test_that("Filters build a q2r expression and narrow the rule evaluation set", {
  app = shinytest2::AppDriver$new(
    template(file),
    name = "filter_groups"
  )

  # Create a question, scope it to the Question 1 section, and add a default
  # rule ("Any node" / "has at least" / 1), which passes on that section.
  app$click("add_question")
  app$click("ast_panel-select_children_3")
  app$click("question_1-add_rule")
  app$wait_for_idle()

  rule_status_html = function() {
    app$get_value(output = "question_1-rule_1-status")$html
  }
  filter_expr = function() {
    app$get_values(export = TRUE)$export$filter_exprs_per_question[[1]]
  }
  tree_html = function() {
    app$get_value(output = "ast_panel-ast_tree_ui")$html
  }

  expect_match(rule_status_html(), "fa-check")
  expect_true(is.null(filter_expr()))

  # Add a filter group: the default condition is node type / Div
  app$click("question_1-add_filter_group")
  app$wait_for_idle()
  expect_equal(filter_expr(), "select_children(nodes, is(q2r::pandoc_div))")

  # Switch the condition to "has class" (re-renders the value control): the
  # value resets to the type's default rather than carrying "Div" over
  app$set_inputs(`question_1-filter_1_1-type` = "has class")
  app$wait_for_idle()
  expect_equal(filter_expr(), 'select_children(nodes, has_class(""))')

  # Set a class no node carries: the rule now evaluates against an empty set
  app$set_inputs(`question_1-filter_1_1-value` = "no-such-class")
  app$wait_for_idle()

  expect_equal(filter_expr(), 'select_children(nodes, has_class("no-such-class"))')
  expect_match(rule_status_html(), "fa-xmark")

  # every selected node is excluded by the filter, so the tree draws the
  # section red (green nodes remain only outside the filtered question)
  expect_match(tree_html(), "selected filtered")

  # Negating the condition inverts the (empty) match, so the rule passes again
  app$set_inputs(`question_1-filter_1_1-negate` = TRUE)
  app$wait_for_idle()
  expect_equal(filter_expr(), 'select_children(nodes, !has_class("no-such-class"))')
  expect_match(rule_status_html(), "fa-check")

  # Group negation cancels the condition negation back to no matches
  app$set_inputs(`question_1-filter_group_1-negate` = TRUE)
  app$wait_for_idle()
  expect_equal(filter_expr(), 'select_children(nodes, !(!has_class("no-such-class")))')
  expect_match(rule_status_html(), "fa-xmark")

  app$set_inputs(`question_1-filter_1_1-negate` = FALSE)
  app$set_inputs(`question_1-filter_group_1-negate` = FALSE)
  app$wait_for_idle()
  expect_equal(filter_expr(), 'select_children(nodes, has_class("no-such-class"))')

  # AND condition within the group and an OR group render as DNF
  app$click("question_1-filter_group_1-add_condition")
  app$wait_for_idle()
  app$set_inputs(`question_1-filter_1_2-type` = "has text")
  app$wait_for_idle()
  app$set_inputs(`question_1-filter_1_2-value` = "quantile")
  app$wait_for_idle()

  app$click("question_1-add_filter_group")
  app$wait_for_idle()

  expect_equal(
    filter_expr(),
    'select_children(nodes, (has_class("no-such-class") & has_text("quantile")) | is(q2r::pandoc_div))'
  )

  # Deleting the second group restores the single-group expression; the rule
  # still fails because the surviving group matches nothing
  app$click("question_1-filter_group_2-delete")
  app$wait_for_idle()
  expect_equal(
    filter_expr(),
    'select_children(nodes, has_class("no-such-class") & has_text("quantile"))'
  )
  expect_match(rule_status_html(), "fa-xmark")

  # Deleting both remaining conditions removes the group and the rule passes
  # against the unfiltered section again
  app$click("question_1-filter_1_2-delete")
  app$wait_for_idle()
  app$click("question_1-filter_1_1-delete")
  app$wait_for_idle()

  expect_true(is.null(filter_expr()))
  expect_match(rule_status_html(), "fa-check")

  # with no filters left the red coloring is gone but the selection green
  # stays ("selected filtered" only ever appears in class attributes; the
  # bare word also occurs in the tree's embedded CSS rules)
  expect_false(grepl("selected filtered", tree_html(), fixed = TRUE))
  expect_match(tree_html(), "selected")
})
