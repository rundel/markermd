# Coverage tests for the validation subsystem (R/utils_template.R, R/utils_tree.R).

# Parse a small qmd string into a q2r pandoc AST
cv_parse = function(text) {
  q2r::parse_qmd(text, quiet = TRUE)
}

# Build a question with the given node-id selection and rules
cv_question = function(node_ids = character(0), rules = list()) {
  markermd:::markermd_question(
    id = 1L,
    name = "Q1",
    selected_nodes = markermd:::markermd_node_selection(node_ids = node_ids),
    rules = rules
  )
}

# A trivially-satisfiable count rule ("has at least 0", any node)
cv_trivial_rule = function() {
  markermd:::markermd_rule(node_type = "Any node", verb = "has at least", values = 0L)
}

# Tree items for the nested H1 > H2 > H3 document used throughout
cv_nested_tree = function() {
  ast = cv_parse("# H1 {#h1}\n\n## H2 {#h2}\n\n### H3 {#h3}\n")
  markermd:::build_ast_tree_structure(ast)
}


test_that("validate_question_rules fails a no-selection question that has one rule", {
  ast = cv_parse("# H1 {#h1}\n\nSome content here.\n\nMore content.\n")

  # The single rule (has at least 0 of any node) would pass document-wide, but
  # with no nodes selected the question targets nothing and must fail.
  question = cv_question(node_ids = character(0), rules = list(cv_trivial_rule()))
  result = markermd:::validate_question_rules(ast, question)

  expect_identical(result$status, "fail")
  expect_true(all(!result$passed))
  expect_length(result$messages, 1L)
  expect_true(all(result$messages == "No nodes selected"))

  details = markermd:::create_rule_details(question, result)
  expect_s3_class(details, "shiny.tag")
})

test_that("validate_question_rules fails a no-selection question that has two rules", {
  ast = cv_parse("# H1 {#h1}\n\nSome content here.\n\nMore content.\n")

  question = cv_question(
    node_ids = character(0),
    rules = list(cv_trivial_rule(), cv_trivial_rule())
  )
  result = markermd:::validate_question_rules(ast, question)

  expect_identical(result$status, "fail")
  expect_true(all(!result$passed))
  expect_length(result$passed, 2L)
  expect_length(result$messages, 2L)
  expect_true(all(result$messages == "No nodes selected"))

  details = markermd:::create_rule_details(question, result)
  expect_s3_class(details, "shiny.tag")
})


test_that("assert_template_compatible rejects a newer template version", {
  ast = cv_parse("# H1 {#h1}\n\nSome content.\n")
  metadata = markermd:::markermd_metadata(version = "99.0")
  template = markermd:::markermd_template(original_ast = ast, metadata = metadata)

  expect_error(
    markermd:::assert_template_compatible(template),
    "newer"
  )
})

test_that("assert_template_compatible rejects an NA (older) template version", {
  ast = cv_parse("# H1 {#h1}\n\nSome content.\n")
  metadata = markermd:::markermd_metadata(version = NA_character_)
  template = markermd:::markermd_template(original_ast = ast, metadata = metadata)

  expect_error(
    markermd:::assert_template_compatible(template),
    "older"
  )
})

test_that("assert_template_compatible accepts the current template version", {
  ast = cv_parse("# H1 {#h1}\n\nSome content.\n")
  metadata = markermd:::markermd_metadata(version = markermd:::markermd_template_version())
  template = markermd:::markermd_template(original_ast = ast, metadata = metadata)

  expect_no_error(markermd:::assert_template_compatible(template))
})


test_that("find_node_children returns the immediate children for each tree node", {
  tree = cv_nested_tree()

  expect_equal(markermd:::find_node_children(tree, 0), 1)
  expect_equal(markermd:::find_node_children(tree, 1), 2)
  expect_equal(markermd:::find_node_children(tree, 2), 3)
  expect_equal(markermd:::find_node_children(tree, 3), integer(0))
  expect_equal(markermd:::find_node_children(tree, NULL), integer(0))
})

test_that("find_all_descendants returns all nested descendants for each tree node", {
  tree = cv_nested_tree()

  expect_equal(markermd:::find_all_descendants(tree, 0), c(1, 2, 3))
  expect_equal(markermd:::find_all_descendants(tree, 1), c(2, 3))
  expect_equal(markermd:::find_all_descendants(tree, 2), 3)
  expect_equal(markermd:::find_all_descendants(tree, 3), integer(0))
  expect_equal(markermd:::find_all_descendants(tree, NULL), integer(0))
})

test_that("find_selected_descendants intersects descendants with the current selection", {
  tree = cv_nested_tree()

  # Empty current selection short-circuits to integer(0)
  expect_equal(markermd:::find_selected_descendants(tree, 1, integer(0)), integer(0))
  # Descendants of node 1 that are currently selected
  expect_equal(markermd:::find_selected_descendants(tree, 1, c(2, 3)), c(2, 3))
  # Only node 3 is both a descendant of 2 and currently selected
  expect_equal(markermd:::find_selected_descendants(tree, 2, c(1, 3)), 3)
  # A sibling/ancestor-only selection yields no selected descendants of leaf 3
  expect_equal(markermd:::find_selected_descendants(tree, 3, c(1, 2)), integer(0))
})

test_that("compute_all_selected_nodes expands a direct selection to include descendants", {
  tree = cv_nested_tree()

  expect_equal(markermd:::compute_all_selected_nodes(tree, c(1)), c(1, 2, 3))
  expect_equal(markermd:::compute_all_selected_nodes(tree, c(2)), c(2, 3))
  expect_equal(markermd:::compute_all_selected_nodes(tree, c(3)), 3)
  expect_equal(markermd:::compute_all_selected_nodes(tree, integer(0)), integer(0))
})

test_that("has_selected_ancestor detects a directly selected ancestor", {
  tree = cv_nested_tree()

  expect_true(markermd:::has_selected_ancestor(tree, 3, c(1)))
  expect_true(markermd:::has_selected_ancestor(tree, 3, c(2)))
  expect_true(markermd:::has_selected_ancestor(tree, 2, c(1)))

  # A descendant or sibling selection is not an ancestor of the node
  expect_false(markermd:::has_selected_ancestor(tree, 1, c(2)))
  expect_false(markermd:::has_selected_ancestor(tree, 2, c(3)))

  # Empty selection and self-only selection are not "ancestor" hits
  expect_false(markermd:::has_selected_ancestor(tree, 3, integer(0)))
  expect_false(markermd:::has_selected_ancestor(tree, 1, c(1)))
})
