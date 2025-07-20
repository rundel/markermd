test_that("create_question_templates works with valid template data", {
  # Load the sample template data (now already an S7 object)
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  
  # Test the function
  result = create_question_templates(template_s7)
  
  # Basic structure tests
  expect_type(result, "list")
  expect_equal(length(result), 4)  # Should have 4 questions
  expect_equal(names(result), c("Question 1", "Question 2", "Question 3", "Question 4"))
  
  # Check that each result is a parsermd template
  for (i in seq_along(result)) {
    expect_s3_class(result[[i]], "rmd_template")
  }
  
  # Test specific question content
  q1_template = result[["Question 1"]]
  expect_true(length(q1_template) > 0)
  
  q2_template = result[["Question 2"]]
  expect_true(length(q2_template) > 0)
})

test_that("create_question_templates handles invalid input gracefully", {
  # Test with NULL input
  expect_error(create_question_templates(NULL), "template_obj must be a markermd_template S7 object")
  
  # Test with non-S7 input
  expect_error(create_question_templates("not a template"), "template_obj must be a markermd_template S7 object")
  
  # Test with empty questions - create S7 object directly
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  empty_template = markermd_template(
    original_ast = template_s7@original_ast,
    questions = list(),
    metadata = template_s7@metadata
  )
  result = create_question_templates(empty_template)
  expect_equal(length(result), 0)
})

test_that("create_question_templates handles invalid AST", {
  # Invalid AST is now caught during S7 object creation
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  expect_error(
    markermd_template(
      original_ast = "not an ast",
      questions = template_s7@questions,
      metadata = template_s7@metadata
    ), 
    "@original_ast must be an rmd_ast object"
  )
})

test_that("create_question_templates handles invalid question structure", {
  # Invalid question structures are now caught during S7 object creation
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  
  # Test with question missing name
  expect_error(
    markermd_question(
      id = 1L,
      name = "",  # Empty name should fail
      selected_nodes = markermd_node_selection(indices = c(1L, 2L))
    ),
    "@name cannot be empty"
  )
})


test_that("create_question_templates handles empty node selections", {
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  
  # Create question with no selected nodes
  empty_question = markermd_question(
    id = 1L,
    name = "Empty Question", 
    selected_nodes = markermd_node_selection(indices = integer(0))
  )
  
  empty_template = markermd_template(
    original_ast = template_s7@original_ast,
    questions = list(empty_question),
    metadata = template_s7@metadata
  )
  
  expect_warning(
    result <- create_question_templates(empty_template), 
    "with no selected nodes"
  )
  expect_equal(length(result), 0)
})

test_that("create_question_templates produces correct template content", {
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  result = create_question_templates(template_s7)
  
  # Check Question 1 specifically (should contain nodes 4 and 5: heading + markdown)
  q1_template = result[["Question 1"]]
  
  # Check that template is a proper rmd_template
  expect_s3_class(q1_template, "rmd_template")
  expect_s3_class(q1_template, "tbl_df")
  
  # The template should have the expected structure for a section
  # rmd_template() groups heading + content into sections, so 2 nodes become 1 row
  expect_true(nrow(q1_template) >= 1)
  
  # Check that the template contains expected section heading
  # Question 1 nodes 4 and 5 are: heading "Question 1: Understanding R" + markdown
  expect_true("sec_h2" %in% names(q1_template))
  if (nrow(q1_template) > 0) {
    expect_true(any(grepl("Question 1.*Understanding R", q1_template$sec_h2, ignore.case = TRUE)))
  }
  
  # Check that different questions produce different templates
  q2_template = result[["Question 2"]]
  expect_false(identical(q1_template, q2_template))
})