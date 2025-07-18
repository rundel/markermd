test_that("create_question_templates works with valid template data", {
  # Load the sample template data
  template_data = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  
  # Test the function
  result = create_question_templates(template_data)
  
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
  expect_error(create_question_templates(NULL), "template_data must be a list")
  
  # Test with non-list input
  expect_error(create_question_templates("not a list"), "template_data must be a list")
  
  # Test with missing components
  incomplete_data = list(questions = list())
  expect_error(create_question_templates(incomplete_data), "template_data missing required components: original_ast")
  
  # Test with empty questions
  template_data = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  template_data$questions = list()
  result = create_question_templates(template_data)
  expect_equal(length(result), 0)
})

test_that("create_question_templates handles invalid AST", {
  template_data = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  
  # Test with invalid AST type
  template_data$original_ast = "not an ast"
  expect_error(create_question_templates(template_data), "original_ast must be an rmd_ast object")
})

test_that("create_question_templates handles invalid question structure", {
  template_data = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  
  # Test with question missing name
  invalid_questions = list(
    list(id = 1, selected_nodes = c(1, 2)),  # Missing name
    list(id = 2, name = "Valid Question", selected_nodes = c(3, 4))
  )
  template_data$questions = invalid_questions
  
  expect_warning(result <- create_question_templates(template_data), "Skipping invalid question")
  expect_equal(length(result), 1)  # Only the valid question should be included
  expect_equal(names(result), "Valid Question")
})

test_that("create_question_templates handles invalid node indices", {
  template_data = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  
  # Create question with invalid node indices
  total_nodes = length(template_data$original_ast@nodes)
  invalid_questions = list(
    list(
      id = 1,
      name = "Invalid Indices Question",
      selected_nodes = c(1, total_nodes + 5, -1)  # Some valid, some invalid
    )
  )
  template_data$questions = invalid_questions
  
  expect_warning(result <- create_question_templates(template_data), "has invalid node indices")
  # Should still create template with valid indices
  expect_equal(length(result), 1)
})

test_that("create_question_templates handles empty node selections", {
  template_data = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  
  # Create question with no selected nodes
  empty_questions = list(
    list(
      id = 1,
      name = "Empty Question", 
      selected_nodes = integer(0)
    )
  )
  template_data$questions = empty_questions
  
  expect_warning(result <- create_question_templates(template_data), "with no selected nodes")
  expect_equal(length(result), 0)
})

test_that("create_question_templates produces correct template content", {
  template_data = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  result = create_question_templates(template_data)
  
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
  
  # Check that templates contain the correct node types
  expect_true("type" %in% names(q1_template))
  # Question 1 should contain markdown content (after the heading)
  expect_true(any(grepl("markdown", q1_template$type, ignore.case = TRUE)))
})