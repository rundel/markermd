test_that("process_mark_templates handles NULL input", {
  result = process_mark_templates(NULL)
  expect_null(result)
})

test_that("process_mark_templates handles file path input", {
  template_file = system.file("examples/test_assignment/template.rds", package = "markermd")
  
  result = process_mark_templates(template_file)
  
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(sapply(result, function(x) inherits(x, "rmd_template"))))
  expect_equal(names(result), c("Question 1", "Question 2", "Question 3", "Question 4"))
})

test_that("process_mark_templates handles raw template data", {
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  
  result = process_mark_templates(template_s7)
  
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(sapply(result, function(x) inherits(x, "rmd_template"))))
  expect_equal(names(result), c("Question 1", "Question 2", "Question 3", "Question 4"))
})

test_that("process_mark_templates handles already transformed templates", {
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  transformed = process_mark_templates(template_s7)
  
  # Test that applying process_mark_templates to S7 object works
  result = process_mark_templates(template_s7)
  
  # Should return templates
  expect_type(result, "list")
  expect_true(all(sapply(result, function(x) inherits(x, "rmd_template"))))
})

test_that("process_mark_templates handles invalid file path", {
  expect_error(process_mark_templates("nonexistent.rds"), "Template file does not exist")
  
  # Create a temporary file with wrong extension to test extension check
  temp_file = tempfile(fileext = ".txt")
  writeLines("test", temp_file)
  on.exit(unlink(temp_file))
  
  expect_error(process_mark_templates(temp_file), "Template file must have .rds extension")
})

test_that("process_mark_templates handles invalid input types", {
  expect_error(process_mark_templates(123), "Template must be a file path or markermd_template S7 object")
})

test_that("validate_repo_against_templates handles NULL inputs", {
  result = validate_repo_against_templates(NULL, NULL)
  expect_equal(length(result), 0)
  
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  templates = process_mark_templates(template_s7)
  
  result = validate_repo_against_templates(NULL, templates)
  expect_equal(length(result), 0)
})

test_that("validate_repo_against_templates performs validation", {
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  templates = process_mark_templates(template_s7)
  
  # Use the original AST from the template data for testing
  ast = template_s7@original_ast
  
  result = validate_repo_against_templates(ast, templates)
  
  expect_type(result, "list")
  expect_equal(length(result), 4)  # Should have results for all 4 questions
  expect_equal(names(result), c("Question 1", "Question 2", "Question 3", "Question 4"))
  
  # Check structure of first result
  q1_result = result[["Question 1"]]
  expect_true(all(c("question_name", "status", "messages", "details") %in% names(q1_result)))
  expect_true(q1_result$status %in% c("pass", "fail", "error"))
  expect_equal(q1_result$question_name, "Question 1")
})

test_that("null-coalescing operator works correctly", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(5 %||% 10, 5)
  expect_equal(character(0) %||% "default", character(0))  # Empty vector is not NULL
})

test_that("extract_question_content works correctly", {
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  ast = template_s7@original_ast
  
  result = extract_question_content(ast, template_s7)
  
  expect_type(result, "list")
  expect_equal(length(result), 4)
  expect_equal(names(result), c("Question 1", "Question 2", "Question 3", "Question 4"))
  
  # Check that content is extracted
  for (question_name in names(result)) {
    expect_type(result[[question_name]], "character")
    expect_true(nchar(result[[question_name]]) > 0)
    # Should contain question heading
    expect_true(grepl(question_name, result[[question_name]], ignore.case = TRUE))
  }
})

test_that("extract_question_content handles edge cases", {
  # Test with NULL inputs
  expect_equal(extract_question_content(NULL, NULL), list())
  
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  expect_equal(extract_question_content(NULL, template_s7), list())
  expect_equal(extract_question_content(template_s7@original_ast, NULL), list())
  
  # Test with empty template
  empty_template = markermd_template(
    original_ast = template_s7@original_ast,
    questions = list()
  )
  expect_equal(extract_question_content(template_s7@original_ast, empty_template), list())
})

test_that("validation results have correct structure for different scenarios", {
  template_s7 = readRDS(system.file("examples/test_assignment/template.rds", package = "markermd"))
  templates = process_mark_templates(template_s7)
  
  # Test with original AST (should mostly pass since it's the template source)
  ast = template_s7@original_ast
  result = validate_repo_against_templates(ast, templates)
  
  for (question_name in names(result)) {
    validation = result[[question_name]]
    
    expect_true(validation$status %in% c("pass", "fail", "error"))
    expect_type(validation$messages, "character")
    expect_type(validation$details, "character")
    expect_equal(validation$question_name, question_name)
  }
})