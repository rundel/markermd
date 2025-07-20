test_that("template app can be created without errors", {
  skip_if_not_installed("shinytest2")
  
  # Test that the template app can be created programmatically
  # This is a more practical test than trying to run the full interactive app
  
  # Create a temporary test assignment directory
  temp_dir = tempdir()
  test_assignment_dir = file.path(temp_dir, "test_assignment")
  dir.create(test_assignment_dir, recursive = TRUE)
  
  # Create a simple test qmd file
  test_qmd = file.path(test_assignment_dir, "assignment.qmd")
  writeLines(c(
    "---",
    "title: Test Assignment",
    "---",
    "",
    "# Introduction",
    "",
    "This is a test assignment.",
    "",
    "## Question 1",
    "",
    "**Answer the following question:**",
    "",
    "R is a programming language.",
    "",
    "## Question 2", 
    "",
    "**Write some code:**",
    "",
    "```{r}",
    "x = 1 + 1",
    "```"
  ), test_qmd)
  
  # Test that the template app can be created
  expect_no_error({
    suppressWarnings({
      app = markermd:::create_template_app(
        assignment_path = test_assignment_dir,
        local_dir = NULL,
        filename = "assignment.qmd",
        is_github_repo = FALSE,
        template_obj = NULL,
        file_path = test_qmd,
        ast = parsermd::parse_qmd(test_qmd)
      )
    })
  })
  
  # Clean up
  unlink(test_assignment_dir, recursive = TRUE)
})

test_that("template app initializes without errors", {
  # Test that the template app can be created without errors
  test_file = file.path("..", "..", "inst", "examples", "test_assignment", "student1-excellent")
  
  # Suppress warnings about bslib navigation structure
  suppressWarnings({
    app = markermd:::create_template_app(
      assignment_path = test_file,
      local_dir = NULL,
      filename = "assignment.qmd",
      is_github_repo = FALSE,
      template_obj = NULL,
      file_path = file.path(test_file, "assignment.qmd"),
      ast = parsermd::parse_qmd(file.path(test_file, "assignment.qmd"))
    )
  })
  
  # Test that the app has the correct structure
  expect_s3_class(app, "shiny.appobj")
  expect_true(is.function(app$serverFuncSource))
  # Note: UI structure may vary due to bslib navigation structure, so we focus on the key functionality
})

test_that("find_node_children function works correctly", {
  # Test the fixed find_node_children function
  library(parsermd)
  
  # Parse a simple test document
  test_file = file.path("..", "..", "inst", "examples", "test_assignment", "student1-excellent", "assignment.qmd")
  test_ast = parse_qmd(test_file)
  tree_items = markermd:::build_ast_tree_structure(test_ast)
  
  # Test that h2 node (node 4) has the correct children
  children_of_4 = markermd:::find_node_children(tree_items, 4)
  expect_true(length(children_of_4) > 0)
  expect_true(5 %in% children_of_4)  # Should include the markdown nodes under h2
  
  # Test that a markdown node (node 5) has no children
  children_of_5 = markermd:::find_node_children(tree_items, 5)
  expect_equal(length(children_of_5), 0)
  
  # Test that document root (node 0) has children
  children_of_root = markermd:::find_node_children(tree_items, 0)
  expect_true(length(children_of_root) > 0)
  expect_true(1 %in% children_of_root)  # Should include YAML
  expect_true(2 %in% children_of_root)  # Should include h1
})