test_that("markermd_node_selection class works correctly", {
  # Test empty node selection
  ns1 = markermd_node_selection()
  expect_s7_class(ns1, markermd_node_selection)
  expect_equal(length(ns1), 0)
  expect_equal(ns1@indices, integer(0))
  
  # Test node selection with indices
  ns2 = markermd_node_selection(indices = c(1L, 3L, 5L))
  expect_equal(length(ns2), 3)
  expect_equal(ns2@indices, c(1L, 3L, 5L))
  
  # Test validation - negative indices should fail
  expect_error(markermd_node_selection(indices = c(-1L, 2L)), "All node indices must be >= 1")
  
  # Test validation - duplicate indices should fail  
  expect_error(markermd_node_selection(indices = c(1L, 1L, 2L)), "Node indices must be unique")
  
  # Test validation - non-finite indices should fail
  expect_error(markermd_node_selection(indices = c(1L, NA_integer_)), "All node indices must be finite")
})

test_that("markermd_question class works correctly", {
  # Test basic markermd_question creation
  ns = markermd_node_selection(indices = c(1L, 2L))
  q1 = markermd_question(id = 1L, name = "Question 1", selected_nodes = ns, strict = TRUE)
  
  expect_s7_class(q1, markermd_question)
  expect_equal(q1@id, 1L)
  expect_equal(q1@name, "Question 1")
  expect_equal(q1@selected_nodes@indices, c(1L, 2L))
  expect_equal(q1@strict, TRUE)
  
  # Test default values
  q2 = markermd_question(id = 2L, name = "Question 2")
  expect_equal(q2@strict, FALSE)
  expect_equal(length(q2@selected_nodes), 0)
  
  # Test validation - invalid ID
  expect_error(markermd_question(id = 0L, name = "Bad"), "@id must be positive")
  expect_error(markermd_question(id = c(1L, 2L), name = "Bad"), "@id must be a single integer")
  
  # Test validation - invalid name
  expect_error(markermd_question(id = 1L, name = ""), "@name cannot be empty")
  expect_error(markermd_question(id = 1L, name = c("A", "B")), "@name must be a single character string")
  
  # Test validation - invalid strict
  expect_error(markermd_question(id = 1L, name = "Test", strict = c(TRUE, FALSE)), "@strict must be a single logical value")
})

test_that("markermd_metadata class works correctly", {
  # Test with defaults
  tm1 = markermd_metadata()
  expect_s7_class(tm1, markermd_metadata)
  expect_true(inherits(tm1@created_at, "POSIXct"))
  expect_true(is.character(tm1@created_by))
  expect_equal(tm1@total_nodes, 0L)
  expect_equal(tm1@version, "1.0")
  
  # Test with custom values
  custom_time = as.POSIXct("2024-01-01 12:00:00")
  tm2 = markermd_metadata(
    created_at = custom_time,
    created_by = "test_user",
    total_nodes = 10L,
    version = "2.0"
  )
  expect_equal(tm2@created_at, custom_time)
  expect_equal(tm2@created_by, "test_user")
  expect_equal(tm2@total_nodes, 10L)
  expect_equal(tm2@version, "2.0")
  
  # Test validation - negative total_nodes
  expect_error(markermd_metadata(total_nodes = -1L), "@total_nodes must be non-negative")
  
  # Test validation - multiple values
  expect_error(markermd_metadata(created_by = c("A", "B")), "@created_by must be a single character string")
})

test_that("markermd_template class works correctly", {
  skip_if_not_installed("parsermd")
  
  # Create a simple AST for testing
  nodes = list(
    parsermd::rmd_heading(name = "Test", level = 1L),
    parsermd::rmd_chunk(code = "x = 1")
  )
  ast = parsermd::rmd_ast(nodes = nodes)
  
  # Create questions
  q1 = markermd_question(id = 1L, name = "Question 1", selected_nodes = markermd_node_selection(indices = 1L))
  q2 = markermd_question(id = 2L, name = "Question 2", selected_nodes = markermd_node_selection(indices = 2L))
  
  # Create markermd_template
  tmpl = markermd_template(original_ast = ast, questions = list(q1, q2))
  
  expect_s7_class(tmpl, markermd_template)
  expect_equal(length(tmpl), 2)
  expect_equal(length(tmpl@questions), 2)
  expect_s7_class(tmpl@metadata, markermd_metadata)
  
  # Test validation - duplicate markermd_question IDs
  q3 = markermd_question(id = 1L, name = "Duplicate ID", selected_nodes = markermd_node_selection(indices = 1L))
  expect_error(markermd_template(original_ast = ast, questions = list(q1, q3)), "Question IDs must be unique")
  
  # Test validation - duplicate markermd_question names  
  q4 = markermd_question(id = 3L, name = "Question 1", selected_nodes = markermd_node_selection(indices = 1L))
  expect_error(markermd_template(original_ast = ast, questions = list(q1, q4)), "Question names must be unique")
  
  # Test validation - invalid node indices
  q5 = markermd_question(id = 3L, name = "Question 3", selected_nodes = markermd_node_selection(indices = 10L))
  expect_error(markermd_template(original_ast = ast, questions = list(q5)), "invalid node indices")
})

test_that("conversion functions work correctly", {
  # Test as_markermd_node_selection
  ns1 = as_markermd_node_selection(c(1, 3, 5))
  expect_s7_class(ns1, markermd_node_selection)
  expect_equal(ns1@indices, c(1L, 3L, 5L))
  
  ns2 = as_markermd_node_selection(list(indices = c(2, 4)))
  expect_equal(ns2@indices, c(2L, 4L))
  
  # Test as_markermd_question  
  q_list = list(id = 1, name = "Test", selected_nodes = c(1, 2), strict = TRUE)
  q = as_markermd_question(q_list)
  expect_s7_class(q, markermd_question)
  expect_equal(q@id, 1L)
  expect_equal(q@name, "Test")
  expect_equal(q@selected_nodes@indices, c(1L, 2L))
  expect_equal(q@strict, TRUE)
  
  # Test as_markermd_metadata
  md_list = list(
    created_by = "test_user",
    total_nodes = 5,
    version = "1.5"
  )
  md = as_markermd_metadata(md_list)
  expect_s7_class(md, markermd_metadata)
  expect_equal(md@created_by, "test_user")
  expect_equal(md@total_nodes, 5L)
  expect_equal(md@version, "1.5")
})

test_that("list conversion functions work correctly", {
  # Create S7 objects
  ns = markermd_node_selection(indices = c(1L, 2L))
  q = markermd_question(id = 1L, name = "Test", selected_nodes = ns, strict = TRUE)
  md = markermd_metadata(created_by = "test", total_nodes = 5L)
  
  # Test conversions to list
  ns_list = as.list.markermd_node_selection(ns)
  expect_equal(ns_list$indices, c(1L, 2L))
  
  q_list = as.list.markermd_question(q)
  expect_equal(q_list$id, 1L)
  expect_equal(q_list$name, "Test")
  expect_equal(q_list$selected_nodes, c(1L, 2L))
  expect_equal(q_list$strict, TRUE)
  
  md_list = as.list.markermd_metadata(md)
  expect_equal(md_list$created_by, "test")
  expect_equal(md_list$total_nodes, 5L)
})

test_that("markermd_template utility functions work correctly", {
  skip_if_not_installed("parsermd")
  
  # Create test markermd_template
  nodes = list(
    parsermd::rmd_heading(name = "Test", level = 1L),
    parsermd::rmd_chunk(code = "x = 1")
  )
  ast = parsermd::rmd_ast(nodes = nodes)
  
  q1 = markermd_question(id = 1L, name = "Question 1", selected_nodes = markermd_node_selection(indices = 1L))
  tmpl = markermd_template(original_ast = ast, questions = list(q1))
  
  # Test add_question
  q2 = markermd_question(id = 2L, name = "Question 2", selected_nodes = markermd_node_selection(indices = 2L))
  tmpl2 = add_question(tmpl, q2)
  expect_equal(length(tmpl2), 2)
  
  # Test duplicate ID error
  q3 = markermd_question(id = 1L, name = "Duplicate", selected_nodes = markermd_node_selection(indices = 1L))
  expect_error(add_question(tmpl, q3), "Question ID 1 already exists")
  
  # Test duplicate name error
  q4 = markermd_question(id = 3L, name = "Question 1", selected_nodes = markermd_node_selection(indices = 1L))
  expect_error(add_question(tmpl, q4), "Question name 'Question 1' already exists")
  
  # Test remove_question
  tmpl3 = remove_question(tmpl2, 1L)
  expect_equal(length(tmpl3), 1)
  expect_equal(tmpl3@questions[[1]]@id, 2L)
  
  # Test get_question
  retrieved_q = get_question(tmpl2, 2L)
  expect_equal(retrieved_q@name, "Question 2")
  
  non_existent = get_question(tmpl2, 99L)
  expect_null(non_existent)
})

test_that("RDS serialization works correctly", {
  skip_if_not_installed("parsermd")
  
  # Create test markermd_template
  nodes = list(
    parsermd::rmd_heading(name = "Test Header", level = 1L),
    parsermd::rmd_chunk(code = "print('hello')")
  )
  ast = parsermd::rmd_ast(nodes = nodes)
  
  q1 = markermd_question(id = 1L, name = "Header Question", selected_nodes = markermd_node_selection(indices = 1L), strict = FALSE)
  q2 = markermd_question(id = 2L, name = "Code Question", selected_nodes = markermd_node_selection(indices = 2L), strict = TRUE)
  
  tmpl = markermd_template(
    original_ast = ast,
    questions = list(q1, q2),
    metadata = markermd_metadata(created_by = "test_user", total_nodes = 2L)
  )
  
  # Test round-trip serialization
  temp_file = tempfile(fileext = ".rds")
  on.exit(unlink(temp_file))
  
  saveRDS(tmpl, temp_file)
  loaded_tmpl = readRDS(temp_file)
  
  # Verify the loaded markermd_template
  expect_s7_class(loaded_tmpl, markermd_template)
  expect_equal(length(loaded_tmpl), 2)
  expect_equal(loaded_tmpl@questions[[1]]@name, "Header Question")
  expect_equal(loaded_tmpl@questions[[2]]@name, "Code Question")
  expect_equal(loaded_tmpl@questions[[1]]@strict, FALSE)
  expect_equal(loaded_tmpl@questions[[2]]@strict, TRUE)
  expect_equal(loaded_tmpl@metadata@created_by, "test_user")
  expect_equal(loaded_tmpl@metadata@total_nodes, 2L)
})

test_that("print methods work correctly", {
  # Test markermd_node_selection print
  ns = markermd_node_selection(indices = c(1L, 2L, 3L))
  expect_output(print(ns), "Node selection with 3 indices")
  expect_output(print(ns), "Indices: 1, 2, 3")
  
  # Test markermd_question print
  q = markermd_question(id = 1L, name = "Test Q", selected_nodes = ns, strict = TRUE)
  expect_output(print(q), "Question: Test Q")
  expect_output(print(q), "Selected nodes: 3")
  expect_output(print(q), "Strict mode: TRUE")
  
  # Test markermd_metadata print
  md = markermd_metadata(created_by = "test", total_nodes = 5L)
  expect_output(print(md), "Template metadata:")
  expect_output(print(md), "Created by: test")
  expect_output(print(md), "Total nodes: 5")
})