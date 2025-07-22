test_that("rule validation handles edge cases for count values", {
  # Test boundary values
  expect_no_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = c(0, 0)  # min = max = 0
    )
  )
  
  expect_no_error(
    markermd_rule(
      node_type = "rmd_heading", 
      verb = "has count of",
      values = c(100, 1000)  # large values
    )
  )
  
  # Test with integer vs numeric
  expect_no_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of", 
      values = c(1L, 5L)  # integers
    )
  )
  
  # Test with floating point precision edge cases
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = c(1.1, 5.9)  # non-integers should still work
    ),
    NA  # Should not error
  )
  
  # Test with very close but invalid range
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = c(5.000001, 5.0)  # min > max by tiny amount
    ),
    "Count range minimum must be <= maximum"
  )
})

test_that("rule validation handles special string cases", {
  # Test empty strings for content patterns
  expect_no_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content",
      values = ""
    )
  )
  
  # Test whitespace-only strings
  expect_no_error(
    markermd_rule(
      node_type = "rmd_chunk", 
      verb = "has content",
      values = "   "
    )
  )
  
  # Test strings with special regex characters
  expect_no_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content", 
      values = ".*[]+{}()^$|\\?"
    )
  )
  
  # Test unicode strings
  expect_no_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content",
      values = "测试内容"
    )
  )
  
  # Test very long strings
  long_string = paste(rep("a", 1000), collapse = "")
  expect_no_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content",
      values = long_string
    )
  )
})

test_that("rule validation handles all node type combinations", {
  node_types = get_allowed_node_types()
  verbs = get_allowed_rule_verbs()
  
  # Test each node type with each verb
  for (node_type in node_types) {
    for (verb in verbs) {
      default_values = get_default_rule_values(verb)
      
      expect_no_error({
        markermd_rule(
          node_type = node_type,
          verb = verb,
          values = default_values
        )
      })
    }
  }
})

test_that("rule validation provides informative error messages", {
  # Test node type error message
  expect_error(
    markermd_rule(
      node_type = "nonexistent_type",
      verb = "has count of",
      values = c(1, 5)
    ),
    "Node type must be one of: Any node, rmd_yaml, rmd_heading, rmd_chunk, rmd_raw_chunk, rmd_markdown, rmd_code_block, rmd_fenced_div_open, rmd_fenced_div_close"
  )
  
  # Test verb error message
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "nonexistent_verb", 
      values = c(1, 5)
    ),
    "Rule verb must be one of: has count of, has content, does not have content, has name"
  )
  
  # Test detailed count validation error
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = c(10, 5)
    ),
    "Count range minimum must be <= maximum"
  )
  
  # Test content validation error
  expect_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content",
      values = c("a", "b")
    ),
    "Content pattern must be a single value"
  )
})

test_that("validate_markermd_rule handles complex error combinations", {
  # Create a rule with multiple errors by bypassing normal validation
  # This tests the standalone validation function
  
  # Test with a valid rule first
  valid_rule = markermd_rule(
    node_type = "rmd_heading",
    verb = "has count of", 
    values = c(1, 5)
  )
  
  result = validate_markermd_rule(valid_rule)
  expect_true(result$valid)
  expect_length(result$errors, 0)
  
  # Test error accumulation by manually creating invalid components
  # Note: We test this through the helper functions since S7 prevents
  # creation of invalid objects
  
  node_error = validate_node_type("invalid")
  verb_error = validate_rule_verb("invalid") 
  values_error = validate_rule_values("has count of", "invalid")
  
  expect_type(node_error, "character")
  expect_type(verb_error, "character") 
  expect_type(values_error, "character")
})

test_that("rule_to_list conversion function handles edge cases", {
  # Test that rule_to_list works with various rule configurations
  
  # Test conversion back to list maintains structure
  rule = markermd_rule(
    node_type = "rmd_chunk",
    verb = "does not have content", 
    values = "TODO"
  )
  
  rule_list = rule_to_list(rule, rule_id = 999)
  expect_equal(rule_list$id, 999)
  expect_equal(rule_list$verb_inputs$content_pattern, "TODO")
  expect_null(rule_list$delete_observer)
})

test_that("default rule values are consistent", {
  # Test that defaults are valid for their respective verbs
  verbs = get_allowed_rule_verbs()
  
  for (verb in verbs) {
    default_values = get_default_rule_values(verb)
    validation_error = validate_rule_values(verb, default_values)
    
    expect_null(validation_error, 
               info = paste("Default values for", verb, "are invalid:", validation_error))
  }
  
  # Test that new_markermd_rule uses same defaults
  for (verb in verbs) {
    expected_defaults = get_default_rule_values(verb)
    
    rule = new_markermd_rule(verb = verb)
    expect_equal(rule@values, expected_defaults,
                info = paste("new_markermd_rule defaults don't match for", verb))
  }
})

test_that("helper functions are used consistently", {
  # Test that the rules module would use the same values
  # (This is more of an integration test)
  
  module_node_types = get_allowed_node_types()
  module_verbs = get_allowed_rule_verbs()
  
  # These should match what's expected in the rules module
  expect_true("Any node" %in% module_node_types)
  expect_true("rmd_heading" %in% module_node_types)
  expect_true("has count of" %in% module_verbs)
  expect_true("has content" %in% module_verbs)
  
  # Test that default rule creation would work with first elements
  expect_no_error(
    markermd_rule(
      node_type = module_node_types[1],  # "Any node"
      verb = module_verbs[1],            # "has count of"
      values = get_default_rule_values(module_verbs[1])
    )
  )
})