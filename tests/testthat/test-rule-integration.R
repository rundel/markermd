test_that("rules module uses helper functions correctly", {
  # Test that the helper functions return the expected values 
  # that the rules module depends on
  
  node_types = get_allowed_node_types()
  verbs = get_allowed_rule_verbs()
  
  # Test specific values expected by the module
  expect_true("Any node" %in% node_types)
  expect_equal(node_types[1], "Any node")  # First option should be "Any node"
  
  expect_true("has count of" %in% verbs)
  expect_equal(verbs[1], "has count of")  # First option should be "has count of"
  
  # Test that these values work with S7 validation
  expect_no_error(
    markermd_rule(
      node_type = node_types[1],
      verb = verbs[1], 
      values = get_default_rule_values(verbs[1])
    )
  )
})

test_that("S7 rules can represent all module rule states", {
  # Test creating S7 rules that match typical module rule structures
  
  # Default new rule (as created by module)
  default_s7 = markermd_rule(
    node_type = "Any node",
    verb = "has count of",
    values = c(0, 10)  # typical default range
  )
  
  default_list = rule_to_list(default_s7, rule_id = 1)
  expect_equal(default_list$node_types, "Any node")
  expect_equal(default_list$verb, "has count of")
  expect_type(default_list$verb_inputs$count_range, "double")
  
  # Content rule with pattern
  content_s7 = markermd_rule(
    node_type = "rmd_chunk",
    verb = "has content",
    values = "*plot*"
  )
  
  content_list = rule_to_list(content_s7, rule_id = 2)
  expect_equal(content_list$verb_inputs$content_pattern, "*plot*")
  
  # Name rule with pattern
  name_s7 = markermd_rule(
    node_type = "rmd_chunk", 
    verb = "has name",
    values = "setup*"
  )
  
  name_list = rule_to_list(name_s7, rule_id = 3)
  expect_equal(name_list$verb_inputs$name_pattern, "setup*")
  
  # Negative content rule
  not_content_s7 = markermd_rule(
    node_type = "rmd_markdown",
    verb = "does not have content",
    values = "TODO"
  )
  
  not_content_list = rule_to_list(not_content_s7, rule_id = 4)
  expect_equal(not_content_list$verb_inputs$content_pattern, "TODO")
})

test_that("S7 rules can be created directly with valid inputs", {
  # Test creating S7 rules directly (replacing list conversion)
  
  # Count rule
  count_s7 = markermd_rule(
    node_type = "rmd_heading",
    verb = "has count of",
    values = c(1, 3)
  )
  expect_true(S7::S7_inherits(count_s7, markermd_rule))
  validation = validate_markermd_rule(count_s7)
  expect_true(validation$valid)
  
  # Content rule  
  content_s7 = markermd_rule(
    node_type = "rmd_chunk",
    verb = "has content",
    values = "ggplot"
  )
  expect_true(S7::S7_inherits(content_s7, markermd_rule))
  validation = validate_markermd_rule(content_s7)
  expect_true(validation$valid)
  
  # Name rule with defaults
  minimal_s7 = markermd_rule(
    node_type = "Any node",
    verb = "has name",
    values = ""
  )
  expect_equal(minimal_s7@values, "")  # Should use default
  validation = validate_markermd_rule(minimal_s7)
  expect_true(validation$valid)
})

test_that("S7 validation catches module rule inconsistencies", {
  # Test that S7 validation would catch invalid rule states 
  # that might occur in the module
  
  # The conversion function is designed to be robust, so we test
  # validation with a manually created invalid S7 object
  
  # Create a rule and then try to validate with mismatched values
  expect_error({
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content",
      values = c(1, 5)  # Wrong type - should be character for content verb
    )
  }, "Content pattern must be a single value")
  
  # Test validation of boundary conditions
  expect_error({
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = "not_numeric"  # Wrong type for count verb
    )
  }, "Count range values must be numeric")
})

test_that("S7 rules can be converted to list format for module compatibility", {
  # Test that rules can go S7 -> list and maintain expected structure
  
  test_rules = list(
    # Count rule
    markermd_rule("rmd_heading", "has count of", c(2, 5)),
    # Content rules
    markermd_rule("rmd_chunk", "has content", "plot"),
    markermd_rule("rmd_markdown", "does not have content", "ERROR"),
    # Name rule
    markermd_rule("rmd_chunk", "has name", "analysis*")
  )
  
  for (i in seq_along(test_rules)) {
    original = test_rules[[i]]
    
    # Convert to list (as module would store)
    as_list = rule_to_list(original, rule_id = i)
    
    # Should have expected structure
    expect_type(as_list, "list")
    expect_equal(as_list$node_types, original@node_type)
    expect_equal(as_list$verb, original@verb)
    expect_equal(as_list$values, original@values)
  }
})

test_that("helper functions provide single source of truth", {
  # Test that changing helper functions would affect both module and S7
  
  # These functions should be the authoritative source
  node_types = get_allowed_node_types()
  verbs = get_allowed_rule_verbs()
  
  # Test that every allowed value works in S7 validation
  for (node_type in node_types) {
    node_error = validate_node_type(node_type)
    expect_null(node_error, 
               info = paste("Helper function returned invalid node type:", node_type))
  }
  
  for (verb in verbs) {
    verb_error = validate_rule_verb(verb)
    expect_null(verb_error,
               info = paste("Helper function returned invalid verb:", verb))
    
    # Test that defaults work for each verb
    defaults = get_default_rule_values(verb)
    values_error = validate_rule_values(verb, defaults)
    expect_null(values_error,
               info = paste("Default values invalid for verb:", verb))
  }
})

test_that("integration with existing template structure", {
  # Test that rules can be integrated with the existing template classes
  
  # Create some rules
  rules = list(
    markermd_rule("rmd_heading", "has count of", c(1, 3)),
    markermd_rule("rmd_chunk", "has content", "*analysis*")
  )
  
  # Convert to lists for template storage
  rule_lists = lapply(seq_along(rules), function(i) {
    rule_to_list(rules[[i]], rule_id = i)
  })
  
  # These should be compatible with template question structure
  for (rule_list in rule_lists) {
    expect_type(rule_list, "list")
    expect_true("id" %in% names(rule_list))
    expect_true("node_types" %in% names(rule_list))
    expect_true("verb" %in% names(rule_list))
    expect_true("verb_inputs" %in% names(rule_list))
  }
})