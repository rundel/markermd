test_that("markermd_rule can be created with valid inputs", {
  # Test count rule
  count_rule = markermd_rule(
    node_type = "rmd_heading",
    verb = "has count of",
    values = c(1, 5)
  )
  
  expect_true(S7::S7_inherits(count_rule, markermd_rule))
  expect_equal(count_rule@node_type, "rmd_heading")
  expect_equal(count_rule@verb, "has count of")
  expect_equal(count_rule@values, c(1, 5))
  
  # Test content rule
  content_rule = markermd_rule(
    node_type = "rmd_chunk",
    verb = "has content",
    values = "*plot*"
  )
  
  expect_true(S7::S7_inherits(content_rule, markermd_rule))
  expect_equal(content_rule@node_type, "rmd_chunk")
  expect_equal(content_rule@verb, "has content")
  expect_equal(content_rule@values, "*plot*")
  
  # Test name rule
  name_rule = markermd_rule(
    node_type = "Any node",
    verb = "has name",
    values = "setup*"
  )
  
  expect_true(S7::S7_inherits(name_rule, markermd_rule))
  expect_equal(name_rule@node_type, "Any node")
  expect_equal(name_rule@verb, "has name")
  expect_equal(name_rule@values, "setup*")
})

test_that("markermd_rule rejects invalid node types", {
  expect_error(
    markermd_rule(
      node_type = "invalid_type",
      verb = "has count of",
      values = c(1, 5)
    ),
    "Node type must be one of"
  )
  
  expect_error(
    markermd_rule(
      node_type = "",
      verb = "has count of",
      values = c(1, 5)
    ),
    "Node type cannot be empty"
  )
  
  expect_error(
    markermd_rule(
      node_type = c("rmd_heading", "rmd_chunk"),
      verb = "has count of",
      values = c(1, 5)
    ),
    "Node type must be a single character string"
  )
})

test_that("markermd_rule rejects invalid verbs", {
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "invalid_verb",
      values = c(1, 5)
    ),
    "Rule verb must be one of"
  )
  
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "",
      values = c(1, 5)
    ),
    "Rule verb cannot be empty"
  )
  
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = c("has content", "has name"),
      values = c(1, 5)
    ),
    "Rule verb must be a single character string"
  )
})

test_that("markermd_rule validates count values correctly", {
  # Valid count values
  expect_no_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = c(0, 10)
    )
  )
  
  # Invalid: max < min
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = c(10, 5)
    ),
    "Count range minimum must be <= maximum"
  )
  
  # Invalid: negative values
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = c(-1, 5)
    ),
    "Count range values must be non-negative"
  )
  
  # Invalid: wrong length
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = c(1)
    ),
    "Count range must have exactly 2 values"
  )
  
  # Invalid: wrong type
  expect_error(
    markermd_rule(
      node_type = "rmd_heading",
      verb = "has count of",
      values = "text"
    ),
    "Count range values must be numeric"
  )
})

test_that("markermd_rule validates content values correctly", {
  # Valid content values
  expect_no_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content",
      values = "*plot*"
    )
  )
  
  expect_no_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content",
      values = ""
    )
  )
  
  # Invalid: multiple values
  expect_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content",
      values = c("a", "b")
    ),
    "Content pattern must be a single value"
  )
  
  # Invalid: wrong type
  expect_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has content",
      values = 123
    ),
    "Content pattern must be a character string"
  )
})

test_that("markermd_rule validates name values correctly", {
  # Valid name values
  expect_no_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has name",
      values = "setup*"
    )
  )
  
  expect_no_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has name",
      values = ""
    )
  )
  
  # Invalid: multiple values
  expect_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has name",
      values = c("a", "b")
    ),
    "Name pattern must be a single value"
  )
  
  # Invalid: wrong type
  expect_error(
    markermd_rule(
      node_type = "rmd_chunk",
      verb = "has name",
      values = 123
    ),
    "Name pattern must be a character string"
  )
})

test_that("new_markermd_rule convenience function works", {
  # Default rule
  default_rule = new_markermd_rule()
  expect_true(S7::S7_inherits(default_rule, markermd_rule))
  expect_equal(default_rule@node_type, "Any node")
  expect_equal(default_rule@verb, "has count of")
  expect_equal(default_rule@values, c(0, 10))
  
  # Custom rule with defaults
  custom_rule = new_markermd_rule(
    node_type = "rmd_chunk",
    verb = "has content"
  )
  expect_equal(custom_rule@node_type, "rmd_chunk")
  expect_equal(custom_rule@verb, "has content")
  expect_equal(custom_rule@values, "")
  
  # Fully custom rule
  full_custom = new_markermd_rule(
    node_type = "rmd_heading",
    verb = "has count of",
    values = c(2, 8)
  )
  expect_equal(full_custom@values, c(2, 8))
})

test_that("validate_markermd_rule function works correctly", {
  # Valid rule
  valid_rule = markermd_rule(
    node_type = "rmd_heading",
    verb = "has count of",
    values = c(1, 5)
  )
  
  result = validate_markermd_rule(valid_rule)
  expect_type(result, "list")
  expect_true(result$valid)
  expect_length(result$errors, 0)
  
  # Test with non-rule object
  result_invalid = validate_markermd_rule("not a rule")
  expect_false(result_invalid$valid)
  expect_length(result_invalid$errors, 1)
  expect_true(grepl("not a markermd_rule", result_invalid$errors[1]))
})

test_that("rule_to_list conversion works correctly", {
  # Count rule conversion
  count_rule = markermd_rule(
    node_type = "rmd_heading",
    verb = "has count of",
    values = c(1, 5)
  )
  
  count_list = rule_to_list(count_rule, rule_id = 42)
  expect_type(count_list, "list")
  expect_equal(count_list$id, 42)
  expect_equal(count_list$node_types, "rmd_heading")
  expect_equal(count_list$verb, "has count of")
  expect_equal(count_list$verb_inputs$count_range, c(1, 5))
  expect_null(count_list$delete_observer)
  
  # Content rule conversion
  content_rule = markermd_rule(
    node_type = "rmd_chunk",
    verb = "has content",
    values = "*plot*"
  )
  
  content_list = rule_to_list(content_rule, rule_id = 7)
  expect_equal(content_list$id, 7)
  expect_equal(content_list$node_types, "rmd_chunk")
  expect_equal(content_list$verb, "has content")
  expect_equal(content_list$verb_inputs$content_pattern, "*plot*")
  
  # Name rule conversion
  name_rule = markermd_rule(
    node_type = "Any node",
    verb = "has name",
    values = "setup*"
  )
  
  name_list = rule_to_list(name_rule, rule_id = 13)
  expect_equal(name_list$verb_inputs$name_pattern, "setup*")
  
  # Test error with non-rule object
  expect_error(
    rule_to_list("not a rule"),
    "Object must be a markermd_rule"
  )
})

