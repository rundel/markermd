test_that("get_allowed_node_types returns expected values", {
  node_types = get_allowed_node_types()
  
  expect_type(node_types, "character")
  expect_length(node_types, 9)
  expect_true("Any node" %in% node_types)
  expect_true("rmd_heading" %in% node_types)
  expect_true("rmd_chunk" %in% node_types)
  expect_true("rmd_yaml" %in% node_types)
  
  # Should be no duplicates
  expect_equal(length(node_types), length(unique(node_types)))
})

test_that("get_allowed_rule_verbs returns expected values", {
  verbs = get_allowed_rule_verbs()
  
  expect_type(verbs, "character")
  expect_length(verbs, 4)
  expect_true("has count of" %in% verbs)
  expect_true("has content" %in% verbs)
  expect_true("does not have content" %in% verbs)
  expect_true("has name" %in% verbs)
  
  # Should be no duplicates
  expect_equal(length(verbs), length(unique(verbs)))
})

test_that("validate_node_type works correctly", {
  # Valid node types
  expect_null(validate_node_type("Any node"))
  expect_null(validate_node_type("rmd_heading"))
  expect_null(validate_node_type("rmd_chunk"))
  
  # Invalid node types
  expect_type(validate_node_type("invalid_type"), "character")
  expect_type(validate_node_type(""), "character")
  expect_type(validate_node_type(NA_character_), "character")
  expect_type(validate_node_type(c("rmd_heading", "rmd_chunk")), "character")
  expect_type(validate_node_type(123), "character")
})

test_that("validate_rule_verb works correctly", {
  # Valid verbs
  expect_null(validate_rule_verb("has count of"))
  expect_null(validate_rule_verb("has content"))
  expect_null(validate_rule_verb("does not have content"))
  expect_null(validate_rule_verb("has name"))
  
  # Invalid verbs
  expect_type(validate_rule_verb("invalid_verb"), "character")
  expect_type(validate_rule_verb(""), "character")
  expect_type(validate_rule_verb(NA_character_), "character")
  expect_type(validate_rule_verb(c("has content", "has name")), "character")
  expect_type(validate_rule_verb(123), "character")
})

test_that("validate_rule_values works for 'has count of' verb", {
  # Valid count values
  expect_null(validate_rule_values("has count of", c(0, 10)))
  expect_null(validate_rule_values("has count of", c(1, 1)))
  expect_null(validate_rule_values("has count of", c(5, 100)))
  
  # Invalid count values
  expect_type(validate_rule_values("has count of", c(10, 5)), "character")  # max < min
  expect_type(validate_rule_values("has count of", c(-1, 5)), "character")  # negative
  expect_type(validate_rule_values("has count of", c(1)), "character")      # wrong length
  expect_type(validate_rule_values("has count of", c(1, 2, 3)), "character") # wrong length
  expect_type(validate_rule_values("has count of", "text"), "character")    # wrong type
  expect_type(validate_rule_values("has count of", c(NA, 5)), "character")  # NA values
  expect_type(validate_rule_values("has count of", c(Inf, 5)), "character") # infinite
})

test_that("validate_rule_values works for content verbs", {
  # Valid content values
  expect_null(validate_rule_values("has content", "pattern"))
  expect_null(validate_rule_values("has content", ""))
  expect_null(validate_rule_values("has content", "*plot*"))
  expect_null(validate_rule_values("does not have content", "pattern"))
  expect_null(validate_rule_values("does not have content", ""))
  
  # Invalid content values
  expect_type(validate_rule_values("has content", c("a", "b")), "character")  # multiple values
  expect_type(validate_rule_values("has content", 123), "character")         # wrong type
  expect_type(validate_rule_values("has content", NA_character_), "character") # NA
  expect_type(validate_rule_values("does not have content", c("a", "b")), "character")
})

test_that("validate_rule_values works for 'has name' verb", {
  # Valid name values
  expect_null(validate_rule_values("has name", "pattern"))
  expect_null(validate_rule_values("has name", ""))
  expect_null(validate_rule_values("has name", "setup*"))
  
  # Invalid name values
  expect_type(validate_rule_values("has name", c("a", "b")), "character")  # multiple values
  expect_type(validate_rule_values("has name", 123), "character")         # wrong type
  expect_type(validate_rule_values("has name", NA_character_), "character") # NA
})

test_that("validate_rule_values handles invalid verbs", {
  expect_type(validate_rule_values("invalid_verb", c(1, 2)), "character")
})

test_that("get_default_rule_values returns appropriate defaults", {
  # Test each verb type
  count_default = get_default_rule_values("has count of")
  expect_type(count_default, "double")
  expect_length(count_default, 2)
  expect_true(count_default[1] <= count_default[2])
  
  content_default = get_default_rule_values("has content")
  expect_type(content_default, "character")
  expect_length(content_default, 1)
  
  not_content_default = get_default_rule_values("does not have content")
  expect_type(not_content_default, "character")
  expect_length(not_content_default, 1)
  
  name_default = get_default_rule_values("has name")
  expect_type(name_default, "character")
  expect_length(name_default, 1)
  
  # Unknown verb should return NULL
  expect_null(get_default_rule_values("unknown_verb"))
})