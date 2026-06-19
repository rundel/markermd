# Coverage tests for rule-classes.R and filter-helpers.R (subsystem: rules-filters)

crf_group = function(...) {
  markermd:::markermd_filter_group(conditions = list(...))
}

crf_cond = function(type, value, negate = FALSE) {
  markermd:::markermd_filter_condition(type = type, value = value, negate = negate)
}


test_that("markermd_rule validator cross-checks verb against values at construction", {
  # "has at least" wants a single numeric, not a string
  expect_error(
    markermd::markermd_rule("Any node", "has at least", "foo"),
    "Count value must be numeric"
  )

  # "has between" requires min <= max
  expect_error(
    markermd::markermd_rule("Any node", "has between", c(5, 1)),
    "minimum must be <= maximum"
  )

  # "has between" requires exactly two values
  expect_error(
    markermd::markermd_rule("Chunk", "has between", 1),
    "exactly 2 values"
  )

  # positive control: a consistent verb/values pair constructs cleanly
  rule = markermd::markermd_rule("Chunk", "has between", c(1, 5))
  expect_s7_class(rule, markermd::markermd_rule)
  expect_equal(rule@node_type, "Chunk")
  expect_equal(rule@verb, "has between")
  expect_equal(rule@values, c(1, 5))
})


test_that("new_markermd_rule fills defaults for every allowed verb", {
  for (verb in markermd::get_allowed_rule_verbs()) {
    rule = markermd::new_markermd_rule(verb = verb)
    expect_s7_class(rule, markermd::markermd_rule)
    expect_equal(rule@node_type, "Any node", label = verb)
    expect_equal(rule@verb, verb, label = verb)
    expect_identical(
      rule@values,
      markermd::get_default_rule_values(verb),
      label = verb
    )
  }
})


test_that("new_markermd_rule honors explicit node_type and values overrides", {
  rule = markermd::new_markermd_rule(
    node_type = "Chunk",
    verb = "has at least",
    values = 3L
  )
  expect_equal(rule@node_type, "Chunk")
  expect_equal(rule@verb, "has at least")
  expect_identical(rule@values, 3L)
})


test_that("validate_markermd_rule rejects non-rules and accepts valid rules", {
  bad = markermd::validate_markermd_rule(list())
  expect_false(bad$valid)
  expect_true(length(bad$errors) > 0)
  expect_true(nzchar(paste(bad$errors, collapse = "")))

  ok = markermd::validate_markermd_rule(markermd::new_markermd_rule())
  expect_true(ok$valid)
  expect_length(ok$errors, 0)
})


test_that("filter_value_warnings flags an uncompilable regex value", {
  warns = markermd:::filter_value_warnings(
    list(crf_group(crf_cond("has text", "[unclosed", negate = FALSE)))
  )
  expect_match(warns, "not a valid regular expression", all = FALSE)
})


test_that("filter_value_warnings flags a 'has option' value with an empty key", {
  warns = markermd:::filter_value_warnings(
    list(crf_group(crf_cond("has option", ": false", negate = FALSE)))
  )
  expect_match(warns, "needs an option key", all = FALSE)
})


test_that("filter_value_warnings is silent for a clean condition", {
  warns = markermd:::filter_value_warnings(
    list(crf_group(crf_cond("has class", "hint", negate = FALSE)))
  )
  expect_length(warns, 0)
})
