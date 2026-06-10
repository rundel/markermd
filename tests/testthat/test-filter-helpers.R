test_that("get_allowed_filter_condition_types returns the expected types", {
  expect_equal(
    markermd::get_allowed_filter_condition_types(),
    c("node type", "has class", "has id", "has text", "has label", "has option", "has engine")
  )
})


test_that("parse_filter_option splits keys from YAML-typed values", {
  expect_equal(markermd:::parse_filter_option("eval"), list(key = "eval", value = NULL))
  expect_equal(markermd:::parse_filter_option(" eval "), list(key = "eval", value = NULL))
  expect_equal(markermd:::parse_filter_option("eval:"), list(key = "eval", value = NULL))

  # value typing mirrors the YAML scalars cell_options() produces
  expect_identical(markermd:::parse_filter_option("eval: false")$value, FALSE)
  expect_identical(markermd:::parse_filter_option("echo: TRUE")$value, TRUE)
  expect_identical(markermd:::parse_filter_option("warning: no")$value, FALSE)
  expect_identical(markermd:::parse_filter_option("fig-width: 5")$value, 5L)
  expect_identical(markermd:::parse_filter_option("fig-width: 5.5")$value, 5.5)
  expect_identical(markermd:::parse_filter_option("label: q1-plot")$value, "q1-plot")

  # only the first colon separates, so values may contain colons
  expect_equal(
    markermd:::parse_filter_option("fig-cap: a: b"),
    list(key = "fig-cap", value = "a: b")
  )
})


test_that("validate_filter_condition_type accepts allowed types and rejects others", {
  for (type in markermd::get_allowed_filter_condition_types()) {
    expect_null(markermd::validate_filter_condition_type(type))
  }

  expect_match(markermd::validate_filter_condition_type("bogus"), "must be one of")
  expect_match(markermd::validate_filter_condition_type(c("has id", "has class")), "single")
  expect_match(markermd::validate_filter_condition_type(NA_character_), "empty or NA")
  expect_match(markermd::validate_filter_condition_type(""), "empty or NA")
})


test_that("validate_filter_condition_value enforces type-specific requirements", {
  expect_null(markermd::validate_filter_condition_value("node type", "Heading"))
  expect_match(markermd::validate_filter_condition_value("node type", "Any node"), "must be one of")
  expect_match(markermd::validate_filter_condition_value("node type", "bogus"), "must be one of")

  # the textual types take any single string, empty allowed
  expect_null(markermd::validate_filter_condition_value("has class", "hint"))
  expect_null(markermd::validate_filter_condition_value("has text", ""))
  expect_match(markermd::validate_filter_condition_value("has class", c("a", "b")), "single")
  expect_match(markermd::validate_filter_condition_value("has class", NA_character_), "NA")

  # an invalid type is reported before the value is checked
  expect_match(markermd::validate_filter_condition_value("bogus", "x"), "must be one of")
})


test_that("get_default_filter_condition_value returns sensible defaults", {
  expect_equal(markermd::get_default_filter_condition_value("node type"), "Div")
  for (type in setdiff(markermd::get_allowed_filter_condition_types(), "node type")) {
    expect_equal(markermd::get_default_filter_condition_value(type), "")
  }
  expect_null(markermd::get_default_filter_condition_value("bogus"))
})


test_that("filter_node_kind_expr covers every node kind except 'Any node'", {
  kinds = setdiff(markermd::get_allowed_node_types(), "Any node")
  for (kind in kinds) {
    expect_true(is.language(markermd:::filter_node_kind_expr(kind)), label = kind)
  }
  expect_error(markermd:::filter_node_kind_expr("Any node"), "Unknown node kind")
})


test_that("negate properties default to FALSE and reject non-scalar logicals", {
  cond = markermd::markermd_filter_condition(type = "has class", value = "hint")
  expect_false(cond@negate)
  expect_true(markermd::new_markermd_filter_condition(negate = TRUE)@negate)

  group = markermd::markermd_filter_group()
  expect_false(group@negate)
  expect_true(markermd::new_markermd_filter_group(negate = TRUE)@negate)

  expect_error(
    markermd::markermd_filter_condition(type = "has class", value = "x", negate = NA),
    "non-NA logical"
  )
  expect_error(
    markermd::markermd_filter_condition(type = "has class", value = "x", negate = c(TRUE, FALSE)),
    "single non-NA logical"
  )
  expect_error(markermd::markermd_filter_group(negate = NA), "non-NA logical")
})


test_that("filter condition and group classes validate their contents", {
  cond = markermd::markermd_filter_condition(type = "has class", value = "hint")
  expect_equal(cond@type, "has class")
  expect_equal(cond@value, "hint")

  expect_error(markermd::markermd_filter_condition(type = "bogus", value = "x"), "must be one of")
  expect_error(markermd::markermd_filter_condition(type = "node type", value = "bogus"), "must be one of")

  group = markermd::markermd_filter_group(conditions = list(cond))
  expect_length(group@conditions, 1)
  expect_error(markermd::markermd_filter_group(conditions = list("x")), "markermd_filter_condition")

  # the default group holds a single default condition
  default_group = markermd::new_markermd_filter_group()
  expect_length(default_group@conditions, 1)
  expect_equal(default_group@conditions[[1]]@type, "node type")
  expect_equal(default_group@conditions[[1]]@value, "Div")
})


test_that("a question validates its filters property", {
  group = markermd::new_markermd_filter_group()
  q = markermd::markermd_question(id = 1L, name = "Q", filters = list(group))
  expect_length(q@filters, 1)

  # filters defaults to an empty list
  expect_equal(markermd::markermd_question(id = 1L, name = "Q")@filters, list())

  expect_error(
    markermd::markermd_question(id = 1L, name = "Q", filters = list("x")),
    "markermd_filter_group"
  )
})
