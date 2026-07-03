test_that("validate_node_type accepts allowed types and rejects everything else", {
  expect_null(markermd:::validate_node_type("Heading"))
  expect_null(markermd:::validate_node_type("Any node"))
  expect_null(markermd:::validate_node_type(c("Markdown", "Raw Block")))
  expect_null(markermd:::validate_node_type(markermd::get_allowed_node_types()))

  expect_match(markermd:::validate_node_type(1), "character")
  expect_match(markermd:::validate_node_type(character(0)), "at least one")
  expect_match(markermd:::validate_node_type(NA_character_), "empty or NA")
  expect_match(markermd:::validate_node_type(c("Heading", "")), "empty or NA")
  expect_match(markermd:::validate_node_type(c("Heading", "Heading")), "unique")
  expect_match(markermd:::validate_node_type("pandoc_header"), "must be one of")
  expect_match(markermd:::validate_node_type("heading"), "must be one of")
})


test_that("validate_rule_verb accepts the verb vocabulary and rejects everything else", {
  for (verb in markermd::get_allowed_rule_verbs()) {
    expect_null(markermd:::validate_rule_verb(verb))
  }

  expect_match(markermd:::validate_rule_verb("contains"), "must be one of")
  expect_match(markermd:::validate_rule_verb(c("has at least", "has at most")), "single")
  expect_match(markermd:::validate_rule_verb(character(0)), "single")
  expect_match(markermd:::validate_rule_verb(NA_character_), "empty or NA")
  expect_match(markermd:::validate_rule_verb(""), "empty or NA")
  expect_match(markermd:::validate_rule_verb(1), "single|character")
})


test_that("validate_rule_values checks 'has between' ranges", {
  expect_null(markermd:::validate_rule_values("has between", c(1, 3)))
  expect_null(markermd:::validate_rule_values("has between", c(0, 0)))
  expect_null(markermd:::validate_rule_values("has between", c(2L, 2L)))

  expect_match(markermd:::validate_rule_values("has between", 1), "exactly 2")
  expect_match(markermd:::validate_rule_values("has between", c(1, 2, 3)), "exactly 2")
  expect_match(markermd:::validate_rule_values("has between", c("1", "2")), "numeric")
  expect_match(markermd:::validate_rule_values("has between", c(1, NA)), "finite")
  expect_match(markermd:::validate_rule_values("has between", c(1, Inf)), "finite")
  expect_match(markermd:::validate_rule_values("has between", c(-1, 2)), "non-negative")
  expect_match(markermd:::validate_rule_values("has between", c(3, 1)), "<=")
  expect_match(markermd:::validate_rule_values("has between", c(1.5, 2.5)), "whole number")
})


test_that("validate_rule_values checks 'has at least' / 'has at most' counts", {
  for (verb in c("has at least", "has at most")) {
    expect_null(markermd:::validate_rule_values(verb, 0))
    expect_null(markermd:::validate_rule_values(verb, 1L))
    expect_null(markermd:::validate_rule_values(verb, 5))

    expect_match(markermd:::validate_rule_values(verb, c(1, 2)), "single")
    expect_match(markermd:::validate_rule_values(verb, "1"), "numeric")
    expect_match(markermd:::validate_rule_values(verb, NA_real_), "finite")
    expect_match(markermd:::validate_rule_values(verb, -1), "non-negative")
    expect_match(markermd:::validate_rule_values(verb, 2.5), "whole number")
  }
})


test_that("validate_rule_values checks pattern verbs", {
  for (verb in c("has content", "lacks content", "has name")) {
    expect_null(markermd:::validate_rule_values(verb, "some pattern"))
    expect_null(markermd:::validate_rule_values(verb, ""))

    expect_match(markermd:::validate_rule_values(verb, c("a", "b")), "single")
    expect_match(markermd:::validate_rule_values(verb, 1), "character")
    expect_match(markermd:::validate_rule_values(verb, NA_character_), "NA")
  }
})


test_that("validate_rule_values rejects values for an invalid verb", {
  expect_match(markermd:::validate_rule_values("contains", "x"), "must be one of")
})


test_that("get_default_rule_values returns defaults that validate for every verb", {
  expect_equal(markermd:::get_default_rule_values("has between"), c(0, 10))
  expect_equal(markermd:::get_default_rule_values("has at least"), 1L)
  expect_equal(markermd:::get_default_rule_values("has at most"), 1L)
  expect_equal(markermd:::get_default_rule_values("has content"), "")
  expect_equal(markermd:::get_default_rule_values("lacks content"), "")
  expect_equal(markermd:::get_default_rule_values("has name"), "")
  expect_null(markermd:::get_default_rule_values("not a verb"))

  for (verb in markermd::get_allowed_rule_verbs()) {
    expect_null(markermd:::validate_rule_values(verb, markermd:::get_default_rule_values(verb)))
  }
})
