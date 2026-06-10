cond = function(type, value, negate = FALSE) {
  markermd::markermd_filter_condition(type = type, value = value, negate = negate)
}

group = function(..., negate = FALSE) {
  markermd::markermd_filter_group(conditions = list(...), negate = negate)
}

expr_text = function(filters) {
  expr = markermd:::filters_expr(filters)
  if (is.null(expr)) NULL else paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}


test_that("a single condition produces a bare predicate call", {
  expect_equal(expr_text(list(group(cond("has class", "hint")))), 'has_class("hint")')
  expect_equal(expr_text(list(group(cond("node type", "Heading")))), "is(q2r::pandoc_header)")
  expect_equal(expr_text(list(group(cond("has label", "fig-*")))), 'has_label("fig-*")')
})


test_that("conditions in a group are AND-combined without outer parens", {
  filters = list(group(cond("node type", "Div"), cond("has class", "hint")))
  expect_equal(expr_text(filters), 'is(q2r::pandoc_div) & has_class("hint")')
})


test_that("groups are OR-combined with parenthesized multi-condition groups", {
  filters = list(
    group(cond("node type", "Div"), cond("has class", "hint")),
    group(cond("node type", "Heading"), cond("has text", "Q2"))
  )
  expect_equal(
    expr_text(filters),
    '(is(q2r::pandoc_div) & has_class("hint")) | (is(q2r::pandoc_header) & has_text("Q2"))'
  )
})


test_that("compound node-kind expressions are parenthesized inside AND groups", {
  # "Markdown" expands to an | expression, which must be wrapped when ANDed
  filters = list(group(cond("node type", "Markdown"), cond("has text", "x")))
  expect_equal(
    expr_text(filters),
    '(is(q2r::pandoc_paragraph) | is(q2r::pandoc_plain)) & has_text("x")'
  )

  # alone in a single group it stays bare
  expect_equal(
    expr_text(list(group(cond("node type", "Markdown")))),
    "is(q2r::pandoc_paragraph) | is(q2r::pandoc_plain)"
  )

  # as a single-condition group among several groups it is wrapped
  filters = list(group(cond("node type", "Markdown")), group(cond("has id", "x")))
  expect_equal(
    expr_text(filters),
    '(is(q2r::pandoc_paragraph) | is(q2r::pandoc_plain)) | has_id("x")'
  )
})


test_that("values containing quotes and backslashes deparse losslessly", {
  tricky = "a\"b\\d 'c'"
  filters = list(group(cond("has text", tricky)))

  reparsed = str2lang(expr_text(filters))
  expect_equal(reparsed[[2]], tricky)
})


test_that("empty filters and empty groups produce no expression", {
  expect_null(markermd:::filters_expr(list()))
  expect_null(markermd:::filters_expr(list(markermd::markermd_filter_group())))
  expect_null(markermd:::filters_expr_text(list()))

  # empty groups are skipped, non-empty groups kept
  filters = list(markermd::markermd_filter_group(), group(cond("has id", "x")))
  expect_equal(expr_text(filters), 'has_id("x")')
})


test_that("has option conditions build presence and typed value tests", {
  expect_equal(expr_text(list(group(cond("has option", "eval")))), 'has_option("eval")')
  expect_equal(expr_text(list(group(cond("has option", "eval: false")))), 'has_option("eval", FALSE)')
  expect_equal(expr_text(list(group(cond("has option", "fig-width: 5")))), 'has_option("fig-width", 5L)')
  expect_equal(expr_text(list(group(cond("has option", "fig-width: 5.5")))), 'has_option("fig-width", 5.5)')
  expect_equal(
    expr_text(list(group(cond("has option", "label: q1-plot")))),
    'has_option("label", "q1-plot")'
  )
})


test_that("has engine conditions build engine tests", {
  expect_equal(expr_text(list(group(cond("has engine", "r")))), 'has_engine("r")')
  expect_equal(
    expr_text(list(group(cond("node type", "Chunk"), cond("has engine", "python")))),
    'is_code_cell() & has_engine("python")'
  )
})


test_that("negated conditions wrap compound expressions in parens", {
  expect_equal(expr_text(list(group(cond("has class", "hint", negate = TRUE)))), '!has_class("hint")')
  expect_equal(
    expr_text(list(group(cond("node type", "Markdown", negate = TRUE)))),
    "!(is(q2r::pandoc_paragraph) | is(q2r::pandoc_plain))"
  )

  # ! binds tighter than &, so negated conditions need no parens in AND chains
  expect_equal(
    expr_text(list(group(cond("node type", "Div"), cond("has class", "x", negate = TRUE)))),
    'is(q2r::pandoc_div) & !has_class("x")'
  )
})


test_that("negated groups wrap their ANDed conditions", {
  expect_equal(
    expr_text(list(group(cond("node type", "Div"), cond("has class", "hint"), negate = TRUE))),
    '!(is(q2r::pandoc_div) & has_class("hint"))'
  )

  # single-condition group negates bare; double negation stays parenthesized
  expect_equal(expr_text(list(group(cond("has id", "x"), negate = TRUE))), '!has_id("x")')
  expect_equal(
    expr_text(list(group(cond("has class", "x", negate = TRUE), negate = TRUE))),
    '!(!has_class("x"))'
  )

  # a negated group composes into a multi-group OR without extra parens
  expect_equal(
    expr_text(list(
      group(cond("node type", "Heading")),
      group(cond("node type", "Div"), cond("has class", "h"), negate = TRUE)
    )),
    'is(q2r::pandoc_header) | !(is(q2r::pandoc_div) & has_class("h"))'
  )
})


test_that("filters_expr_text wraps the predicate in a select_children call", {
  expect_equal(
    markermd:::filters_expr_text(list(group(cond("has label", "fig-*")))),
    'select_children(nodes, has_label("fig-*"))'
  )
})
