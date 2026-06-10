# Fixture document parsed once per file. Top-level nodes: 2 headings (with
# explicit ids), 2 markdown paragraphs, a div carrying an id and a class (with
# a nested div inside it), and 1 labelled chunk.
filter_eval_fixture_ast = local({
  qmd = tempfile(fileext = ".qmd")
  writeLines(c(
    "---",
    "title: Filter evaluation fixture",
    "---",
    "",
    "# Question 1 {#q1}",
    "",
    "Intro text for question one.",
    "",
    "::: {#hint-1 .hint}",
    "A helpful hint.",
    "",
    "::: {.nested}",
    "Nested div content.",
    ":::",
    ":::",
    "",
    "```{r q1-plot}",
    "plot(1)",
    "```",
    "",
    "# Question 2 {#q2}",
    "",
    "Second question text."
  ), qmd)
  markermd::parse_assignment_document(qmd)
})

fcond = function(type, value, negate = FALSE) {
  markermd::markermd_filter_condition(type = type, value = value, negate = negate)
}

fgroup = function(..., negate = FALSE) {
  markermd::markermd_filter_group(conditions = list(...), negate = negate)
}

# Build a question targeting the given node ids with the given filter groups
fquestion = function(filters = list(), node_ids = character(0), rules = list()) {
  markermd::markermd_question(
    id = 1L, name = "Q",
    selected_nodes = markermd::markermd_node_selection(node_ids = node_ids),
    rules = rules,
    filters = filters
  )
}

question_node_count = function(question) {
  length(markermd:::get_question_ast(filter_eval_fixture_ast, question)@blocks@content)
}


test_that("a question without filters keeps the full node set", {
  expect_equal(question_node_count(fquestion()), 6)
})


test_that("filters narrow the top-level node set", {
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has class", "hint"))))), 1)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has id", "hint-1"))))), 1)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("node type", "Heading"))))), 2)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("node type", "Chunk"))))), 1)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("node type", "Code block"))))), 0)

  # has_label glob-matches heading/div ids and, for chunks without an attr
  # id, the cell's "#| label:" option (here the q1-plot chunk)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has label", "q*"))))), 3)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has label", "q1-plot"))))), 1)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has label", "hint-*"))))), 1)
})


test_that("has option conditions match Quarto cell options", {
  # the fixture chunk's knitr-style {r q1-plot} header is normalized to a
  # "#| label: q1-plot" option line
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has option", "label"))))), 1)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has option", "label: q1-plot"))))), 1)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has option", "label: wrong"))))), 0)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has option", "eval"))))), 0)
})


test_that("has engine conditions match a cell's engine", {
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has engine", "r"))))), 1)
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has engine", "python"))))), 0)
})


test_that("conditions in a group are ANDed", {
  filters = list(fgroup(fcond("node type", "Div"), fcond("has class", "hint")))
  expect_equal(question_node_count(fquestion(filters)), 1)

  filters = list(fgroup(fcond("node type", "Heading"), fcond("has class", "hint")))
  expect_equal(question_node_count(fquestion(filters)), 0)
})


test_that("groups are ORed", {
  filters = list(
    fgroup(fcond("has class", "hint")),
    fgroup(fcond("node type", "Heading"))
  )
  expect_equal(question_node_count(fquestion(filters)), 3)
})


test_that("negation inverts conditions and groups", {
  # 6 top-level nodes, 1 chunk: not-Chunk keeps the other 5
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("node type", "Chunk", negate = TRUE))))), 5)

  # group negation is equivalent for a single condition
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("node type", "Chunk"), negate = TRUE)))), 5)

  # negated AND group: everything except the hint div
  filters = list(fgroup(fcond("node type", "Div"), fcond("has class", "hint"), negate = TRUE))
  expect_equal(question_node_count(fquestion(filters)), 5)

  # double negation cancels
  filters = list(fgroup(fcond("node type", "Chunk", negate = TRUE), negate = TRUE))
  expect_equal(question_node_count(fquestion(filters)), 1)

  # in the tree, not-Chunk on section q1 reds only the chunk (index 7)
  q = fquestion(list(fgroup(fcond("node type", "Chunk", negate = TRUE))), node_ids = "q1")
  expect_equal(markermd:::question_filtered_indices(filter_eval_fixture_ast, q), 7)
})


test_that("filtering tests top-level nodes only, never descending into children", {
  # the .nested div exists only inside the .hint div, so it is not matched
  expect_equal(question_node_count(fquestion(list(fgroup(fcond("has class", "nested"))))), 0)
})


test_that("empty filter groups are ignored", {
  filters = list(markermd::markermd_filter_group(), fgroup(fcond("node type", "Chunk")))
  expect_equal(question_node_count(fquestion(filters)), 1)

  expect_equal(question_node_count(fquestion(list(markermd::markermd_filter_group()))), 6)
})


test_that("filters compose with section selection and rule evaluation", {
  # section q1 holds: heading, paragraph, hint div, chunk
  q = fquestion(node_ids = "q1")
  expect_equal(question_node_count(q), 4)

  # the same section narrowed to its markdown content
  q = fquestion(list(fgroup(fcond("node type", "Markdown"))), node_ids = "q1")
  expect_equal(question_node_count(q), 1)

  # rules evaluate against the filtered set: section q1 has a chunk, but not
  # after filtering to headings
  rule = markermd::markermd_rule("Any node", "has at least", 1L)
  passing = fquestion(list(fgroup(fcond("node type", "Chunk"))), node_ids = "q1", rules = list(rule))
  failing = fquestion(list(fgroup(fcond("has class", "missing"))), node_ids = "q1", rules = list(rule))

  expect_true(markermd:::validate_question_rules(filter_eval_fixture_ast, passing)$passed)
  expect_false(markermd:::validate_question_rules(filter_eval_fixture_ast, failing)$passed)
})


test_that("filters apply in the headless validate_repo_against_rules path", {
  template = markermd::markermd_template(
    original_ast = filter_eval_fixture_ast,
    questions = list(
      markermd::markermd_question(
        id = 1L, name = "Q1",
        selected_nodes = markermd::markermd_node_selection(node_ids = "q1"),
        rules = list(markermd::markermd_rule("Any node", "has between", c(1, 1))),
        filters = list(fgroup(fcond("has class", "hint")))
      )
    )
  )

  results = markermd:::validate_repo_against_rules(filter_eval_fixture_ast, template)
  expect_true(results$Q1$passed)
})


test_that("question_filtered_indices maps dropped blocks to tree indices", {
  # flatten indices: 1 heading q1, 2 paragraph, 3 div #hint-1, 4 hint paragraph,
  # 5 nested div, 6 nested paragraph, 7 chunk, 8 heading q2, 9 paragraph
  red = function(filters, ids) {
    markermd:::question_filtered_indices(filter_eval_fixture_ast, fquestion(filters, node_ids = ids))
  }

  # chunk-only filter on section q1: heading, paragraph and div (with its
  # nested children) drop; the dropped heading does not color section content
  expect_equal(red(list(fgroup(fcond("node type", "Chunk"))), "q1"), 1:6)

  # the kept div keeps its nested children green
  expect_equal(red(list(fgroup(fcond("has class", "hint"))), "q1"), c(1, 2, 7))

  # div selection: only the div's child blocks are countable
  expect_equal(red(list(fgroup(fcond("node type", "Div"))), "hint-1"), 4)
  expect_equal(red(list(fgroup(fcond("node type", "Markdown"))), "hint-1"), c(5, 6))

  # no effective filters or no selection: nothing is drawn red
  expect_equal(red(list(), "q1"), integer(0))
  expect_equal(red(list(markermd::markermd_filter_group()), "q1"), integer(0))
  expect_equal(red(list(fgroup(fcond("node type", "Chunk"))), character(0)), integer(0))
})


test_that("invalid predicate values warn via q2r rather than failing silently", {
  filters = list(fgroup(fcond("has text", "(")))

  caught = list()
  n = withCallingHandlers(
    question_node_count(fquestion(filters)),
    warning = function(w) {
      caught[[length(caught) + 1]] <<- w
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(vapply(caught, function(w) inherits(w, "q2r_predicate_error"), logical(1))))
  expect_equal(n, 0)
})
