# Section resolution and validation semantics (follow-up review, phase 3)

vs_parse = function(...) {
  q2r::parse_qmd(paste(c(...), collapse = "\n"), quiet = TRUE)
}

vs_question = function(ids, rules = list()) {
  markermd_question(
    id = 1L, name = "Q",
    selected_nodes = markermd_node_selection(node_ids = ids),
    rules = rules
  )
}

vs_rule = function(type, verb, values) {
  markermd_rule(node_type = type, verb = verb, values = values)
}

test_that("a selected section missing from the document fails negative rules", {
  ast = vs_parse("## Q1 answer", "", "```{r}", "secret_bad()", "```", "", "## Question 2", "", "x")
  q = vs_question("question-1", list(
    vs_rule("Chunk", "lacks content", "secret_bad"),
    vs_rule("Chunk", "has at most", 1L)
  ))

  res = validate_question_rules(ast, q)
  expect_equal(res$status, "fail")
  expect_false(any(res$passed))
  expect_match(res$messages, "#question-1 not found in this document", fixed = TRUE)
  expect_match(res$details, "Not found in this document: #question-1", fixed = TRUE)

  # the editor's badges come from the same evaluator
  expect_false(any(vapply(evaluate_question_rules(ast, q), function(r) r$passed, logical(1))))
})

test_that("a present section still evaluates its rules normally", {
  ast = vs_parse("## Question 1", "", "```{r}", "fine()", "```")
  q = vs_question("question-1", list(vs_rule("Chunk", "lacks content", "secret_bad")))
  expect_equal(validate_question_rules(ast, q)$status, "pass")
})

test_that("a question without rules passes but notes the missing section", {
  ast = vs_parse("## Something else", "", "x")
  res = validate_question_rules(ast, vs_question("question-1"))
  expect_equal(res$status, "pass")
  expect_match(res$details, "Not found in this document: #question-1", fixed = TRUE)
})

test_that("unresolved_node_ids lists only ids no heading or div carries", {
  ast = vs_parse("# A", "", "::: {#box}", "x", ":::")
  expect_equal(unresolved_node_ids(ast, c("a", "box", "gone")), "gone")
  expect_equal(unresolved_node_ids(ast, character(0)), character(0))
})

test_that("a heading plus a div in its section counts the div's content once", {
  ast = vs_parse(
    "# Q1", "", "intro", "", "::: {#ans}", "answer text", "", "```{r}", "1", "```", ":::", "",
    "# Q2", "", "other"
  )
  q = vs_question(c("q1", "ans"), list(vs_rule("Any node", "has between", c(0, 4))))
  kinds = vapply(get_question_ast(ast, q)@blocks@content, q2r_node_kind, character(1))
  expect_equal(kinds, c("Heading", "Markdown", "Markdown", "Chunk"))
  expect_equal(validate_question_rules(ast, q)$status, "pass")

  # selecting the heading alone keeps the div whole
  q_heading = vs_question("q1")
  kinds = vapply(get_question_ast(ast, q_heading)@blocks@content, q2r_node_kind, character(1))
  expect_equal(kinds, c("Heading", "Markdown", "Div"))
})

test_that("a selected div nested in another selected div is not double counted", {
  ast = vs_parse("::: {#outer}", "a", "", "::: {#inner}", "b", ":::", ":::")
  q = vs_question(c("outer", "inner"))
  kinds = vapply(get_question_ast(ast, q)@blocks@content, q2r_node_kind, character(1))
  expect_equal(kinds, c("Markdown", "Markdown"))
})

test_that("the tree highlights and counts an in-div heading's whole section", {
  ast = vs_parse("# Outer", "", "intro", "", "::: {#box}", "## Inner", "", "text a", "", "```{r}", "1", "```", ":::")
  items = build_ast_tree_structure(ast)
  inner = node_ids_to_indices(ast, "inner")
  highlighted = compute_all_selected_nodes(items, inner)
  q = vs_question("inner")
  expect_equal(length(highlighted), length(get_question_ast(ast, q)@blocks@content))
  expect_true(all(highlighted >= inner))
})

test_that("ids shared by several nodes are reported and listed", {
  ast = vs_parse("# A {#x}", "", "a", "", "# B {#x}", "", "b", "", "::: {#x}", "c", ":::", "", "# C", "", "d")
  expect_equal(duplicate_node_ids(ast), "x")
  notes = template_document_notes(ast)
  expect_length(notes, 1)
  expect_match(notes, "The id #x is used by 3 nodes", fixed = TRUE)
  expect_equal(template_document_notes(vs_parse("# A", "", "# B")), character(0))
})

test_that("line id chains skip front matter and HTML comments", {
  lines = c(
    "---", "title: x", "# a yaml comment", "---", "",
    "<!--", "# Instructions: do not edit", "-->", "",
    "## Q1", "", "text", "", "## Q2", "", "more"
  )
  chains = line_node_id_chains(lines, vs_parse(lines))
  expect_equal(chains[[3]], character(0))
  expect_equal(chains[[7]], character(0))
  expect_equal(chains[[12]], "q1")
  expect_equal(chains[[16]], "q2")
})

test_that("a longer code fence is not closed by a shorter inner fence", {
  lines = c("## Q1", "", "````md", "```{r}", "# not a heading", "```", "````", "", "## Q2", "", "x")
  chains = line_node_id_chains(lines, vs_parse(lines))
  expect_equal(chains[[5]], "q1")
  expect_equal(chains[[11]], "q2")

  normalized = normalize_knitr_chunks(c("````md", "```{r label}", "1", "```", "````", "```{r other}", "2", "```"))
  # the inner knitr header is shown, not executed, so it stays as written; the
  # chunk after the outer block is rewritten
  expect_true("```{r label}" %in% normalized)
  expect_true("#| label: other" %in% normalized)
})
