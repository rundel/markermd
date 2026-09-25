# The "HTML comment" node kind: raw blocks that are nothing but <!-- ... -->
# comments never render, so they are distinguished from real raw-HTML content
# by rules, filters, the tree labels, and the outline counts alike.

parse_lines = function(...) {
  q2r::parse_qmd(paste(c(...), collapse = "\n"), quiet = TRUE)
}

kinds_of = function(ast) {
  vapply(ast@blocks@content, markermd:::q2r_node_kind, character(1))
}

comment_doc = function() {
  parse_lines(
    "## Q1 {#q1}",
    "",
    "<!-- placeholder: write your answer here -->",
    "",
    "<!-- a write-up left",
    "inside the comment -->",
    "",
    "```{=html}",
    "<p>a write-up in a raw html fence</p>",
    "```",
    "",
    "```{=html}",
    "<!-- only a comment inside a raw fence -->",
    "```",
    "",
    "<!-- markermd: automated grading fix, not part of the students' work. Moved a fence. -->",
    "",
    "A markdown paragraph."
  )
}


test_that("raw blocks that are only comments are their own node kind", {
  ast = comment_doc()
  expect_equal(
    kinds_of(ast),
    c("Heading", "HTML comment", "HTML comment", "Raw Block", "HTML comment", "HTML comment", "Markdown")
  )
})


test_that("raw html with markup outside its comments stays a Raw Block", {
  ast = parse_lines("<!-- comment -->", "<p>", "", "<div>", "text", "</div>")
  kinds = kinds_of(ast)
  expect_false("HTML comment" %in% kinds)
  expect_true("Raw Block" %in% kinds)
})


test_that("is_html_comment_block handles dashes and multi-line bodies", {
  raw = function(text) q2r::pandoc_raw_block(format = "html", text = text)
  expect_true(markermd:::is_html_comment_block(raw("<!-- a -- b -->")))
  expect_true(markermd:::is_html_comment_block(raw("<!-- a ---->")))
  expect_true(markermd:::is_html_comment_block(raw("<!-- a -->\n\n<!-- b\nc -->")))
  expect_false(markermd:::is_html_comment_block(raw("<!-- a --> x <!-- b -->")))
  expect_false(markermd:::is_html_comment_block(raw("<!-- unterminated")))
  expect_false(markermd:::is_html_comment_block(raw("")))
})


test_that("a Markdown + Raw Block write-up rule counts {=html} but not comments", {
  q = function(rules, filters = list()) {
    markermd::markermd_question(
      1L, "Q1", markermd::markermd_node_selection(node_ids = "q1"), rules, filters = filters
    )
  }
  writeup = list(markermd::markermd_rule(c("Markdown", "Raw Block"), "has at least", 1L))

  with_fence = comment_doc()
  expect_equal(markermd:::validate_question_rules(with_fence, q(writeup))$status, "pass")

  only_comments = parse_lines(
    "## Q1 {#q1}", "", "<!-- placeholder -->", "", "<!-- the answer, left inside the comment -->"
  )
  expect_equal(markermd:::validate_question_rules(only_comments, q(writeup))$status, "fail")

  comments_targeted = list(markermd::markermd_rule("HTML comment", "has at least", 2L))
  expect_equal(markermd:::validate_question_rules(only_comments, q(comments_targeted))$status, "pass")

  any_node = list(markermd::markermd_rule("Any node", "has at least", 2L))
  expect_equal(markermd:::validate_question_rules(only_comments, q(any_node))$status, "pass")
})


test_that("node type filters split Raw Block from HTML comment", {
  ast = comment_doc()
  keep_kind = function(kind) {
    list(markermd::markermd_filter_group(conditions = list(
      markermd::markermd_filter_condition(type = "node type", value = kind)
    )))
  }
  q = function(filters) {
    markermd::markermd_question(
      1L, "Q1", markermd::markermd_node_selection(node_ids = "q1"), list(), filters = filters
    )
  }

  raw_only = markermd:::get_question_ast(ast, q(keep_kind("Raw Block")))
  expect_equal(kinds_of(raw_only), "Raw Block")

  comments_only = markermd:::get_question_ast(ast, q(keep_kind("HTML comment")))
  expect_equal(kinds_of(comments_only), rep("HTML comment", 4))
})


test_that("HTML comment is in the rule vocabulary and the schema", {
  expect_true("HTML comment" %in% markermd::get_allowed_node_types())
  expect_null(markermd:::validate_node_type("HTML comment"))
  expect_null(markermd:::validate_filter_condition_value("node type", "HTML comment"))

  schema = jsonlite::fromJSON(
    system.file("schema/markermd-template.json", package = "markermd"),
    simplifyVector = FALSE
  )
  expect_true("HTML comment" %in% unlist(schema[["$defs"]][["node_type_enum"]][["enum"]]))
})


test_that("the outline does not count HTML comments as content", {
  f = withr::local_tempfile(fileext = ".qmd")
  writeLines(c(
    "## Q1 {#q1}", "", "<!-- placeholder -->", "", "Some prose.", "",
    "```{=html}", "<p>raw</p>", "```"
  ), f)
  o = markermd::assignment_outline(f)
  row = o[o$id == "q1", ]
  expect_equal(row$n_markdown, 1L)
  expect_equal(row$n_other, 1L)
  expect_equal(row$n_content, 2L)
})


test_that("tree labels and details name HTML comments", {
  ast = comment_doc()
  node = ast@blocks@content[[2]]
  expect_equal(markermd:::q2r_node_label(node), "HTML comment")
  expect_match(markermd:::q2r_node_detail(node), "placeholder: write your answer here")
  expect_equal(markermd:::q2r_node_label(ast@blocks@content[[4]]), "Raw Block [html]")
})
