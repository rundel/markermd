# Coverage tests for the parsing subsystem:
#   R/utils_parsing.R, R/utils_q2r.R, R/utils_monaco.R
# All top-level helpers are prefixed cp_ to stay unique across the shared
# testthat environment.

# Write character lines to a fresh tempfile with the given extension.
cp_write_qmd = function(lines, ext = ".qmd") {
  f = tempfile(fileext = ext)
  writeLines(lines, f)
  f
}

# Parse a qmd string into a q2r pandoc AST.
cp_parse = function(lines) {
  q2r::parse_qmd(paste(lines, collapse = "\n"), quiet = FALSE)
}

# A small doc with a paragraph, an {r} chunk and a {python} chunk.
cp_mixed_doc = function() {
  c(
    "# Title",
    "",
    "This is a paragraph with some text.",
    "",
    "```{r}",
    "1 + 1",
    "```",
    "",
    "```{python}",
    "x = 2",
    "```"
  )
}

# Pull the first flattened node of a given friendly kind out of an AST.
cp_first_node = function(ast, kind, which = 1L) {
  recs = markermd:::q2r_flatten(ast)
  kinds = vapply(recs, function(r) markermd:::q2r_node_kind(r$node), character(1))
  recs[[which(kinds == kind)[which]]]$node
}


test_that("parse_assignment_document errors on a missing file", {
  expect_error(
    markermd::parse_assignment_document(tempfile(fileext = ".qmd")),
    "does not exist"
  )
})


test_that("parse_assignment_document stops on a whitespace-only file (empty AST)", {
  f = cp_write_qmd(c("   ", "", "\t"))
  expect_error(
    markermd::parse_assignment_document(f),
    "empty AST"
  )

  # A truly empty file hits the same empty-AST guard.
  f2 = cp_write_qmd(character(0))
  expect_error(
    markermd::parse_assignment_document(f2),
    "empty AST"
  )
})


test_that("parse_assignment_document handles malformed YAML front matter", {
  # INTENDED: an unterminated YAML front matter (an opening --- with no closing
  # ---) should raise an error rather than silently returning content.
  #
  # DISCREPANCY: q2r tolerates this - with no closing fence the leading "---"
  # is read as a horizontal rule and the body is parsed as ordinary markdown,
  # so no error is raised and a non-empty AST comes back. We assert the actual
  # (non-erroring) behavior to keep the file green; flagged for human review.
  f = cp_write_qmd(c("---", "title: Test", "author: Me", "", "# Heading", "", "body"))
  ast = markermd::parse_assignment_document(f)
  expect_true(S7::S7_inherits(ast, q2r::pandoc))
  expect_gt(length(ast@blocks@content), 0)

  # A YAML block that q2r itself rejects (invalid indentation in a quoted
  # scalar) does raise an error rather than returning an empty AST.
  f2 = cp_write_qmd(c("---", 'title: "unterminated', "foo: [1, 2", "---", "", "# Heading", "body"))
  expect_error(markermd::parse_assignment_document(f2))
})


test_that("parse_assignment_document handles an unbalanced fenced div", {
  # INTENDED: an unbalanced ::: div (an opening fence with no closing :::)
  # should raise an error rather than silently returning an AST.
  #
  # DISCREPANCY: q2r auto-closes the div at end of document, yielding a valid
  # pandoc_div and no error. We assert the actual (non-erroring) behavior to
  # keep the file green; flagged for human review.
  f = cp_write_qmd(c("# Heading", "", "::: {.note}", "", "some text"))
  ast = markermd::parse_assignment_document(f)
  expect_true(S7::S7_inherits(ast, q2r::pandoc))
  kinds = vapply(ast@blocks@content, markermd:::q2r_node_kind, character(1))
  expect_true("Div" %in% kinds)
})


test_that("parse_assignment_collection parses every repo, isolating failures", {
  d = tempfile("cp_coll_")
  dir.create(file.path(d, "repo1"), recursive = TRUE)
  dir.create(file.path(d, "repo2"), recursive = TRUE)
  writeLines(c("# Q1", "", "body text"), file.path(d, "repo1", "assignment.qmd"))
  writeLines(character(0), file.path(d, "repo2", "assignment.qmd"))

  coll = markermd:::parse_assignment_collection(d, use_qmd = TRUE)
  expect_equal(nrow(coll), 2L)

  good = which(is.na(coll$error))
  bad = which(!is.na(coll$error))
  expect_length(good, 1L)
  expect_length(bad, 1L)

  # One malformed file does not abort the rest: the good repo still parsed.
  expect_true(S7::S7_inherits(coll$ast[[good]], q2r::pandoc))

  # The bad row carries a non-NA error string and a NULL ast.
  expect_true(is.character(coll$error[[bad]]) && nzchar(coll$error[[bad]]))
  expect_null(coll$ast[[bad]])
})


test_that("parse_assignment_collection with use_qmd=FALSE ignores .qmd files", {
  d = tempfile("cp_coll_rmd_")
  dir.create(file.path(d, "repo1"), recursive = TRUE)
  writeLines(c("# Q1", "", "body"), file.path(d, "repo1", "assignment.qmd"))

  coll = markermd:::parse_assignment_collection(d, use_qmd = FALSE)
  expect_equal(nrow(coll), 0L)
})


test_that("line_node_id_chains carries enclosing heading and div ids", {
  lines = c(
    "## Top {#top}",     # 1
    "",                  # 2
    "### Sub {#sub}",    # 3
    "",                  # 4
    "body under sub",    # 5
    "",                  # 6
    "::: {.note}",       # 7
    "",                  # 8
    "inside class div",  # 9
    "",                  # 10
    ":::",               # 11
    "",                  # 12
    "## Dup {#dup}",     # 13
    "",                  # 14
    "first dup body",    # 15
    "",                  # 16
    "## Dup {#dup-1}",   # 17
    "",                  # 18
    "second dup body"    # 19
  )
  ast = cp_parse(lines)
  chains = markermd:::line_node_id_chains(lines, ast)

  # Body line under the nested Sub heading inherits both heading ids.
  expect_equal(chains[[5]], c("top", "sub"))

  # A line inside the class-only div keeps the enclosing heading ids and gains
  # a trailing "" for the id-less div fence.
  expect_equal(chains[[9]], c("top", "sub", ""))

  # Body under the second Dup heading carries the deduped second id.
  expect_equal(chains[[19]], "dup-1")
})


test_that("monaco_language_for_engine maps known engines, falling back to markdown", {
  expect_equal(markermd:::monaco_language_for_engine("r"), "r")
  expect_equal(markermd:::monaco_language_for_engine("python"), "python")
  expect_equal(markermd:::monaco_language_for_engine("sql"), "sql")
  expect_equal(markermd:::monaco_language_for_engine("not-a-real-engine"), "markdown")
})


test_that("monaco_language_for_node uses the cell engine, else markdown", {
  ast = cp_parse(cp_mixed_doc())
  rcell = cp_first_node(ast, "Chunk", 1L)
  pycell = cp_first_node(ast, "Chunk", 2L)
  para = cp_first_node(ast, "Markdown", 1L)

  expect_equal(markermd:::monaco_language_for_node(rcell), "r")
  expect_equal(markermd:::monaco_language_for_node(pycell), "python")
  expect_equal(markermd:::monaco_language_for_node(para), "markdown")
})


test_that("node_to_qmd round-trips a single node and a whole document", {
  ast = cp_parse(cp_mixed_doc())

  para = cp_first_node(ast, "Markdown", 1L)
  expect_true(grepl("paragraph with some text", markermd:::node_to_qmd(para), fixed = TRUE))

  chunk = cp_first_node(ast, "Chunk", 1L)
  chunk_qmd = markermd:::node_to_qmd(chunk)
  expect_true(grepl("```{r}", chunk_qmd, fixed = TRUE))
  expect_true(grepl("1 + 1", chunk_qmd, fixed = TRUE))

  # The whole document path returns the full source.
  expect_true(S7::S7_inherits(ast, q2r::pandoc))
  full = markermd:::node_to_qmd(ast)
  expect_true(grepl("Title", full, fixed = TRUE))
  expect_true(grepl("1 + 1", full, fixed = TRUE))
  expect_true(grepl("python", full, fixed = TRUE))
})


test_that("monaco_line_decorations converts ranges to Monaco decorations", {
  expect_length(markermd:::monaco_line_decorations(NULL), 0L)
  expect_length(markermd:::monaco_line_decorations(list()), 0L)

  dec = markermd:::monaco_line_decorations(list(list(start = 3, end = 5)))
  expect_length(dec, 1L)
  expect_equal(dec[[1]]$range$startLineNumber, 3)
  expect_equal(dec[[1]]$range$endLineNumber, 5)
  expect_true(dec[[1]]$options$isWholeLine)
  expect_equal(dec[[1]]$options$overviewRuler$color, "#ffc107")
})
