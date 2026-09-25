# Document-scope questions: a question about the document as a whole (the YAML
# header, render settings) rather than selected sections.

document_scope_qmd = function() {
  system.file(
    "examples/test_assignment/repos/student1-excellent/assignment.qmd",
    package = "markermd"
  )
}

document_scope_template = function() {
  qmd = document_scope_qmd()
  markermd::markermd_template(
    original_ast = markermd:::parse_assignment_document(qmd),
    questions = list(
      markermd::markermd_question(
        1L, "Q2",
        markermd::markermd_node_selection(node_ids = "question-2-basic-programming"),
        list(markermd::markermd_rule("Chunk", "has at least", 1L))
      ),
      markermd::markermd_question(2L, "Document", scope = "document")
    ),
    metadata = markermd::markermd_metadata()
  )
}

test_that("a question defaults to sections scope and validates its scope", {
  q = markermd::markermd_question(1L, "Q1")
  expect_equal(q@scope, "sections")
  expect_false(markermd:::is_document_question(q))

  doc = markermd::markermd_question(2L, "Document", scope = "document")
  expect_true(markermd:::is_document_question(doc))

  expect_error(markermd::markermd_question(3L, "Bad", scope = "chapter"), "scope")
  expect_error(
    markermd::markermd_question(
      4L, "Bad",
      markermd::markermd_node_selection(node_ids = "question-2-basic-programming"),
      scope = "document"
    ),
    "cannot select nodes"
  )
})

test_that("scope is written only for document questions and bumps the format to 3.1", {
  tmpl = document_scope_template()
  lst = markermd:::template_to_list(tmpl)
  expect_equal(lst$format_version, "3.1")
  expect_null(lst$questions[[1]]$scope)
  expect_equal(lst$questions[[1]]$node_ids, list("question-2-basic-programming"))
  expect_equal(lst$questions[[2]]$scope, "document")
  expect_null(lst$questions[[2]]$node_ids)

  sections_only = tmpl
  sections_only@questions = tmpl@questions[1]
  expect_equal(markermd:::template_to_list(sections_only)$format_version, "3.0")
})

test_that("document scope round-trips through YAML and the database", {
  tmpl = document_scope_template()
  qmd = document_scope_qmd()

  path = tempfile(fileext = ".yaml")
  markermd::write_template_yaml(tmpl, path, source_path = qmd)
  back = markermd::read_template_yaml(path, require_ast = TRUE)
  expect_equal(vapply(back@questions, function(q) q@scope, character(1)), c("sections", "document"))

  d = tempfile("docscope_"); dir.create(d)
  markermd:::save_template_to_db(d, tmpl, source_path = qmd)
  from_db = markermd:::load_template_from_db(d, base_dir = d, assignment = qmd, require_ast = TRUE)
  expect_equal(vapply(from_db@questions, function(q) q@scope, character(1)), c("sections", "document"))
})

test_that("3.0 templates still load and 3.1 files validate against the schema", {
  qmd = document_scope_qmd()
  old = tempfile(fileext = ".yaml")
  writeLines(c(
    'format_version: "3.0"',
    paste0('source: {path: "', qmd, '"}'),
    "questions:",
    "- {id: 1, name: Q2, node_ids: [question-2-basic-programming], rules: []}"
  ), old)
  expect_equal(markermd::read_template_yaml(old, require_ast = TRUE)@questions[[1]]@scope, "sections")

  skip_if_not_installed("jsonvalidate")
  good = tempfile(fileext = ".yaml")
  markermd::write_template_yaml(document_scope_template(), good, source_path = qmd)
  expect_true(isTRUE(markermd::validate_template_file(good)))

  bad = tempfile(fileext = ".yaml")
  writeLines(c(
    'format_version: "3.1"',
    "questions:",
    "- {id: 1, name: Document, scope: document, node_ids: [question-2-basic-programming]}"
  ), bad)
  expect_false(isTRUE(markermd::validate_template_file(bad)))

  bad_scope = tempfile(fileext = ".yaml")
  writeLines(c(
    'format_version: "3.1"',
    "questions:",
    "- {id: 1, name: Document, scope: chapter}"
  ), bad_scope)
  expect_false(isTRUE(markermd::validate_template_file(bad_scope)))
})

test_that("a document question passes without rules and evaluates rules on the whole document", {
  ast = markermd:::parse_assignment_document(document_scope_qmd())

  empty = markermd::markermd_question(1L, "Document", scope = "document")
  result = markermd:::validate_question_rules(ast, empty)
  expect_equal(result$status, "pass")
  expect_match(result$details, "whole document")

  ruled = markermd::markermd_question(
    2L, "Document",
    rules = list(markermd::markermd_rule("Chunk", "has at least", 1L)),
    scope = "document"
  )
  expect_equal(markermd:::validate_question_rules(ast, ruled)$status, "pass")

  # A sections question with rules but no nodes still fails
  unselected = markermd::markermd_question(
    3L, "Q", rules = list(markermd::markermd_rule("Chunk", "has at least", 1L))
  )
  expect_equal(markermd:::validate_question_rules(ast, unselected)$status, "fail")
})

test_that("export_comments writes document-scope feedback first", {
  d = tempfile("docfeedback_")
  dir.create(file.path(d, "repos", "team01"), recursive = TRUE)
  d = normalizePath(d, winslash = "/")
  template_path = file.path(d, "template.yaml")
  writeLines(c(
    "format_version: '3.1'",
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - {id: 1, name: Q1}",
    "  - {id: 2, name: Document, scope: document}"
  ), template_path)
  suppressMessages(markermd::init_project(d))
  suppressMessages(markermd::template_import(template_path, project = d))

  info = markermd:::project_question_names(d)
  expect_equal(info$names, c("Q1", "Document"))
  expect_equal(info$document, "Document")

  markermd:::save_rubric_item(d, "Q1", "item_0", markermd::markermd_rubric_item(1L, -1, "Q1 issue"))
  markermd:::save_rubric_item(d, "Document", "item_1", markermd::markermd_rubric_item(1L, -5, "Document does not render"))
  markermd:::save_grade_selection(d, "Q1", "team01", "item_0", TRUE)
  markermd:::save_grade_selection(d, "Document", "team01", "item_1", TRUE)

  suppressMessages(markermd::export_comments(d))
  lines = readLines(file.path(d, "comments", "team01.md"))
  headings = grep("^## ", lines, value = TRUE)
  expect_equal(headings, c("## Document", "## Q1"))
})
