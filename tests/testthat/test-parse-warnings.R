# Parser warnings are kept on the AST and shown in the apps, not printed

pw_doc = function(dir, name = "doc.qmd") {
  path = file.path(dir, name)
  writeLines(c("# Title", "", "Some text<br/>more", "", "## Q1", "", "Answer"), path)
  path
}

test_that("parse_assignment_document keeps warnings on the AST without signalling them", {
  d = withr::local_tempdir()
  path = pw_doc(d)
  expect_no_warning(ast <- parse_assignment_document(path))
  warnings = markermd:::document_parse_warnings(ast)
  expect_length(warnings, 1)
  text = markermd:::format_parse_diagnostics(warnings)
  expect_match(text, "Q-2-9", fixed = TRUE)
  expect_match(text, "doc.qmd:", fixed = TRUE)
  expect_false(grepl("\033[", text, fixed = TRUE))
  expect_null(markermd:::parse_warnings_ui(list()))
  expect_match(as.character(markermd:::parse_warnings_ui(warnings)), "1 parser warning in the document", fixed = TRUE)
})

test_that("parse errors are still raised", {
  d = withr::local_tempdir()
  path = file.path(d, "bad.qmd")
  writeLines(c("# Title", "", "```{r setup, echo=FALSE", "x = 1", "```"), path)
  expect_error(parse_assignment_document(path))
})

test_that("parse_project reports a document with warnings", {
  root = make_mark_fixture()$project
  qmd = file.path(root, "repos", "student3-poor", "assignment.qmd")
  write(c("", "A line<br/>break"), qmd, append = TRUE)
  res = parse_project(root)
  expect_equal(res$status[res$repo == "student3-poor"], "warning")
  expect_match(res$detail[res$repo == "student3-poor"], "Q-2-9", fixed = TRUE)
  expect_true(all(res$status[res$repo != "student3-poor"] == "ok"))
})

test_that("the table's validation cell marks parser warnings", {
  results = list(r = list(Q1 = list(status = "pass")))
  template = markermd_template(
    original_ast = markermd:::empty_pandoc_ast(),
    questions = list(markermd_question(1L, "Q1"))
  )
  plain = markermd:::validation_status_cell("r", results, template)
  marked = markermd:::validation_status_cell("r", results, template, n_warnings = 2)
  expect_false(grepl("parser warning", plain))
  expect_match(marked, "2 parser warnings", fixed = TRUE)
})

test_that("mark and template show parser warnings in the UI", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  write(c("", "A line<br/>break"), file.path(root, "repos", "student3-poor", "assignment.qmd"), append = TRUE)

  app = new_app_driver(markermd:::mark_app(root), name = "mark_parse_warnings")
  app$wait_for_js("(document.querySelector('#repo_table') || {}).innerHTML.includes('parser warning')", timeout = 30000)
  expect_match(app$get_html("#repo_table"), "1 parser warning", fixed = TRUE)
  app$run_js("Shiny.setInputValue('validation_clicked', 3, {priority: 'event'})")
  app$wait_for_js("!!document.querySelector('#shiny-modal #repo_parse_warnings details')", timeout = 10000)
  expect_match(app$get_text("#repo_parse_warnings"), "1 parser warning in this repository's document", fixed = TRUE)

  d = withr::local_tempdir()
  pw_doc(d, "assignment.qmd")
  tapp = new_app_driver(template_app(d), name = "template_parse_warnings")
  tapp$wait_for_js("!!document.getElementById('show_parse_warnings')", timeout = 10000)
  tapp$click("show_parse_warnings")
  tapp$wait_for_js("document.getElementById('shiny-modal')?.innerText?.includes('Q-2-9')", timeout = 10000)
})
