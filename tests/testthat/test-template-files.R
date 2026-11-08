# Templates whose questions target the assignment's other files (format 3.2)

tf_ast = function() {
  markermd:::parse_assignment_document(
    system.file("examples/test_assignment/repos/student1-excellent/assignment.qmd", package = "markermd")
  )
}

tf_template = function() {
  markermd_template(
    original_ast = tf_ast(),
    files = c("get_data.R", "scripts/clean.py"),
    questions = list(
      markermd_question(1L, "Q2", markermd_node_selection(node_ids = "question-2-basic-programming")),
      markermd_question(2L, "get_data.R", file = "get_data.R"),
      markermd_question(3L, "clean.py", file = "scripts/clean.py")
    )
  )
}

test_that("a question on a file covers the whole file", {
  q = markermd_question(1L, "get_data.R", file = "get_data.R")
  expect_true(markermd:::is_file_question(q))
  expect_false(markermd:::is_file_question(markermd_question(2L, "Q1")))

  expect_error(
    markermd_question(1L, "x", markermd_node_selection(node_ids = "a"), file = "get_data.R"),
    "cannot select nodes"
  )
  expect_error(
    markermd_question(1L, "x", rules = list(markermd_rule(node_type = "Any node", verb = "has content", values = "*")), file = "get_data.R"),
    "rules or filters"
  )
  expect_error(markermd_question(1L, "x", scope = "document", file = "get_data.R"), "document-scope")
})

test_that("template file paths are repository-relative and unique", {
  expect_error(markermd_question(1L, "x", file = "/abs/a.R"), "relative")
  expect_error(markermd_question(1L, "x", file = "../a.R"), "'\\.\\.'")
  expect_error(markermd_template(original_ast = tf_ast(), files = c("a.R", "a.R")), "unique")
  expect_error(
    markermd_template(original_ast = tf_ast(), files = "a.R", questions = list(markermd_question(1L, "b", file = "b.R"))),
    "not among the template's files"
  )
})

test_that("files and file questions round-trip through YAML as format 3.2", {
  template = tf_template()
  path = tempfile(fileext = ".yaml")
  src = system.file("examples/test_assignment/repos/student1-excellent/assignment.qmd", package = "markermd")
  write_template_yaml(template, path, source_path = src)

  raw = yaml::read_yaml(path)
  expect_equal(raw$format_version, "3.2")
  expect_equal(unlist(raw$files), c("get_data.R", "scripts/clean.py"))
  expect_equal(raw$questions[[2]]$file, "get_data.R")
  expect_null(raw$questions[[2]]$node_ids)
  expect_null(raw$questions[[2]]$rules)

  testthat::skip_if_not_installed("jsonvalidate")
  expect_true(validate_template_file(path))

  back = read_template_yaml(path)
  expect_equal(back@files, template@files)
  expect_equal(vapply(back@questions, function(q) q@file, character(1)), c(NA, "get_data.R", "scripts/clean.py"))
})

test_that("a template without files still writes its old format version", {
  template = markermd_template(
    original_ast = tf_ast(),
    questions = list(markermd_question(1L, "Q2", markermd_node_selection(node_ids = "question-2-basic-programming")))
  )
  expect_equal(markermd:::template_to_list(template)$format_version, "3.0")
  expect_null(markermd:::template_to_list(template)$files)
})

test_that("the schema accepts a single file and rejects rules on a file question", {
  testthat::skip_if_not_installed("jsonvalidate")
  ok = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '3.2'",
    "files: [get_data.R]",
    "questions:",
    "  - id: 1",
    "    name: get_data.R",
    "    file: get_data.R"
  ), ok)
  expect_true(validate_template_file(ok))

  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '3.2'",
    "files: [get_data.R]",
    "questions:",
    "  - id: 1",
    "    name: get_data.R",
    "    file: get_data.R",
    "    rules:",
    "      - node_type: Any node",
    "        verb: has content",
    "        pattern: '*'"
  ), path)
  expect_false(suppressMessages(suppressWarnings(validate_template_file(path))))
})

test_that("the template diff reports file changes", {
  a = markermd:::template_to_list(tf_template())
  b = a
  b$files = list("get_data.R")
  b$questions = b$questions[1:2]
  b$questions[[2]]$file = NULL
  b$questions[[2]]$node_ids = list()
  changes = markermd:::diff_template(a, b)
  expect_true(any(changes$change == "file removed" & changes$detail == "scripts/clean.py"))
  expect_true(any(changes$question == "get_data.R" & grepl("file", changes$detail)))
})

# A mark fixture project whose template adds two file questions: get_data.R
# (at the root of student1, nested in student2, missing elsewhere) and
# plot.png (an image in student1)
tf_mark_project = function() {
  root = make_mark_fixture()$project
  repos = file.path(root, "repos")
  writeLines("x = read.csv('data.csv')", file.path(repos, "student1-excellent", "get_data.R"))
  dir.create(file.path(repos, "student2-average", "R"))
  writeLines("x = 1", file.path(repos, "student2-average", "R", "get_data.R"))
  file.copy(system.file("help", "figures", "logo.png", package = "fs"), file.path(repos, "student1-excellent", "plot.png"))

  template = markermd:::load_template_from_db(root, base_dir = root)
  template@files = c("get_data.R", "plot.png")
  template@questions = c(template@questions, list(
    markermd_question(10L, "Data script", file = "get_data.R"),
    markermd_question(11L, "Plot", file = "plot.png")
  ))
  suppressMessages(project_set(root, template = template))
  root
}

test_that("find_repo_file prefers the exact path, then the shallowest name match", {
  d = withr::local_tempdir()
  dir.create(file.path(d, "a", "b"), recursive = TRUE)
  dir.create(file.path(d, ".git"))
  writeLines("1", file.path(d, "a", "b", "f.R"))
  writeLines("2", file.path(d, "a", "f.R"))
  writeLines("3", file.path(d, ".git", "g.R"))
  expect_equal(basename(dirname(markermd:::find_repo_file(d, "f.R"))), "a")
  expect_equal(basename(dirname(markermd:::find_repo_file(d, "a/b/f.R"))), "b")
  expect_true(is.na(markermd:::find_repo_file(d, "g.R")))
  expect_true(is.na(markermd:::find_repo_file(d, "missing.R")))
})

test_that("file questions pass when the repository has the file", {
  root = tf_mark_project()
  result = validate_project(root)
  data = result[result$question == "Data script", ]
  expect_equal(data$status[data$repo == "student1-excellent"], "pass")
  expect_equal(data$status[data$repo == "student2-average"], "pass")
  expect_match(data$detail[data$repo == "student2-average"], "at R/get_data.R", fixed = TRUE)
  expect_equal(data$status[data$repo == "student3-poor"], "fail")
  expect_match(data$detail[data$repo == "student3-poor"], "not found")

  q = markermd_question(1L, "x", file = "get_data.R")
  expect_equal(markermd:::validate_file_question(q, NULL)$status, "fail")
  expect_equal(markermd:::get_question_ast(markermd:::empty_pandoc_ast(), q)@blocks@content, list())
})

test_that("the file viewer shows text, images and misses", {
  d = withr::local_tempdir()
  writeLines("all: data", file.path(d, "Makefile"))
  file.copy(system.file("help", "figures", "logo.png", package = "fs"), file.path(d, "p.png"))
  text = as.character(markermd:::file_view_ui(file.path(d, "Makefile"), "Makefile", "ed"))
  expect_match(text, "id=\"ed\"", fixed = TRUE)
  expect_match(text, "'plaintext'", fixed = TRUE)
  expect_match(as.character(markermd:::file_view_ui(file.path(d, "p.png"), "p.png", "ed")), "data:image/png;base64,", fixed = TRUE)
  expect_match(as.character(markermd:::file_view_ui(NA_character_, "gone.R", "ed")), "was not found")
  expect_equal(markermd:::file_monaco_language("x/get_lq.py"), "python")
})

test_that("mark shows a file question's file and its validation", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = tf_mark_project()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_file_questions")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  app$set_inputs(`rubric_module-question_select` = "Data script")
  app$wait_for_js("!!document.getElementById('markermd-file-editor')", timeout = 10000)
  expect_match(app$get_html("#content_module-content_display"), "get_data.R", fixed = TRUE)

  app$set_inputs(`content_module-content_repo_select` = "student3-poor")
  app$wait_for_js("document.getElementById('content_module-content_display').innerText.includes('was not found')", timeout = 10000)

  app$set_inputs(`rubric_module-question_select` = "Plot")
  app$set_inputs(`content_module-content_repo_select` = "student1-excellent")
  app$wait_for_js("!!document.querySelector('#content_module-content_display img[src^=\"data:image/png\"]')", timeout = 10000)

  # Back on a document question the report shows again
  app$set_inputs(`rubric_module-question_select` = "Q2")
  app$wait_for_js("!!document.getElementById('artifact-content-frame')", timeout = 10000)

  # The validation modal lists the file questions with their result
  app$set_inputs(main_navbar = "assignments")
  app$run_js("Shiny.setInputValue('validation_clicked', 3, {priority: 'event'})")
  app$wait_for_js("!!document.querySelector('#shiny-modal.show #explore_module-qcard_Datascript')", timeout = 10000)
  card = app$get_js("document.getElementById('explore_module-qcard_Datascript').innerText")
  expect_match(card, "not found in the repository")
})

test_that("assignment_outline lists the assignment's other files", {
  d = withr::local_tempdir()
  file.copy(system.file("examples/test_assignment/repos/student1-excellent/assignment.qmd", package = "markermd"), d)
  writeLines("x = 1", file.path(d, "get_data.R"))
  writeLines("# Read me", file.path(d, "README.md"))
  files = attr(assignment_outline(d), "files")
  expect_setequal(files$path, c("get_data.R", "README.md"))
  expect_equal(files$filtered[match(c("get_data.R", "README.md"), files$path)], c(FALSE, TRUE))
  expect_true(all(files$size > 0))
})
