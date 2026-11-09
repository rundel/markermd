# A project whose key holds the example assignment plus other files: a
# script, a Makefile, and files the Add files list filters by default
make_files_project = function() {
  src = system.file("examples/test_assignment/repos/student1-excellent", package = "markermd")
  root = tempfile("filesproj_")
  key = file.path(root, "key")
  dir.create(key, recursive = TRUE)
  dir.create(file.path(root, "repos"))
  file.copy(list.files(src, full.names = TRUE), key)
  writeLines("x = read.csv('data.csv')", file.path(key, "get_data.R"))
  writeLines("all: data.csv", file.path(key, "Makefile"))
  writeLines("# Instructions", file.path(key, "README.md"))
  writeLines("version = 1", file.path(key, "uv.lock"))

  suppressMessages(markermd::init_project(root))
  suppressMessages(markermd::project_set(root, key = "key"))
  root
}

test_that("template() adds files and questions that cover them", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_files_project()
  app = new_app_driver(template(root), name = "template_files")

  list_text = function() app$get_js("document.getElementById('add_files_list').innerText")

  # The Add files list offers the key's files, hiding the filtered ones
  # until asked
  app$click("add_files")
  app$wait_for_js("!!document.querySelector('#add_files_list input[type=checkbox]')")
  expect_match(list_text(), "get_data.R", fixed = TRUE)
  expect_match(list_text(), "Makefile", fixed = TRUE)
  expect_false(grepl("README.md", list_text(), fixed = TRUE))
  expect_false(grepl("assignment.qmd", list_text(), fixed = TRUE))

  app$set_inputs(add_files_show_all = TRUE)
  app$wait_for_js("document.getElementById('add_files_list').innerText.includes('README.md')")
  expect_match(list_text(), "uv.lock (lock file)", fixed = TRUE)

  app$set_inputs(add_files_selected = c("get_data.R", "Makefile"))
  app$click("apply_files")
  app$wait_for_idle()

  # The Add modal puts an entry on a file; the left pane then shows that
  # file's source in place of the document tree
  add_entry(app, file = "get_data.R")
  app$wait_for_js("!!document.getElementById('markermd-template-file-editor')")
  expect_equal(app$get_value(output = "template_shown_file"), "get_data.R")
  expect_equal(unname(unlist(app$get_values(export = "question_names")$export$question_names)), "get_data.R")

  # A document entry shows the tree again
  add_entry(app, name = "Q1")
  expect_equal(app$get_value(output = "template_shown_file"), "")

  add_entry(app, file = "Makefile", name = "Build")
  expect_equal(app$get_value(output = "template_shown_file"), "Makefile")
  expect_match(app$get_text("#question_wrapper_3 .question-file-label"), "Makefile")

  app$click("save_to_project")
  app$wait_for_idle()

  saved = markermd:::load_template_from_db(root, base_dir = root)
  expect_setequal(saved@files, c("get_data.R", "Makefile"))
  files = vapply(saved@questions, function(q) q@file, character(1))
  expect_equal(unname(files), c("get_data.R", NA, "Makefile"))

  # Files a question covers stay in the template
  app$click("add_files")
  app$wait_for_js("!!document.querySelector('#add_files_list input[type=checkbox]')")
  app$set_inputs(add_files_selected = character(0), allow_no_input_binding_ = TRUE)
  app$click("apply_files")
  app$wait_for_idle()
  app$click("save_to_project")
  app$wait_for_idle()
  expect_setequal(markermd:::load_template_from_db(root, base_dir = root)@files, c("get_data.R", "Makefile"))
})

test_that("the Add modal adds sections and entire-document entries", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_files_project()
  app = new_app_driver(template(root), name = "template_files_coverage")

  add_entry(app, coverage = "document")
  add_entry(app)
  # A taken name is refused and the modal stays open
  app$click("add_question")
  app$wait_for_js("!!document.getElementById('confirm_add_entry')")
  app$set_inputs(new_entry_name = "Document")
  app$click("confirm_add_entry")
  app$wait_for_idle()
  expect_true(app$get_js("!!document.getElementById('confirm_add_entry')"))
  app$set_inputs(new_entry_name = "")
  app$click("confirm_add_entry")
  app$wait_for_js("!document.getElementById('confirm_add_entry')")
  app$wait_for_idle()

  names = unname(unlist(app$get_values(export = "question_names")$export$question_names))
  expect_equal(names, c("Document", "Question 2", "Question 3"))
  expect_match(app$get_text("#question_wrapper_1 .question-file-label"), "assignment.qmd")
  expect_match(app$get_text("#question_1-summary_line"), "Entire document")

  # An entire-document entry does not take heading selections
  app$run_js("document.getElementById('question_wrapper_1').click()")
  app$wait_for_idle()
  click_tree_node(app, 3)
  app$wait_for_idle()
  expect_equal(length(app$get_values(export = "selected_nodes")$export$selected_nodes[["1"]]), 0)

  app$click("save_to_project")
  app$wait_for_idle()
  saved = markermd:::load_template_from_db(root, base_dir = root)
  expect_equal(vapply(saved@questions, function(q) q@scope, character(1)), c("document", "sections", "sections"))
})
