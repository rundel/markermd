# Self-contained fixture (mirrors make_mark_fixture in test-mark-app.R but with a
# unique name, since testthat does not share helpers across files reliably). Two
# questions: Q2 graded on "quantile" content, Q3 on "ggplot".
mg_make_fixture = function() {
  src = system.file("examples/test_assignment/repos", package = "markermd")
  root = tempfile("markproj_")
  repos = file.path(root, "repos")
  dir.create(repos, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), repos, recursive = TRUE)

  html_dir = file.path(root, "html")
  dir.create(html_dir)
  writeLines("<html><body>r</body></html>", file.path(html_dir, "student1-excellent.html"))

  qmd = file.path(repos, "student1-excellent", "assignment.qmd")
  ast = markermd:::parse_assignment_document(qmd)
  template = markermd::markermd_template(
    original_ast = ast,
    questions = list(
      markermd::markermd_question(
        1L, "Q2", markermd::markermd_node_selection(node_ids = "question-2-basic-programming"),
        list(markermd::markermd_rule(node_type = "Any node", verb = "has content", values = "quantile"))
      ),
      markermd::markermd_question(
        2L, "Q3", markermd::markermd_node_selection(node_ids = "question-3-data-visualization"),
        list(markermd::markermd_rule(node_type = "Any node", verb = "has content", values = "ggplot"))
      )
    ),
    metadata = markermd::markermd_metadata()
  )
  markermd::write_template_yaml(template, file.path(root, "template.yaml"), source_path = qmd)

  suppressMessages(markermd::init_project(root))
  suppressMessages(markermd::project_set(root, template = "template.yaml"))
  root
}

mg_score = function(app, q) {
  trimws(app$get_js(sprintf(
    "document.getElementById('rubric_module-grade_%s-score_display').innerHTML", q
  )))
}

mg_set_points = function(app, item, value) {
  app$run_js(sprintf("Shiny.setInputValue('rubric_module-%s-points_text', %s)", item, value))
  app$wait_for_idle()
}


test_that("negative-mode score recomputes live and clamps at zero", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()
  # Seed Q2 as negative-mode out of 10 so the grade widget loads in that mode.
  markermd:::save_grade_state(
    root, "Q2",
    markermd::markermd_grade_state(
      current_score = 10, total_score = 10, grading_mode = "negative",
      bound_above_zero = TRUE, bound_below_max = TRUE
    )
  )

  app = shinytest2::AppDriver$new(markermd:::mark_app(root), name = "mark_negative_score")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  # No deductions selected: negative mode starts at the maximum
  expect_equal(mg_score(app, "Q2"), "10 / 10 pts")

  # A -3 deduction: total + selected = 7
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  mg_set_points(app, "item_0", -3)
  app$click("rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "7 / 10 pts")

  # An additional -8 takes the raw score to -1, clamped to 0 by bound_above_zero
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  mg_set_points(app, "item_1", -8)
  app$click("rubric_module-item_1-hotkey_btn")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "0 / 10 pts")
})


test_that("positive-mode score clamps at the question total", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()  # Q2 has no settings row, so it defaults to positive / out of 10

  app = shinytest2::AppDriver$new(markermd:::mark_app(root), name = "mark_belowmax_clamp")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "0 / 10 pts")

  # A +15 award exceeds the total; bound_below_max clamps the display to 10
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  mg_set_points(app, "item_0", 15)
  app$click("rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "10 / 10 pts")

  # Deselecting returns to zero
  app$click("rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "0 / 10 pts")
})


test_that("moving a rubric item reorders it and renumbers hotkeys, including the wrap branch", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()

  app = shinytest2::AppDriver$new(markermd:::mark_app(root), name = "mark_item_reorder")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  for (i in 0:2) {
    app$click("rubric_module-add_item")
    app$wait_for_idle()
    app$run_js(sprintf(
      "Shiny.setInputValue('rubric_module-item_%d-description_text', '%s')", i, LETTERS[i + 1]
    ))
    app$wait_for_idle()
  }

  descriptions = function() {
    app$get_values(export = TRUE)$export[["rubric_module-rubric_item_descriptions"]]
  }
  expect_equal(descriptions(), c("A", "B", "C"))

  # Move A (item_0) down: order becomes B, A, C and hotkeys renumber 1..3
  app$click("rubric_module-item_0-move_down_btn")
  app$wait_for_idle()
  expect_equal(descriptions(), c("B", "A", "C"))

  items = markermd:::load_rubric_items(root, "Q2")
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), 1:3)
  expect_equal(unname(purrr::map_chr(items, ~ .x@description)), c("B", "A", "C"))

  # Move B (item_1, now at the top) up: it wraps to the bottom -> A, C, B
  app$click("rubric_module-item_1-move_up_btn")
  app$wait_for_idle()
  expect_equal(descriptions(), c("A", "C", "B"))
  items2 = markermd:::load_rubric_items(root, "Q2")
  expect_equal(unname(purrr::map_int(items2, ~ .x@hotkey)), 1:3)
  expect_equal(unname(purrr::map_chr(items2, ~ .x@description)), c("A", "C", "B"))
})


test_that("a pending comment edit is saved to the pair it was typed against on navigation", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()

  app = shinytest2::AppDriver$new(markermd:::mark_app(root), name = "mark_comment_attribution")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  # Type into Q2's comment but do NOT wait out the 1s debounce
  app$set_inputs(`rubric_module-question_comment` = "feedback for Q2")
  app$wait_for_idle()

  # Switch question before the debounce fires: the nav flush must persist the
  # pending edit to (Q2, student1-excellent), not the new pair
  app$set_inputs(`rubric_module-question_select` = "Q3")
  app$wait_for_idle()

  expect_equal(
    markermd:::load_comment(root, "Q2", "student1-excellent"),
    "feedback for Q2"
  )
  expect_null(markermd:::load_comment(root, "Q3", "student1-excellent"))
})


test_that("the assignments-table status filter narrows the repo list", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()
  # Make student1-excellent fully graded (both questions get a public comment)
  markermd:::save_comment(root, "Q2", "student1-excellent", "ok")
  markermd:::save_comment(root, "Q3", "student1-excellent", "ok")

  app = shinytest2::AppDriver$new(markermd:::mark_app(root), name = "mark_status_filter")
  app$wait_for_idle()  # default tab is "validation", where the table lives

  table_has = function(repo) grepl(repo, app$get_html("#repo_table"), fixed = TRUE)

  app$set_inputs(repo_status_filter = "all")
  app$wait_for_idle()
  expect_true(table_has("student1-excellent"))
  expect_true(table_has("student3-poor"))

  # Complete: only the fully-graded repo
  app$set_inputs(repo_status_filter = "graded")
  app$wait_for_idle()
  expect_true(table_has("student1-excellent"))
  expect_false(table_has("student3-poor"))

  # Incomplete: everything except the fully-graded repo
  app$set_inputs(repo_status_filter = "ungraded")
  app$wait_for_idle()
  expect_false(table_has("student1-excellent"))
  expect_true(table_has("student3-poor"))

  # Failed validation: the repos with a failing section, not the clean graded one
  app$set_inputs(repo_status_filter = "failed")
  app$wait_for_idle()
  expect_false(table_has("student1-excellent"))
  expect_true(table_has("student3-poor"))     # Q2 quantile rule fails
  expect_true(table_has("student4-incomplete"))  # Q3 ggplot rule fails
  expect_true(table_has("student5-messy"))    # Q3 anchor no longer resolves
})


test_that("navigation hotkeys move repos (z/x), questions (,/.), and the html toggle (h)", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()

  app = shinytest2::AppDriver$new(markermd:::mark_app(root), name = "mark_nav_hotkeys")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  press = function(key) {
    app$run_js(glue::glue(
      "document.dispatchEvent(new KeyboardEvent('keydown', {key: '<<key>>'}));",
      .open = "<<", .close = ">>"
    ))
    app$wait_for_idle()
  }

  expect_equal(app$get_value(input = "content_module-content_repo_select"), "student1-excellent")
  expect_equal(app$get_value(input = "rubric_module-question_select"), "Q2")

  press("x")
  expect_equal(app$get_value(input = "content_module-content_repo_select"), "student2-average")

  # z steps back, then wraps from the first repo to the last
  press("z")
  expect_equal(app$get_value(input = "content_module-content_repo_select"), "student1-excellent")
  press("z")
  expect_equal(app$get_value(input = "content_module-content_repo_select"), "student5-messy")

  press(".")
  expect_equal(app$get_value(input = "rubric_module-question_select"), "Q3")
  press(",")
  expect_equal(app$get_value(input = "rubric_module-question_select"), "Q2")

  expect_true(app$get_value(input = "content_module-html_toggle"))
  press("h")
  expect_false(app$get_value(input = "content_module-html_toggle"))
})
