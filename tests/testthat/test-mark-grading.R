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

  app = new_app_driver(markermd:::mark_app(root), name = "mark_negative_score")
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

  app = new_app_driver(markermd:::mark_app(root), name = "mark_belowmax_clamp")
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

  app = new_app_driver(markermd:::mark_app(root), name = "mark_item_reorder")
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

  app = new_app_driver(markermd:::mark_app(root), name = "mark_comment_attribution")
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

  app = new_app_driver(markermd:::mark_app(root), name = "mark_status_filter")
  app$wait_for_idle()  # default tab is "assignments", where the table lives

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

  app = new_app_driver(markermd:::mark_app(root), name = "mark_nav_hotkeys")
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


test_that("items past the tenth have no hotkey, toggle by click, and keep their order", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()
  # Seed eleven items headlessly so the app opens with an un-hotkeyed tail
  markermd:::save_rubric_items(root, "Q2", setNames(lapply(1:11, function(i) {
    markermd::markermd_rubric_item(markermd:::position_hotkey(i), -1, LETTERS[i])
  }), paste0("item_", 0:10)))

  app = new_app_driver(markermd:::mark_app(root), name = "mark_item_eleven")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  label = function(item) {
    trimws(app$get_js(sprintf(
      "document.getElementById('rubric_module-%s-hotkey_btn').textContent", item
    )))
  }
  selected = function(item) {
    app$get_js(sprintf(
      "document.getElementById('rubric_module-%s-hotkey_btn').classList.contains('btn-success')", item
    ))
  }
  press = function(key) {
    app$run_js(glue::glue(
      "document.dispatchEvent(new KeyboardEvent('keydown', {key: '<<key>>'}));",
      .open = "<<", .close = ">>"
    ))
    app$wait_for_idle()
  }

  expect_equal(label("item_0"), "1")
  expect_equal(label("item_9"), "0")
  expect_equal(label("item_10"), "")

  # the eleventh item is reachable by clicking ...
  app$click("rubric_module-item_10-hotkey_btn")
  app$wait_for_idle()
  expect_true(selected("item_10"))

  # ... but not from the keyboard: 1 toggles the first item and 0 the tenth
  press("1")
  expect_true(selected("item_0"))
  press("0")
  expect_true(selected("item_9"))
  expect_true(selected("item_10"))

  # Add Item appends a twelfth, also without a hotkey
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  ids = app$get_values(export = TRUE)$export[["rubric_module-rubric_item_ids"]]
  expect_length(ids, 12)
  expect_equal(label(ids[12]), "")

  # moving the twelfth above the eleventh is persisted in that order
  app$click(paste0("rubric_module-", ids[12], "-move_up_btn"))
  app$wait_for_idle()
  items = markermd:::load_rubric_items(root, "Q2")
  expect_equal(names(items)[11:12], c(ids[12], "item_10"))
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), c(1:10, NA, NA))
})


test_that("the Scores tab tabulates rubric scores per repository and per question", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_scores_tab")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  # Grade Q2 for the first repo: one 4-point item selected
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  app$run_js("Shiny.setInputValue('rubric_module-item_0-points_text', 4)")
  app$wait_for_idle()
  app$click("rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()

  app$set_inputs(main_navbar = "scores")
  app$wait_for_idle()

  exports = app$get_values(export = TRUE)$export
  tbl = exports[["scores_module-scores_table"]]
  expect_equal(names(tbl), c("Repository", "Q2", "Q3", "Total", "Graded"))
  first = tbl[tbl$Repository == "student1-excellent", ]
  expect_equal(first$Q2, 4)
  expect_true(is.na(first$Q3))
  expect_equal(first$Total, 4)
  expect_equal(first$Graded, 1L)
  expect_true(all(is.na(tbl$Q2[tbl$Repository != "student1-excellent"])))

  summary = exports[["scores_module-scores_summary"]]
  expect_equal(summary$Repository, c("Graded", "Mean", "Median", "SD"))
  expect_equal(summary$Q2, c(1, 4, 4, NA))
  expect_equal(summary$Q3, c(0, NA, NA, NA))

  # The interactive table is rendered into the tab with point-carrying labels
  # and the summary rows in its footer; the caption reports that no repo is
  # fully graded yet
  expect_true(app$get_js("document.querySelector('#scores_module-scores_table .rt-tfoot') !== null"))
  table_html = app$get_html("#scores_module-scores_table")
  expect_match(table_html, "10 pts", fixed = TRUE)
  expect_match(table_html, "Median")
  expect_match(table_html, "progress-bar bg-success", fixed = TRUE)
  expect_match(app$get_html("#scores_module-caption"), "0 of 5 repositories with every question graded")

  # Clicking a score cell (second row, Q3 column) opens that repository and
  # question in the Rubric tab
  app$run_js("
    var row = document.querySelectorAll('#scores_module-scores_table .rt-tbody .rt-tr')[1];
    row.querySelectorAll('.rt-td')[2].click();
  ")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "main_navbar"), "rubric")
  expect_equal(app$get_value(input = "content_module-content_repo_select"), "student2-average")
  expect_equal(app$get_value(input = "rubric_module-question_select"), "Q3")
})


test_that("the Validation tab follows the app-wide repo selection and the table opens it", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_validation_tab")
  app$wait_for_idle()

  # The landing tab holds the table, whose validation markers are buttons
  expect_equal(app$get_value(input = "main_navbar"), "assignments")
  expect_match(app$get_html("#repo_table"), "validation_clicked(&#39;|'), 3,")

  # Clicking the third repo's marker opens it in the Validation tab
  app$run_js("Shiny.setInputValue('validation_clicked', 3, {priority: 'event'})")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "main_navbar"), "validation")
  expect_equal(app$get_value(input = "validation_repo_select"), "student3-poor")
  expect_equal(app$get_value(input = "content_module-content_repo_select"), "student3-poor")
  expect_match(app$get_html("#explore_module-template_validation_ui"), "Q2")

  # Choosing another repository in the tab moves the app-wide selection
  app$set_inputs(validation_repo_select = "student2-average")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "content_module-content_repo_select"), "student2-average")
})
