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
  item_event(app, item, "points", value)
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
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  # No deductions selected: negative mode starts at the maximum
  expect_equal(mg_score(app, "Q2"), "10 / 10 pts")

  # A -3 deduction: total + selected = 7
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  mg_set_points(app, "item_0", -3)
  app$click(selector = "#rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "7 / 10 pts")

  # An additional -8 takes the raw score to -1, clamped to 0 by bound_above_zero
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  mg_set_points(app, "item_1", -8)
  app$click(selector = "#rubric_module-item_1-hotkey_btn")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "0 / 10 pts")
})


test_that("positive-mode score clamps at the question total", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()  # Q2 has no settings row, so it defaults to positive / out of 10

  app = new_app_driver(markermd:::mark_app(root), name = "mark_belowmax_clamp")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "0 / 10 pts")

  # A +15 award exceeds the total; bound_below_max clamps the display to 10
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  mg_set_points(app, "item_0", 15)
  app$click(selector = "#rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "10 / 10 pts")

  # Deselecting returns to zero
  app$click(selector = "#rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()
  expect_equal(mg_score(app, "Q2"), "0 / 10 pts")
})


test_that("moving a rubric item reorders it and renumbers hotkeys, including the wrap branch", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()

  app = new_app_driver(markermd:::mark_app(root), name = "mark_item_reorder")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  for (i in 0:2) {
    app$click("rubric_module-add_item")
    app$wait_for_idle()
    item_event(app, paste0("item_", i), "description", LETTERS[i + 1])
    app$wait_for_idle()
  }

  descriptions = function() {
    app$get_values(export = TRUE)$export[["rubric_module-rubric_item_descriptions"]]
  }
  expect_equal(descriptions(), c("A", "B", "C"))

  # Move A (item_0) down: order becomes B, A, C and hotkeys renumber 1..3
  app$click(selector = "#rubric_module-item_0-move_down_btn")
  app$wait_for_idle()
  expect_equal(descriptions(), c("B", "A", "C"))

  items = markermd:::load_rubric_items(root, "Q2")
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), 1:3)
  expect_equal(unname(purrr::map_chr(items, ~ .x@description)), c("B", "A", "C"))

  # Move B (item_1, now at the top) up: it wraps to the bottom -> A, C, B
  app$click(selector = "#rubric_module-item_1-move_up_btn")
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
  app$set_inputs(main_navbar = "grade")
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
  app$set_inputs(main_navbar = "grade")
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
  app$set_inputs(main_navbar = "grade")
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
  app$click(selector = "#rubric_module-item_10-hotkey_btn")
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
  app$click(selector = paste0("#rubric_module-", ids[12], "-move_up_btn"))
  app$wait_for_idle()
  items = markermd:::load_rubric_items(root, "Q2")
  expect_equal(names(items)[11:12], c(ids[12], "item_10"))
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), c(1:10, NA, NA))
})


test_that("the Assignments table tabulates rubric scores per repository and per question", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_scores_table")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  # Grade Q2 for the first repo: one 4-point item selected
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  item_event(app, "item_0", "points", 4)
  app$wait_for_idle()
  app$click(selector = "#rubric_module-item_0-hotkey_btn")
  app$wait_for_idle()

  app$set_inputs(main_navbar = "assignments")
  app$wait_for_idle()

  exports = app$get_values(export = TRUE)$export
  tbl = exports[["scores_table"]]
  expect_equal(names(tbl), c("Repository", "Q2", "Q3", "Total", "Total_partial", "Graded"))
  first = tbl[tbl$Repository == "student1-excellent", ]
  expect_equal(first$Q2, 4)
  expect_true(is.na(first$Q3))
  # the total waits for every question; the running sum shows what is graded
  expect_true(is.na(first$Total))
  expect_equal(first$Total_partial, 4)
  expect_equal(first$Graded, 1L)
  expect_true(all(is.na(tbl$Q2[tbl$Repository != "student1-excellent"])))

  summary = exports[["scores_summary"]]
  expect_equal(summary$Repository, c("Graded", "Mean", "Median", "SD"))
  expect_equal(summary$Q2, c(1, 4, 4, NA))
  expect_equal(summary$Q3, c(0, NA, NA, NA))

  # The table is rendered into the tab with the link buttons, progress bars,
  # point-carrying score labels and the summary rows in its footer
  expect_true(app$get_js("document.querySelector('#repo_table .rt-tfoot') !== null"))
  table_html = app$get_html("#repo_table")
  expect_match(table_html, "folder_clicked", fixed = TRUE)
  expect_match(table_html, "10 pts", fixed = TRUE)
  expect_match(table_html, "Median")
  expect_match(table_html, "progress-bar bg-success", fixed = TRUE)

  # Clicking a score cell (second row, Q3: the ninth visible cell, after the
  # repository, four link, validation and progress columns and Q2) opens
  # that repository and question in the Rubric tab
  app$run_js("
    var row = document.querySelectorAll('#repo_table .rt-tbody .rt-tr')[1];
    row.querySelectorAll('.rt-td')[8].click();
  ")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "main_navbar"), "grade")
  expect_equal(app$get_value(input = "content_module-content_repo_select"), "student2-average")
  expect_equal(app$get_value(input = "rubric_module-question_select"), "Q3")
})


test_that("the table's validation marker opens the repo's results in a modal", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_validation_modal")
  app$wait_for_idle()

  # There is no Validation tab; the table's validation markers are buttons
  expect_false(app$get_js("!!document.querySelector('a[data-value=\"validation\"]')"))
  expect_match(app$get_html("#repo_table"), "validation_clicked(&#39;|'), 3,")

  open_validation = function(index) {
    app$run_js(paste0("Shiny.setInputValue('validation_clicked', ", index, ", {priority: 'event'})"))
    app$wait_for_js("!!document.querySelector('#shiny-modal.show #explore_module-template_validation_ui .card')")
    app$wait_for_idle()
  }
  close_modal = function() {
    app$run_js("document.querySelector('#shiny-modal [data-bs-dismiss=\"modal\"]').click()")
    app$wait_for_js("!document.getElementById('shiny-modal')")
  }
  modal_title = function() app$get_js("document.querySelector('#shiny-modal .modal-title').innerText")

  # Clicking the third repo's marker shows its results without moving the
  # app-wide selection or the tab
  open_validation(3)
  expect_match(modal_title(), "student3-poor")
  expect_equal(app$get_value(input = "main_navbar"), "assignments")
  expect_equal(app$get_value(input = "content_module-content_repo_select"), "student1-excellent")
  expect_match(app$get_html("#explore_module-template_validation_ui"), "Q2")
  close_modal()

  # The question shortcut select labels each question with the repository's
  # result, coloured
  option_labels = function() {
    app$get_js("
      var sel = document.getElementById('explore_module-question_select').selectize;
      Object.keys(sel.options).map(function(k) { return sel.options[k].label; }).join('|');
    ")
  }
  open_validation(4)
  expect_match(modal_title(), "student4-incomplete")
  expect_equal(option_labels(), "\u2713 Q2|\u2717 Q3")
  close_modal()

  # Opening the same repository twice renders its results again
  open_validation(1)
  close_modal()
  open_validation(1)
  expect_equal(option_labels(), "\u2713 Q2|\u2713 Q3")
  expect_equal(app$get_value(input = "explore_module-question_select"), "")

  # Choosing a question scrolls its card into view
  expect_true(app$get_js("document.getElementById('explore_module-qcard_Q3') !== null"))
  app$run_js("
    var body = document.querySelector('#shiny-modal .modal-body');
    body.style.maxHeight = '120px'; body.style.overflow = 'auto';
  ")
  app$set_inputs(`explore_module-question_select` = "Q3")
  app$wait_for_idle()
  Sys.sleep(1)
  expect_gt(app$get_js("document.querySelector('#shiny-modal .modal-body').scrollTop"), 0)
  expect_match(
    app$get_js("document.getElementById('explore_module-question_select').parentElement.querySelector('.selectize-control .item').outerHTML"),
    "text-success", fixed = TRUE
  )

  # Cards start with their rule results showing and the matched content
  # collapsed (the question shortcut expanded Q3's). Expanding one from its chevron keeps it
  # expanded for the next repository opened, and collapsing it again sticks
  is_shown = function(q) {
    app$get_js(paste0("document.getElementById('explore_module-qcard_", q, "_body').classList.contains('show')"))
  }
  toggle = function(q) {
    app$run_js(paste0("document.querySelector('#explore_module-qcard_", q, " .validation-collapse-toggle').click()"))
    app$wait_for_idle()
    Sys.sleep(1)
  }
  expect_false(is_shown("Q2"))
  expect_true(is_shown("Q3"))
  expect_true(app$get_js("document.querySelector('#explore_module-qcard_Q2 .card-body').innerText.trim().length > 0"))

  toggle("Q2")
  expect_true(is_shown("Q2"))
  expect_setequal(unlist(app$get_value(input = "explore_module-expanded_questions")), c("Q2", "Q3"))
  toggle("Q3")
  expect_equal(unlist(app$get_value(input = "explore_module-expanded_questions")), "Q2")
  close_modal()

  open_validation(4)
  expect_true(is_shown("Q2"))
  expect_false(is_shown("Q3"))

  # A node preview replaces the modal; its Back button returns to it
  app$run_js("document.querySelector('#explore_module-template_validation_ui button[id^=\"explore_module-preview_\"]').click()")
  app$wait_for_js("!!document.getElementById('explore_module-preview_back')")
  app$run_js("document.getElementById('explore_module-preview_back').click()")
  app$wait_for_js("!!document.querySelector('#shiny-modal.show #explore_module-template_validation_ui .card')")
  expect_match(modal_title(), "student4-incomplete")
})

test_that("a rubric item's toggle button keeps its size under a long description", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mg_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_item_button_size")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  app$click("rubric_module-add_item")
  app$wait_for_idle()
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  # The uiOutput wrapper is display: contents, so the button is the flex
  # item; a description far wider than the pane used to squeeze it
  app$run_js(glue::glue(
    "var d = document.getElementById('rubric_module-item_1-description_text');
     d.innerText = '<<strrep('long description ', 20)>>';
     d.dispatchEvent(new Event('input'));",
    .open = "<<", .close = ">>"
  ))
  app$wait_for_idle()

  size = function(item) {
    app$get_js(glue::glue(
      "(function() {
        var r = document.getElementById('rubric_module-<<item>>-hotkey_btn').getBoundingClientRect();
        return Math.round(r.width) + 'x' + Math.round(r.height);
      })()",
      .open = "<<", .close = ">>"
    ))
  }
  expect_equal(size("item_0"), "28x28")
  expect_equal(size("item_1"), "28x28")
})
