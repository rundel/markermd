test_that("the Rubric tab edits every question's rubric in one place", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  markermd:::save_rubric_item(root, "Q2", "item_0", markermd::markermd_rubric_item(1L, -2, "Missing quantile"))
  markermd:::save_rubric_item(root, "Q3", "item_1", markermd::markermd_rubric_item(1L, -1, "No labels"))
  markermd:::save_grade_selection(root, "Q2", "student2-average", "item_0", TRUE)
  markermd:::save_grade_selection(root, "Q2", "student3-poor", "item_0", TRUE)

  app = new_app_driver(markermd:::mark_app(root), name = "mark_rubric_editor")
  container_count = function(view, item_id) {
    app$get_js(paste0("document.querySelectorAll('[id=\"rubric_module-", view, "-", item_id, "-container\"]').length"))
  }

  # Every question shows, each item with its selection count in place of
  # the toggle
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_js("!!document.querySelector('#rubric_module-rubric_editor_ui .rubric-editor')", timeout = 10000)
  editor = app$get_text("#rubric_module-rubric_editor_ui")
  expect_match(editor, "Q2")
  expect_match(editor, "Q3")
  expect_match(app$get_text("#rubric_module-editor-item_0-container"), "2/[0-9]+")
  expect_equal(container_count("editor", "item_0"), 1)
  expect_false(app$get_js("!!document.getElementById('rubric_module-editor-item_0-hotkey_btn')"))

  # The scoring settings, in the question's popover, save through the
  # question's grade server
  app$click(selector = "#rubric_module-grade_Q3-editor_settings")
  app$wait_for_js("!!document.querySelector('#rubric_module-grade_Q3-editor_total_score_input.shiny-bound-input')", timeout = 10000)
  app$set_inputs(`rubric_module-grade_Q3-editor_total_score_input` = 15)
  app$wait_for_idle()
  expect_equal(markermd:::load_grade_state(root, "Q3")@total_score, 15)

  # Adding an item to a question that is not on screen in the Grade tab
  app$click("rubric_module-editor_add_item_2")
  app$wait_for_idle()
  q3_items = markermd:::load_rubric_items(root, "Q3")
  expect_length(q3_items, 2)
  new_id = setdiff(names(q3_items), "item_1")
  expect_equal(container_count("editor", new_id), 1)

  # Back in the Grade tab its own rows show the new item and total; both
  # views' rows are in the page under their own ids, never the same one twice
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  expect_equal(container_count("grade", "item_0"), 1)
  expect_equal(container_count("editor", "item_0"), 1)
  app$set_inputs(`rubric_module-question_select` = "Q3")
  app$wait_for_idle()
  expect_equal(container_count("grade", new_id), 1)
  expect_match(app$get_text("#rubric_module-grade_Q3-score_display"), "/ 15")
  duplicate_ids = app$get_js(
    "(function() { var seen = {}, dup = 0; document.querySelectorAll('[id]').forEach(function(el) { if (seen[el.id]) dup++; seen[el.id] = true; }); return dup; })()"
  )
  expect_equal(duplicate_ids, 0)
})

test_that("edits in either tab show in the other when it is next visited", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  markermd:::save_rubric_item(root, "Q2", "item_0", markermd::markermd_rubric_item(1L, -2, "Missing quantile"))
  markermd:::save_rubric_item(root, "Q2", "item_1", markermd::markermd_rubric_item(2L, -1, "No labels"))
  markermd:::save_item_group(root, "Q2", "group_0", markermd::markermd_rubric_group(label = "Plots"))

  app = new_app_driver(markermd:::mark_app(root), name = "mark_rubric_both_views")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  grade_text = function(item) app$get_text(paste0("#rubric_module-grade-", item, "-description_text"))
  editor_text = function(item) app$get_text(paste0("#rubric_module-editor-", item, "-description_text"))

  # A description edited in the Grade tab is in the Rubric tab's row
  item_event(app, "item_0", "description", "Missing the quantile")
  app$wait_for_idle()
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_js("!!document.getElementById('rubric_module-editor-item_0-container')", timeout = 10000)
  expect_equal(editor_text("item_0"), "Missing the quantile")

  # A group relabelled in the Rubric tab patches both tabs' headers, and an
  # item moved there is in its new place back in the Grade tab
  group_setting(app, "group_0", "label", "Figures", view = "editor")
  expect_equal(markermd:::load_item_groups(root, "Q2")$group_0@label, "Figures")
  for (view in c("grade", "editor")) {
    expect_equal(
      app$get_js(paste0("document.querySelector('#rubric_module-", view, "-group_0-container .rubric-group-name').textContent")),
      "Figures"
    )
  }
  app$click(selector = "#rubric_module-editor-item_1-move_up_btn")
  app$wait_for_idle()
  expect_equal(names(markermd:::load_rubric_items(root, "Q2")), c("item_1", "item_0"))

  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  rows = app$get_js("Array.from(document.querySelectorAll('#rubric_module-rubric_blocks .rubric-item-row')).map(function(el) { return el.id; })")
  expect_equal(unlist(rows), c("rubric_module-grade-item_1-container", "rubric_module-grade-item_0-container"))
  expect_equal(grade_text("item_0"), "Missing the quantile")

  # A group moved and ungrouped through the delegated events, from the
  # Grade tab
  item_event(app, "item_0", "group", "group_0")
  app$wait_for_idle()
  expect_equal(markermd:::load_rubric_items(root, "Q2")$item_0@group, "group_0")
  app$click(selector = "#rubric_module-grade-group_0-move_up_btn")
  app$wait_for_idle()
  expect_equal(names(markermd:::load_rubric_items(root, "Q2")), c("item_0", "item_1"))
  app$run_js("document.getElementById('rubric_module-grade-group_0-settings_btn').click();")
  app$wait_for_idle()
  app$click(selector = "#rubric_module-grade-group_0-ungroup_btn")
  app$wait_for_idle()
  app$click("rubric_module-confirm_ungroup")
  app$wait_for_idle()
  expect_length(markermd:::load_item_groups(root, "Q2"), 0)
  expect_false(app$get_js("!!document.getElementById('rubric_module-grade-group_0-container')"))
})

test_that("a question's rubric loads when first shown, never reusing a stored id", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  markermd:::save_rubric_item(root, "Q2", "item_0", markermd::markermd_rubric_item(1L, -2, "Missing quantile"))
  markermd:::save_rubric_item(root, "Q3", "item_1", markermd::markermd_rubric_item(1L, -1, "No labels"))

  app = new_app_driver(markermd:::mark_app(root), name = "mark_rubric_lazy")
  expect_false(app$get_js("!!document.getElementById('rubric_module-rubric_editor_ui').querySelector('.rubric-editor')"))
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  expect_true(app$get_js("!!document.getElementById('rubric_module-grade-item_0-container')"))

  # A new item in Q2 must not take Q3's item_1, which is not loaded yet
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  q2 = names(markermd:::load_rubric_items(root, "Q2"))
  expect_length(q2, 2)
  expect_false("item_1" %in% q2)

  app$set_inputs(`rubric_module-question_select` = "Q3")
  app$wait_for_js("!!document.getElementById('rubric_module-grade-item_1-container')", timeout = 10000)
})
