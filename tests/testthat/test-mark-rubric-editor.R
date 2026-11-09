test_that("the Rubric tab edits every question's rubric in one place", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  markermd:::save_rubric_item(root, "Q2", "item_0", markermd::markermd_rubric_item(1L, -2, "Missing quantile"))
  markermd:::save_rubric_item(root, "Q3", "item_1", markermd::markermd_rubric_item(1L, -1, "No labels"))
  markermd:::save_grade_selection(root, "Q2", "student2-average", "item_0", TRUE)
  markermd:::save_grade_selection(root, "Q2", "student3-poor", "item_0", TRUE)

  app = new_app_driver(markermd:::mark_app(root), name = "mark_rubric_editor")
  container_count = function(item_id) {
    app$get_js(paste0("document.querySelectorAll('[id=\"rubric_module-", item_id, "-container\"]').length"))
  }

  # Every question shows, each item with its selection count in place of
  # the toggle
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_js("!!document.querySelector('#rubric_module-rubric_editor_ui .rubric-editor')", timeout = 10000)
  editor = app$get_text("#rubric_module-rubric_editor_ui")
  expect_match(editor, "Q2")
  expect_match(editor, "Q3")
  expect_match(app$get_text("#rubric_module-item_0-container"), "2/[0-9]+")
  expect_equal(container_count("item_0"), 1)
  expect_false(app$get_js("!!document.getElementById('rubric_module-item_0-hotkey_btn')"))

  # The scoring settings save through the question's grade server
  app$set_inputs(`rubric_module-grade_Q3-total_score_input` = 15)
  app$wait_for_idle()
  expect_equal(markermd:::load_grade_state(root, "Q3")@total_score, 15)

  # Adding an item to a question that is not on screen in the Grade tab
  app$click("rubric_module-editor_add_item_2")
  app$wait_for_idle()
  q3_items = markermd:::load_rubric_items(root, "Q3")
  expect_length(q3_items, 2)
  new_id = setdiff(names(q3_items), "item_1")
  expect_equal(container_count(new_id), 1)

  # Back in the Grade tab the items are drawn there only, and the new item
  # and total are in place
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  expect_equal(container_count("item_0"), 1)
  expect_equal(app$get_text("#rubric_module-rubric_editor_ui"), "")
  app$set_inputs(`rubric_module-question_select` = "Q3")
  app$wait_for_idle()
  expect_equal(container_count(new_id), 1)
  expect_match(app$get_text("#rubric_module-grade_Q3-score_display"), "/ 15")
})

test_that("a question's rubric loads when first shown, keeping its ids reserved", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  markermd:::save_rubric_item(root, "Q2", "item_0", markermd::markermd_rubric_item(1L, -2, "Missing quantile"))
  markermd:::save_rubric_item(root, "Q3", "item_1", markermd::markermd_rubric_item(1L, -1, "No labels"))

  app = new_app_driver(markermd:::mark_app(root), name = "mark_rubric_lazy")
  expect_false(app$get_js("!!document.getElementById('rubric_module-rubric_editor_ui').querySelector('.rubric-editor')"))
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  expect_true(app$get_js("!!document.getElementById('rubric_module-item_0-container')"))

  # A new item in Q2 must not take Q3's item_1, which is not loaded yet
  app$click("rubric_module-add_item")
  app$wait_for_idle()
  q2 = names(markermd:::load_rubric_items(root, "Q2"))
  expect_length(q2, 2)
  expect_false("item_1" %in% q2)

  app$set_inputs(`rubric_module-question_select` = "Q3")
  app$wait_for_js("!!document.getElementById('rubric_module-item_1-container')", timeout = 10000)
})
