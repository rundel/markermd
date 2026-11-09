# shinytest2 coverage of rubric item groups in mark(): grouped rendering,
# the live subtotals and cap badge, exclusive (radio) groups, and authoring
# (Add Group, the item group menu, block moves, ungrouping), on the shared
# fixture in helper-mark-fixture.R.

mgr_make_fixture = function() {
  root = make_mark_fixture(report = FALSE)$project

  # Q2 out of 12 in negative mode: two ungrouped items (item_0, item_1), a
  # "Sample game" group floored at -2 (item_2..item_4) and an exclusive
  # "Write-up" tier (item_5, item_6); the groups get ids group_0 and group_1
  rubric = file.path(root, "rubric.yaml")
  writeLines(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q2",
    "  scoring:",
    "    total_score: 12",
    "    grading_mode: negative",
    "  items:",
    "  - points: 0",
    "    description: Looks good",
    "  - points: -1.5",
    "    description: Wrong comparison",
    "  - group: Sample game",
    "    floor: -2",
    "    items:",
    "    - points: -1",
    "      description: Sample A",
    "    - points: -1",
    "      description: Sample B",
    "    - points: -0.5",
    "      description: Sample C",
    "  - group: Write-up",
    "    exclusive: true",
    "    items:",
    "    - points: -1.5",
    "      description: Write-up absent",
    "    - points: -1",
    "      description: Write-up partial"
  ), rubric)
  suppressMessages(markermd::rubric_import(rubric, project = root))
  root
}

mgr_score = function(app) {
  trimws(app$get_js("document.getElementById('rubric_module-grade_Q2-score_display').innerHTML"))
}

mgr_selected = function(app, item) {
  app$get_js(sprintf(
    "document.getElementById('rubric_module-grade-%s-hotkey_btn').classList.contains('btn-success')", item
  ))
}

# Toggle an item row's group menu (open, or closed again)
open_group_menu = function(app, item) {
  app$run_js(paste0("document.querySelector('#rubric_module-grade-", item, "-container .rubric-group-menu-btn').click();"))
}

mgr_click = function(app, item) {
  app$click(selector = sprintf("#rubric_module-grade-%s-hotkey_btn", item))
  app$wait_for_idle()
}

mgr_export = function(app, name) {
  app$get_values(export = TRUE)$export[[paste0("rubric_module-", name)]]
}


test_that("groups render as sections, cap their subtotal live and flag the score", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mgr_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_groups_render")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  # Each group wraps its member rows under a header with its label and chip;
  # hotkeys follow the flat order across the group boundaries
  header = app$get_html("#rubric_module-grade-group_0-container .rubric-group-head")
  expect_match(header, "Sample game", fixed = TRUE)
  expect_match(header, "floor -2", fixed = TRUE)
  expect_match(app$get_html("#rubric_module-grade-group_1-container .rubric-group-head"), "pick one", fixed = TRUE)
  expect_true(app$get_js("document.querySelector('#rubric_module-grade-group_0-container #rubric_module-grade-item_3-container') !== null"))
  expect_equal(
    trimws(app$get_js("document.getElementById('rubric_module-grade-item_4-hotkey_btn').textContent")),
    "5"
  )
  expect_equal(mgr_score(app), "12 / 12 pts")
  badge_hidden = function() {
    app$get_js("document.getElementById('rubric_module-grade_Q2-cap_badge').classList.contains('d-none')")
  }
  expect_true(badge_hidden())

  # Two sample-game items stay within the floor: nothing is flagged
  mgr_click(app, "item_2")
  mgr_click(app, "item_3")
  expect_equal(mgr_score(app), "10 / 12 pts")
  subtotal = function() app$get_html("#rubric_module-grade-group_0-container .rubric-group-subtotal")
  expect_match(subtotal(), "-2 pts", fixed = TRUE)
  expect_no_match(subtotal(), "raw")
  expect_false(app$get_js("document.getElementById('rubric_module-grade-group_0-container').classList.contains('rubric-group-capped')"))
  expect_true(badge_hidden())

  # The third takes the raw sum past the floor: the subtotal shows the sum
  # struck through, the group turns amber, the score holds and the badge
  # shows what was capped
  mgr_click(app, "item_4")
  expect_equal(mgr_score(app), "10 / 12 pts")
  expect_match(subtotal(), 'class="raw">-2.5 pts', fixed = TRUE)
  expect_match(subtotal(), 'class="eff">-2 pts', fixed = TRUE)
  expect_true(app$get_js("document.getElementById('rubric_module-grade-group_0-container').classList.contains('rubric-group-capped')"))
  expect_false(badge_hidden())
  expect_equal(
    trimws(app$get_js("document.getElementById('rubric_module-grade_Q2-cap_badge_text').textContent")),
    "0.5 pts capped"
  )

  # The badge opens the breakdown
  app$run_js("document.getElementById('rubric_module-grade_Q2-cap_badge').click();")
  app$wait_for_idle()
  breakdown = app$get_html("#rubric_module-score_breakdown")
  expect_match(breakdown, "Start (negative mode)", fixed = TRUE)
  expect_match(breakdown, "-2.5, floor -2", fixed = TRUE)
  expect_match(breakdown, "Without group bounds: 9.5", fixed = TRUE)

  # The recorded selections score the same way headlessly, and the
  # Assignments table shows the score without a bounds marker
  scores = markermd:::collect_score_data(root, "Q2", "student1-excellent")
  expect_equal(scores$score, 10)
  app$set_inputs(main_navbar = "assignments")
  app$wait_for_idle()
  expect_no_match(app$get_html("#repo_table"), "Group bounds applied", fixed = TRUE)
})


test_that("a capped selection loaded from the database survives the question switch", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mgr_make_fixture()
  for (item in c("item_2", "item_3", "item_4")) {
    markermd:::save_grade_selection(root, "Q2", "student1-excellent", item, TRUE)
  }
  app = new_app_driver(markermd:::mark_app(root), name = "mark_groups_loaded")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  app$set_inputs(`rubric_module-question_select` = "Q3")
  app$wait_for_idle()
  app$set_inputs(`rubric_module-question_select` = "Q2")
  app$wait_for_idle()

  expect_equal(mgr_score(app), "10 / 12 pts")
  expect_match(app$get_html("#rubric_module-grade-group_0-container .rubric-group-subtotal"), 'class="raw">-2.5 pts', fixed = TRUE)
  expect_false(app$get_js("document.getElementById('rubric_module-grade_Q2-cap_badge').classList.contains('d-none')"))
})


test_that("an exclusive group behaves like radio buttons and logs both grade events", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mgr_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_groups_exclusive")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  # Round buttons announced as radios
  expect_true(app$get_js("document.getElementById('rubric_module-grade-item_5-hotkey_btn').classList.contains('rubric-hk-radio')"))
  expect_equal(app$get_js("document.getElementById('rubric_module-grade-item_5-hotkey_btn').getAttribute('role')"), "radio")
  expect_false(app$get_js("document.getElementById('rubric_module-grade-item_2-hotkey_btn').classList.contains('rubric-hk-radio')"))

  mgr_click(app, "item_5")
  expect_true(mgr_selected(app, "item_5"))
  expect_equal(mgr_score(app), "10.5 / 12 pts")

  # Selecting the sibling deselects the first, in the UI and in the database
  mgr_click(app, "item_6")
  expect_true(mgr_selected(app, "item_6"))
  expect_false(mgr_selected(app, "item_5"))
  expect_equal(mgr_score(app), "11 / 12 pts")
  sel = markermd:::load_grade_selections(root, "Q2", "student1-excellent")
  expect_false(sel$item_5)
  expect_true(sel$item_6)

  # A capped group is not exclusive: its items stack
  mgr_click(app, "item_2")
  mgr_click(app, "item_3")
  expect_true(mgr_selected(app, "item_2"))
  expect_true(mgr_selected(app, "item_3"))

  # Loading another repository's stored state never triggers the rule
  markermd:::save_grade_selection(root, "Q2", "student2-average", "item_5", TRUE)
  app$set_inputs(`content_module-content_repo_select` = "student2-average")
  app$wait_for_idle()
  expect_true(mgr_selected(app, "item_5"))
  expect_false(mgr_selected(app, "item_6"))
  expect_equal(mgr_score(app), "10.5 / 12 pts")
})


test_that("the exclusive switch is refused while a repository has two of the group's items selected", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mgr_make_fixture()
  markermd:::save_grade_selection(root, "Q2", "student3-poor", "item_2", TRUE)
  markermd:::save_grade_selection(root, "Q2", "student3-poor", "item_3", TRUE)

  app = new_app_driver(markermd:::mark_app(root), name = "mark_groups_exclusive_refused")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  # The switch lives in the settings popover; a refusal puts it back off
  app$run_js("document.getElementById('rubric_module-grade-group_0-settings_btn').click();")
  app$wait_for_idle()
  group_setting(app, "group_0", "exclusive", TRUE)

  expect_match(app$get_html(".modal-content"), "Cannot make \"Sample game\" exclusive", fixed = TRUE)
  expect_match(app$get_html(".modal-content"), "student3-poor", fixed = TRUE)
  expect_false(markermd:::load_item_groups(root, "Q2")$group_0@exclusive)
  expect_false(app$get_js("document.querySelector('[data-settings-for=\"rubric_module-grade-group_0-container\"] [data-field=exclusive]').checked"))

  # A bound edit in the same popover saves and patches the chip in place
  app$run_js("document.querySelector('.modal .btn-secondary, .modal button[data-bs-dismiss]').click();")
  app$wait_for_idle()
  group_setting(app, "group_0", "floor", -1)
  expect_equal(markermd:::load_item_groups(root, "Q2")$group_0@floor, -1)
  expect_match(app$get_html("#rubric_module-grade-group_0-container .rubric-group-chips"), "floor -1", fixed = TRUE)
})


test_that("groups can be added, joined through the item menu, moved as blocks and ungrouped", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mgr_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_groups_authoring")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  ids = function() mgr_export(app, "rubric_item_ids")
  groups = function() unlist(mgr_export(app, "rubric_item_groups"))
  expect_equal(ids(), paste0("item_", 0:6))

  # Add Group creates an empty, uniquely named group after the last item and
  # opens its settings popover
  app$click("rubric_module-add_group")
  app$wait_for_idle()
  app$wait_for_js("document.querySelector('.popover .rubric-group-settings') !== null")
  expect_equal(unname(unlist(mgr_export(app, "rubric_group_labels"))), c("Sample game", "Write-up", "New group"))
  expect_true(app$get_js("document.querySelector('#rubric_module-rubric_empty_groups #rubric_module-grade-group_2-container') !== null"))
  expect_equal(markermd:::load_item_groups(root, "Q2")$group_2@label, "New group")
  expect_true(app$get_js("document.querySelector('.popover .rubric-group-settings') !== null"))

  # Renaming it in the popover patches the header and every item's menu in
  # place, and a positive floor is refused inline without being saved
  group_setting(app, "group_2", "label", "Style")
  expect_equal(markermd:::load_item_groups(root, "Q2")$group_2@label, "Style")
  expect_equal(
    app$get_js("document.querySelector('#rubric_module-grade-group_2-container .rubric-group-name').textContent"),
    "Style"
  )
  open_group_menu(app, "item_0")
  expect_equal(
    app$get_js("document.querySelector('#rubric_module-grade-item_0-container a[data-group=group_2] .rubric-group-menu-label').textContent"),
    "Style"
  )
  open_group_menu(app, "item_0")
  group_setting(app, "group_2", "floor", 2)
  expect_match(app$get_html("#rubric_module-grade-group_2-settings_feedback"), "must be 0 or negative")
  expect_true(is.na(markermd:::load_item_groups(root, "Q2")$group_2@floor))

  # A label another group uses is refused and the field put back
  group_setting(app, "group_2", "label", "Write-up")
  expect_match(app$get_html("#rubric_module-grade-group_2-settings_feedback"), "already named")
  expect_equal(markermd:::load_item_groups(root, "Q2")$group_2@label, "Style")
  expect_equal(
    app$get_js("document.querySelector('[data-settings-for=\"rubric_module-grade-group_2-container\"] [data-field=label]').value"),
    "Style"
  )
  app$run_js("document.getElementById('rubric_module-grade-group_2-settings_btn').click();")
  app$wait_for_idle()

  # Every item row carries the group menu, listing the question's groups
  # (filled in when it opens)
  open_group_menu(app, "item_3")
  menu = app$get_html("#rubric_module-grade-item_3-container .rubric-group-menu")
  open_group_menu(app, "item_3")
  expect_match(menu, "No group", fixed = TRUE)
  expect_match(menu, "Style", fixed = TRUE)

  # The item menu moves an ungrouped item to the end of a group ...
  item_event(app, "item_1", "group", "group_0")
  app$wait_for_idle()
  expect_equal(ids(), c("item_0", "item_2", "item_3", "item_4", "item_1", "item_5", "item_6"))
  expect_equal(unname(groups()["item_1"]), "group_0")
  items = markermd:::load_rubric_items(root, "Q2")
  expect_equal(names(items), ids())
  expect_equal(items$item_1@group, "group_0")
  expect_equal(unname(purrr::map_int(items, ~ .x@hotkey)), 1:7)

  # ... and a grouped item out of it, right after the group
  item_event(app, "item_2", "group", "")
  app$wait_for_idle()
  expect_equal(ids(), c("item_0", "item_3", "item_4", "item_1", "item_2", "item_5", "item_6"))
  expect_true(is.na(groups()["item_2"]))

  # An item's chevrons move it within its group, wrapping within it
  app$click(selector = "#rubric_module-grade-item_3-move_up_btn")
  app$wait_for_idle()
  expect_equal(ids(), c("item_0", "item_4", "item_1", "item_3", "item_2", "item_5", "item_6"))

  # An ungrouped item jumps over a whole group
  app$click(selector = "#rubric_module-grade-item_0-move_down_btn")
  app$wait_for_idle()
  expect_equal(ids(), c("item_4", "item_1", "item_3", "item_0", "item_2", "item_5", "item_6"))

  # The group header's chevrons move the whole block
  app$click(selector = "#rubric_module-grade-group_1-move_up_btn")
  app$wait_for_idle()
  expect_equal(ids(), c("item_4", "item_1", "item_3", "item_0", "item_5", "item_6", "item_2"))
  expect_equal(names(markermd:::load_rubric_items(root, "Q2")), ids())

  # Ungrouping (confirmed) leaves the members in place as ungrouped items
  app$run_js("document.getElementById('rubric_module-grade-group_0-settings_btn').click();")
  app$wait_for_idle()
  app$click(selector = "#rubric_module-grade-group_0-ungroup_btn")
  app$wait_for_idle()
  app$click("rubric_module-confirm_ungroup")
  app$wait_for_idle()
  expect_equal(ids(), c("item_4", "item_1", "item_3", "item_0", "item_5", "item_6", "item_2"))
  expect_true(all(is.na(groups()[c("item_4", "item_1", "item_3")])))
  expect_equal(names(markermd:::load_item_groups(root, "Q2")), c("group_1", "group_2"))
  expect_true(all(is.na(purrr::map_chr(markermd:::load_rubric_items(root, "Q2")[1:4], ~ .x@group))))

  # The rubric export carries the surviving group and its members
  app$run_js("document.getElementById('rubric_module-rubric_io_menu').click();")
  app$wait_for_idle()
  exported = markermd::read_rubric_yaml(app$get_download("rubric_module-export_question"))
  expect_equal(exported$format_version, "1.1")
  q2 = exported$questions[[1]]
  expect_equal(length(q2$groups), 1)
  expect_equal(q2$groups[[1]]@label, "Write-up")
})


test_that("a rubric import with groups appends into existing groups in the running app", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mgr_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_groups_import")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  more = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.1'",
    "questions:",
    "- name: Q2",
    "  items:",
    "  - group: Sample game",
    "    floor: -3",
    "    items:",
    "    - points: -1",
    "      description: Sample D",
    "  - group: Style",
    "    floor: -1",
    "    items:",
    "    - points: -0.5",
    "      description: Dead code"
  ), more)
  app$upload_file(`rubric_module-rubric_import_file` = more)
  app$wait_for_idle()
  app$click("rubric_module-confirm_rubric_import")
  app$wait_for_idle()

  expect_equal(
    mgr_export(app, "rubric_item_descriptions"),
    c("Looks good", "Wrong comparison", "Sample A", "Sample B", "Sample C", "Sample D",
      "Write-up absent", "Write-up partial", "Dead code")
  )
  groups = unlist(mgr_export(app, "rubric_item_groups"))
  expect_equal(unname(groups[c("item_7", "item_8")]), c("group_0", "group_2"))
  expect_equal(unname(unlist(mgr_export(app, "rubric_group_labels"))), c("Sample game", "Write-up", "Style"))
  expect_match(app$get_html("#rubric_module-grade-group_0-container .rubric-group-chips"), "floor -3", fixed = TRUE)
  expect_true(app$get_js("document.querySelector('#rubric_module-grade-group_2-container #rubric_module-grade-item_8-container') !== null"))
})
