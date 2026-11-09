# The marking app's History tab (mod_mark_history.R): the snapshot listing,
# the take-snapshot modal, the navbar status, and a restore that rebuilds the
# rubric pane from the database.

# A project on the bundled fixtures with a stored template, a rubric for Q2
# and Q3, and marks for student1 (so the rubric import leaves an automatic
# snapshot behind and there is state to restore)
mh_make_fixture = function() {
  src = system.file("examples/test_assignment/repos", package = "markermd")
  root = tempfile("markproj_")
  repos = file.path(root, "repos")
  dir.create(repos, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), repos, recursive = TRUE)

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

  rubric = file.path(root, "rubric.yaml")
  writeLines(c(
    "format_version: '1.0'", "questions:",
    "- name: Q2", "  scoring:", "    total_score: 10", "    grading_mode: negative",
    "  items:", "  - points: 0", "    description: Looks good", "  - points: -3", "    description: Missing quartiles",
    "- name: Q3", "  items:", "  - points: 10", "    description: Full credit"
  ), rubric)
  suppressMessages(markermd::rubric_import(rubric, project = root, summary = "Initial rubric"))
  suppressMessages(markermd::marks_set("student1-excellent", "Q2", items = "Looks good", project = root))
  root
}


# Open a History timeline entry's detail modal, as a click on its row does
open_history_entry = function(app, id) {
  app$run_js(paste0("Shiny.setInputValue('history_module-open_entry', '", id, "', {priority: 'event'});"))
  app$wait_for_idle()
}


test_that("the History tab lists snapshots, takes one from the modal and shows the status", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mh_make_fixture()
  app = new_app_driver(markermd:::mark_app(root), name = "mark_history")

  app$set_inputs(main_navbar = "history")
  app$wait_for_idle()
  # The timeline also lists the activity log; these checks are about snapshots
  app$set_inputs(`history_module-kind_filter` = "snapshots")
  app$wait_for_idle()

  # The rubric import's automatic snapshot is listed, and the state has moved
  # on since (the marks were recorded after it)
  expect_length(app$get_value(export = "history_module-snapshot_ids"), 1)
  expect_true(app$get_value(export = "history_module-state_changed"))
  expect_match(app$get_html("#snapshot_status"), "Last snapshot")
  expect_match(app$get_html("#snapshot_status"), "change")

  # Clicking the only snapshot's row opens its detail modal (the compare-with
  # select then offers just the current state)
  app$run_js("document.querySelector('#history_module-snapshot_table .rt-tbody .rt-tr .rt-td').click();")
  app$wait_for_idle()
  expect_equal(app$get_value(export = "history_module-selected_snapshot"), app$get_value(export = "history_module-snapshot_ids"))
  expect_match(app$get_html("#history_module-detail_ui"), "Initial rubric")
  expect_match(app$get_html("#history_module-detail_ui"), "Current state")

  # Take a named snapshot through the modal
  app$click("history_module-take_snapshot")
  app$wait_for_idle()
  app$set_inputs(
    `history_module-snapshot_label` = "Checkpoint",
    `history_module-snapshot_summary` = "Before the review",
    `history_module-snapshot_metadata` = "ticket: 7"
  )
  app$click("history_module-confirm_snapshot")
  app$wait_for_idle()

  rows = markermd::snapshot_list(root)
  expect_equal(nrow(rows), 2)
  expect_equal(rows$label[1], "Checkpoint")
  expect_equal(rows$summary[1], "Before the review")
  expect_equal(rows$kind[1], "manual")
  info = markermd::snapshot_info(rows$snapshot_id[1], project = root)
  expect_equal(info@notes$key, "ticket")

  # The new snapshot is selected, and its detail modal shows it
  expect_equal(app$get_value(export = "history_module-selected_snapshot"), rows$snapshot_id[1])
  expect_false(app$get_value(export = "history_module-state_changed"))
  open_history_entry(app, rows$snapshot_id[1])
  detail = app$get_html("#history_module-detail_ui")
  expect_match(detail, "Before the review")
  expect_match(detail, "ticket")
  expect_match(app$get_html("#snapshot_status"), "0 changes")

  # A note added from the pane lands in the log
  app$set_inputs(`history_module-new_note` = "Reviewed with the TA")
  app$click("history_module-add_note")
  app$wait_for_idle()
  info = markermd::snapshot_info(rows$snapshot_id[1], project = root)
  expect_true("Reviewed with the TA" %in% info@notes$value)

  # The kind filter narrows the table to automatic snapshots
  app$set_inputs(`history_module-kind_filter` = "auto")
  app$wait_for_idle()
  expect_no_match(app$get_html("#history_module-snapshot_table"), "Checkpoint")
})


test_that("restoring a snapshot from the History tab rebuilds the rubric pane", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = mh_make_fixture()
  id = suppressMessages(markermd::snapshot_create(root, label = "Before"))
  # Changes made after the snapshot: a reworded item and a different mark
  markermd:::save_rubric_item(root, "Q2", "item_1", markermd::markermd_rubric_item(2L, -5, "Quartiles are missing"))
  suppressMessages(markermd::marks_set(
    "student1-excellent", "Q2", items = "Quartiles are missing", project = root, overwrite = TRUE
  ))

  app = new_app_driver(markermd:::mark_app(root), name = "mark_history_restore")
  expect_equal(
    app$get_value(export = "rubric_module-rubric_item_descriptions"),
    c("Looks good", "Quartiles are missing")
  )

  app$set_inputs(main_navbar = "history")
  app$wait_for_idle()
  # The timeline also lists the activity log; these checks are about snapshots
  app$set_inputs(`history_module-kind_filter` = "snapshots")
  app$wait_for_idle()
  # Open the snapshot's detail, then restore it with a summary
  open_history_entry(app, id)
  expect_equal(app$get_value(export = "history_module-selected_snapshot"), id)

  app$click("history_module-restore")
  app$wait_for_idle()
  app$set_inputs(`history_module-restore_summary` = "Undo the review")
  app$wait_for_idle()
  app$click("history_module-confirm_restore")
  app$wait_for_idle()

  rows = markermd::snapshot_list(root)
  expect_equal(rows$trigger[1], glue::glue("snapshot_restore('{id}')"))
  expect_equal(rows$summary[1], "Undo the review")

  # The database and the rubric pane both show the snapshot's rubric, and the
  # restored selection
  items = markermd:::load_rubric_items(root, "Q2")
  expect_equal(unname(vapply(items, function(item) item@description, character(1))), c("Looks good", "Missing quartiles"))
  expect_equal(
    app$get_value(export = "rubric_module-rubric_item_descriptions"),
    c("Looks good", "Missing quartiles")
  )
  sel = markermd:::load_grade_selections(root, "Q2", "student1-excellent")
  expect_true(sel[[names(items)[1]]])
  expect_false(sel[[names(items)[2]]])

  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  expect_match(app$get_html("#rubric_module-rubric_items_container"), "Missing quartiles")
})
