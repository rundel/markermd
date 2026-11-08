test_that("changes made outside the app show up live, badged and in Activity", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  fixture = make_mark_fixture()
  root = fixture$project
  markermd:::save_rubric_item(root, "Q2", "item_0", markermd::markermd_rubric_item(1L, -2, "Missing quantile"))
  markermd:::save_rubric_item(root, "Q2", "item_1", markermd::markermd_rubric_item(2L, 0, "Looks good"))

  app = new_app_driver(markermd:::mark_app(root), name = "mark_activity")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  repo = app$get_value(input = "content_module-content_repo_select")
  withr::with_envvar(c(MARKERMD_ACTOR = "claude"), {
    suppressMessages(markermd::marks_set(
      repo, "Q2", items = "Looks good", private_comment = "All quartiles present.",
      project = root, summary = "first pass"
    ))
  })

  # The watcher notices the commit, the pair on screen is re-read and the
  # agent's selection is badged
  app$wait_for_js("!!document.querySelector('#rubric_module-item_1-agent_badge .agent-badge')", timeout = 10000)
  expect_true(app$get_js("document.getElementById('rubric_module-item_1-hotkey_btn').classList.contains('btn-success')"))
  expect_true(app$get_js("!!document.getElementById('shiny-notification-external_change_notice')"))
  expect_equal(
    app$get_js("document.getElementById('rubric_module-question_private_comment').value"),
    "All quartiles present."
  )

  # Any change to the pair in the app clears the badge
  app$click("rubric_module-item_0-hotkey_btn")
  app$wait_for_js("!document.querySelector('#rubric_module-item_1-agent_badge .agent-badge')", timeout = 10000)

  # The Activity tab lists the call and the app's own grading
  app$set_inputs(main_navbar = "activity")
  app$wait_for_js("document.getElementById('activity_module-activity_table').innerText.includes('marks_set')", timeout = 10000)
  table_text = app$get_js("document.getElementById('activity_module-activity_table').innerText")
  expect_match(table_text, "first pass")
  expect_match(table_text, "Grading in the app")

  # A rubric change cannot be swapped in place, so it raises a banner
  rubric_path = tempfile(fileext = ".yaml")
  markermd::rubric_export(rubric_path, project = root)
  rubric = yaml::read_yaml(rubric_path)
  q2 = which(vapply(rubric$questions, function(q) q$name, character(1)) == "Q2")
  rubric$questions[[q2]]$items = c(rubric$questions[[q2]]$items, list(list(points = -1, description = "Late")))
  yaml::write_yaml(rubric, rubric_path)
  withr::with_envvar(c(MARKERMD_ACTOR = "codex"), {
    suppressMessages(markermd::rubric_import(rubric_path, project = root))
  })
  app$wait_for_js("document.getElementById('external_change_banner').innerText.includes('rubric_import by codex')", timeout = 10000)
})
