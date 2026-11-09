# shinytest2 coverage of mark() features the earlier suites never drove: the
# report sandbox, the validation modal's Re-check, the Rubric tab's YAML
# controls, the hotkey modifier guard, the Source view's line-mapped
# highlight and the real DOM path of a rubric description edit.

press_key = function(app, key, modifiers = "") {
  app$run_js(paste0(
    "document.dispatchEvent(new KeyboardEvent('keydown', {key: '", key, "'",
    if (nzchar(modifiers)) paste0(", ", modifiers) else "", "}));"
  ))
  app$wait_for_idle()
}

item_selected = function(app, item) {
  app$get_js(paste0("document.getElementById('rubric_module-grade-", item, "-hotkey_btn').classList.contains('btn-success')"))
}


test_that("a student report runs sandboxed and cannot reach the app", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  # A hostile report: it tries to drive the app through its parent and, when
  # that throws, reports what happened by postMessage (the only channel a
  # sandboxed, cross-origin frame has)
  writeLines(c(
    "<html><body><p>report</p><script>",
    "var outcome = 'reached';",
    "try {",
    "  window.parent.document.body.setAttribute('data-pwned', '1');",
    "  window.parent.Shiny.setInputValue('pwned', 1);",
    "} catch (e) { outcome = 'blocked:' + e.name; }",
    "window.parent.postMessage({markermd_sandbox_probe: outcome}, '*');",
    "</script></body></html>"
  ), file.path(root, "html", "student1-excellent.html"))

  app = new_app_driver(markermd:::mark_app(root), name = "mark_report_sandbox")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  app$wait_for_js("!!document.getElementById('artifact-content-frame')")

  sandbox = app$get_js("document.getElementById('artifact-content-frame').getAttribute('sandbox')")
  expect_match(sandbox, "allow-scripts", fixed = TRUE)
  expect_false(grepl("allow-same-origin", sandbox, fixed = TRUE))

  # Listen for the probe, then reload the frame so its script runs again
  app$run_js("
    window.__probe = null;
    window.addEventListener('message', function(e) {
      if (e.data && e.data.markermd_sandbox_probe) window.__probe = e.data.markermd_sandbox_probe;
    });
    var f = document.getElementById('artifact-content-frame');
    f.src = f.src;
  ")
  app$wait_for_js("window.__probe !== null")
  expect_match(app$get_js("window.__probe"), "^blocked:")
  expect_false(app$get_js("document.body.hasAttribute('data-pwned')"))
  app$wait_for_idle()
  expect_null(app$get_value(input = "pwned"))
})


test_that("Re-check re-validates a submission repaired while the modal is open", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  app = new_app_driver(markermd:::mark_app(root), name = "mark_recheck")
  app$wait_for_idle()

  option_labels = function() {
    app$get_js("
      var sel = document.getElementById('explore_module-question_select').selectize;
      Object.keys(sel.options).map(function(k) { return sel.options[k].label; }).sort().join('|');
    ")
  }

  # student4 fails Q3: its plot is commented out
  app$run_js("Shiny.setInputValue('validation_clicked', 4, {priority: 'event'})")
  app$wait_for_js("!!document.querySelector('#shiny-modal.show #explore_module-template_validation_ui .card')")
  app$wait_for_idle()
  expect_equal(option_labels(), "✓ Q2|✗ Q3")

  # Repair the document on disk, as an agent in the Terminal tab would
  qmd = file.path(root, "repos", "student4-incomplete", "assignment.qmd")
  lines = readLines(qmd)
  lines = sub("^# plot\\(penguins", "ggplot(penguins", lines)
  writeLines(lines, qmd)

  app$click("revalidate_repo")
  app$wait_for_idle()
  app$wait_for_js("!!document.querySelector('#shiny-modal.show #explore_module-template_validation_ui .card')")
  app$wait_for_idle()
  expect_equal(option_labels(), "✓ Q2|✓ Q3")
  expect_match(app$get_js("document.querySelector('#shiny-modal .modal-title').innerText"), "student4-incomplete")
})


test_that("the Rubric tab exports and imports the rubric as YAML", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  markermd:::save_rubric_item(root, "Q2", "item_0", markermd::markermd_rubric_item(1L, -2, "Seeded"))

  app = new_app_driver(markermd:::mark_app(root), name = "mark_rubric_tab_io")
  app$set_inputs(main_navbar = "rubric")
  app$wait_for_idle()

  # The popover holds the download button, so open it first
  app$run_js("document.getElementById('rubric_module-editor_io_menu').click();")
  # The download link only gets its href once Shiny binds it inside the popover
  app$wait_for_js("(document.getElementById('rubric_module-editor_export_all') || {getAttribute: function() { return ''; }}).getAttribute('href')")
  app$wait_for_idle()
  exported = markermd::read_rubric_yaml(app$get_download("rubric_module-editor_export_all"))
  expect_equal(vapply(exported$questions, function(q) q$name, character(1)), c("Q2", "Q3"))
  expect_equal(vapply(exported$questions[[1]]$items, function(i) i@description, character(1)), "Seeded")

  rubric_yaml = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'",
    "questions:",
    "- name: Q3",
    "  items:",
    "  - points: 5",
    "    description: Plot labelled"
  ), rubric_yaml)
  app$upload_file(`rubric_module-editor_import_file` = rubric_yaml)
  app$wait_for_js("!!document.getElementById('rubric_module-confirm_rubric_import')")
  app$click("rubric_module-confirm_rubric_import")
  app$wait_for_idle()

  q3 = markermd:::load_rubric_items(root, "Q3")
  expect_equal(unname(vapply(q3, function(i) i@description, character(1))), "Plot labelled")
  expect_match(app$get_html("#rubric_module-rubric_editor_ui"), "Plot labelled", fixed = TRUE)
  expect_equal(unname(vapply(markermd:::load_rubric_items(root, "Q2"), function(i) i@description, character(1))), "Seeded")
})


test_that("item hotkeys ignore digits pressed with a modifier", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  markermd:::save_rubric_item(root, "Q2", "item_0", markermd::markermd_rubric_item(1L, -2, "Seeded"))
  app = new_app_driver(markermd:::mark_app(root), name = "mark_hotkey_modifiers")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  # Browser and OS shortcuts (Cmd+1 switches tabs, Ctrl+1 likewise) must not
  # grade the student
  press_key(app, "1", "metaKey: true")
  press_key(app, "1", "ctrlKey: true")
  press_key(app, "1", "altKey: true")
  expect_false(item_selected(app, "item_0"))
  expect_equal(nrow(db_rows_after(root, "grades")), 0)

  press_key(app, "1")
  expect_true(item_selected(app, "item_0"))
})


test_that("the Source view highlights the question's lines as written in the file", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  # A knitr-style chunk header above Question 2 expands to three lines before
  # parsing; the highlight must still land on the file's own lines
  qmd = file.path(root, "repos", "student2-average", "assignment.qmd")
  lines = readLines(qmd)
  first_chunk = which(lines == "```{r}")[1]
  lines[first_chunk] = "```{r setup, echo=FALSE}"
  writeLines(lines, qmd)
  q2_line = which(startsWith(lines, "## Question 2"))
  q3_line = which(startsWith(lines, "## Question 3"))
  expect_lt(first_chunk, q2_line)

  app = new_app_driver(markermd:::mark_app(root), name = "mark_source_highlight")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()
  app$set_inputs(`content_module-content_repo_select` = "student2-average")
  app$wait_for_idle()
  app$set_inputs(`content_module-html_toggle` = FALSE)
  app$wait_for_idle()

  # The editor's configuration script carries the decorations it applies
  html = app$get_html("#content_module-content_display")
  starts = as.integer(regmatches(html, gregexpr('(?<="startLineNumber":)[0-9]+', html, perl = TRUE))[[1]])
  ends = as.integer(regmatches(html, gregexpr('(?<="endLineNumber":)[0-9]+', html, perl = TRUE))[[1]])
  expect_gt(length(starts), 0)
  expect_equal(min(starts), q2_line)
  expect_lt(max(ends), q3_line)
  expect_gte(max(ends), q3_line - 5)
})


test_that("typing into an item's description saves it through the delegated handler", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  root = make_mark_fixture()$project
  app = new_app_driver(markermd:::mark_app(root), name = "mark_description_dom")
  app$set_inputs(main_navbar = "grade")
  app$wait_for_idle()

  app$click("rubric_module-add_item")
  app$wait_for_idle()
  app$wait_for_js("!!document.getElementById('rubric_module-grade-item_0-description_text')")

  # Focus, type and blur as a user does: the page head's script reports the
  # text on focusout as one item_event, no test-side payload involved
  app$run_js("
    var el = document.getElementById('rubric_module-grade-item_0-description_text');
    el.focus();
    el.innerText = 'Missing axis labels';
    el.dispatchEvent(new InputEvent('input', {bubbles: true}));
    el.blur();
  ")
  app$wait_for_idle()
  wait_until(function() {
    items = markermd:::load_rubric_items(root, "Q2")
    length(items) == 1 && identical(items[[1]]@description, "Missing axis labels")
  })
  expect_equal(markermd:::load_rubric_items(root, "Q2")[[1]]@description, "Missing axis labels")

  # A blank edit is refused and the row put back
  app$run_js("
    var el = document.getElementById('rubric_module-grade-item_0-description_text');
    el.focus();
    el.innerText = '   ';
    el.dispatchEvent(new InputEvent('input', {bubbles: true}));
    el.blur();
  ")
  app$wait_for_idle()
  app$wait_for_js("document.getElementById('rubric_module-grade-item_0-description_text').innerText.trim() === 'Missing axis labels'")
  expect_equal(markermd:::load_rubric_items(root, "Q2")[[1]]@description, "Missing axis labels")
})
