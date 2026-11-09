# Build a temporary markermd project (repos/ populated from the bundled
# fixtures, a rendered report under html/, and a configured template that
# grades Question 2 on "quantile" content and Question 3 on "ggplot").
make_mark_fixture = function() {
  src = system.file("examples/test_assignment/repos", package = "markermd")
  root = tempfile("markproj_")
  repos = file.path(root, "repos")
  dir.create(repos, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), repos, recursive = TRUE)

  html_dir = file.path(root, "html")
  dir.create(html_dir)
  writeLines(
    "<html><body>student1 report</body></html>",
    file.path(html_dir, "student1-excellent.html")
  )

  qmd = file.path(repos, "student1-excellent", "assignment.qmd")
  ast = markermd:::parse_assignment_document(qmd)

  template = markermd::markermd_template(
    original_ast = ast,
    questions = list(
      markermd::markermd_question(
        1L, "Q2", markermd::markermd_node_selection(node_ids = "question-2-basic-programming"),
        list(markermd::markermd_rule(node_type = "Any node", verb = "has content", values = "*quantile*"))
      ),
      markermd::markermd_question(
        2L, "Q3", markermd::markermd_node_selection(node_ids = "question-3-data-visualization"),
        list(markermd::markermd_rule(node_type = "Any node", verb = "has content", values = "*ggplot*"))
      )
    ),
    metadata = markermd::markermd_metadata()
  )
  markermd::write_template_yaml(template, file.path(root, "template.yaml"), source_path = qmd)

  suppressMessages(markermd::init_project(root))
  suppressMessages(markermd::project_set(root, template = "template.yaml"))

  list(project = root)
}

# Send one rubric item event as the item rows' delegated handler does (see
# rubric_item_editing_head()): action is "toggle", "move_up", "move_down",
# "group", "delete", "description" or "points", value its payload
item_event = function(app, item, action, value = NULL) {
  payload = jsonlite::toJSON(
    list(item = item, action = action, value = value, nonce = stats::runif(1)),
    auto_unbox = TRUE, null = "null"
  )
  app$run_js(paste0("Shiny.setInputValue('rubric_module-item_event', ", payload, ", {priority: 'event'})"))
}
