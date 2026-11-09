# Build a temporary markermd project (repos/ populated from the bundled
# fixtures, a rendered report for student1 under html/ unless report = FALSE,
# and a configured template that grades Question 2 on "quantile" content and
# Question 3 on "ggplot"). Every mark() test file builds on it.
make_mark_fixture = function(report = TRUE) {
  src = system.file("examples/test_assignment/repos", package = "markermd")
  root = tempfile("markproj_")
  repos = file.path(root, "repos")
  dir.create(repos, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), repos, recursive = TRUE)

  if (report) {
    html_dir = file.path(root, "html")
    dir.create(html_dir)
    writeLines(
      "<html><body>student1 report</body></html>",
      file.path(html_dir, "student1-excellent.html")
    )
  }

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

# Click the Assignments table's score cell of a question in a row (1-based,
# in the table's current order). The column is found by its header, whose
# first line is the question name, so the test does not depend on how many
# link and status columns come before the scores.
click_score_cell = function(app, row, question) {
  app$run_js(paste0(
    "var headers = Array.from(document.querySelectorAll('#repo_table .rt-thead .rt-tr:last-child .rt-th'));",
    "var col = headers.findIndex(function(h) { return h.innerText.trim().split('\\n')[0].trim() === ",
    jsonlite::toJSON(question, auto_unbox = TRUE), "; });",
    "var tr = document.querySelectorAll('#repo_table .rt-tbody .rt-tr')[", row - 1, "];",
    "tr.querySelectorAll('.rt-td')[col].click();"
  ))
}

# Rows of a project database table written after a given id (the whole table
# when after = 0), for asserting exactly which events an interaction wrote
db_rows_after = function(root, table, after = 0) {
  markermd:::with_database(root, function(conn) {
    DBI::dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE id > ? ORDER BY id"), params = list(after))
  })
}

# The largest id of a project database table (0 when it is empty)
db_max_id = function(root, table) {
  markermd:::with_database(root, function(conn) {
    out = DBI::dbGetQuery(conn, paste0("SELECT MAX(id) AS id FROM ", table))$id
    if (is.na(out)) 0 else out
  })
}

# Poll an R-side condition (a database write behind a debounce, say) until it
# holds, failing the test after timeout seconds instead of guessing a fixed
# sleep long enough for every machine
wait_until = function(condition, timeout = 10, interval = 0.1) {
  deadline = Sys.time() + timeout
  while (!isTRUE(condition())) {
    if (Sys.time() > deadline) {
      testthat::fail(paste0("Condition not met within ", timeout, " seconds"))
      return(invisible(FALSE))
    }
    Sys.sleep(interval)
  }
  invisible(TRUE)
}
