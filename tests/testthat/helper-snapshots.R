# Announce a test file's existing snapshot files before its skip guards run.
# shinytest2 only announces snapshot files once an AppDriver is created, so if
# the app tests skip first (shinytest2 missing, or on CRAN), testthat's
# end-of-run cleanup would delete the committed snapshots as unused. Announced
# files are preserved even when every test in the file skips. The flip side is
# that a genuinely stale snapshot in these directories must be deleted by hand.
#
# stem: the test file's snapshot directory name, e.g. "template-app_ast"

announce_app_snapshots = function(stem) {
  for (f in list.files(testthat::test_path("_snaps", stem))) {
    testthat::announce_snapshot_file(name = f)
  }
}

# --- Snapshot fixtures (shared by test-snapshots.R and test-review-fixes.R) ---

# An initialized project with a two-question template, a rubric (Q1 with an
# exclusive group, Q2 plain) and marks for two repositories
sn_project = function() {
  d = tempfile("snapproj_")
  dir.create(file.path(d, "repos", "hw01-team01"), recursive = TRUE)
  dir.create(file.path(d, "repos", "hw01-team02"))
  d = normalizePath(d, winslash = "/")
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
    "source:", "  path: hw1.qmd", "questions:",
    "  - id: 1", "    name: Q1", "  - id: 2", "    name: Q2"
  ), file.path(d, "template.yaml"))
  suppressMessages(init_project(d))
  suppressMessages(template_import("template.yaml", project = d))
  d
}

sn_rubric_yaml = function() {
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.1'", "questions:",
    "- name: Q1", "  scoring:", "    total_score: 10", "    grading_mode: negative",
    "  items:", "  - points: 0", "    description: Looks good", "  - points: -3", "    description: Issue A",
    "  - group: Tiers", "    exclusive: true", "    items:",
    "    - points: -1", "      description: Minor", "    - points: -4", "      description: Major",
    "- name: Q2", "  items:", "  - points: 5", "    description: Half credit", "  - points: 10", "    description: Full credit"
  ), path)
  path
}

# The project above with the rubric imported and three pairs marked
sn_graded_project = function() {
  d = sn_project()
  suppressMessages(rubric_import(sn_rubric_yaml(), project = d, summary = "Initial rubric"))
  suppressMessages({
    marks_set("hw01-team01", "Q1", items = c("Issue A", "Minor"), private_comment = "note", project = d)
    marks_set("hw01-team02", "Q1", items = "Looks good", comment = "Nice", project = d)
    marks_set("hw01-team01", "Q2", items = "Full credit", project = d)
  })
  d
}

sn_query = function(d, sql) {
  markermd:::with_database(d, function(conn) DBI::dbGetQuery(conn, sql))
}

# Descriptions of the items currently selected for a pair. A restore keeps
# item ids, but a replace import re-mints them, so tests compare selections
# by description.
sn_selected = function(d, question, repo) {
  items = markermd:::load_rubric_items(d, question)
  sel = markermd:::load_grade_selections(d, question, repo)
  chosen = names(sel)[unlist(sel)]
  unname(vapply(items[intersect(names(items), chosen)], function(item) item@description, character(1)))
}


