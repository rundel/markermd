# Optional questions: a question whose ungraded pairs count as graded and score
# as if no rubric item were selected (a document-level question most
# submissions never need).

oq_project = function() {
  dir = tempfile("optq_")
  dir.create(dir)
  markermd:::save_grade_state(dir, "Q1", markermd_grade_state(current_score = 10, total_score = 10, grading_mode = "negative"))
  markermd:::save_grade_state(dir, "Document", markermd_grade_state(
    current_score = 0, total_score = 0, grading_mode = "negative",
    bound_above_zero = FALSE, optional = TRUE
  ))
  markermd:::save_rubric_items(dir, "Q1", list(
    item_0 = markermd_rubric_item(hotkey = 1L, points = 0, description = "Looks good"),
    item_1 = markermd_rubric_item(hotkey = 2L, points = -2, description = "Wrong answer")
  ))
  markermd:::save_rubric_items(dir, "Document", list(
    item_0 = markermd_rubric_item(hotkey = 1L, points = -5, description = "Document does not render")
  ))
  markermd:::with_database(dir, function(conn) {
    add = function(question, repo, item) {
      DBI::dbExecute(conn,
        "INSERT INTO grades (question_name, assignment_repo, item_id, selected, timestamp, username) VALUES (?,?,?,1,?,?)",
        params = list(question, repo, item, "2024-01-01 00:00:01", "tester"))
    }
    add("Q1", "repoA", "item_0")
    add("Q1", "repoB", "item_1")
    add("Document", "repoB", "item_0")
    invisible()
  })
  dir
}

test_that("optional defaults to FALSE and is validated", {
  expect_false(markermd_grade_state()@optional)
  expect_true(markermd_grade_state(optional = TRUE)@optional)
  expect_error(markermd_grade_state(optional = NA), "optional")
})

test_that("optional round-trips through the settings table", {
  dir = oq_project()
  expect_true(markermd:::load_grade_state(dir, "Document")@optional)
  expect_false(markermd:::load_grade_state(dir, "Q1")@optional)
})

test_that("optional questions count as graded for every given repository", {
  dir = oq_project()

  recorded = markermd:::graded_question_pairs(dir)
  expect_equal(nrow(recorded[recorded$question_name == "Document", ]), 1)

  pairs = markermd:::graded_question_pairs(dir, c("repoA", "repoB", "repoC"))
  doc = pairs[pairs$question_name == "Document", ]
  expect_setequal(doc$assignment_repo, c("repoA", "repoB", "repoC"))
  expect_false(anyDuplicated(pairs) > 0)

  progress = markermd:::calculate_grading_progress(dir, c("Q1", "Document"), c("repoA", "repoB", "repoC"))
  expect_equal(unname(progress), c(2L, 2L, 1L))
})

test_that("an ungraded optional pair scores its starting score and does not hold the total back", {
  dir = oq_project()
  repos = c("repoA", "repoB", "repoC")
  scores = markermd:::collect_score_data(dir, c("Q1", "Document"), repos)
  doc = scores[scores$question_name == "Document", ]
  expect_equal(doc$score, c(0, -5, 0))

  tbl = markermd:::build_scores_table_data(scores, c("Q1", "Document"), repos)
  # repoC's Q1 is still ungraded, so only its running sum is defined
  expect_equal(tbl$Total, c(10, 3, NA))
  expect_equal(tbl$Total_partial, c(10, 3, 0))
  expect_equal(tbl$Graded, c(2L, 2L, 1L))
})

test_that("export_scores totals include optional questions without NA", {
  d = tempfile("optproj_")
  dir.create(file.path(d, "repos", "repoA"), recursive = TRUE)
  dir.create(file.path(d, "repos", "repoB"), recursive = TRUE)
  d = normalizePath(d, winslash = "/")
  template_path = file.path(d, "template.yaml")
  writeLines(c(
    "format_version: '3.1'",
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - {id: 1, name: Q1}",
    "  - {id: 2, name: Document, scope: document}"
  ), template_path)
  suppressMessages(init_project(d))
  suppressMessages(template_import(template_path, project = d))

  markermd:::save_rubric_item(d, "Q1", "item_0", markermd_rubric_item(1L, -2, "Issue A"))
  markermd:::save_rubric_item(d, "Document", "item_1", markermd_rubric_item(1L, -5, "Does not render"))
  markermd:::save_grade_state(d, "Q1", markermd_grade_state(current_score = 10, total_score = 10, grading_mode = "negative"))
  markermd:::save_grade_state(d, "Document", markermd_grade_state(
    current_score = 0, total_score = 0, grading_mode = "negative",
    bound_above_zero = FALSE, optional = TRUE
  ))
  markermd:::save_grade_selection(d, "Q1", "repoA", "item_0", TRUE)
  markermd:::save_grade_selection(d, "Q1", "repoB", "item_0", TRUE)
  markermd:::save_grade_selection(d, "Document", "repoB", "item_1", TRUE)

  suppressMessages(export_scores(d))
  out = utils::read.csv(file.path(d, "scores.csv"), check.names = FALSE)
  expect_equal(out$Document, c(0, -5))
  expect_equal(out$total, c(8, 3))
})

test_that("optional is written to rubric YAML only when set, as format 1.2", {
  plain = list(format_version = "1.1", questions = list(list(
    name = "Q1", scoring = markermd_grade_state(total_score = 10),
    items = list(markermd_rubric_item(hotkey = 1L, points = 1, description = "a")), groups = list()
  )))
  lst = markermd:::rubric_to_list(plain)
  expect_equal(lst$format_version, "1.0")
  expect_null(lst$questions[[1]]$scoring$optional)

  opt = plain
  opt$questions[[1]]$scoring = markermd_grade_state(total_score = 0, grading_mode = "negative", optional = TRUE)
  lst = markermd:::rubric_to_list(opt)
  expect_equal(lst$format_version, "1.2")
  expect_true(lst$questions[[1]]$scoring$optional)

  path = tempfile(fileext = ".yaml")
  yaml::write_yaml(lst, path)
  back = markermd::read_rubric_yaml(path)
  expect_true(back$questions[[1]]$scoring@optional)

  skip_if_not_installed("jsonvalidate")
  expect_true(isTRUE(markermd::validate_rubric_file(path)))
})

test_that("a schema-version-3 database gains settings.optional", {
  dir = tempfile("optmigrate")
  dir.create(file.path(dir, ".markermd"), recursive = TRUE)

  conn = DBI::dbConnect(RSQLite::SQLite(), file.path(dir, ".markermd", "markermd.sqlite"))
  DBI::dbExecute(conn, "
    CREATE TABLE settings (
      question_name TEXT PRIMARY KEY,
      current_score REAL NOT NULL,
      total_score REAL NOT NULL,
      grading_mode TEXT NOT NULL CHECK (grading_mode IN ('positive', 'negative')),
      bound_above_zero INTEGER NOT NULL CHECK (bound_above_zero IN (0, 1)),
      bound_below_max INTEGER NOT NULL CHECK (bound_below_max IN (0, 1)),
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")
  DBI::dbExecute(conn, "CREATE TABLE metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL, updated_at TEXT NOT NULL)")
  DBI::dbExecute(conn, "INSERT INTO metadata VALUES ('schema_version', '3', '2024-01-01 00:00:00')")
  DBI::dbExecute(conn, "INSERT INTO settings VALUES ('Q1', 10, 10, 'negative', 1, 1, '2024-01-01 00:00:00', '2024-01-01 00:00:00')")
  DBI::dbDisconnect(conn)

  state = markermd:::load_grade_state(dir, "Q1")
  expect_false(state@optional)
  expect_equal(state@total_score, 10)

  markermd:::with_database(dir, function(conn) {
    expect_equal(markermd:::get_metadata(conn, "schema_version"), markermd:::markermd_db_version())
    expect_true("optional" %in% DBI::dbListFields(conn, "settings"))
  })
})
