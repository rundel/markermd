# Coverage tests for R/project.R and R/utils_git.R.
#
# Every top-level helper here is prefixed with cpr_ so it cannot clobber the
# helpers defined in the neighbouring test-project.R / test-export-marks.R files
# (testthat sources all test files into one shared environment).

# Build an initialized project with a stored two-question (Q1, Q2) template and
# two empty student repo directories, mirroring make_export_project() in
# test-export-marks.R.
cpr_make_project = function() {
  d = tempfile("cprproj_")
  dir.create(file.path(d, "repos", "hw01-team01"), recursive = TRUE)
  dir.create(file.path(d, "repos", "hw01-team02"), recursive = TRUE)
  d = normalizePath(d, winslash = "/")

  template_path = file.path(d, "template.yaml")
  writeLines(c(
    paste0("format_version: '", markermd:::markermd_template_version(), "'"),
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - id: 1",
    "    name: Q1",
    "  - id: 2",
    "    name: Q2"
  ), template_path)

  suppressMessages(init_project(d))
  suppressMessages(template_import(template_path, project = d))
  d
}

# Write a chunk-count template YAML (targets the #q1 section, requires a chunk).
cpr_write_chunk_template = function(path) {
  writeLines(c(
    paste0("format_version: '", markermd:::markermd_template_version(), "'"),
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - id: 1",
    "    name: Q1",
    "    node_ids:",
    "      - q1",
    "    rules:",
    "      - node_type: Chunk",
    "        verb: has at least",
    "        count: 1"
  ), path)
}

# Lay out three student repos (pass / fail / empty) inside an initialized
# project, using the given assignment-file extension so the same body can be
# checked under both the .qmd and .Rmd code paths.
cpr_make_validate_project = function(ext) {
  d = tempfile("cprval_")
  dir.create(file.path(d, "repos", "team-pass"), recursive = TRUE)
  dir.create(file.path(d, "repos", "team-fail"), recursive = TRUE)
  dir.create(file.path(d, "repos", "team-empty"), recursive = TRUE)
  d = normalizePath(d, winslash = "/")

  writeLines(c("## Question 1 {#q1}", "", "```{r}", "1 + 1", "```"),
             file.path(d, "repos", "team-pass", paste0("hw1", ext)))
  writeLines(c("## Something Else {#other}", "", "no code here"),
             file.path(d, "repos", "team-fail", paste0("hw1", ext)))

  suppressMessages(init_project(d))
  cpr_write_chunk_template(file.path(d, "markermd-template.yaml"))
  suppressMessages(project_set(d, template = "markermd-template.yaml"))
  d
}


test_that("validate_project uses a markermd_template object override over the stored template", {
  # The project DB stores the chunk-count template (team-fail -> fail). The
  # override has the same #q1 selection but NO rules, so team-fail flips to
  # pass. team-fail's status therefore proves the override was applied and the
  # DB template ignored (the DB template can only ever report fail there).
  d = cpr_make_validate_project(".qmd")

  override_path = file.path(d, "override.yaml")
  writeLines(c(
    paste0("format_version: '", markermd:::markermd_template_version(), "'"),
    "source:",
    "  path: hw1.qmd",
    "questions:",
    "  - id: 1",
    "    name: Q1",
    "    node_ids:",
    "      - q1"
  ), override_path)
  tmpl = markermd:::read_template_yaml(override_path, require_ast = FALSE)
  expect_true(S7::S7_inherits(tmpl, markermd:::markermd_template))

  res = validate_project(d, template = tmpl)

  expect_s3_class(res, "data.frame")
  expect_identical(names(res), c("repo", "question", "status", "detail"))
  expect_equal(res$status[res$repo == "team-pass"], "pass")
  expect_equal(res$status[res$repo == "team-fail"], "pass")   # no-rule override; DB template would say fail
  expect_equal(res$status[res$repo == "team-empty"], "error")

  # Sanity: the stored template (no override) reports fail for team-fail, so the
  # override genuinely changed the outcome.
  res_db = validate_project(d)
  expect_equal(res_db$status[res_db$repo == "team-fail"], "fail")
})


test_that("validate_project errors on a non-existent template path override", {
  d = cpr_make_validate_project(".qmd")
  expect_error(
    validate_project(d, template = file.path(d, "nope.yaml")),
    "does not exist"
  )
})


test_that("validate_project errors on a non-string, non-template override", {
  d = cpr_make_validate_project(".qmd")
  expect_error(
    validate_project(d, template = 42),
    "must be a template file path"
  )
})


test_that("validate_project with use_qmd = FALSE matches the .qmd statuses", {
  d_rmd = cpr_make_validate_project(".Rmd")
  res_rmd = validate_project(d_rmd, use_qmd = FALSE)

  d_qmd = cpr_make_validate_project(".qmd")
  res_qmd = validate_project(d_qmd, use_qmd = TRUE)

  ord = function(res) res[order(res$repo, res$question), c("repo", "status")]
  expect_equal(ord(res_rmd), ord(res_qmd), ignore_attr = TRUE)

  expect_equal(res_rmd$status[res_rmd$repo == "team-pass"], "pass")
  expect_equal(res_rmd$status[res_rmd$repo == "team-fail"], "fail")
  expect_equal(res_rmd$status[res_rmd$repo == "team-empty"], "error")
})


test_that("project_from_list pins the database path, ignoring a config override", {
  d = cpr_make_project()
  cfg_path = file.path(d, ".markermd", "config.yml")
  raw = yaml::read_yaml(cfg_path)
  raw$paths$database = "somewhere/else.sqlite"
  yaml::write_yaml(raw, cfg_path)

  expect_equal(project_config(d)@database, ".markermd/markermd.sqlite")
})


test_that("marks_export orders a non-template question after the template questions", {
  d = cpr_make_project()

  markermd:::save_rubric_item(d, "Q1", "item_q1", markermd_rubric_item(1L, -1, "Q1 issue"))
  markermd:::save_grade_selection(d, "Q1", "hw01-team01", "item_q1", TRUE)

  markermd:::save_rubric_item(d, "Q9", "item_q9", markermd_rubric_item(1L, -1, "Q9 issue"))
  markermd:::save_grade_selection(d, "Q9", "hw01-team01", "item_q9", TRUE)

  out = file.path(d, "marks.yaml")
  suppressMessages(marks_export(out, project = d))

  raw = yaml::read_yaml(out)
  repo = Filter(function(r) r$name == "hw01-team01", raw$repos)[[1]]
  question_names = vapply(repo$questions, function(q) q$name, character(1))

  expect_equal(question_names, c("Q1", "Q9"))
})


test_that("detect_keyish_dir leaves the key NA when two key-named dirs hold a document", {
  d = tempfile("cprkey_")
  dir.create(d)
  d = normalizePath(d, winslash = "/")
  for (nm in c("hw-key", "solution-key")) {
    dir.create(file.path(d, nm))
    writeLines("# Solution", file.path(d, nm, "assignment.qmd"))
  }

  key = markermd:::detect_keyish_dir(d, c("hw-key", "solution-key"))
  expect_true(is.na(key))

  p = suppressWarnings(suppressMessages(init_project(d)))
  expect_true(is.na(p@key))
  expect_true(all(c("hw-key", "solution-key") %in% p@artifacts))
})


test_that("resolve_assignment_file errors when multiple files match", {
  d = tempfile("cprglob_")
  dir.create(d)
  writeLines("a", file.path(d, "one.qmd"))
  writeLines("b", file.path(d, "two.qmd"))

  err = expect_error(
    markermd:::resolve_assignment_file(d, "*.qmd"),
    "Multiple files match"
  )
  expect_match(conditionMessage(err), "one.qmd")
  expect_match(conditionMessage(err), "two.qmd")
})


test_that("resolve_assignment_file errors when no file matches", {
  d = tempfile("cprglob_")
  dir.create(d)

  expect_error(
    markermd:::resolve_assignment_file(d, "*.qmd"),
    "No files matching"
  )
})
