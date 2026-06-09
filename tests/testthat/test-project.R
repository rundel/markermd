# Builds a fake ghclass::org_grade_assignment() layout in a temp directory.
make_fake_project = function(with_repos = TRUE, with_artifact = TRUE, with_key = FALSE) {
  d = tempfile("markproj_")
  dir.create(d)
  if (with_repos) {
    dir.create(file.path(d, "repos", "hw01-team01"), recursive = TRUE)
    dir.create(file.path(d, "repos", "hw01-team02"), recursive = TRUE)
  }
  dir.create(file.path(d, "comments"))
  writeLines("nice work", file.path(d, "comments", "hw01-team01.md"))
  dir.create(file.path(d, ".git"))
  if (with_artifact) {
    dir.create(file.path(d, "html"))
    writeLines("<html></html>", file.path(d, "html", "hw01-team01.html"))
  }
  if (with_key) {
    make_fake_repo(file.path(d, "hw01-key"))
  }
  normalizePath(d, winslash = "/")
}

# Creates a directory that looks like a git working tree (has a .git directory).
make_fake_repo = function(path) {
  dir.create(path, recursive = TRUE)
  dir.create(file.path(path, ".git"))
  invisible(path)
}

# Writes a minimal template targeting the #q1 section and requiring a chunk.
write_chunk_template = function(path) {
  writeLines(c(
    sprintf("format_version: '%s'", markermd:::markermd_template_version()),
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


test_that("init_project creates the .markermd config and database", {
  d = make_fake_project()
  init_project(d)

  expect_true(fs::file_exists(fs::path(d, ".markermd/config.yml")))
  expect_true(fs::file_exists(fs::path(d, ".markermd/markermd.sqlite")))
})


test_that("init_project records the layout in the returned project", {
  d = make_fake_project()
  p = init_project(d)

  expect_true(S7::S7_inherits(p, markermd_project))
  expect_equal(p@repos, "repos")
  expect_equal(p@comments, "comments")
  expect_equal(p@artifacts, "html")
  expect_true(is.na(p@key))
  expect_false(any(project_reserved_names() %in% p@artifacts))
})


test_that("init_project detects a top-level git repo as the key", {
  d = make_fake_project(with_key = TRUE)
  p = init_project(d)

  expect_equal(p@key, "hw01-key")
  expect_equal(p@artifacts, "html")
  expect_false("hw01-key" %in% p@artifacts)
})


test_that("init_project treats a non-repo key-named dir as an artifact", {
  d = make_fake_project()
  dir.create(file.path(d, "key-notes"))

  p = init_project(d)
  expect_true(is.na(p@key))
  expect_true("key-notes" %in% p@artifacts)
})


test_that("init_project disambiguates multiple repos by the key name", {
  d = make_fake_project()
  make_fake_repo(file.path(d, "hw01-key"))
  make_fake_repo(file.path(d, "reference"))

  p = init_project(d)
  expect_equal(p@key, "hw01-key")
  expect_false(any(c("hw01-key", "reference") %in% p@artifacts))
})


test_that("init_project warns and leaves key unset when repos are ambiguous", {
  d = make_fake_project()
  make_fake_repo(file.path(d, "answer-key"))
  make_fake_repo(file.path(d, "hw01-key"))

  expect_warning(init_project(d), "look like a key")
  expect_true(is.na(project_config(d)@key))
})


test_that("re-init preserves a manually configured key the heuristic would miss", {
  d = make_fake_project()
  init_project(d)
  make_fake_repo(file.path(d, "reference"))
  project_set(d, key = "reference")

  p = init_project(d)
  expect_equal(p@key, "reference")
  expect_false("reference" %in% p@artifacts)
})


test_that("init_project installs bundled skills without flattening", {
  skip_if(!fs::dir_exists(markermd:::markermd_skills_path()), "bundled skills not found")
  d = make_fake_project()
  init_project(d)

  expect_true(fs::file_exists(fs::path(d, ".claude/skills/scaffold-markermd-template/SKILL.md")))
  expect_false(fs::file_exists(fs::path(d, ".claude/skills/SKILL.md")))
})


test_that("init_project warns but proceeds when repos/ is absent", {
  d = make_fake_project(with_repos = FALSE)
  expect_warning(init_project(d), "repos")
  expect_true(is.na(project_config(d)@repos))
})


test_that("project_config round-trips the project config", {
  d = make_fake_project()
  p = init_project(d)
  q = project_config(d)

  expect_equal(q@repos, p@repos)
  expect_equal(q@comments, p@comments)
  expect_equal(q@artifacts, p@artifacts)
  expect_equal(q@database, p@database)
  expect_equal(q@created_at, p@created_at)
  expect_null(markermd:::load_template_from_db(d))
})


test_that("the config YAML has the expected shape", {
  d = make_fake_project()
  init_project(d)
  raw = yaml::read_yaml(fs::path(d, ".markermd/config.yml"))

  expect_equal(raw$format_version, "1.0")
  expect_null(raw$paths$template)
  expect_null(raw$paths$key)
  expect_equal(as.character(unlist(raw$artifacts)), "html")

  d2 = make_fake_project(with_artifact = FALSE)
  init_project(d2)
  expect_equal(project_config(d2)@artifacts, character(0))
})


test_that("re-init preserves the grading database", {
  d = make_fake_project()
  p1 = init_project(d)

  markermd:::with_database(d, function(conn) {
    DBI::dbExecute(
      conn,
      "INSERT INTO grades (question_name, assignment_repo, item_id, selected, timestamp, username) VALUES (?,?,?,?,?,?)",
      params = list("Q1", "repoA", "i1", 1L, "2024-01-01 00:00:01", "tester")
    )
    invisible()
  })

  p2 = init_project(d)

  n = markermd:::with_database(d, function(conn) {
    DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM grades")$n
  })
  expect_equal(n, 1L)
  expect_equal(p2@created_at, p1@created_at)
  expect_true(p2@updated_at >= p1@updated_at)
})


test_that("re-init preserves the template stored in the database", {
  d = make_fake_project()
  init_project(d)
  write_chunk_template(fs::path(d, "markermd-template.yaml"))
  project_set(d, template = "markermd-template.yaml")
  expect_false(is.null(markermd:::load_template_from_db(d)))

  init_project(d)
  expect_false(is.null(markermd:::load_template_from_db(d)))
})


test_that("project_config errors on an uninitialized directory", {
  d = tempfile("notproj_")
  dir.create(d)
  expect_error(project_config(d), "not a markermd project")
})


test_that("project_config rejects a config from a newer markermd", {
  d = make_fake_project()
  init_project(d)
  cfg = fs::path(d, ".markermd/config.yml")
  raw = yaml::read_yaml(cfg)
  raw$format_version = "9.9"
  yaml::write_yaml(raw, cfg)

  expect_error(project_config(d), "newer version of markermd")
})


test_that("project_set imports, clears, and validates a template", {
  d = make_fake_project()
  init_project(d)
  write_chunk_template(fs::path(d, "t.yml"))

  project_set(d, template = "t.yml")
  project_set(d, repos = "repos")
  expect_false(is.null(markermd:::load_template_from_db(d)))

  project_set(d, template = NA)
  expect_null(markermd:::load_template_from_db(d))

  expect_error(project_set(d, template = "nope.yml"), "does not exist")

  expect_warning(project_set(d, repos = "nope"), "does not exist")
  expect_equal(project_config(d)@repos, "nope")
})


test_that("template_import and template_export round-trip via the database", {
  d = make_fake_project()
  init_project(d)
  write_chunk_template(fs::path(d, "src.yaml"))

  template_import("src.yaml", project = d)
  expect_false(is.null(markermd:::load_template_from_db(d)))

  out = fs::path(d, "exported.yaml")
  template_export(out, project = d)
  expect_true(fs::file_exists(out))

  raw = yaml::read_yaml(out)
  expect_equal(raw$format_version, markermd:::markermd_template_version())
  expect_equal(raw$source$path, "hw1.qmd")
  expect_equal(length(raw$questions), 1)
})


test_that("template_export errors when no template is stored", {
  d = make_fake_project()
  init_project(d)
  expect_error(template_export(fs::path(d, "x.yaml"), project = d), "No template is stored")
})


test_that("project_set records and clears the key", {
  d = make_fake_project()
  init_project(d)
  make_fake_repo(file.path(d, "solution"))

  project_set(d, key = "solution")
  expect_equal(project_config(d)@key, "solution")

  project_set(d, key = NA)
  expect_true(is.na(project_config(d)@key))

  expect_warning(project_set(d, key = "missing"), "does not exist")
  expect_equal(project_config(d)@key, "missing")
})


test_that("project_sitrep reports counts and returns invisibly", {
  d = make_fake_project()
  init_project(d)

  out = cli::cli_fmt(project_sitrep(d))
  expect_true(any(grepl("repos", out)))
  expect_true(any(grepl("html", out)))
  expect_invisible(project_sitrep(d))
})


test_that("validate_project validates student repos against the template", {
  d = make_fake_project()
  unlink(file.path(d, "repos"), recursive = TRUE)
  dir.create(file.path(d, "repos", "team-pass"), recursive = TRUE)
  dir.create(file.path(d, "repos", "team-fail"), recursive = TRUE)
  dir.create(file.path(d, "repos", "team-empty"), recursive = TRUE)
  writeLines(c("## Question 1 {#q1}", "", "```{r}", "1 + 1", "```"),
             file.path(d, "repos", "team-pass", "hw1.qmd"))
  writeLines(c("## Something Else {#other}", "", "no code here"),
             file.path(d, "repos", "team-fail", "hw1.qmd"))

  init_project(d)
  write_chunk_template(file.path(d, "markermd-template.yaml"))
  project_set(d, template = "markermd-template.yaml")

  res = validate_project(d)

  expect_s3_class(res, "data.frame")
  expect_equal(res$status[res$repo == "team-pass"], "pass")
  expect_equal(res$status[res$repo == "team-fail"], "fail")
  expect_equal(res$status[res$repo == "team-empty"], "error")
})


test_that("validate_project errors when the template is unconfigured", {
  d = make_fake_project()
  init_project(d)
  expect_error(validate_project(d), "No template is configured")
})


test_that("validate_project errors when repos is unconfigured", {
  d = make_fake_project(with_repos = FALSE)
  suppressWarnings(init_project(d))
  write_chunk_template(file.path(d, "markermd-template.yaml"))
  project_set(d, template = "markermd-template.yaml")
  expect_error(validate_project(d), "No repos directory")
})


test_that("markermd_project_version is 1.0", {
  expect_identical(markermd:::markermd_project_version(), "1.0")
})
