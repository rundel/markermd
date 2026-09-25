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


test_that("init_project falls back to a key-named non-repo dir holding an assignment", {
  d = make_fake_project()
  dir.create(file.path(d, "hw01-key"))
  writeLines("# Solution", file.path(d, "hw01-key", "assignment.qmd"))

  p = init_project(d)
  expect_equal(p@key, "hw01-key")
  expect_false("hw01-key" %in% p@artifacts)
  expect_equal(p@artifacts, "html")
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


bundled_skill_names = c(
  "markermd-scaffold-template", "markermd-scaffold-rubric",
  "markermd-apply-rubric", "markermd-fix-parsing"
)

skill_frontmatter_field = function(skill_md, field) {
  line = grep(paste0("^", field, ":"), readLines(skill_md), value = TRUE)[1]
  trimws(sub(paste0("^", field, ":"), "", line))
}


test_that("init_project installs both bundled skill sets without flattening", {
  skip_if(!fs::dir_exists(markermd:::markermd_skills_path("claude")), "bundled skills not found")
  d = make_fake_project()
  init_project(d)

  for (skill in bundled_skill_names) {
    expect_true(fs::file_exists(fs::path(d, ".claude/skills", skill, "SKILL.md")))
    expect_true(fs::file_exists(fs::path(d, ".agents/skills", skill, "SKILL.md")))
    expect_true(fs::file_exists(fs::path(d, ".agents/skills", skill, "agents/openai.yaml")))
  }
  expect_false(fs::file_exists(fs::path(d, ".claude/skills/SKILL.md")))
  expect_false(fs::file_exists(fs::path(d, ".agents/skills/SKILL.md")))
  expect_true(fs::file_exists(fs::path(d, "CLAUDE.md")))
  expect_true(fs::file_exists(fs::path(d, "AGENTS.md")))
  expect_false(fs::file_exists(fs::path(d, ".claude/CLAUDE.md")))
})


test_that("init_project installs only the requested agents' skills", {
  skip_if(!fs::dir_exists(markermd:::markermd_skills_path("codex")), "bundled skills not found")
  d = make_fake_project()
  init_project(d, agents = "codex")

  expect_false(fs::dir_exists(fs::path(d, ".claude/skills")))
  expect_true(fs::file_exists(fs::path(d, ".agents/skills/markermd-fix-parsing/SKILL.md")))
  expect_false(fs::file_exists(fs::path(d, "CLAUDE.md")))
  expect_true(fs::file_exists(fs::path(d, "AGENTS.md")))
  expect_error(init_project(d, agents = "cursor"), "should be one of")
})


test_that("init_project never overwrites an existing instructions file", {
  skip_if(!fs::dir_exists(markermd:::markermd_skills_path("claude")), "bundled skills not found")
  d = make_fake_project()
  writeLines("# my notes", fs::path(d, "CLAUDE.md"))
  init_project(d)

  expect_equal(readLines(fs::path(d, "CLAUDE.md")), "# my notes")
  expect_equal(
    readLines(fs::path(d, "AGENTS.md")),
    readLines(fs::path(markermd:::markermd_skills_path("codex"), "AGENTS.md"))
  )

  init_project(d)
  expect_equal(readLines(fs::path(d, "CLAUDE.md")), "# my notes")
})


test_that("init_project(instructions = FALSE) skips the instructions files", {
  skip_if(!fs::dir_exists(markermd:::markermd_skills_path("claude")), "bundled skills not found")
  d = make_fake_project()
  init_project(d, instructions = FALSE)

  expect_false(fs::file_exists(fs::path(d, "CLAUDE.md")))
  expect_false(fs::file_exists(fs::path(d, "AGENTS.md")))
  expect_true(fs::file_exists(fs::path(d, ".claude/skills/markermd-fix-parsing/SKILL.md")))
  expect_error(init_project(d, instructions = NA), "instructions")
})


test_that("each bundled skill set ships its agent's project instructions file", {
  skip_if(!fs::dir_exists(markermd:::markermd_skills_path("claude")), "bundled skills not found")
  claude_md = fs::path(markermd:::markermd_skills_path("claude"), "CLAUDE.md")
  agents_md = fs::path(markermd:::markermd_skills_path("codex"), "AGENTS.md")
  expect_true(fs::file_exists(claude_md))
  expect_true(fs::file_exists(agents_md))

  for (skill in bundled_skill_names) {
    expect_true(any(grepl(paste0("/", skill), readLines(claude_md), fixed = TRUE)))
    expect_true(any(grepl(paste0("$", skill), readLines(agents_md), fixed = TRUE)))
  }
  for (f in c(claude_md, agents_md)) {
    for (schema in c("markermd-template.json", "markermd-rubric.json", "markermd-marks.json")) {
      expect_true(any(grepl(schema, readLines(f), fixed = TRUE)))
    }
  }
})


test_that("each bundled skill set follows the Agent Skills frontmatter rules", {
  skip_if(!fs::dir_exists(markermd:::markermd_skills_path("claude")), "bundled skills not found")

  names_by_agent = list()
  for (agent in c("claude", "codex")) {
    skill_dirs = fs::dir_ls(markermd:::markermd_skills_path(agent), type = "directory")
    expect_gt(length(skill_dirs), 0)

    for (skill_dir in skill_dirs) {
      skill_md = fs::path(skill_dir, "SKILL.md")
      expect_true(fs::file_exists(skill_md))

      name = skill_frontmatter_field(skill_md, "name")
      expect_equal(name, fs::path_file(skill_dir))
      expect_match(name, "^[a-z0-9]+(-[a-z0-9]+)*$")
      expect_lte(nchar(name), 64)

      description = skill_frontmatter_field(skill_md, "description")
      expect_gt(nchar(description), 0)
      expect_lte(nchar(description), 1024)
    }
    names_by_agent[[agent]] = fs::path_file(skill_dirs)
  }
  expect_setequal(names_by_agent$claude, names_by_agent$codex)
  expect_setequal(names_by_agent$claude, bundled_skill_names)
})


test_that("the Codex skill set carries no Claude-specific tool references", {
  skip_if(!fs::dir_exists(markermd:::markermd_skills_path("codex")), "bundled skills not found")
  files = fs::dir_ls(markermd:::markermd_skills_path("codex"), recurse = TRUE, glob = "*.md")
  expect_gt(length(files), 0)
  for (f in files) {
    hits = grep("Claude|AskUserQuestion|Agent tool|Edit tool", readLines(f), value = TRUE)
    expect_length(hits, 0)
  }
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


test_that("parse_project reports per-repo parse status", {
  d = make_fake_project()
  unlink(file.path(d, "repos"), recursive = TRUE)
  dir.create(file.path(d, "repos", "team-ok"), recursive = TRUE)
  dir.create(file.path(d, "repos", "team-broken"), recursive = TRUE)
  dir.create(file.path(d, "repos", "team-html"), recursive = TRUE)
  dir.create(file.path(d, "repos", "team-empty"), recursive = TRUE)
  writeLines(c("## Question 1 {#q1}", "", "```{r}", "1 + 1", "```"),
             file.path(d, "repos", "team-ok", "hw1.qmd"))
  writeLines(c("## Question 1 {#q1}", "", "The cost was $5 today."),
             file.path(d, "repos", "team-broken", "hw1.qmd"))
  writeLines(c("## Question 1 {#q1}", "", "Line one<br>line two"),
             file.path(d, "repos", "team-html", "hw1.qmd"))
  init_project(d)

  res = parse_project(d)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("repo", "file", "status", "detail"))
  expect_equal(res$status[res$repo == "team-ok"], "ok")
  expect_true(is.na(res$detail[res$repo == "team-ok"]))
  expect_equal(basename(res$file[res$repo == "team-ok"]), "hw1.qmd")
  expect_equal(res$status[res$repo == "team-broken"], "error")
  expect_match(res$detail[res$repo == "team-broken"], "cost was $5", fixed = TRUE)
  expect_equal(res$status[res$repo == "team-html"], "warning")
  expect_match(res$detail[res$repo == "team-html"], "Q-2-9")
  expect_equal(res$status[res$repo == "team-empty"], "missing")
  expect_true(is.na(res$file[res$repo == "team-empty"]))
})


test_that("parse_project errors when repos is unconfigured", {
  d = make_fake_project(with_repos = FALSE)
  suppressWarnings(init_project(d))
  expect_error(parse_project(d), "No repos directory")
})
