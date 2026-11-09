# Project management for ghclass::org_grade_assignment() layouts.
#
# init_project() bootstraps a grading project: it keeps all markermd state under
# a single .markermd/ directory at the project root (the SQLite grading DB and a
# YAML config), installs the bundled agent skills into the project's
# .claude/skills/ (Claude Code) and .agents/skills/ (Codex) so they are
# discoverable, and writes each agent's project instructions file (CLAUDE.md /
# AGENTS.md) at the root when none exists. project_config()/project_sitrep()
# read and report that state; project_set() edits it.

# Absolute path to a project's config file.
#
# path: project root

project_config_path = function(path) {
  fs::path(path, ".markermd", "config.yml")
}

# Whether a path is an initialized markermd project (has .markermd/config.yml).
#
# path: directory to test

is_markermd_project = function(path) {
  is.character(path) && length(path) == 1 && fs::file_exists(project_config_path(path))
}

# Top-level directory names that are never treated as artifact directories.

project_reserved_names = function() {
  c("repos", "comments", ".markermd", ".claude", ".git")
}

# Top-level project directories that could be the key repository or an artifact
# directory: every directory that is not a reserved name and not a dot-directory.
#
# path: project root
# Returns: sorted character vector of relative directory names

project_candidate_dirs = function(path) {
  dirs = fs::dir_ls(path, type = "directory")
  names = fs::path_file(dirs)
  names = names[!names %in% project_reserved_names()]
  names = names[!startsWith(names, ".")]
  sort(names)
}

# Whether a directory is a git working tree (has a .git directory or file).
#
# path: directory to test

is_git_repo = function(path) {
  file.exists(fs::path(path, ".git"))
}

# Guess which candidate directory holds the key (solution) repository. The key
# is cloned into the project root as a git repository, so a top-level git repo
# is the key (student repositories live under repos/). When several top-level
# repos exist, the one whose name contains "key" wins; a remaining tie is left
# unset with a warning.
#
# repo_dirs: candidate directory names that are git repositories

detect_key_dir = function(repo_dirs) {
  if (length(repo_dirs) == 0) {
    return(NA_character_)
  }
  if (length(repo_dirs) == 1) {
    return(repo_dirs)
  }
  keyish = repo_dirs[grepl("key", repo_dirs, ignore.case = TRUE)]
  if (length(keyish) == 1) {
    return(keyish)
  }
  cli::cli_warn(c(
    "Multiple top-level git repositories look like a key: {.val {repo_dirs}}.",
    "i" = "Record it explicitly with {.code project_set(key = ...)}; leaving it unset."
  ))
  NA_character_
}

# Name-based key fallback for projects with no top-level git repository (a
# copied or unpacked layout, e.g. the bundled example project, since R CMD
# build strips .git directories): a single non-repo candidate whose name
# contains "key" and that holds an assignment document at its top level.
# Key-named directories without a document (e.g. downloaded artifacts) are
# left to be recorded as artifacts.
#
# root: project root
# other_dirs: candidate directory names that are not git repositories

detect_keyish_dir = function(root, other_dirs) {
  keyish = other_dirs[grepl("key", other_dirs, ignore.case = TRUE)]
  has_doc = vapply(keyish, function(nm) {
    length(fs::dir_ls(fs::path(root, nm), type = "file", regexp = "[.](qmd|Rmd)$")) > 0
  }, logical(1))
  keyish = keyish[has_doc]
  if (length(keyish) == 1) keyish else NA_character_
}

# Project-relative skills directory each supported agent discovers skills in
#
# agent: "claude" or "codex"

agent_skills_dir = function(agent) {
  c(claude = ".claude/skills", codex = ".agents/skills")[[agent]]
}

# Copy every bundled skill directory of each requested agent's set into that
# agent's project skills directory (<path>/.claude/skills/<skill> for Claude
# Code, <path>/.agents/skills/<skill> for Codex), overwriting any existing copy
# so the project's skills track the installed package. The explicit per-skill
# target avoids fs::dir_copy() flattening a skill's contents into the skills
# directory directly.
#
# path: project root
# agents: character vector of agents to install for ("claude", "codex")
# Returns: list named by agent of the skill names copied for it

install_project_skills = function(path, agents = c("claude", "codex")) {
  installed = list()
  for (agent in agents) {
    src = markermd_skills_path(agent)
    skill_dirs = if (nzchar(src) && fs::dir_exists(src)) {
      fs::dir_ls(src, type = "directory")
    } else {
      character(0)
    }
    names = fs::path_file(skill_dirs)

    if (length(skill_dirs) > 0) {
      dest_root = fs::path(path, agent_skills_dir(agent))
      fs::dir_create(dest_root)
      for (i in seq_along(skill_dirs)) {
        fs::dir_copy(skill_dirs[[i]], fs::path(dest_root, names[[i]]), overwrite = TRUE)
      }
    }
    installed[[agent]] = names
  }
  installed
}

# Project instructions file each supported agent reads from the project root
#
# agent: "claude" or "codex"

agent_instructions_file = function(agent) {
  c(claude = "CLAUDE.md", codex = "AGENTS.md")[[agent]]
}

# Copy each requested agent's bundled project instructions file (CLAUDE.md for
# Claude Code, AGENTS.md for Codex, shipped at the root of that agent's skill
# set) to the project root. An existing file is the user's to customize, so it
# is kept unless overwrite is TRUE.
#
# path: project root
# agents: character vector of agents to install for ("claude", "codex")
# overwrite: replace an existing file with the packaged version
# Returns: character vector named by agent, "created", "kept" or
#   "overwritten" per file

install_project_instructions = function(path, agents = c("claude", "codex"), overwrite = FALSE) {
  status = character(0)
  for (agent in agents) {
    src = fs::path(markermd_skills_path(agent), agent_instructions_file(agent))
    if (!fs::file_exists(src)) {
      next
    }
    dest = fs::path(path, agent_instructions_file(agent))
    if (fs::file_exists(dest) && overwrite) {
      fs::file_copy(src, dest, overwrite = TRUE)
      status[[agent]] = "overwritten"
    } else if (fs::file_exists(dest)) {
      status[[agent]] = "kept"
    } else {
      fs::file_copy(src, dest)
      status[[agent]] = "created"
    }
  }
  status
}

# NULL -> NA_character_, otherwise coerce to character. Used when reading
# optional path fields out of a parsed config list.
#
# x: a parsed YAML value or NULL

chr_or_na = function(x) {
  if (is.null(x)) NA_character_ else as.character(x)
}

# NA scalar -> NULL (which yaml writes as `~`), otherwise the value unchanged.
#
# x: a length-one value

na_to_null = function(x) {
  if (length(x) == 1 && is.na(x)) NULL else x
}

# TRUE when x is the NA "clear this value" sentinel accepted by project_set().
#
# x: a project_set() argument value

is_clear_sentinel = function(x) {
  length(x) == 1 && is.na(x)
}

# Reject a config written by a newer markermd than this one.
#
# version: format_version string read from the config file

assert_project_compatible = function(version) {
  if (utils::compareVersion(version, markermd_project_version()) > 0) {
    cli::cli_abort(c(
      "This project config requires a newer version of markermd.",
      "x" = "Config format_version is {version}; this markermd supports up to {markermd_project_version()}.",
      "i" = "Please upgrade the markermd package."
    ))
  }
}

# markermd_project -> plain list for yaml::write_yaml. root is not written (it
# is where the file lives); NA path fields are written as `~`; artifacts is a
# list so a single artifact still serializes as a YAML sequence.
#
# project: markermd_project object

project_to_list = function(project) {
  list(
    format_version = project@version,
    created_at = project@created_at,
    updated_at = project@updated_at,
    paths = list(
      repos = na_to_null(project@repos),
      comments = na_to_null(project@comments),
      key = na_to_null(project@key),
      database = project@database
    ),
    artifacts = as.list(project@artifacts)
  )
}

# plain list -> markermd_project, version-checked against this package.
#
# x: list parsed from a config YAML file
# root: absolute project root (parent of .markermd)

project_from_list = function(x, root) {
  version = x$format_version
  if (is.null(version)) {
    cli::cli_abort("Project config is missing the required {.field format_version} field.")
  }
  assert_project_compatible(as.character(version))

  paths = if (is.null(x$paths)) list() else x$paths

  markermd_project(
    root = root,
    repos = chr_or_na(paths$repos),
    comments = chr_or_na(paths$comments),
    # The database path is fixed at .markermd/markermd.sqlite (it is intrinsic to
    # the project marker and is the only path the DB layer ever opens). Any value
    # in the config is ignored so a hand-edited path cannot silently split
    # grading data across two files.
    database = ".markermd/markermd.sqlite",
    key = chr_or_na(paths$key),
    artifacts = if (is.null(x$artifacts)) character(0) else as.character(unlist(x$artifacts)),
    created_at = as.character(if (is.null(x$created_at)) get_current_timestamp() else x$created_at),
    updated_at = as.character(if (is.null(x$updated_at)) get_current_timestamp() else x$updated_at),
    version = as.character(version)
  )
}

# Stamp updated_at, write the config to <root>/.markermd/config.yml, and return
# the stamped project. This is the single place updated_at is set, so the file
# and the returned object never disagree.
#
# project: markermd_project object

write_project_config = function(project) {
  project@updated_at = get_current_timestamp()
  fs::dir_create(fs::path(project@root, ".markermd"))
  yaml::write_yaml(project_to_list(project), project_config_path(project@root))
  invisible(project)
}

#' Initialize a markermd grading project
#'
#' Bootstraps a markermd project in a directory laid out by
#' `ghclass::org_grade_assignment()` (a `repos/` directory of student
#' repositories, a `comments/` directory, an optional key/solution repository,
#' and a directory per downloaded artifact). It creates a `.markermd/` directory
#' at the project root holding the SQLite grading database and a `config.yml`
#' recording the project's file locations, and installs the bundled agent
#' skills so they are discoverable from the project: the Claude Code set into
#' `<path>/.claude/skills/` and the Codex set into `<path>/.agents/skills/`
#' (see `agents`). It also writes each selected agent's project instructions
#' file at the project root (`CLAUDE.md` for Claude Code, `AGENTS.md` for
#' Codex) describing the project layout, the grading workflow and skills, and
#' the rules an agent must follow when working with student submissions. An
#' existing instructions file is kept, so it can be customized freely, unless
#' `overwrite_instructions = TRUE`; pass `instructions = FALSE` to skip this
#' step.
#'
#' The key (solution) repository is detected as the top-level git repository
#' (student repositories live under `repos/`); when several top-level repos
#' exist, the one whose name contains "key" is used. When no top-level git
#' repository exists (a copied or unpacked layout, such as the bundled example
#' project), a directory whose name contains "key" and that holds an assignment
#' document (`.qmd`/`.Rmd`) is used instead. On the first run the remaining
#' top-level directories that are not git repositories are recorded as
#' artifact directories.
#'
#' Re-running `init_project()` on an already-initialized directory updates it in
#' place: the existing grading database (including any stored grading template)
#' is preserved, the skills are refreshed to the packaged version, existing
#' instructions files are left untouched (unless `overwrite_instructions =
#' TRUE`), and a previously configured key is kept (or cleared if it no longer
#' exists). The configured artifact directories are kept too, minus any that
#' no longer exist: top-level directories created since (working notes,
#' backups) are listed but not added, so add one with
#' `project_set(artifacts = )`, or pass `rescan_artifacts = TRUE` to rebuild
#' the list from every non-repository top-level directory as on the first run.
#'
#' @param path Path to the project directory.
#' @param agents Which agents' skill sets to install: `"claude"` (into
#'   `<path>/.claude/skills/`), `"codex"` (into `<path>/.agents/skills/`), or
#'   both, the default. The same selection decides which instructions files
#'   are written.
#' @param instructions Whether to write the selected agents' project
#'   instructions files (`CLAUDE.md` / `AGENTS.md`) at the project root when
#'   they do not already exist. Defaults to `TRUE`.
#' @param overwrite_instructions Whether to replace existing instructions
#'   files with the packaged versions, discarding any local edits to them (for
#'   example to pick up the instructions shipped with a newer markermd).
#'   Defaults to `FALSE`; has no effect when `instructions = FALSE`.
#' @param rescan_artifacts Whether a re-run rebuilds the artifact directory
#'   list from every non-repository top-level directory instead of keeping the
#'   configured one. Defaults to `FALSE`; the first run always scans.
#'
#' @return A `markermd_project` object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' ghclass::org_grade_assignment("hw01", org = "my-course", repo_filter = "hw01-")
#' init_project("hw01")
#' }
init_project = function(path, agents = c("claude", "codex"), instructions = TRUE,
                        overwrite_instructions = FALSE, rescan_artifacts = FALSE) {
  agents = match.arg(agents, several.ok = TRUE)
  for (arg in c("instructions", "overwrite_instructions", "rescan_artifacts")) {
    if (!rlang::is_bool(get(arg))) {
      cli::cli_abort("{.arg {arg}} must be {.code TRUE} or {.code FALSE}.")
    }
  }
  if (!fs::dir_exists(path)) {
    cli::cli_abort("{.arg path} does not exist or is not a directory: {.path {path}}")
  }
  root = normalizePath(path, winslash = "/", mustWork = TRUE)

  has_repos = fs::dir_exists(fs::path(root, "repos"))
  has_comments = fs::dir_exists(fs::path(root, "comments"))
  if (!has_repos) {
    cli::cli_warn("No {.path repos/} directory found in {.path {root}}; proceeding anyway.")
  }

  candidates = project_candidate_dirs(root)
  is_repo = vapply(candidates, function(nm) is_git_repo(fs::path(root, nm)), logical(1))
  repo_dirs = candidates[is_repo]

  cfg_path = project_config_path(root)
  prior = if (fs::file_exists(cfg_path)) project_from_list(yaml::read_yaml(cfg_path), root) else NULL
  created_at = if (!is.null(prior)) prior@created_at else get_current_timestamp()

  key = if (!is.null(prior) && !is.na(prior@key) && fs::dir_exists(fs::path(root, prior@key))) {
    prior@key
  } else {
    detect_key_dir(repo_dirs)
  }
  if (is.na(key) && length(repo_dirs) == 0) {
    key = detect_keyish_dir(root, candidates[!is_repo])
  }

  # The first run (or a rescan) records every non-repo top-level directory as
  # an artifact directory. Otherwise the configured list is authoritative,
  # minus directories that no longer exist, and directories that appeared
  # since are reported rather than added, so working folders such as notes
  # or backups stay out of it.
  scanned = candidates[!is_repo]
  unlisted = character(0)
  if (is.null(prior) || rescan_artifacts) {
    artifacts = scanned
  } else {
    artifacts = prior@artifacts[fs::dir_exists(fs::path(root, prior@artifacts))]
    unlisted = setdiff(scanned, c(artifacts, key))
  }
  if (!is.na(key)) {
    artifacts = setdiff(artifacts, key)
  }

  conn = initialize_database(root)
  close_database(conn)

  skills = install_project_skills(root, agents)
  skills_lines = vapply(names(skills), function(agent) {
    names_txt = if (length(skills[[agent]])) paste(skills[[agent]], collapse = ", ") else "none"
    as.character(glue::glue("skills ({agent}): {names_txt} ({agent_skills_dir(agent)}/)"))
  }, character(1))
  names(skills_lines) = rep("*", length(skills_lines))

  instructions_status = if (instructions) {
    install_project_instructions(root, agents, overwrite = overwrite_instructions)
  } else {
    character(0)
  }
  instructions_txt = if (length(instructions_status)) {
    paste(
      glue::glue("{vapply(names(instructions_status), agent_instructions_file, character(1))} ({instructions_status})"),
      collapse = ", "
    )
  } else if (instructions) {
    "none"
  } else {
    "skipped"
  }

  project = markermd_project(
    root = root,
    repos = if (has_repos) "repos" else NA_character_,
    comments = if (has_comments) "comments" else NA_character_,
    database = ".markermd/markermd.sqlite",
    key = key,
    artifacts = artifacts,
    created_at = created_at,
    version = markermd_project_version()
  )
  project = write_project_config(project)

  if (is.null(prior)) {
    cli::cli_alert_success("Initialized markermd project at {.path {root}}")
  } else {
    cli::cli_alert_success("Updated markermd project at {.path {root}}")
  }
  cli::cli_bullets(c(
    "*" = "database: {.path {project@database}} ({if (is.null(prior)) 'created' else 'preserved'})",
    skills_lines,
    "*" = "instructions: {instructions_txt}",
    "*" = "repos: {if (has_repos) 'repos/' else 'not found'}",
    "*" = "comments: {if (has_comments) 'comments/' else 'not found'}",
    "*" = "key: {if (is.na(key)) 'not found' else key}",
    "*" = "artifacts: {if (length(artifacts)) paste(artifacts, collapse = ', ') else 'none'}"
  ))
  if (length(unlisted) > 0) {
    cli::cli_bullets(c(
      "i" = "Not recorded as artifact directories: {.path {unlisted}}. Add one with {.code project_set(artifacts = )} or rescan with {.code init_project(rescan_artifacts = TRUE)}."
    ))
  }

  invisible(project)
}

#' Read a markermd project's configuration
#'
#' Loads the `config.yml` written by [init_project()] and returns it as a
#' `markermd_project` object. Errors if the directory has not been initialized.
#'
#' @param path Path to the project directory. Defaults to the working directory.
#'
#' @return A `markermd_project` object.
#' @export
#'
#' @examples
#' src = system.file("examples/test_assignment", package = "markermd")
#' project = fs::dir_copy(src, fs::file_temp("markermd_example_"))
#' init_project(project)
#'
#' project_config(project)
project_config = function(path = ".") {
  root = normalizePath(path, winslash = "/", mustWork = FALSE)
  cfg_path = project_config_path(root)
  if (!fs::file_exists(cfg_path)) {
    cli::cli_abort(c(
      "{.path {path}} is not a markermd project.",
      "x" = "No config found at {.path {cfg_path}}.",
      "i" = "Run {.code markermd::init_project(\"{path}\")} to initialize it."
    ))
  }
  project_from_list(yaml::read_yaml(cfg_path), root)
}

#' Report a markermd project's status
#'
#' Prints a situation report for a markermd project: its root, version and
#' timestamps, the configured locations along with whether they exist (repo
#' and comment counts, artifact directories, the grading database, and the
#' template), and the state of its snapshots (how many, the latest one,
#' whether the grading state changed since it, and when the scores and
#' comments were last exported relative to the last change).
#'
#' @param path Path to the project directory. Defaults to the working directory.
#'
#' @return The project's `markermd_project` object, invisibly.
#' @export
#'
#' @examples
#' src = system.file("examples/test_assignment", package = "markermd")
#' project = fs::dir_copy(src, fs::file_temp("markermd_example_"))
#' init_project(project)
#'
#' project_sitrep(project)
project_sitrep = function(path = ".") {
  project = project_config(path)
  print(project)
  invisible(project)
}

#' Change a markermd project's configuration
#'
#' Updates path entries in a project's `config.yml`. Only supplied arguments are
#' changed; an omitted (`NULL`) argument is left untouched. Paths are interpreted
#' relative to the project root.
#'
#' @param path Path to the project directory. Defaults to the working directory.
#' @param repos Root-relative repos directory to record. Warns if it does not exist.
#' @param comments Root-relative comments directory to record. Warns if it does not exist.
#' @param template A grading template to store in the project database: a
#'   `markermd_template` object or a path to a template YAML file (which is
#'   parsed and imported, so an invalid file errors immediately). Pass `NA` to
#'   clear the stored template. See also [template_import()].
#' @param key Root-relative key (solution) repository directory to record. Warns
#'   if it does not exist. Pass `NA` to clear a previously configured key.
#' @param artifacts Character vector of root-relative directories holding the
#'   students' rendered reports, replacing the recorded list. Warns for any
#'   that does not exist. Pass `NA` or `character(0)` to record none.
#'
#' @return The updated `markermd_project` object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Record the key repository and store a template from YAML
#' project_set("hw01", key = "hw01-key")
#' project_set("hw01", template = "hw01-template.yaml")
#'
#' # Record the directories of rendered reports
#' project_set("hw01", artifacts = c("html", "md"))
#' }
project_set = function(path = ".", repos = NULL, comments = NULL, template = NULL, key = NULL,
                       artifacts = NULL) {
  project = project_config(path)

  if (!is.null(repos)) {
    rel = as.character(repos)
    if (!isTRUE(fs::dir_exists(fs::path(project@root, rel)))) {
      cli::cli_warn("repos directory {.path {rel}} does not exist under the project root; recording it anyway.")
    }
    project@repos = rel
  }
  if (!is.null(comments)) {
    rel = as.character(comments)
    if (!isTRUE(fs::dir_exists(fs::path(project@root, rel)))) {
      cli::cli_warn("comments directory {.path {rel}} does not exist under the project root; recording it anyway.")
    }
    project@comments = rel
  }
  if (!is.null(template)) with_action(project@root, "project_set", call = "project_set(template = ...)", parts = "template", {
    spec = list(trigger = "project_set(template = ...)")
    if (S7::S7_inherits(template, markermd_template)) {
      assert_template_compatible(template)
      source_path = attr(template, "markermd_source_raw")
      if (is.null(source_path)) source_path = attr(template, "markermd_source_path")
      save_template_to_db(project@root, template, source_path = source_path, snapshot = spec)
    } else if (is_clear_sentinel(template)) {
      with_database(project@root, function(conn) {
        DBI::dbBegin(conn)
        snapshot_before_change(conn, project@root, list(trigger = "project_set(template = NA)"))
        delete_metadata(conn, "template")
        DBI::dbCommit(conn)
      })
    } else {
      import_template_yaml_to_db(project@root, as.character(template), snapshot = spec)
    }
  })
  if (!is.null(key)) {
    if (is_clear_sentinel(key)) {
      project@key = NA_character_
    } else {
      rel = as.character(key)
      if (!isTRUE(fs::dir_exists(fs::path(project@root, rel)))) {
        cli::cli_warn("key directory {.path {rel}} does not exist under the project root; recording it anyway.")
      }
      project@key = rel
    }
  }
  if (!is.null(artifacts)) {
    rel = if (is_clear_sentinel(artifacts)) character(0) else as.character(artifacts)
    if (anyNA(rel) || any(!nzchar(rel))) {
      cli::cli_abort("{.arg artifacts} must be directory names, or {.code NA} to record none.")
    }
    rel = unique(sub("/+$", "", rel))
    missing = rel[!fs::dir_exists(fs::path(project@root, rel))]
    if (length(missing) > 0) {
      cli::cli_warn("Artifact director{?y/ies} {.path {missing}} do{?es/} not exist under the project root; recording anyway.")
    }
    project@artifacts = rel
  }

  project = write_project_config(project)
  invisible(project)
}

# Read a template YAML file and store it in the project database. The path is
# used as-is when absolute or already resolvable, otherwise relative to the
# project root. Returns the parsed template invisibly.
#
# root: absolute project root
# path: path to a template YAML file
# snapshot: NULL, or a list with trigger and summary for the automatic
#   snapshot taken before the stored template is overwritten

import_template_yaml_to_db = function(root, path, snapshot = NULL) {
  template_path = resolve_project_file(root, path, "Template")
  template_obj = read_template_yaml(template_path, require_ast = FALSE)
  assert_template_compatible(template_obj)
  changes = save_template_to_db(
    root, template_obj, source_path = attr(template_obj, "markermd_source_raw"), snapshot = snapshot
  )
  report_question_name_changes(changes)
  invisible(template_obj)
}

# Tell the user what a template save did to the grading keyed by question
# name (see reconcile_question_names_conn())
#
# changes: The list save_template_to_db() returns

report_question_name_changes = function(changes) {
  for (i in seq_len(nrow(changes$renamed))) {
    cli::cli_alert_info("Carried the grading of question {.val {changes$renamed$from[i]}} over to its new name {.val {changes$renamed$to[i]}}.")
  }
  for (i in seq_len(nrow(changes$skipped))) {
    cli::cli_warn("Question {.val {changes$skipped$from[i]}} was renamed to {.val {changes$skipped$to[i]}}, which already holds grading; its own grading stays under the old name.")
  }
  if (length(changes$orphaned) > 0) {
    cli::cli_warn(c(
      "Question{?s} {.val {changes$orphaned}} {?is/are} no longer in the template but still hold{?s/} grading, which the app and the exports no longer show.",
      "i" = "Add the question back under the same name to see it again."
    ))
  }
  invisible(changes)
}

# The snapshot spec (trigger plus summary) an import passes down when it
# wants an automatic snapshot, NULL when it does not
#
# snapshot: The import's snapshot argument (TRUE or FALSE)
# trigger: What is running
# summary: The import's summary argument

import_snapshot_spec = function(snapshot, trigger, summary = NULL) {
  if (!rlang::is_bool(snapshot)) {
    cli::cli_abort("{.arg snapshot} must be {.code TRUE} or {.code FALSE}.")
  }
  if (!is.null(summary) && (!is.character(summary) || length(summary) != 1)) {
    cli::cli_abort("{.arg summary} must be a single string.")
  }
  if (snapshot) list(trigger = trigger, summary = summary) else NULL
}

#' Import a grading template from YAML into a project's database
#'
#' Reads a template YAML file and stores it in the project's grading database,
#' which is the canonical store for templates. This is the inverse of
#' [template_export()]. Equivalent to `project_set(project, template = path)`.
#' An automatic snapshot of the state being replaced is taken first (see
#' [snapshot_create()]).
#'
#' @param path Path to a template `.yaml`/`.yml` file. Absolute or relative to
#'   the current directory; a bare relative path is also resolved against the
#'   project root.
#' @param project Path to the project directory. Defaults to the working directory.
#' @param snapshot Whether to take an automatic snapshot before the import.
#'   `FALSE` is for scripts that take their own snapshot once before several
#'   imports.
#' @param summary Optional text recorded on the automatic snapshot: what the
#'   import changes and why.
#'
#' @return The imported `markermd_template` object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' template_import("hw01-template.yaml", project = "hw01")
#' }
template_import = function(path, project = ".", snapshot = TRUE, summary = NULL) {
  proj = project_config(project)
  spec = import_snapshot_spec(snapshot, "template_import()", summary)
  with_action(
    proj@root, "template_import", call = "template_import()", summary = summary, parts = "template",
    import_template_yaml_to_db(proj@root, path, snapshot = spec)
  )
}

#' Export a project's grading template to YAML
#'
#' Writes the template stored in the project's grading database to a YAML file
#' (the optional import/export format). This is the inverse of [template_import()].
#'
#' @param path Output path for the template `.yaml` file.
#' @param project Path to the project directory. Defaults to the working directory.
#' @param snapshot Optional snapshot reference (see [snapshot_info()]): export
#'   the template that snapshot recorded instead of the stored one.
#'
#' @return The output `path`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' template_export("hw01-template.yaml", project = "hw01")
#' }
template_export = function(path, project = ".", snapshot = NULL) {
  proj = project_config(project)
  if (!is.null(snapshot)) {
    part = with_database(proj@root, function(conn) {
      read_snapshot_part(conn, resolve_snapshot_id(conn, snapshot), "template")
    })
    if (is.null(part)) {
      cli::cli_abort("Snapshot {.val {snapshot}} holds no template.")
    }
    yaml::write_yaml(part, path)
    return(invisible(path))
  }
  template_obj = load_template_from_db(proj@root, base_dir = proj@root)
  if (is.null(template_obj)) {
    cli::cli_abort(c(
      "No template is stored in this project's database.",
      "i" = "Author one with {.code template(\"{proj@root}\")} or import one with {.code template_import()}."
    ))
  }
  write_template_yaml(template_obj, path, source_path = attr(template_obj, "markermd_source_raw"))
  invisible(path)
}

# Question names stored in a project, in template order when a template is
# stored, otherwise the distinct names found in the rubric items table
# (sorted). Returns a list with names, a had_template flag, and document (the
# names of the template's document-scope questions; empty without a template).
# Read off the stored template JSON (db_question_names()), so the assignment
# document is not re-parsed.
#
# root: absolute project root

project_question_names = function(root) {
  with_database(root, db_question_names)
}

#' Export a project's grading rubric to YAML
#'
#' Writes the rubric stored in the project's grading database (each question's
#' rubric items, in display order, with its item groups nested around their
#' members, plus its scoring setup when one has been configured) to a YAML
#' file. The file contains no per-repository grading data, so it can be
#' shared, edited by hand or by an LLM tool, and brought back with
#' [rubric_import()]. The file format is described by the JSON Schema at
#' `system.file("schema/markermd-rubric.json", package = "markermd")`.
#'
#' @param path Output path for the rubric `.yaml` file.
#' @param project Path to the project directory. Defaults to the working directory.
#' @param question Optional character vector of question names to export.
#'   Defaults to every question in the project's template.
#' @param snapshot Optional snapshot reference (see [snapshot_info()]): export
#'   the rubric that snapshot recorded instead of the live one.
#'
#' @return The output `path`, invisibly.
#' @seealso [rubric_import()], [template_export()]
#' @export
#'
#' @examples
#' \dontrun{
#' rubric_export("hw01-rubric.yaml", project = "hw01")
#'
#' # Export only some questions
#' rubric_export("hw01-q1.yaml", project = "hw01", question = "Question 1")
#' }
rubric_export = function(path, project = ".", question = NULL, snapshot = NULL) {
  proj = project_config(project)

  if (!is.null(snapshot)) {
    rubric = rubric_from_list(with_database(proj@root, function(conn) {
      read_snapshot_part(conn, resolve_snapshot_id(conn, snapshot), "rubric")
    }))
    names = vapply(rubric$questions, function(q) q$name, character(1))
    selected = restrict_names(
      question, names,
      "Unknown question{?s}: {.val {unknown}}.",
      "The snapshot's questions: {.val {available}}."
    )
    rubric$questions = rubric$questions[names %in% selected]
    write_rubric_yaml(rubric, path)
    return(invisible(path))
  }

  universe = project_question_names(proj@root)
  if (!universe$had_template && length(universe$names) > 0) {
    cli::cli_warn(
      "No template is stored in this project's database; exporting the questions found in the rubric database."
    )
  }
  if (length(universe$names) == 0) {
    cli::cli_abort(c(
      "No rubric data to export.",
      "i" = "Add rubric items in {.code mark()} first, or store a template with {.code template_import()}."
    ))
  }

  selected = restrict_names(
    question, universe$names,
    "Unknown question{?s}: {.val {unknown}}.",
    "This project's questions: {.val {available}}."
  )

  write_rubric_yaml(collect_rubric_data(proj@root, selected), path)
  invisible(path)
}

#' Import a grading rubric from YAML into a project's database
#'
#' Reads a rubric YAML file (see [rubric_export()] for the format) and applies
#' it to the project's grading database. Item order in the file determines
#' display order and keyboard hotkeys (the first ten items of a question get
#' the keys 1-9 and 0; any further items are toggled by clicking, with no
#' limit on their number). This is the inverse of [rubric_export()].
#'
#' @details
#' With `mode = "append"` (the default) the file's items are added after each
#' question's existing items. With `mode = "replace"` each affected question's
#' existing items and item groups are deleted first; this also deletes any
#' recorded selections of those items, for every repository (recoverable
#' only from the automatic snapshot, see below). A question's scoring setup
#' is only updated when the file provides a `scoring` block.
#'
#' Item groups (format `"1.1"`; see [markermd_rubric_group]) are matched to
#' the question's existing groups by label. Items for an existing group are
#' appended to the end of that group and the file's `floor`, `ceiling` and
#' `exclusive` overwrite the stored ones; an unknown label creates a new
#' group at the end of the question. Making an existing group exclusive
#' while some repository has more than one of its items selected aborts
#' before anything is written, listing those repositories.
#'
#' Question names in the file must exactly match question names in the
#' project's stored template; mismatches abort before anything is written.
#'
#' An automatic snapshot of the state being changed is taken in the same
#' transaction as the import (see [snapshot_create()]), so a replace import
#' can be undone with [snapshot_restore()].
#'
#' @param path Path to a rubric `.yaml`/`.yml` file. Absolute or relative to
#'   the current directory; a bare relative path is also resolved against the
#'   project root.
#' @param project Path to the project directory. Defaults to the working directory.
#' @param mode Either `"append"` or `"replace"`; see Details.
#' @param question Optional character vector restricting the import to those
#'   question names within the file.
#' @param snapshot Whether to take an automatic snapshot before the import.
#'   `FALSE` is for scripts that take their own snapshot once before several
#'   imports.
#' @param summary Optional text recorded on the automatic snapshot: what the
#'   import changes and why.
#'
#' @return The parsed rubric list, invisibly.
#' @seealso [rubric_export()], [read_rubric_yaml()], [validate_rubric_file()]
#' @export
#'
#' @examples
#' \dontrun{
#' rubric_import("hw01-rubric.yaml", project = "hw01")
#'
#' # Replace each affected question's existing items, recording why
#' rubric_import("hw01-rubric.yaml", project = "hw01", mode = "replace",
#'   summary = "Feedback review: reworded the Q1 items and capped the examples group")
#' }
rubric_import = function(path, project = ".", mode = c("append", "replace"), question = NULL,
                         snapshot = TRUE, summary = NULL) {
  mode = match.arg(mode)
  proj = project_config(project)
  trigger = if (is.null(question)) {
    glue::glue("rubric_import(mode = '{mode}')")
  } else {
    glue::glue("rubric_import(mode = '{mode}', question = '{paste(question, collapse = \"', '\")}')")
  }
  spec = import_snapshot_spec(snapshot, as.character(trigger), summary)

  rubric_path = resolve_project_file(proj@root, path, "Rubric")

  rubric = read_rubric_yaml(rubric_path)
  yaml_names = vapply(rubric$questions, function(q) q$name, character(1))

  if (!is.null(question)) {
    restrict_names(
      question, yaml_names,
      "Question{cli::qty(unknown)}{?s} {.val {unknown}} not found in the rubric file.",
      "The file contains: {.val {available}}."
    )
    keep = yaml_names %in% question
    rubric$questions = rubric$questions[keep]
    yaml_names = yaml_names[keep]
  }

  if (length(rubric$questions) == 0) {
    cli::cli_abort("The rubric file contains no questions to import.")
  }

  # Validate question names against the stored template before any write, so a
  # typo'd name cannot create rubric rows the mark app would never display
  # (the names are read off the stored JSON; the key document is not re-parsed)
  stored = with_database(proj@root, db_question_names)
  if (stored$had_template) {
    template_names = stored$names
    unknown = setdiff(yaml_names, template_names)
    if (length(unknown) > 0) {
      cli::cli_abort(c(
        "Questions in the rubric file are not in this project's template: {.val {unknown}}.",
        "i" = "Stored template questions: {.val {template_names}}.",
        "i" = "Question names are matched exactly; fix the YAML or update the template."
      ))
    }
  } else {
    cli::cli_warn(
      "No template is stored in this project's database; question names cannot be validated."
    )
  }

  summaries = with_action(
    proj@root, "rubric_import", call = as.character(trigger), summary = summary, parts = "rubric",
    apply_rubric_import(proj@root, rubric, mode, snapshot = spec)
  )

  n_items = sum(vapply(summaries, function(s) length(s$new_ids), integer(1)))
  cli::cli_alert_success(
    "Imported {n_items} rubric item{?s} into {length(summaries)} question{?s} ({mode} mode)."
  )
  for (question_name in names(summaries)) {
    s = summaries[[question_name]]
    action = if (identical(s$mode, "replace")) {
      "{length(s$new_ids)} item{?s} (replaced {s$n_existing} existing)"
    } else {
      "{length(s$new_ids)} item{?s} appended after {s$n_existing} existing"
    }
    scoring_note = if (is.null(s$scoring)) "" else "; scoring updated"
    cli::cli_bullets(c("*" = paste0("{.field {question_name}}: ", action, scoring_note)))
  }

  invisible(rubric)
}

#' Reword or re-point one rubric item
#'
#' Changes the description and/or points of a single stored rubric item in
#' place. The item keeps its id, position, hotkey and group, so every
#' recorded selection of it (and so every repository's marks) carries over to
#' the new wording: unlike replacing the question's rubric with
#' [rubric_import()], nothing has to be re-recorded.
#'
#' @details
#' The item is identified by its current description, matched verbatim as in
#' [marks_set()] and [marks_import()]. A new description must be non-empty
#' and must not equal another item's description in the same question, since
#' descriptions identify items on import. Changing the points changes the
#' score of every repository that has the item selected; use
#' [snapshot_diff()] afterwards to review the effect.
#'
#' An automatic snapshot of the state being changed is taken in the same
#' transaction (see [snapshot_create()]), and the change is recorded in the
#' activity log (see [project_activity()]). A running [mark()] session picks
#' the change up and offers to reload its rubric.
#'
#' @param question Question name from the project's template.
#' @param item Current description of the item, matched verbatim.
#' @param description New description, or `NULL` to keep the current one.
#' @param points New point value, or `NULL` to keep the current one.
#' @param project Path to the project directory. Defaults to the working directory.
#' @param snapshot Whether to take an automatic snapshot before the change.
#' @param summary Optional text recorded on the automatic snapshot and in the
#'   activity log: what the change is and why.
#'
#' @return Invisibly, the updated [markermd_rubric_item].
#' @seealso [rubric_import()], [rubric_export()], [marks_set()]
#' @export
#'
#' @examples
#' \dontrun{
#' rubric_item_set("Document", "Repository check failed",
#'   description = "The repository check did not complete successfully on the final submission",
#'   project = "hw03",
#'   summary = "Feedback review: clearer wording"
#' )
#' }
rubric_item_set = function(question, item, description = NULL, points = NULL, project = ".",
                           snapshot = TRUE, summary = NULL) {
  proj = project_config(project)

  for (field in c("question", "item")) {
    value = get(field)
    if (!is.character(value) || length(value) != 1 || is.na(value) || !nzchar(trimws(value))) {
      cli::cli_abort("{.arg {field}} must be a single non-empty string.")
    }
  }
  if (is.null(description) && is.null(points)) {
    cli::cli_abort("Supply {.arg description}, {.arg points}, or both.")
  }
  if (!is.null(description) &&
      (!is.character(description) || length(description) != 1 || is.na(description) || !nzchar(trimws(description)))) {
    cli::cli_abort("{.arg description} must be a single non-empty string.")
  }
  if (!is.null(points) && (!is.numeric(points) || length(points) != 1 || !is.finite(points))) {
    cli::cli_abort("{.arg points} must be a single finite number.")
  }

  question_names = project_question_names(proj@root)$names
  if (!question %in% question_names) {
    cli::cli_abort(c(
      "Question {.val {question}} is not in this project's template.",
      "i" = "Questions: {.val {question_names}}."
    ))
  }

  items = load_rubric_items(proj@root, question)
  descriptions = vapply(items, function(x) x@description, character(1))
  matched = names(items)[descriptions == item]
  if (length(matched) == 0) {
    cli::cli_abort(c(
      "{.val {question}} has no rubric item described {.val {item}}.",
      "i" = "Descriptions are matched verbatim; list them with {.code rubric_export()}."
    ))
  }
  if (length(matched) > 1) {
    cli::cli_abort("{.val {question}} has {length(matched)} rubric items described {.val {item}}; cannot tell which to change.")
  }
  item_id = matched

  updated = items[[item_id]]
  if (!is.null(description)) {
    if (description %in% descriptions[names(items) != item_id]) {
      cli::cli_abort("{.val {question}} already has another rubric item described {.val {description}}.")
    }
    updated@description = description
  }
  if (!is.null(points)) {
    updated@points = as.numeric(points)
  }
  if (identical(updated@description, items[[item_id]]@description) && identical(updated@points, items[[item_id]]@points)) {
    cli::cli_alert_info("No change: {.val {question}} item {.val {item}} already has that description and points.")
    return(invisible(updated))
  }

  call = as.character(glue::glue("rubric_item_set(question = '{question}', item = '{item}')"))
  spec = import_snapshot_spec(snapshot, call, summary)
  with_action(
    proj@root, "rubric_item_set", call = call, summary = summary, parts = "rubric",
    with_database(proj@root, function(conn) {
      with_transaction(conn, "Failed to update the rubric item", {
        snapshot_before_change(conn, proj@root, spec)
        upsert_items(conn, question, item_id, updated)
      })
    })
  )

  changes = c(
    if (!identical(updated@description, item)) "description",
    if (!identical(updated@points, items[[item_id]]@points)) "points"
  )
  n_selected = with_database(proj@root, function(conn) {
    grades = latest_rows(conn, "grades", c("question_name", "assignment_repo", "item_id"))
    sum(grades$question_name == question & grades$item_id == item_id & grades$selected == 1)
  })
  cli::cli_alert_success(
    "Updated the {paste(changes, collapse = ' and ')} of {.val {question}} item {.val {updated@description}} (selected by {n_selected} repositor{?y/ies})."
  )

  invisible(updated)
}

# Student repository names for a project: the top-level directories under the
# configured repos dir, matching how mark() enumerates repositories. Aborts
# when no repos directory is configured or it does not exist, since marks
# recorded for unenumerable repos would never be shown in mark().
#
# proj: markermd_project object

project_repo_names = function(proj) {
  fs::path_file(fs::dir_ls(project_repos_dir(proj), type = "directory"))
}

# Absolute path of a project's configured repos directory. Aborts when no
# repos directory is configured or the configured directory is missing.
# Shared by mark(), validate_project(), and the marks import/export surface so
# they enumerate repositories identically.
#
# proj: markermd_project object

project_repos_dir = function(proj) {
  if (is.na(proj@repos)) {
    cli::cli_abort(c(
      "No repos directory is configured for this project.",
      "i" = "Record it with {.code project_set(\"{proj@root}\", repos = ...)}."
    ))
  }
  repos_dir = fs::path(proj@root, proj@repos)
  if (!fs::dir_exists(repos_dir)) {
    cli::cli_abort("Configured repos directory does not exist: {.path {repos_dir}}")
  }
  repos_dir
}

# Resolve a user-supplied file path against a project root: absolute paths and
# paths that resolve from the working directory are used as-is, anything else
# is taken as root-relative. Aborts when the file does not exist.
#
# root: absolute project root
# path: user-supplied path
# what: Capitalized noun for the error message (e.g. "Template")

resolve_project_file = function(root, path, what) {
  resolved = if (fs::is_absolute_path(path) || fs::file_exists(path)) {
    path
  } else {
    fs::path(root, path)
  }
  if (!fs::file_exists(resolved)) {
    cli::cli_abort("{what} file does not exist: {.path {resolved}}")
  }
  resolved
}

# Restrict a requested set of names to those available, aborting when any
# requested name is unknown. The message templates are cli strings evaluated
# with {unknown} and {available} in scope, so every call site keeps its exact
# wording.
#
# requested: Character vector of requested names, or NULL for all available
# available: Character vector of known names (result keeps this order)
# unknown_msg: cli template for the error
# hint_msg: cli template for the "i" hint

restrict_names = function(requested, available, unknown_msg, hint_msg) {
  if (is.null(requested)) {
    return(available)
  }
  unknown = setdiff(requested, available)
  if (length(unknown) > 0) {
    cli::cli_abort(c(unknown_msg, "i" = hint_msg), call = parent.frame())
  }
  available[available %in% requested]
}

# Resolve a marks exchange list against a project: validate repository names,
# question names, and rubric item descriptions, check that no exclusive
# group has more than one item selected, and translate each (repo, question)
# entry into the item_id-keyed selections that apply_marks_import() writes.
# All validation happens here, before any write. An entry whose items is
# NULL (only possible via marks_set()) leaves selections untouched and
# carries comments only.
#
# proj: markermd_project object
# marks: Exchange-list marks (read_marks_yaml() shape)
# Returns: List of plan entries (repo, question, selections, comment, private_comment)

resolve_marks_plan = function(proj, marks) {
  repo_universe = project_repo_names(proj)
  with_database(proj@root, function(conn) {
    resolve_marks_plan_conn(conn, repo_universe, db_question_names(conn)$names, marks)
  })
}

# resolve_marks_plan() on an open connection, against the rubric that
# connection sees (the snapshot restore resolves the marks against the
# rubric it has just written in the same transaction)
#
# conn: DBI connection object
# repo_universe: The project's repository names
# question_universe: The project's question names
# marks: Exchange-list marks (read_marks_yaml() shape)

resolve_marks_plan_conn = function(conn, repo_universe, question_universe, marks) {
  all_items = load_all_items(conn)
  all_groups = load_all_item_groups(conn)

  items_cache = list()
  groups_cache = list()
  plan = list()

  for (repo_entry in marks$repos) {
    if (!repo_entry$name %in% repo_universe) {
      cli::cli_abort(c(
        "Repository {.val {repo_entry$name}} is not in this project's repos directory.",
        "i" = "Known repositories: {.val {repo_universe}}."
      ))
    }

    for (question in repo_entry$questions) {
      if (!question$name %in% question_universe) {
        cli::cli_abort(c(
          "Question {.val {question$name}} is not in this project's template.",
          "i" = "This project's questions: {.val {question_universe}}.",
          "i" = "Question names are matched exactly; fix the marks file."
        ))
      }

      selections = NULL
      if (!is.null(question$items)) {
        if (is.null(items_cache[[question$name]])) {
          items_cache[[question$name]] = items_df_to_rubric_list(all_items, question$name)
          groups_cache[[question$name]] = groups_df_to_list(all_groups, all_items, question$name)
        }
        items = items_cache[[question$name]]
        groups = groups_cache[[question$name]]
        descriptions = vapply(items, function(item) item@description, character(1))

        dupes = unique(descriptions[duplicated(descriptions)])
        if (length(dupes) > 0) {
          cli::cli_abort(c(
            "Question {.val {question$name}} has rubric items with duplicate descriptions: {.val {dupes}}.",
            "i" = "Descriptions identify items in a marks file, so they must be unique; edit them in {.code mark()} or reimport the rubric first."
          ))
        }

        unknown = setdiff(question$items, descriptions)
        if (length(unknown) > 0) {
          hint = if (length(descriptions) > 0) {
            c("i" = "The question's rubric items: {.val {descriptions}}.",
              "i" = "Descriptions are matched exactly; copy them verbatim.")
          } else {
            c("i" = "The question has no rubric items; import a rubric with {.code rubric_import()} first.")
          }
          cli::cli_abort(c(
            "Marks for {.val {repo_entry$name}} / {.val {question$name}} list items not in the question's rubric: {.val {unknown}}.",
            hint
          ))
        }

        selections = stats::setNames(descriptions %in% question$items, names(items))

        for (group_id in names(groups)) {
          group = groups[[group_id]]
          if (!isTRUE(group@exclusive)) {
            next
          }
          members = vapply(items, function(item) identical(item@group, group_id), logical(1))
          chosen = descriptions[members & selections]
          if (length(chosen) > 1) {
            cli::cli_abort(c(
              "Marks for {.val {repo_entry$name}} / {.val {question$name}} select {length(chosen)} items from the exclusive group {.val {group@label}}: {.val {chosen}}.",
              "i" = "At most one item of an exclusive group can be selected."
            ))
          }
        }
      }

      plan[[length(plan) + 1]] = list(
        repo = repo_entry$name,
        question = question$name,
        selections = selections,
        comment = question$comment,
        private_comment = question$private_comment
      )
    }
  }

  plan
}

# One-row-per-pair summary data frame for a marks plan
#
# plan: List of plan entries (resolve_marks_plan() shape)
# action: "written" or "skipped", recycled across rows

marks_plan_summary = function(plan, action) {
  data.frame(
    repo = vapply(plan, function(e) e$repo, character(1)),
    question = vapply(plan, function(e) e$question, character(1)),
    action = rep(action, length.out = length(plan)),
    n_selected = vapply(plan, function(e) {
      if (is.null(e$selections)) NA_integer_ else sum(e$selections)
    }, integer(1)),
    stringsAsFactors = FALSE
  )
}

#' Import grading marks from YAML into a project's database
#'
#' Reads a marks YAML file (see [marks_export()] for the format) and records
#' the rubric item selections and comments it describes in the project's
#' grading database, as if a grader had toggled them in [mark()]. Rubric items
#' are identified by their description text, matched verbatim against each
#' question's rubric.
#'
#' @details
#' Each (repository, question) entry is declarative: the listed items are
#' selected and every other rubric item of that question is explicitly
#' deselected, so `items: []` records that no items apply. A pair imported
#' with no selected items and no public comment still shows as ungraded in
#' [mark()] until a human confirms it, by design: machine-written marks are
#' suggestions awaiting review.
#'
#' Pairs that already have any grading activity (any recorded selection event,
#' or a non-empty public or private comment) are skipped and reported unless
#' `overwrite = TRUE`, so an automated pass cannot silently clobber a human's
#' grading. An omitted `comment` / `private_comment` field leaves the stored
#' comment unchanged, while an empty string clears it.
#'
#' All validation (repository names against the project's repos directory,
#' question names against the stored template, item descriptions against each
#' question's rubric, and at most one selected item per exclusive item
#' group) happens before anything is written, and the import itself is a
#' single transaction.
#'
#' When the import re-marks pairs that already have grading activity
#' (`overwrite = TRUE`), an automatic snapshot of the state being replaced is
#' taken in the same transaction (see [snapshot_create()]); an import that
#' only fills empty pairs replaces nothing and takes none.
#'
#' @param path Path to a marks `.yaml`/`.yml` file. Absolute or relative to
#'   the current directory; a bare relative path is also resolved against the
#'   project root.
#' @param project Path to the project directory. Defaults to the working directory.
#' @param overwrite When `TRUE`, pairs with existing grading activity are
#'   re-marked instead of skipped.
#' @param repo Optional character vector restricting the import to those
#'   repository names within the file.
#' @param question Optional character vector restricting the import to those
#'   question names within the file.
#' @param snapshot Whether to take an automatic snapshot before an import
#'   that overwrites existing grading. `FALSE` is for scripts that take their
#'   own snapshot once before several imports.
#' @param summary Optional text recorded on the automatic snapshot: what the
#'   import changes and why.
#'
#' @return Invisibly, a data frame with one row per (repository, question)
#'   pair: columns `repo`, `question`, `action` (`"written"` or `"skipped"`),
#'   and `n_selected`.
#' @seealso [marks_export()], [marks_set()], [read_marks_yaml()],
#'   [validate_marks_file()], [rubric_import()]
#' @export
#'
#' @examples
#' \dontrun{
#' marks_import("hw01-marks.yaml", project = "hw01")
#'
#' # Re-mark pairs that already have grading activity, recording why
#' marks_import("hw01-marks.yaml", project = "hw01", overwrite = TRUE,
#'   summary = "Adversarial re-check of the first-pass grading")
#' }
marks_import = function(path, project = ".", overwrite = FALSE, repo = NULL, question = NULL,
                        snapshot = TRUE, summary = NULL) {
  proj = project_config(project)
  spec = import_snapshot_spec(snapshot, as.character(glue::glue("marks_import(overwrite = {overwrite})")), summary)

  marks_path = resolve_project_file(proj@root, path, "Marks")

  marks = read_marks_yaml(marks_path)

  if (!is.null(repo)) {
    file_repos = vapply(marks$repos, function(r) r$name, character(1))
    restrict_names(
      repo, file_repos,
      "Repositor{cli::qty(unknown)}{?y/ies} {.val {unknown}} not found in the marks file.",
      "The file contains: {.val {available}}."
    )
    marks$repos = marks$repos[file_repos %in% repo]
  }

  if (!is.null(question)) {
    file_questions = unique(unlist(lapply(marks$repos, function(r) {
      vapply(r$questions, function(q) q$name, character(1))
    })))
    restrict_names(
      question, file_questions,
      "Question{cli::qty(unknown)}{?s} {.val {unknown}} not found in the marks file.",
      "The file contains: {.val {available}}."
    )
    marks$repos = lapply(marks$repos, function(r) {
      keep = vapply(r$questions, function(q) q$name %in% question, logical(1))
      r$questions = r$questions[keep]
      r
    })
    marks$repos = marks$repos[vapply(marks$repos, function(r) length(r$questions) > 0, logical(1))]
  }

  plan = resolve_marks_plan(proj, marks)
  if (length(plan) == 0) {
    cli::cli_abort("The marks file contains no marks to import.")
  }

  marked = marked_question_pairs(proj@root)
  # Join on a unit-separator that cannot appear in a repo or question name, so
  # two distinct (repo, question) pairs never collapse to the same key.
  marked_keys = paste(marked$assignment_repo, marked$question_name, sep = "\x1f")
  is_marked = vapply(plan, function(e) paste(e$repo, e$question, sep = "\x1f") %in% marked_keys, logical(1))
  skipped = list()
  if (!overwrite) {
    skipped = plan[is_marked]
    plan = plan[!is_marked]
    is_marked = is_marked[!is_marked]
  }

  if (length(plan) > 0) {
    # Only an import that replaces existing grading needs the snapshot
    with_action(
      proj@root, "marks_import", call = as.character(glue::glue("marks_import(overwrite = {overwrite})")),
      summary = summary,
      apply_marks_import(proj@root, plan, snapshot = if (any(is_marked)) spec else NULL)
    )
  }

  n_selected = sum(vapply(plan, function(e) sum(e$selections), integer(1)))
  cli::cli_alert_success(
    "Imported marks for {length(plan)} repository/question pair{?s} ({n_selected} item selection{?s})."
  )
  if (length(skipped) > 0) {
    skipped_labels = vapply(skipped, function(e) paste0(e$repo, "/", e$question), character(1))
    cli::cli_bullets(c(
      "!" = "Skipped {length(skipped)} pair{?s} with existing grading activity: {.val {skipped_labels}}.",
      "i" = "Re-run with {.code overwrite = TRUE} to re-mark them."
    ))
  }

  invisible(rbind(
    marks_plan_summary(plan, "written"),
    marks_plan_summary(skipped, "skipped")
  ))
}

#' Export a project's grading marks to YAML
#'
#' Writes the per-repository grading state stored in the project's grading
#' database (each repository/question pair's selected rubric items, identified
#' by description, plus public and private comments) to a YAML file. Only
#' pairs with grading activity are written; a pair whose items were all
#' deselected exports as `items: []`. The file can be edited and brought back
#' with [marks_import()], and its format is described by the JSON Schema at
#' `system.file("schema/markermd-marks.json", package = "markermd")`.
#'
#' @param path Output path for the marks `.yaml` file.
#' @param project Path to the project directory. Defaults to the working directory.
#' @param repo Optional character vector of repository names to export.
#' @param question Optional character vector of question names to export.
#' @param snapshot Optional snapshot reference (see [snapshot_info()]): export
#'   the marks that snapshot recorded instead of the live ones.
#'
#' @return The output `path`, invisibly.
#' @seealso [marks_import()], [rubric_export()]
#' @export
#'
#' @examples
#' \dontrun{
#' marks_export("hw01-marks.yaml", project = "hw01")
#' }
marks_export = function(path, project = ".", repo = NULL, question = NULL, snapshot = NULL) {
  proj = project_config(project)

  if (!is.null(snapshot)) {
    marks = marks_from_list(marks_without_ids(with_database(proj@root, function(conn) {
      read_snapshot_part(conn, resolve_snapshot_id(conn, snapshot), "marks")
    })))
    repo_names = vapply(marks$repos, function(r) r$name, character(1))
    repo_names = restrict_names(
      repo, repo_names,
      "No marks recorded for repositor{cli::qty(unknown)}{?y/ies} {.val {unknown}}.",
      "The snapshot's repositories with marks: {.val {available}}."
    )
    marks$repos = Filter(function(r) r$name %in% repo_names, marks$repos)
    if (!is.null(question)) {
      marks$repos = lapply(marks$repos, function(r) {
        r$questions = Filter(function(q) q$name %in% question, r$questions)
        r
      })
      marks$repos = Filter(function(r) length(r$questions) > 0, marks$repos)
    }
    write_marks_yaml(marks, path)
    return(invisible(path))
  }

  marked = marked_question_pairs(proj@root)

  repo_names = restrict_names(
    repo, sort(unique(marked$assignment_repo)),
    "No marks recorded for repositor{cli::qty(unknown)}{?y/ies} {.val {unknown}}.",
    "Repositories with marks: {.val {available}}."
  )

  template_order = project_question_names(proj@root)$names
  marked_questions = unique(marked$question_name)
  question_names = c(
    template_order[template_order %in% marked_questions],
    sort(setdiff(marked_questions, template_order))
  )
  question_names = restrict_names(
    question, question_names,
    "No marks recorded for question{cli::qty(unknown)}{?s} {.val {unknown}}.",
    "Questions with marks: {.val {available}}."
  )

  marks = collect_marks_data(proj@root, repo_names = repo_names, question_names = question_names)
  if (length(marks$repos) == 0) {
    cli::cli_abort(c(
      "No marks to export.",
      "i" = "Record marks in {.code mark()} or with {.code marks_import()} first."
    ))
  }

  write_marks_yaml(marks, path)
  invisible(path)
}

#' Record grading marks for one repository/question pair
#'
#' Programmatically marks a single (repository, question) pair in the
#' project's grading database, without going through a marks YAML file: the
#' one-off counterpart to [marks_import()]. Rubric items are identified by
#' their description text, matched verbatim.
#'
#' @details
#' `items` is declarative: the listed items are selected and every other
#' rubric item of the question is deselected, so `character(0)` records that
#' no items apply. `items = NULL` leaves the pair's selections untouched and
#' only writes the supplied comments. At least one of `items`, `comment`, or
#' `private_comment` must be supplied.
#'
#' Unlike [marks_import()], which skips pairs that already have grading
#' activity, this targeted setter errors on such a pair unless
#' `overwrite = TRUE`, so a script cannot believe a write happened when it was
#' ignored.
#'
#' @param repo Repository name (a directory under the project's repos directory).
#' @param question Question name from the project's template.
#' @param items Character vector of rubric item descriptions to select
#'   (`character(0)` to deselect everything), or `NULL` to leave selections
#'   unchanged.
#' @param comment Public, student-facing comment; an empty string clears the
#'   stored comment, `NULL` leaves it unchanged.
#' @param private_comment Private grader note, never shown to students; an
#'   empty string clears it, `NULL` leaves it unchanged.
#' @param project Path to the project directory. Defaults to the working directory.
#' @param overwrite When `TRUE`, a pair with existing grading activity is
#'   re-marked instead of raising an error.
#' @param summary Optional text recorded in the project's activity log (see
#'   [project_activity()]): what the change is and why.
#'
#' @return Invisibly, a one-row data frame with columns `repo`, `question`,
#'   `action` (`"written"`), and `n_selected` (`NA` when `items` is `NULL`).
#' @seealso [marks_import()], [marks_export()]
#' @export
#'
#' @examples
#' \dontrun{
#' marks_set("student1-hw01", "Question 2",
#'   items = "Looks good",
#'   private_comment = "Quartiles and summary statistics all present.",
#'   project = "hw01"
#' )
#' }
marks_set = function(repo, question, items = NULL, comment = NULL, private_comment = NULL,
                     project = ".", overwrite = FALSE, summary = NULL) {
  proj = project_config(project)

  if (!is.character(repo) || length(repo) != 1 || !nzchar(trimws(repo))) {
    cli::cli_abort("{.arg repo} must be a single repository name.")
  }
  if (!is.character(question) || length(question) != 1 || !nzchar(trimws(question))) {
    cli::cli_abort("{.arg question} must be a single question name.")
  }
  if (is.null(items) && is.null(comment) && is.null(private_comment)) {
    cli::cli_abort("Supply at least one of {.arg items}, {.arg comment}, or {.arg private_comment}.")
  }
  if (!is.null(items)) {
    dupes = unique(items[duplicated(items)])
    if (length(dupes) > 0) {
      cli::cli_abort("{.arg items} contains duplicate descriptions: {.val {dupes}}.")
    }
  }
  for (field in c("comment", "private_comment")) {
    value = get(field)
    if (!is.null(value) && (!is.character(value) || length(value) != 1)) {
      cli::cli_abort("{.arg {field}} must be a single string.")
    }
  }

  marks = list(repos = list(list(
    name = repo,
    questions = list(list(
      name = question,
      items = items,
      comment = comment,
      private_comment = private_comment
    ))
  )))
  plan = resolve_marks_plan(proj, marks)

  if (!overwrite) {
    marked = marked_question_pairs(proj@root)
    if (any(marked$assignment_repo == repo & marked$question_name == question)) {
      activity = c(
        if (length(load_grade_selections(proj@root, question, repo)) > 0) "recorded selections",
        if (!is.null(load_comment(proj@root, question, repo))) "a public comment",
        if (!is.null(load_private_comment(proj@root, question, repo))) "a private comment"
      )
      cli::cli_abort(c(
        "{.val {repo}} / {.val {question}} already has grading activity ({activity}).",
        "i" = "Pass {.code overwrite = TRUE} to re-mark it."
      ))
    }
  }

  if (!is.null(summary) && (!is.character(summary) || length(summary) != 1)) {
    cli::cli_abort("{.arg summary} must be a single string.")
  }
  with_action(
    proj@root, "marks_set", call = as.character(glue::glue("marks_set(repo = '{repo}', question = '{question}')")),
    summary = summary,
    apply_marks_import(proj@root, plan)
  )

  n_selected = if (is.null(items)) "no selection changes" else "{sum(plan[[1]]$selections)} item{?s} selected"
  cli::cli_alert_success(paste0("Marked {.val {repo}} / {.val {question}} (", n_selected, ")."))

  invisible(marks_plan_summary(plan, "written"))
}

#' Export per-repository scores to a CSV file
#'
#' Computes every student repository's per-question scores from the project's
#' grading database and writes them to `scores.csv` in the project root, one
#' row per repository with one column per question plus a `total` column.
#' Scores are recomputed the same way [mark()] displays them: the points of
#' the selected rubric items are summed (each item group's combined points
#' first clamped to its floor and ceiling) and passed through the question's
#' grading mode and score bounds. A repository/question pair that is not yet
#' graded (no selected rubric item and no public comment) is written as `NA`,
#' and a repository's `total` stays `NA` until all of its questions are
#' graded.
#'
#' @param project Path to the project directory. Defaults to the working directory.
#'
#' @return The path of the written CSV file, invisibly.
#' @seealso [export_comments()], [export_marks()], [marks_export()]
#' @export
#'
#' @examples
#' \dontrun{
#' export_scores("hw01")
#' }
export_scores = function(project = ".") {
  proj = project_config(project)

  repo_names = project_repo_names(proj)
  if (length(repo_names) == 0) {
    cli::cli_abort("No student repositories found under {.path {fs::path(proj@root, proj@repos)}}.")
  }

  question_names = project_question_names(proj@root)$names
  if (length(question_names) == 0) {
    cli::cli_abort(c(
      "No questions found in this project.",
      "i" = "Store a template with {.code template_import()} or author one with {.code template()} first."
    ))
  }
  reserved = intersect(question_names, c("repo", "total"))
  if (length(reserved) > 0) {
    cli::cli_abort(
      "The question name{?s} {.val {reserved}} cannot be exported: {.path scores.csv} reserves the {.field repo} and {.field total} columns."
    )
  }

  scores = collect_score_data(proj@root, question_names, repo_names)

  # The same wide table the Assignments tab shows (build_scores_table_data());
  # its Total is NA until every question is graded
  table = build_scores_table_data(scores, question_names, repo_names)
  result = data.frame(repo = table$Repository, stringsAsFactors = FALSE, check.names = FALSE)
  for (question_name in question_names) {
    result[[question_name]] = table[[question_name]]
  }
  result$total = table$Total

  n_ungraded = sum(is.na(as.matrix(result[question_names])))
  if (n_ungraded == nrow(result) * length(question_names)) {
    cli::cli_abort(c(
      "No scores to export: no repository/question pair has been graded.",
      "i" = "Grade in {.code mark()} or record marks with {.code marks_import()} first."
    ))
  }

  csv_path = fs::path(proj@root, "scores.csv")
  utils::write.csv(result, csv_path, row.names = FALSE)
  record_export_note(proj@root, "scores", csv_path)

  cli::cli_alert_success("Wrote scores for {nrow(result)} repositor{?y/ies} to {.path {csv_path}}.")
  if (n_ungraded > 0) {
    cli::cli_bullets(c(
      "!" = "{n_ungraded} ungraded repository/question pair{?s} exported as NA."
    ))
  }

  invisible(csv_path)
}

#' Export student-facing feedback to per-repository markdown files
#'
#' Writes each student repository's public feedback to
#' `<comments>/<repo>.md` under the project root, using the project's
#' configured comments directory (`comments/` when none is configured; it is
#' created when missing). Each graded question appears as a heading, in
#' template order, followed by a bulleted markdown list of its selected
#' rubric item descriptions and its public comment. Document-scope
#' questions (feedback about the document as a whole) come first, ahead of
#' the per-question sections. Private comments are never exported, and a
#' repository with no public feedback gets no file.
#'
#' Item groups never change the default output: selected items are listed
#' as they are and a group's floor or ceiling is never mentioned, in keeping
#' with the rule that student-facing text does not describe grading
#' mechanics. With `group_headings = TRUE` the selected items that belong to
#' a group are written under a `### <label>` subheading beneath the
#' question's heading, in display order; the question's ungrouped items and
#' its public comment come first, without a subheading, and a group with no
#' selected items gets none. Group labels then reach students, so choose
#' them accordingly.
#'
#' @param project Path to the project directory. Defaults to the working directory.
#' @param group_headings Whether to write each item group's selected items
#'   under a subheading carrying the group's label. Defaults to `FALSE`.
#'
#' @return The paths of the written markdown files, invisibly.
#' @seealso [export_scores()], [export_marks()], [marks_export()]
#' @export
#'
#' @examples
#' \dontrun{
#' export_comments("hw01")
#'
#' # Group the feedback under the rubric's item group labels
#' export_comments("hw01", group_headings = TRUE)
#' }
export_comments = function(project = ".", group_headings = FALSE) {
  proj = project_config(project)
  if (!rlang::is_bool(group_headings)) {
    cli::cli_abort("{.arg group_headings} must be {.code TRUE} or {.code FALSE}.")
  }

  repo_names = project_repo_names(proj)
  if (length(repo_names) == 0) {
    cli::cli_abort("No student repositories found under {.path {fs::path(proj@root, proj@repos)}}.")
  }

  question_info = project_question_names(proj@root)
  # Document-level feedback reads first, before the per-question sections
  question_names = c(
    intersect(question_info$names, question_info$document),
    setdiff(question_info$names, question_info$document)
  )
  if (length(question_names) == 0) {
    cli::cli_abort(c(
      "No questions found in this project.",
      "i" = "Store a template with {.code template_import()} or author one with {.code template()} first."
    ))
  }

  marks = collect_marks_data(proj@root, repo_names = repo_names, question_names = question_names)
  questions_by_repo = stats::setNames(
    lapply(marks$repos, function(r) r$questions),
    vapply(marks$repos, function(r) r$name, character(1))
  )

  # Each question's items with their group ids and its groups in display
  # order, for placing selected items under their group's subheading
  rubric_by_question = if (group_headings) {
    rubric = collect_rubric_data(proj@root, question_names)
    stats::setNames(rubric$questions, vapply(rubric$questions, function(q) q$name, character(1)))
  }

  comments_rel = if (is.na(proj@comments)) "comments" else proj@comments
  comments_dir = fs::path(proj@root, comments_rel)
  fs::dir_create(comments_dir)

  # A markdown list item from possibly multiline text: continuation lines are
  # indented so they stay inside the bullet
  as_bullet = function(text) {
    lines = strsplit(text, "\n", fixed = TRUE)[[1]]
    if (length(lines) == 0) {
      lines = ""
    }
    paste0(c("- ", rep("  ", length(lines) - 1)), lines)
  }

  # The feedback for one question: its heading, then the bullets for its
  # ungrouped (or, by default, all) selected items and its comment, then
  # with group_headings one subsection per group with a selected item. Every
  # part ends in a blank line; NULL when the question has nothing to say.
  question_section = function(q) {
    items = as.character(q$items)
    group_of = rep(NA_character_, length(items))
    groups = list()
    if (group_headings && !is.null(rubric_by_question[[q$name]])) {
      rubric_q = rubric_by_question[[q$name]]
      by_description = stats::setNames(
        vapply(rubric_q$items, function(item) item@group, character(1)),
        vapply(rubric_q$items, function(item) item@description, character(1))
      )
      group_of = unname(by_description[items])
      groups = rubric_q$groups
    }

    top = unlist(lapply(items[is.na(group_of)], as_bullet))
    if (!is.null(q$comment)) {
      top = c(top, as_bullet(q$comment))
    }
    parts = list()
    if (length(top) > 0) {
      parts[[length(parts) + 1]] = top
    }
    for (group_id in names(groups)) {
      members = items[!is.na(group_of) & group_of == group_id]
      if (length(members) == 0) {
        next
      }
      parts[[length(parts) + 1]] = c(
        paste0("### ", groups[[group_id]]@label), "",
        unlist(lapply(members, as_bullet))
      )
    }
    if (length(parts) == 0) {
      return(NULL)
    }
    c(paste0("## ", q$name), "", unlist(lapply(parts, function(part) c(part, ""))))
  }

  written = character(0)
  skipped = character(0)
  for (repo_name in repo_names) {
    sections = list()
    for (q in questions_by_repo[[repo_name]]) {
      section = question_section(q)
      if (!is.null(section)) {
        sections[[length(sections) + 1]] = section
      }
    }

    if (length(sections) == 0) {
      skipped = c(skipped, repo_name)
      next
    }

    lines = unlist(sections)
    md_path = fs::path(comments_dir, paste0(repo_name, ".md"))
    writeLines(lines[-length(lines)], md_path)
    written = c(written, md_path)
  }

  if (length(written) == 0) {
    cli::cli_abort(c(
      "No feedback to export.",
      "i" = "Select rubric items or write public comments in {.code mark()} first."
    ))
  }
  record_export_note(proj@root, "comments", comments_dir)

  cli::cli_alert_success("Wrote feedback for {length(written)} repositor{?y/ies} to {.path {comments_dir}}.")
  if (length(skipped) > 0) {
    cli::cli_bullets(c(
      "!" = "Skipped {length(skipped)} repositor{?y/ies} with no public feedback: {.val {skipped}}."
    ))
  }

  invisible(written)
}

#' Export scores and feedback for a graded project
#'
#' Runs [export_scores()] and [export_comments()] in one call: the final
#' hand-off step of a grading project, writing `scores.csv` and the
#' per-repository feedback files from the project's grading database.
#'
#' @param project Path to the project directory. Defaults to the working directory.
#' @param group_headings Passed to [export_comments()]: whether to write each
#'   item group's selected items under a subheading carrying the group's
#'   label. Defaults to `FALSE`.
#'
#' @return Invisibly, a list with elements `scores` (the CSV path) and
#'   `comments` (the feedback file paths).
#' @seealso [export_scores()], [export_comments()]
#' @export
#'
#' @examples
#' \dontrun{
#' export_marks("hw01")
#' }
export_marks = function(project = ".", group_headings = FALSE) {
  scores = export_scores(project)
  comments = export_comments(project, group_headings = group_headings)
  invisible(list(scores = scores, comments = comments))
}

# Basename of a path string, or NULL for a NULL/empty input.
#
# x: a path string or NULL

basename_or_null = function(x) {
  if (is.null(x) || !nzchar(x)) NULL else basename(x)
}

# Locate a repository's assignment file: prefer one whose basename matches
# assignment_file, otherwise a document at the repo root, otherwise the first
# file matching the extension pattern. Returns NA_character_ when none is found.
#
# repo_dir: a student repository directory
# assignment_file: expected assignment basename (e.g. "hw1.qmd"), or NULL
# ext: regex matching the assignment extension

find_repo_assignment = function(repo_dir, assignment_file, ext) {
  files = list.files(repo_dir, pattern = ext, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    return(NA_character_)
  }
  if (!is.null(assignment_file)) {
    match = files[basename(files) == assignment_file]
    if (length(match) >= 1) {
      return(match[1])
    }
  }
  # Prefer a document at the repo root over one nested in a subdirectory:
  # recursive listing sorts some subdirectory paths ahead of root-level files.
  root_files = files[
    normalizePath(dirname(files), mustWork = FALSE) == normalizePath(repo_dir, mustWork = FALSE)
  ]
  if (length(root_files) >= 1) {
    return(root_files[1])
  }
  files[1]
}

#' Validate a project's student repositories against its template
#'
#' Loads the project's grading template and validates every student repository
#' (discovered from the project config's `repos` directory) against the
#' template's questions and rules. This is the same section-based matching used
#' during grading, run headlessly so a freshly scaffolded template can be
#' sanity-checked: a rule that fails across most repositories is usually too
#' strict and should be relaxed in `template()`.
#'
#' Each repository's assignment file is matched by the basename recorded in the
#' template's `source.path` (falling back to the first matching document), so the
#' template's heading/div anchors line up with the student documents.
#'
#' @param path Path to the project directory. Defaults to the working directory.
#' @param template Optional override for the template stored in the project
#'   database: a path to a template `.yaml` file or a `markermd_template` object.
#'   When omitted, the database-stored template is used (and an error is raised
#'   if none has been stored).
#' @param use_qmd Logical. Match `.qmd` files (TRUE) or `.Rmd` files (FALSE).
#'
#' @return A data frame with one row per repository/question: columns `repo`,
#'   `question`, `status` (`"pass"`, `"fail"`, or `"error"`), and `detail`. A repo
#'   whose assignment cannot be found or parsed yields a single `"error"` row.
#' @export
#'
#' @examples
#' \dontrun{
#' # Check every student repository against the stored template
#' validate_project("hw01")
#'
#' # Validate against a template YAML instead of the stored one
#' validate_project("hw01", template = "hw01-template.yaml")
#' }
validate_project = function(path = ".", template = NULL, use_qmd = TRUE) {
  project = project_config(path)

  template_obj = if (S7::S7_inherits(template, markermd_template)) {
    template
  } else if (is.character(template) && length(template) == 1) {
    if (!fs::file_exists(template)) {
      cli::cli_abort("Template file does not exist: {.path {template}}")
    }
    read_template_yaml(template, require_ast = FALSE)
  } else if (!is.null(template)) {
    cli::cli_abort("{.arg template} must be a template file path or a markermd_template object.")
  } else {
    load_template_from_db(project@root, base_dir = project@root)
  }
  if (is.null(template_obj)) {
    cli::cli_abort(c(
      "No template is configured for this project.",
      "i" = "Author one with {.code template(\"{project@root}\")}, import one with {.code template_import()}, or pass {.arg template}."
    ))
  }
  assert_template_compatible(template_obj)

  repos_dir = project_repos_dir(project)

  assignment_file = basename_or_null(attr(template_obj, "markermd_source_raw"))
  ext = if (use_qmd) "\\.qmd$" else "\\.Rmd$"

  repo_dirs = fs::dir_ls(repos_dir, type = "directory")
  rows = list()

  for (repo_dir in repo_dirs) {
    repo = fs::path_file(repo_dir)
    file = find_repo_assignment(repo_dir, assignment_file, ext)

    if (is.na(file)) {
      rows[[length(rows) + 1]] = data.frame(
        repo = repo, question = NA_character_, status = "error",
        detail = "No assignment file found", stringsAsFactors = FALSE
      )
      next
    }

    res = purrr::safely(function() validate_repo_against_rules(parse_assignment_document(file), template_obj, repo_dir = repo_dir))()

    if (!is.null(res$error)) {
      rows[[length(rows) + 1]] = data.frame(
        repo = repo, question = NA_character_, status = "error",
        detail = conditionMessage(res$error), stringsAsFactors = FALSE
      )
      next
    }

    for (qn in names(res$result)) {
      r = res$result[[qn]]
      rows[[length(rows) + 1]] = data.frame(
        repo = repo, question = qn, status = r$status,
        detail = paste(r$messages, collapse = "; "), stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      repo = character(0), question = character(0),
      status = character(0), detail = character(0), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}


#' Check that every student repository's assignment document parses
#'
#' Locates each student repository's assignment document (discovered from the
#' project config's `repos` directory and matched by the stored template's
#' `source.path` basename when a template exists) and parses it with
#' [parse_assignment_document()], reporting the outcome per repository. The
#' q2r parser is strict, so small markdown slips in a submission (a stray `$`
#' or `{`, an unclosed quote, emphasis mark, or bracket, malformed YAML front
#' matter) fail the parse and leave the repository ungradable until repaired.
#' This headless check finds them and returns the parser's diagnostic
#' verbatim, which is what the bundled `markermd-fix-parsing` skill works
#' from; unlike [validate_project()] it needs no stored template.
#'
#' @param path Path to an initialized markermd project directory.
#' @param use_qmd Logical. Match `.qmd` files (`TRUE`, the default) or `.Rmd`
#'   files (`FALSE`).
#' @return A data frame with one row per repository and columns `repo`, `file`
#'   (path of the document that was parsed, `NA` when none was found), `status`
#'   (`"ok"`, `"warning"`, `"error"`, or `"missing"`), and `detail` (the
#'   parser's diagnostics with ANSI styling stripped, or `NA` when there are
#'   none).
#' @examples
#' \dontrun{
#' res = parse_project("hw01")
#' res[res$status != "ok", ]
#' }
#' @export
parse_project = function(path = ".", use_qmd = TRUE) {
  project = project_config(path)
  repos_dir = project_repos_dir(project)

  stored = with_database(project@root, db_template_list)
  assignment_file = basename_or_null(stored$source$path)
  ext = if (use_qmd) "\\.qmd$" else "\\.Rmd$"

  parse_captured = purrr::safely(parse_assignment_document)

  rows = lapply(fs::dir_ls(repos_dir, type = "directory"), function(repo_dir) {
    repo = fs::path_file(repo_dir)
    file = find_repo_assignment(repo_dir, assignment_file, ext)

    if (is.na(file)) {
      return(data.frame(
        repo = repo, file = NA_character_, status = "missing",
        detail = "No assignment file found", stringsAsFactors = FALSE
      ))
    }

    res = parse_captured(file)
    if (!is.null(res$error)) {
      status = "error"
      detail = conditionMessage(res$error)
    } else if (length(document_parse_warnings(res$result)) > 0) {
      status = "warning"
      detail = paste(format_parse_diagnostics(document_parse_warnings(res$result)), collapse = "\n")
    } else {
      status = "ok"
      detail = NA_character_
    }

    data.frame(
      repo = repo, file = as.character(file), status = status,
      detail = cli::ansi_strip(detail), stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0) {
    return(data.frame(
      repo = character(0), file = character(0),
      status = character(0), detail = character(0), stringsAsFactors = FALSE
    ))
  }
  out = do.call(rbind, rows)
  rownames(out) = NULL
  out
}
