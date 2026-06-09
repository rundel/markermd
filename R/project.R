# Project management for ghclass::org_grade_assignment() layouts.
#
# init_project() bootstraps a grading project: it keeps all markermd state under
# a single .markermd/ directory at the project root (the SQLite grading DB and a
# YAML config), and installs the bundled Claude Code skills into the project's
# .claude/skills/ so they are discoverable. project_config()/project_sitrep()
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

# Copy every bundled skill directory into <path>/.claude/skills/<skill>,
# overwriting any existing copy so the project's skills track the installed
# package. The explicit per-skill target avoids fs::dir_copy() flattening a
# skill's contents into .claude/skills/ directly.
#
# path: project root
# Returns: character vector of skill names copied

install_project_skills = function(path) {
  src = markermd_skills_path()
  if (!nzchar(src) || !fs::dir_exists(src)) {
    return(character(0))
  }

  skill_dirs = fs::dir_ls(src, type = "directory")
  if (length(skill_dirs) == 0) {
    return(character(0))
  }

  dest_root = fs::path(path, ".claude", "skills")
  fs::dir_create(dest_root)

  names = fs::path_file(skill_dirs)
  for (i in seq_along(skill_dirs)) {
    fs::dir_copy(skill_dirs[[i]], fs::path(dest_root, names[[i]]), overwrite = TRUE)
  }
  names
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
    database = if (is.null(paths$database)) ".markermd/markermd.sqlite" else as.character(paths$database),
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
#' recording the project's file locations, and installs the bundled Claude Code
#' skills into `<path>/.claude/skills/` so they are discoverable from the
#' project.
#'
#' The key (solution) repository is detected as the top-level git repository
#' (student repositories live under `repos/`); when several top-level repos
#' exist, the one whose name contains "key" is used. Remaining top-level
#' directories that are not git repositories are recorded as artifact
#' directories.
#'
#' Re-running `init_project()` on an already-initialized directory updates it in
#' place: the existing grading database (including any stored grading template)
#' is preserved, the skills are refreshed to the packaged version, the key and
#' artifact directories are re-scanned, and a previously configured key is kept
#' (or cleared if it no longer exists).
#'
#' @param path Path to the project directory.
#'
#' @return A `markermd_project` object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' ghclass::org_grade_assignment("hw01", org = "my-course", repo_filter = "hw01-")
#' init_project("hw01")
#' }
init_project = function(path) {
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

  artifacts = candidates[!is_repo]
  if (!is.na(key)) {
    artifacts = setdiff(artifacts, key)
  }

  conn = initialize_database(root)
  close_database(conn)

  skills = install_project_skills(root)

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
    "*" = "skills: {if (length(skills)) paste(skills, collapse = ', ') else 'none'} (.claude/skills/)",
    "*" = "repos: {if (has_repos) 'repos/' else 'not found'}",
    "*" = "comments: {if (has_comments) 'comments/' else 'not found'}",
    "*" = "key: {if (is.na(key)) 'not found' else key}",
    "*" = "artifacts: {if (length(artifacts)) paste(artifacts, collapse = ', ') else 'none'}"
  ))

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
#' timestamps, and the configured locations along with whether they exist (repo
#' and comment counts, artifact directories, the grading database, and the
#' template).
#'
#' @param path Path to the project directory. Defaults to the working directory.
#'
#' @return The project's `markermd_project` object, invisibly.
#' @export
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
#'
#' @return The updated `markermd_project` object, invisibly.
#' @export
project_set = function(path = ".", repos = NULL, comments = NULL, template = NULL, key = NULL) {
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
  if (!is.null(template)) {
    if (S7::S7_inherits(template, markermd_template)) {
      assert_template_compatible(template)
      source_path = attr(template, "markermd_source_raw")
      if (is.null(source_path)) source_path = attr(template, "markermd_source_path")
      save_template_to_db(project@root, template, source_path = source_path)
    } else if (is_clear_sentinel(template)) {
      with_database(project@root, function(conn) delete_metadata(conn, "template"))
    } else {
      import_template_yaml_to_db(project@root, as.character(template))
    }
  }
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

  project = write_project_config(project)
  invisible(project)
}

# Read a template YAML file and store it in the project database. The path is
# used as-is when absolute or already resolvable, otherwise relative to the
# project root. Returns the parsed template invisibly.
#
# root: absolute project root
# path: path to a template YAML file

import_template_yaml_to_db = function(root, path) {
  template_path = if (fs::is_absolute_path(path) || fs::file_exists(path)) {
    path
  } else {
    fs::path(root, path)
  }
  if (!fs::file_exists(template_path)) {
    cli::cli_abort("Template file does not exist: {.path {template_path}}")
  }
  template_obj = read_template_yaml(template_path, require_ast = FALSE)
  assert_template_compatible(template_obj)
  save_template_to_db(root, template_obj, source_path = attr(template_obj, "markermd_source_raw"))
  invisible(template_obj)
}

#' Import a grading template from YAML into a project's database
#'
#' Reads a template YAML file and stores it in the project's grading database,
#' which is the canonical store for templates. This is the inverse of
#' [template_export()]. Equivalent to `project_set(project, template = path)`.
#'
#' @param path Path to a template `.yaml`/`.yml` file. Absolute or relative to
#'   the current directory; a bare relative path is also resolved against the
#'   project root.
#' @param project Path to the project directory. Defaults to the working directory.
#'
#' @return The imported `markermd_template` object, invisibly.
#' @export
template_import = function(path, project = ".") {
  proj = project_config(project)
  import_template_yaml_to_db(proj@root, path)
}

#' Export a project's grading template to YAML
#'
#' Writes the template stored in the project's grading database to a YAML file
#' (the optional import/export format). This is the inverse of [template_import()].
#'
#' @param path Output path for the template `.yaml` file.
#' @param project Path to the project directory. Defaults to the working directory.
#'
#' @return The output `path`, invisibly.
#' @export
template_export = function(path, project = ".") {
  proj = project_config(project)
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

# Basename of a path string, or NULL for a NULL/empty input.
#
# x: a path string or NULL

basename_or_null = function(x) {
  if (is.null(x) || !nzchar(x)) NULL else basename(x)
}

# Locate a repository's assignment file: prefer one whose basename matches
# assignment_file, otherwise the first file matching the extension pattern.
# Returns NA_character_ when none is found.
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

  if (is.na(project@repos)) {
    cli::cli_abort(c(
      "No repos directory is configured for this project.",
      "i" = "Record it with {.code project_set(repos = ...)}."
    ))
  }
  repos_dir = fs::path(project@root, project@repos)
  if (!fs::dir_exists(repos_dir)) {
    cli::cli_abort("Configured repos directory does not exist: {.path {repos_dir}}")
  }

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

    res = purrr::safely(function() validate_repo_against_rules(parse_assignment_document(file), template_obj))()

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
