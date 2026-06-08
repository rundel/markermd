#' S7 Class Definition for Markermd Projects
#'
#' @description S7 class representing a markermd grading project: a directory
#' (typically produced by `ghclass::org_grade_assignment()`) whose markermd
#' state lives under a `.markermd/` directory. The object records the locations
#' of the project's pieces relative to its root so it stays portable.
#'
#' @name project_classes
NULL

# Current project config format version. Bumped whenever the persisted
# .markermd/config.yml shape changes incompatibly. assert_project_compatible()
# rejects configs written by a newer markermd than this.

markermd_project_version = function() "1.0"

#' @title Markermd Project
#' @description S7 class describing an initialized markermd project. Path
#'   properties are stored relative to `root` (resolved to absolute paths at
#'   read time); `root` itself is the absolute project directory and is not
#'   serialized to the config file. Timestamps are stored as strings (matching
#'   the database layer's `get_current_timestamp()` format) so they round-trip
#'   through YAML without timezone/format fragility.
#' @param root Character. Absolute, normalized project root (parent of `.markermd/`).
#' @param repos Character. Root-relative repos directory, or `NA` if absent.
#' @param comments Character. Root-relative comments directory, or `NA` if absent.
#' @param database Character. Root-relative path to the SQLite grading database.
#' @param template Character. Root-relative path to a grading template, or `NA` if unset.
#' @param key Character. Root-relative key (solution) repository directory, or `NA` if unset.
#' @param artifacts Character vector. Root-relative artifact directory names.
#' @param created_at Character. Project creation timestamp.
#' @param updated_at Character. Last config-update timestamp.
#' @param version Character. Config format version.
#' @export
markermd_project = S7::new_class(
  "markermd_project",
  properties = list(
    root = S7::class_character,
    repos = S7::new_property(S7::class_character, default = quote(NA_character_)),
    comments = S7::new_property(S7::class_character, default = quote(NA_character_)),
    database = S7::new_property(S7::class_character, default = quote(".markermd/markermd.sqlite")),
    template = S7::new_property(S7::class_character, default = quote(NA_character_)),
    key = S7::new_property(S7::class_character, default = quote(NA_character_)),
    artifacts = S7::new_property(S7::class_character, default = quote(character(0))),
    created_at = S7::new_property(S7::class_character, default = quote(get_current_timestamp())),
    updated_at = S7::new_property(S7::class_character, default = quote(get_current_timestamp())),
    version = S7::new_property(S7::class_character, default = quote(markermd_project_version()))
  ),
  validator = function(self) {
    scalars = list(
      root = self@root, repos = self@repos, comments = self@comments,
      database = self@database, template = self@template, key = self@key,
      created_at = self@created_at, updated_at = self@updated_at,
      version = self@version
    )
    for (nm in names(scalars)) {
      if (length(scalars[[nm]]) != 1) {
        return(paste0("@", nm, " must be a single character value"))
      }
    }
    if (is.na(self@root) || !nzchar(self@root)) {
      return("@root must be a non-empty path")
    }
    if (anyNA(self@artifacts)) {
      return("@artifacts must not contain NA")
    }
    if (any(duplicated(self@artifacts))) {
      return("@artifacts must be unique")
    }
    NULL
  },
  package = "markermd"
)

#' @export
S7::method(print, markermd_project) = function(x, ...) {
  resolve = function(rel) if (length(rel) == 1 && !is.na(rel)) fs::path(x@root, rel) else NA_character_

  cli::cli_h1("markermd project")
  cli::cli_dl(c(
    "root" = "{.path {x@root}}",
    "version" = x@version,
    "created" = x@created_at,
    "updated" = x@updated_at
  ))

  repos_dir = resolve(x@repos)
  n_repos = if (!is.na(repos_dir) && fs::dir_exists(repos_dir)) {
    length(fs::dir_ls(repos_dir, type = "directory"))
  } else {
    0L
  }
  comments_dir = resolve(x@comments)
  n_comments = if (!is.na(comments_dir) && fs::dir_exists(comments_dir)) {
    length(fs::dir_ls(comments_dir, glob = "*.md"))
  } else {
    0L
  }
  db_path = resolve(x@database)
  db_exists = !is.na(db_path) && fs::file_exists(db_path)
  template_path = resolve(x@template)

  cli::cli_h2("Locations")
  cli::cli_ul()
  if (is.na(x@repos)) {
    cli::cli_li("repos: {.emph not found}")
  } else {
    cli::cli_li("repos: {.path {x@repos}} ({n_repos} repo{?s})")
  }
  if (is.na(x@comments)) {
    cli::cli_li("comments: {.emph not found}")
  } else {
    cli::cli_li("comments: {.path {x@comments}} ({n_comments} file{?s})")
  }
  if (is.na(x@key)) {
    cli::cli_li("key: {.emph not set}")
  } else {
    key_state = if (fs::dir_exists(resolve(x@key))) "present" else "missing"
    cli::cli_li("key: {.path {x@key}} ({key_state})")
  }
  if (length(x@artifacts) == 0) {
    cli::cli_li("artifacts: {.emph none}")
  } else {
    cli::cli_li("artifacts: {paste(x@artifacts, collapse = ', ')}")
  }
  cli::cli_li("database: {.path {x@database}} ({if (db_exists) 'present' else 'missing'})")
  if (is.na(x@template)) {
    cli::cli_li("template: {.emph not set}")
  } else {
    template_state = if (fs::file_exists(template_path)) "present" else "missing"
    cli::cli_li("template: {.path {x@template}} ({template_state})")
  }
  cli::cli_end()

  invisible(x)
}
