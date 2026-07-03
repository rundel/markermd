# YAML serialization for grading marks
#
# Marks are per-repository grading state: which rubric items are selected for
# each (repository, question) pair, plus optional public and private comments.
# They are persisted as a human/LLM-friendly YAML file with a published JSON
# Schema (inst/schema/markermd-marks.json). Unlike the rubric format this file
# carries per-student data. Rubric items are identified by their description
# text, copied verbatim from the question's rubric; item ids are an
# app/database implementation detail and are never serialized. Listing an item
# selects it and omitting it deselects it, so a question entry is a complete
# declarative statement of the pair's selections (`items: []` means no items
# apply).
#
# The in-memory exchange representation used by read_marks_yaml() /
# write_marks_yaml() is a plain list:
#
#   list(
#     format_version = "1.0",
#     repos = list(
#       list(name = "hw01-team01",
#            questions = list(
#              list(name = "Q1",
#                   items = c("desc A", "desc B"),    # character(0) = none apply
#                   comment = NULL,                    # public, student-facing
#                   private_comment = NULL)            # internal, never shared
#            ))
#     )
#   )

# Current marks YAML format version

markermd_marks_version = function() {
  "1.0"
}

# One exchange-list question entry -> plain list. items is wrapped so an empty
# entry still serializes as `items: []`; comments are emitted only when set.
#
# question: List with name, items, comment and private_comment

marks_question_to_list = function(question) {
  out = list(name = question$name, items = as.list(question$items))
  if (!is.null(question$comment)) {
    out$comment = question$comment
  }
  if (!is.null(question$private_comment)) {
    out$private_comment = question$private_comment
  }
  out
}

# plain list -> exchange-list question entry. The items key is required even
# when empty: an omitted key may not silently mean "deselect everything".
#
# x: List with name, items, and optionally comment / private_comment

marks_question_from_list = function(x) {
  name = if (is.null(x$name)) "" else as.character(x$name)
  if (length(name) != 1 || !nzchar(trimws(name))) {
    cli::cli_abort("Each marks question requires a non-empty 'name'.")
  }

  if (!"items" %in% names(x)) {
    cli::cli_abort("Marks question '{name}' is missing the required 'items' field. Use 'items: []' to state that no rubric items apply.")
  }
  items = as.character(unlist(x$items))
  if (any(!nzchar(trimws(items)))) {
    cli::cli_abort("Marks question '{name}' has a blank rubric item description.")
  }
  dupes = unique(items[duplicated(items)])
  if (length(dupes) > 0) {
    dupes_str = paste0("'", dupes, "'", collapse = ", ")
    cli::cli_abort("Marks question '{name}' lists duplicate rubric item descriptions: {dupes_str}.")
  }

  read_comment = function(field) {
    if (is.null(x[[field]])) {
      return(NULL)
    }
    value = as.character(x[[field]])
    if (length(value) != 1) {
      cli::cli_abort("Marks question '{name}' has a non-scalar '{field}'.")
    }
    value
  }

  list(
    name = name,
    items = items,
    comment = read_comment("comment"),
    private_comment = read_comment("private_comment")
  )
}

# One exchange-list repo entry -> plain list
#
# repo: List with name and questions

marks_repo_to_list = function(repo) {
  list(
    name = repo$name,
    questions = lapply(repo$questions, marks_question_to_list)
  )
}

# plain list -> exchange-list repo entry
#
# x: List with name and questions

marks_repo_from_list = function(x) {
  name = if (is.null(x$name)) "" else as.character(x$name)
  if (length(name) != 1 || !nzchar(trimws(name))) {
    cli::cli_abort("Each marks repository requires a non-empty 'name'.")
  }

  questions = if (is.null(x$questions)) list() else lapply(x$questions, marks_question_from_list)

  question_names = vapply(questions, function(q) q$name, character(1))
  dupes = unique(question_names[duplicated(question_names)])
  if (length(dupes) > 0) {
    dupes_str = paste0("'", dupes, "'", collapse = ", ")
    cli::cli_abort("Marks repository '{name}' contains duplicate question names: {dupes_str}.")
  }

  list(name = name, questions = questions)
}

# Exchange-list marks -> plain list ready for yaml::write_yaml
#
# marks: Exchange list with format_version and repos

marks_to_list = function(marks) {
  list(
    format_version = marks$format_version,
    repos = lapply(marks$repos, marks_repo_to_list)
  )
}

# plain list -> exchange-list marks. Errors on a missing format_version, a
# format_version newer than this package understands, and duplicate repo names.
#
# x: List parsed from a marks YAML file

marks_from_list = function(x) {
  version = x$format_version
  if (is.null(version)) {
    cli::cli_abort("Marks file is missing the required 'format_version' field.")
  }
  if (utils::compareVersion(as.character(version), markermd_marks_version()) > 0) {
    cli::cli_abort("Marks file format_version '{version}' is newer than this version of markermd understands ({markermd_marks_version()}). Update markermd to import it.")
  }

  repos = if (is.null(x$repos)) list() else lapply(x$repos, marks_repo_from_list)

  repo_names = vapply(repos, function(r) r$name, character(1))
  dupes = unique(repo_names[duplicated(repo_names)])
  if (length(dupes) > 0) {
    dupes_str = paste0("'", dupes, "'", collapse = ", ")
    cli::cli_abort("Marks file contains duplicate repository names: {dupes_str}.")
  }

  list(
    format_version = as.character(version),
    repos = repos
  )
}

# Build the exchange-list marks from a project's grading database. A pair is
# included when it has any grading activity (see marked_question_pairs()), so
# an all-deselected pair round trips as `items: []` rather than vanishing.
# Selected items are reported by description in the question's display order;
# comments are included only when the most recent row in their channel is
# non-empty. Repos and questions are emitted in the order of repo_names /
# question_names, which also act as filters when supplied.
#
# collection_path: Project root containing the grading database
# repo_names: Repository names to include, in output order (NULL = all, sorted)
# question_names: Question names to include, in output order (NULL = all, sorted)

collect_marks_data = function(collection_path, repo_names = NULL, question_names = NULL) {
  data = with_database(collection_path, function(conn) {
    list(
      grades = load_most_recent_grades(conn),
      comments = load_most_recent_comments(conn),
      private_comments = load_most_recent_private_comments(conn),
      items = DBI::dbGetQuery(conn, "
        SELECT question_name, item_id, description FROM items
        ORDER BY question_name, (hotkey IS NULL), hotkey, id")
    )
  })

  comments = nonempty_comments(data$comments)
  private_comments = nonempty_comments(data$private_comments)

  pairs = unique(rbind(
    data$grades[, c("question_name", "assignment_repo"), drop = FALSE],
    comments[, c("question_name", "assignment_repo"), drop = FALSE],
    private_comments[, c("question_name", "assignment_repo"), drop = FALSE]
  ))

  if (is.null(repo_names)) {
    repo_names = sort(unique(pairs$assignment_repo))
  }
  if (is.null(question_names)) {
    question_names = sort(unique(pairs$question_name))
  }

  lookup_comment = function(df, question_name, repo_name) {
    hit = df$question_name == question_name & df$assignment_repo == repo_name
    if (any(hit)) df$comment_text[hit][1] else NULL
  }

  repos = list()
  for (repo_name in repo_names) {
    questions = list()
    for (question_name in question_names) {
      active = any(pairs$question_name == question_name & pairs$assignment_repo == repo_name)
      if (!active) {
        next
      }

      selected_ids = data$grades$item_id[
        data$grades$question_name == question_name &
          data$grades$assignment_repo == repo_name &
          data$grades$selected == 1
      ]
      question_items = data$items[data$items$question_name == question_name, , drop = FALSE]
      selected = question_items$description[question_items$item_id %in% selected_ids]

      questions[[length(questions) + 1]] = list(
        name = question_name,
        items = selected,
        comment = lookup_comment(comments, question_name, repo_name),
        private_comment = lookup_comment(private_comments, question_name, repo_name)
      )
    }

    if (length(questions) > 0) {
      repos[[length(repos) + 1]] = list(name = repo_name, questions = questions)
    }
  }

  list(
    format_version = markermd_marks_version(),
    repos = repos
  )
}

#' Write grading marks to a YAML file
#'
#' Serializes grading marks (per-repository rubric item selections plus
#' optional public and private comments) to a human-readable,
#' schema-validatable YAML file. Each question entry is declarative: the
#' listed rubric item descriptions are the selected items, and `items: []`
#' states that no items apply.
#'
#' @param marks A marks list as returned by [read_marks_yaml()] or built by
#'   [marks_export()]: `format_version` plus a `repos` list whose entries have
#'   `name` and `questions`, each question with `name`, `items` (a character
#'   vector of rubric item descriptions) and optionally `comment` /
#'   `private_comment`.
#' @param path Output file path (`.yaml`).
#'
#' @return The output `path`, invisibly.
#' @seealso [read_marks_yaml()], [marks_export()], [marks_import()]
#' @export
write_marks_yaml = function(marks, path) {
  if (!is.list(marks) || !is.list(marks$repos)) {
    cli::cli_abort("`marks` must be a list with a 'repos' list.")
  }
  if (is.null(marks$format_version)) {
    marks$format_version = markermd_marks_version()
  }

  yaml::write_yaml(
    marks_to_list(marks), path,
    handlers = list(logical = yaml::verbatim_logical)
  )
  invisible(path)
}

#' Read grading marks from a YAML file
#'
#' Parses a marks YAML file into the marks list structure used by
#' [marks_import()] and [write_marks_yaml()]. Rubric items are identified by
#' their description text, which must match the question's rubric verbatim;
#' the match itself is performed (and validated) by [marks_import()], not
#' here.
#'
#' @param path Path to a marks `.yaml`/`.yml` file.
#'
#' @return A marks list: `format_version` plus `repos`, each with `name` and
#'   `questions`, each question with `name`, `items` (a character vector of
#'   rubric item descriptions) and `comment` / `private_comment` (a string, or
#'   `NULL` when the file omits it).
#' @seealso [write_marks_yaml()], [marks_import()], [validate_marks_file()]
#' @export
read_marks_yaml = function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Marks file does not exist: {path}")
  }
  x = yaml::read_yaml(path)
  marks_from_list(x)
}

#' Validate a marks file against the markermd JSON Schema
#'
#' Structurally validates a marks YAML (or JSON) file against the bundled
#' JSON Schema (`inst/schema/markermd-marks.json`). This is an optional check
#' aimed at tooling and LLM-generated files; the authoritative validation
#' (including matching item descriptions against the project's rubric) happens
#' in [read_marks_yaml()] and [marks_import()]. Requires the suggested
#' `jsonvalidate` package.
#'
#' @param path Path to a marks `.yaml`/`.yml`/`.json` file.
#'
#' @return `TRUE` when the file conforms to the schema, otherwise `FALSE` with
#'   the validation errors attached as attributes (see [jsonvalidate::json_validate()]).
#' @seealso [validate_rubric_file()], [validate_template_file()]
#' @export
validate_marks_file = function(path) {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    cli::cli_abort("validate_marks_file() requires the 'jsonvalidate' package.")
  }
  if (!file.exists(path)) {
    cli::cli_abort("Marks file does not exist: {path}")
  }

  schema = system.file("schema/markermd-marks.json", package = "markermd")
  x = yaml::read_yaml(path)

  # Protect single-element item lists from auto_unbox collapsing them to a
  # scalar, which the schema would reject as a non-array
  if (is.list(x$repos)) {
    x$repos = lapply(x$repos, function(repo) {
      if (is.list(repo$questions)) {
        repo$questions = lapply(repo$questions, function(question) {
          if (!is.null(question$items)) {
            question$items = I(as.list(question$items))
          }
          question
        })
      }
      repo
    })
  }

  json = jsonlite::toJSON(x, auto_unbox = TRUE)

  jsonvalidate::json_validate(json, schema, engine = "ajv", verbose = TRUE, error = FALSE)
}
