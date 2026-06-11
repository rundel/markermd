# YAML serialization for grading rubrics
#
# Rubrics are persisted as a human/LLM-friendly YAML file with a published
# JSON Schema (inst/schema/markermd-rubric.json). A file holds one or more
# questions, each with its rubric items in display order plus an optional
# scoring setup. Item order is canonical: hotkeys are not serialized and are
# reassigned 1-10 by position on import (NA beyond ten). Item ids are an
# app/database implementation detail and are likewise never serialized; fresh
# ids are minted on import. No per-repository grading data (selections,
# comments, scores) is ever written.
#
# There is no S7 container class for a whole rubric; the in-memory exchange
# representation used by read_rubric_yaml() / write_rubric_yaml() is a plain
# list:
#
#   list(
#     format_version = "1.0",
#     questions = list(
#       list(name = "Q2",
#            scoring = <markermd_grade_state or NULL>,
#            items = <unnamed list of markermd_rubric_item>)
#     )
#   )

# Current rubric YAML format version

markermd_rubric_version = function() {
  "1.0"
}

# markermd_rubric_item -> plain list. Hotkey and selection state are not
# serialized: order encodes the hotkey and selection is per-repo runtime state.
#
# item: markermd_rubric_item S7 object

rubric_item_to_list = function(item) {
  list(
    points = clean_number(item@points),
    description = item@description
  )
}

# plain list -> markermd_rubric_item with a position-derived hotkey. The S7
# validators are the authoritative checks (scalar points/description).
#
# x: List with points and description
# hotkey: Integer hotkey for the item's display position (NA beyond 10)

rubric_item_from_list = function(x, hotkey) {
  markermd_rubric_item(
    hotkey = hotkey,
    points = as.numeric(x$points),
    description = as.character(x$description)
  )
}

# markermd_grade_state -> plain list. current_score is runtime state and is
# never serialized.
#
# grade_state: markermd_grade_state S7 object

scoring_to_list = function(grade_state) {
  list(
    total_score = clean_number(grade_state@total_score),
    grading_mode = grade_state@grading_mode,
    bound_above_zero = grade_state@bound_above_zero,
    bound_below_max = grade_state@bound_below_max
  )
}

# plain list -> markermd_grade_state. Omitted fields fall back to the class
# defaults; current_score is synthesized for the grading mode (the settings
# table column is NOT NULL) and is recomputed from live selections by the app.
#
# x: List with total_score and optionally grading_mode / bound_above_zero /
#   bound_below_max

scoring_from_list = function(x) {
  args = list(total_score = as.numeric(x$total_score))
  if (!is.null(x$grading_mode)) args$grading_mode = as.character(x$grading_mode)
  if (!is.null(x$bound_above_zero)) args$bound_above_zero = as.logical(x$bound_above_zero)
  if (!is.null(x$bound_below_max)) args$bound_below_max = as.logical(x$bound_below_max)

  mode = if (is.null(args$grading_mode)) "positive" else args$grading_mode
  args$current_score = if (identical(mode, "negative")) args$total_score else 0

  do.call(markermd_grade_state, args)
}

# One exchange-list question entry -> plain list. items is wrapped so an empty
# question still serializes as `items: []`, and scoring is only emitted when
# the question has stored settings.
#
# question: List with name, scoring (markermd_grade_state or NULL) and items

rubric_question_to_list = function(question) {
  out = list(name = question$name)
  if (!is.null(question$scoring)) {
    out$scoring = scoring_to_list(question$scoring)
  }
  out$items = lapply(question$items, rubric_item_to_list)
  out
}

# plain list -> exchange-list question entry, assigning hotkeys 1-10 by item
# position (NA beyond ten) to match the mark app's renumbering invariant.
#
# x: List with name, optionally scoring, and items

rubric_question_from_list = function(x) {
  name = if (is.null(x$name)) "" else as.character(x$name)
  if (length(name) != 1 || !nzchar(trimws(name))) {
    stop("Each rubric question requires a non-empty 'name'.", call. = FALSE)
  }

  items_in = if (is.null(x$items)) list() else x$items
  items = lapply(seq_along(items_in), function(i) {
    hotkey = if (i <= 10) as.integer(i) else NA_integer_
    rubric_item_from_list(items_in[[i]], hotkey)
  })

  list(
    name = name,
    scoring = if (is.null(x$scoring)) NULL else scoring_from_list(x$scoring),
    items = items
  )
}

# Exchange-list rubric -> plain list ready for yaml::write_yaml
#
# rubric: Exchange list with format_version and questions

rubric_to_list = function(rubric) {
  list(
    format_version = rubric$format_version,
    questions = lapply(rubric$questions, rubric_question_to_list)
  )
}

# plain list -> exchange-list rubric. Errors on a missing format_version, a
# format_version newer than this package understands, and duplicate question
# names.
#
# x: List parsed from a rubric YAML file

rubric_from_list = function(x) {
  version = x$format_version
  if (is.null(version)) {
    stop("Rubric file is missing the required 'format_version' field.", call. = FALSE)
  }
  if (utils::compareVersion(as.character(version), markermd_rubric_version()) > 0) {
    stop(
      "Rubric file format_version '", version, "' is newer than this version of ",
      "markermd understands (", markermd_rubric_version(), "). Update markermd to import it.",
      call. = FALSE
    )
  }

  questions = if (is.null(x$questions)) list() else lapply(x$questions, rubric_question_from_list)

  names = vapply(questions, function(q) q$name, character(1))
  dupes = unique(names[duplicated(names)])
  if (length(dupes) > 0) {
    stop(
      "Rubric file contains duplicate question names: ",
      paste0("'", dupes, "'", collapse = ", "), ".",
      call. = FALSE
    )
  }

  list(
    format_version = as.character(version),
    questions = questions
  )
}

# Build the exchange-list rubric for a set of questions from a project's
# grading database. Items come back in display order from load_rubric_items()
# and scoring is included only when the question has a settings row, so an
# export -> import round trip does not fabricate settings. Shared by
# rubric_export() and the mark app's download handlers.
#
# collection_path: Project root containing the grading database
# question_names: Character vector of question names to include

collect_rubric_data = function(collection_path, question_names) {
  questions = lapply(question_names, function(question_name) {
    list(
      name = question_name,
      scoring = load_grade_state(collection_path, question_name),
      items = unname(load_rubric_items(collection_path, question_name))
    )
  })

  list(
    format_version = markermd_rubric_version(),
    questions = questions
  )
}

#' Write a grading rubric to a YAML file
#'
#' Serializes a rubric (one or more questions' rubric items plus optional
#' scoring setup) to a human-readable, schema-validatable YAML file. Items are
#' written in display order; order is meaningful, as the first ten items are
#' bound to keyboard hotkeys on import. No per-repository grading data is
#' written.
#'
#' @param rubric A rubric list as returned by [read_rubric_yaml()] or
#'   [rubric_export()]: `format_version` plus a `questions` list whose entries
#'   have `name`, `items` (a list of `markermd_rubric_item` objects) and
#'   optionally `scoring` (a `markermd_grade_state` object).
#' @param path Output file path (`.yaml`).
#'
#' @return The output `path`, invisibly.
#' @seealso [read_rubric_yaml()], [rubric_export()], [rubric_import()]
#' @export
write_rubric_yaml = function(rubric, path) {
  if (!is.list(rubric) || !is.list(rubric$questions)) {
    stop("`rubric` must be a list with a 'questions' list.", call. = FALSE)
  }
  if (is.null(rubric$format_version)) {
    rubric$format_version = markermd_rubric_version()
  }
  for (question in rubric$questions) {
    for (item in question$items) {
      if (!S7::S7_inherits(item, markermd_rubric_item)) {
        stop("Rubric items must be markermd_rubric_item objects.", call. = FALSE)
      }
    }
    if (!is.null(question$scoring) && !S7::S7_inherits(question$scoring, markermd_grade_state)) {
      stop("Question scoring must be a markermd_grade_state object.", call. = FALSE)
    }
  }

  # verbatim_logical writes true/false rather than YAML 1.1's yes/no, matching
  # the JSON Schema's boolean vocabulary for LLM-authored files
  yaml::write_yaml(
    rubric_to_list(rubric), path,
    handlers = list(logical = yaml::verbatim_logical)
  )
  invisible(path)
}

#' Read a grading rubric from a YAML file
#'
#' Parses a rubric YAML file into the rubric list structure used by
#' [rubric_import()] and [write_rubric_yaml()]. Item order in the file
#' determines display order and keyboard hotkeys (1-10 by position, none
#' beyond ten). Validation is performed by the S7 constructors as items are
#' built.
#'
#' @param path Path to a rubric `.yaml`/`.yml` file.
#'
#' @return A rubric list: `format_version` plus `questions`, each with `name`,
#'   `items` (a list of `markermd_rubric_item` objects) and `scoring` (a
#'   `markermd_grade_state` object, or `NULL` when the file omits it).
#' @seealso [write_rubric_yaml()], [rubric_import()], [validate_rubric_file()]
#' @export
read_rubric_yaml = function(path) {
  if (!file.exists(path)) {
    stop("Rubric file does not exist: ", path, call. = FALSE)
  }
  x = yaml::read_yaml(path)
  rubric_from_list(x)
}

#' Validate a rubric file against the markermd JSON Schema
#'
#' Structurally validates a rubric YAML (or JSON) file against the bundled
#' JSON Schema (`inst/schema/markermd-rubric.json`). This is an optional check
#' aimed at tooling and LLM-generated files; the authoritative validation
#' happens via the S7 constructors in [read_rubric_yaml()]. Requires the
#' suggested `jsonvalidate` package.
#'
#' @param path Path to a rubric `.yaml`/`.yml`/`.json` file.
#'
#' @return `TRUE` when the file conforms to the schema, otherwise `FALSE` with
#'   the validation errors attached as attributes (see [jsonvalidate::json_validate()]).
#' @seealso [validate_template_file()]
#' @export
validate_rubric_file = function(path) {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    stop("validate_rubric_file() requires the 'jsonvalidate' package.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Rubric file does not exist: ", path, call. = FALSE)
  }

  schema = system.file("schema/markermd-rubric.json", package = "markermd")
  x = yaml::read_yaml(path)

  json = jsonlite::toJSON(x, auto_unbox = TRUE)

  jsonvalidate::json_validate(json, schema, engine = "ajv", verbose = TRUE, error = FALSE)
}
