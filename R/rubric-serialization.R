# YAML serialization for grading rubrics
#
# Rubrics are persisted as a human/LLM-friendly YAML file with a published
# JSON Schema (inst/schema/markermd-rubric.json). A file holds one or more
# questions, each with its rubric items in display order plus an optional
# scoring setup. Item order is canonical: hotkeys are not serialized and are
# reassigned by position on import (1-10 for the first ten items, NA beyond;
# there is no limit on the number of items). Item ids are an
# app/database implementation detail and are likewise never serialized; fresh
# ids are minted on import. No per-repository grading data (selections,
# comments, scores) is ever written.
#
# Format 1.1 adds item groups: an entry of a question's items list may be a
# group (`group: <label>`, optional `floor` / `ceiling` / `exclusive`) whose
# own `items` are the group's members, so the nesting exists only in the
# file. A file without groups is written as format 1.0 so it still loads in
# older markermd; the reader accepts both.
#
# Format 1.2 adds `optional` to a question's scoring (see
# markermd_grade_state). It is only written when TRUE, and a file is only
# stamped 1.2 when some question is optional.
#
# There is no S7 container class for a whole rubric; the in-memory exchange
# representation used by read_rubric_yaml() / write_rubric_yaml() is a plain
# list in which the items stay flat, in display order, with each item's
# `group` naming its group in the question's `groups` list:
#
#   list(
#     format_version = "1.1",
#     questions = list(
#       list(name = "Q2",
#            scoring = <markermd_grade_state or NULL>,
#            items = <unnamed list of markermd_rubric_item>,
#            groups = <named list of markermd_rubric_group, keyed by group id>)
#     )
#   )
#
# Group ids in the exchange list are opaque: the database's ids when the
# rubric was collected from a project, transient "group_<k>" ids when it was
# read from a file. The rubric import matches groups by label.

# Current rubric YAML format version

markermd_rubric_version = function() {
  "1.2"
}

# markermd_rubric_item -> plain list. Hotkey, group and selection state are
# not serialized: order encodes the hotkey, nesting encodes the group, and
# selection is per-repo runtime state. The database id is written only for
# a snapshot (id given), never for the public YAML format.
#
# item: markermd_rubric_item S7 object
# id: The item's database id, or NULL

rubric_item_to_list = function(item, id = NULL) {
  out = list()
  if (!is.null(id)) {
    out$id = id
  }
  out$points = clean_number(item@points)
  out$description = item@description
  out
}

# plain list -> markermd_rubric_item with a position-derived hotkey. The S7
# validators are the authoritative checks (scalar points/description).
#
# x: List with points and description
# hotkey: Integer hotkey for the item's display position (NA beyond 10)
# group: Group id the item belongs to, or NA when ungrouped

rubric_item_from_list = function(x, hotkey, group = NA_character_) {
  markermd_rubric_item(
    hotkey = hotkey,
    points = as.numeric(x$points),
    description = as.character(x$description),
    group = group
  )
}

# Whether an entry of a question's items list is a group entry (a group has
# a `group` label; an item has a `description`)
#
# x: One parsed entry

is_rubric_group_entry = function(x) {
  is.list(x) && "group" %in% names(x)
}

# markermd_rubric_group plus its member items -> plain list. Optional bounds
# and the exclusive flag are emitted only when set.
#
# group: markermd_rubric_group S7 object
# items: List of markermd_rubric_item members, in display order
# id: The group's database id, or NULL (snapshots only)
# item_ids: The members' database ids, parallel to items, or NULL

rubric_group_to_list = function(group, items, id = NULL, item_ids = NULL) {
  out = list()
  if (!is.null(id)) {
    out$id = id
  }
  out$group = group@label
  if (!is.na(group@floor)) out$floor = clean_number(group@floor)
  if (!is.na(group@ceiling)) out$ceiling = clean_number(group@ceiling)
  if (isTRUE(group@exclusive)) out$exclusive = TRUE
  out$items = lapply(seq_along(items), function(i) rubric_item_to_list(items[[i]], id = item_ids[i]))
  out
}

# plain group entry -> markermd_rubric_group. The S7 validator enforces the
# sign rules (floor <= 0 <= ceiling).
#
# x: List with group and optionally floor / ceiling / exclusive

rubric_group_from_list = function(x) {
  label = if (is.null(x$group)) "" else as.character(x$group)
  if (length(label) != 1 || !nzchar(trimws(label))) {
    cli::cli_abort("Each rubric item group requires a non-empty 'group' label.")
  }
  number_or_na = function(value) if (is.null(value)) NA_real_ else as.numeric(value)
  markermd_rubric_group(
    label = label,
    floor = number_or_na(x$floor),
    ceiling = number_or_na(x$ceiling),
    exclusive = if (is.null(x$exclusive)) FALSE else as.logical(x$exclusive)
  )
}

# markermd_grade_state -> plain list. current_score is runtime state and is
# never serialized; optional is only written when TRUE, so a rubric without
# optional questions serializes exactly as before.
#
# grade_state: markermd_grade_state S7 object

scoring_to_list = function(grade_state) {
  out = list(
    total_score = clean_number(grade_state@total_score),
    grading_mode = grade_state@grading_mode,
    bound_above_zero = grade_state@bound_above_zero,
    bound_below_max = grade_state@bound_below_max
  )
  if (grade_state@optional) {
    out$optional = TRUE
  }
  out
}

# plain list -> markermd_grade_state. Omitted fields fall back to the class
# defaults; current_score is synthesized for the grading mode (the settings
# table column is NOT NULL) and is recomputed from live selections by the app.
#
# x: List with total_score and optionally grading_mode / bound_above_zero /
#   bound_below_max / optional

scoring_from_list = function(x) {
  args = list(total_score = as.numeric(x$total_score))
  if (!is.null(x$grading_mode)) args$grading_mode = as.character(x$grading_mode)
  if (!is.null(x$bound_above_zero)) args$bound_above_zero = as.logical(x$bound_above_zero)
  if (!is.null(x$bound_below_max)) args$bound_below_max = as.logical(x$bound_below_max)
  if (!is.null(x$optional)) args$optional = as.logical(x$optional)

  mode = if (is.null(args$grading_mode)) "positive" else args$grading_mode
  args$current_score = if (identical(mode, "negative")) args$total_score else 0

  do.call(markermd_grade_state, args)
}

# One exchange-list question entry -> plain list. The flat items are
# regrouped into the file's nesting: each contiguous run of one group's
# items becomes a group entry holding them. items is wrapped so an empty
# question still serializes as `items: []`, scoring is only emitted when the
# question has stored settings, and a group with no items is not written.
# A group whose items are not contiguous is an error (the database
# normalizes order, so it never is).
#
# question: List with name, scoring (markermd_grade_state or NULL), items
#   and optionally groups
# ids: Whether to write the item ids (the names of items) and group ids
#   (the names of groups); snapshots only

rubric_question_to_list = function(question, ids = FALSE) {
  out = list(name = question$name)
  if (!is.null(question$scoring)) {
    out$scoring = scoring_to_list(question$scoring)
  }

  items = question$items
  groups = if (is.null(question$groups)) list() else question$groups
  item_ids = if (ids && !is.null(names(items))) names(items) else NULL
  n = length(items)
  entries = list()
  seen = character(0)
  i = 1
  while (i <= n) {
    group_id = items[[i]]@group
    if (is.na(group_id)) {
      entries[[length(entries) + 1]] = rubric_item_to_list(items[[i]], id = item_ids[i])
      i = i + 1
      next
    }
    if (!group_id %in% names(groups)) {
      cli::cli_abort("Rubric question '{question$name}' has an item in an unknown group '{group_id}'.")
    }
    if (group_id %in% seen) {
      cli::cli_abort("Rubric question '{question$name}': the items of group '{groups[[group_id]]@label}' are not contiguous.")
    }
    seen = c(seen, group_id)
    j = i
    while (j <= n && identical(items[[j]]@group, group_id)) {
      j = j + 1
    }
    entries[[length(entries) + 1]] = rubric_group_to_list(
      groups[[group_id]], items[i:(j - 1)],
      id = if (ids) group_id else NULL, item_ids = item_ids[i:(j - 1)]
    )
    i = j
  }

  out$items = entries
  out
}

# plain list -> exchange-list question entry: the file's group entries are
# flattened into the item list (each member's `group` set to a transient
# "group_<k>" id, k in file order) and hotkeys are assigned by flat item
# position (position_hotkey(): 1-10, NA beyond ten) to match the mark app's
# renumbering invariant. Groups cannot nest, and labels must be unique
# within the question. A snapshot's rubric carries the database ids; when
# every entry has one, the items come back named by id and the groups keyed
# by id, so a diff can match items across snapshots.
#
# x: List with name, optionally scoring, and items

rubric_question_from_list = function(x) {
  name = if (is.null(x$name)) "" else as.character(x$name)
  if (length(name) != 1 || !nzchar(trimws(name))) {
    cli::cli_abort("Each rubric question requires a non-empty 'name'.")
  }

  entries = if (is.null(x$items)) list() else x$items
  items = list()
  item_ids = character(0)
  groups = list()
  entry_id = function(entry) if (is.null(entry$id)) NA_character_ else as.character(entry$id)
  for (entry in entries) {
    if (!is_rubric_group_entry(entry)) {
      items[[length(items) + 1]] = rubric_item_from_list(entry, position_hotkey(length(items) + 1))
      item_ids = c(item_ids, entry_id(entry))
      next
    }
    group_id = if (is.na(entry_id(entry))) paste0("group_", length(groups) + 1) else entry_id(entry)
    groups[[group_id]] = rubric_group_from_list(entry)
    members = if (is.null(entry$items)) list() else entry$items
    for (member in members) {
      if (is_rubric_group_entry(member)) {
        cli::cli_abort("Rubric question '{name}': group '{entry$group}' contains a nested group, which is not allowed.")
      }
      items[[length(items) + 1]] = rubric_item_from_list(member, position_hotkey(length(items) + 1), group = group_id)
      item_ids = c(item_ids, entry_id(member))
    }
  }
  if (length(items) > 0 && !anyNA(item_ids) && !anyDuplicated(item_ids)) {
    names(items) = item_ids
  }

  labels = vapply(groups, function(group) group@label, character(1))
  dupes = unique(labels[duplicated(labels)])
  if (length(dupes) > 0) {
    dupes_str = paste0("'", dupes, "'", collapse = ", ")
    cli::cli_abort("Rubric question '{name}' lists duplicate group labels: {dupes_str}.")
  }

  # Descriptions are the cross-format identity key used by the marks layer, so
  # they must be non-empty and unique within a question. Reject violations here
  # rather than letting them surface far away at marking time. Mirrors the
  # marks-side guard in marks_question_from_list().
  descriptions = vapply(items, function(item) item@description, character(1))
  if (any(!nzchar(trimws(descriptions)))) {
    cli::cli_abort("Rubric question '{name}' has a blank rubric item description.")
  }
  dupes = unique(descriptions[duplicated(descriptions)])
  if (length(dupes) > 0) {
    dupes_str = paste0("'", dupes, "'", collapse = ", ")
    cli::cli_abort("Rubric question '{name}' lists duplicate rubric item descriptions: {dupes_str}.")
  }

  list(
    name = name,
    scoring = if (is.null(x$scoring)) NULL else scoring_from_list(x$scoring),
    items = items,
    groups = groups
  )
}

# Exchange-list rubric -> plain list ready for yaml::write_yaml. The written
# format_version is the lowest that can represent the content: "1.2" when
# any question is optional, "1.1" when any question has a group with items,
# otherwise "1.0" (so a file without these still loads in older markermd);
# the exchange list's own format_version is informational.
#
# rubric: Exchange list with format_version and questions
# ids: Whether to write item and group ids (snapshots only; the public YAML
#   format never carries them)

rubric_to_list = function(rubric, ids = FALSE) {
  questions = lapply(rubric$questions, rubric_question_to_list, ids = ids)
  has_groups = any(vapply(questions, function(question) {
    any(vapply(question$items, is_rubric_group_entry, logical(1)))
  }, logical(1)))
  has_optional = any(vapply(questions, function(question) {
    isTRUE(question$scoring$optional)
  }, logical(1)))
  list(
    format_version = if (has_optional) "1.2" else if (has_groups) "1.1" else "1.0",
    questions = questions
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
    cli::cli_abort("Rubric file is missing the required 'format_version' field.")
  }
  if (utils::compareVersion(as.character(version), markermd_rubric_version()) > 0) {
    cli::cli_abort("Rubric file format_version '{version}' is newer than this version of markermd understands ({markermd_rubric_version()}). Update markermd to import it.")
  }

  questions = if (is.null(x$questions)) list() else lapply(x$questions, rubric_question_from_list)

  names = vapply(questions, function(q) q$name, character(1))
  dupes = unique(names[duplicated(names)])
  if (length(dupes) > 0) {
    dupes_str = paste0("'", dupes, "'", collapse = ", ")
    cli::cli_abort("Rubric file contains duplicate question names: {dupes_str}.")
  }

  list(
    format_version = as.character(version),
    questions = questions
  )
}

# Build the exchange-list rubric for a set of questions from a project's
# grading database. Items come back in display order (items_df_to_rubric_list())
# with their group ids, groups in display order (groups_df_to_list()), and
# scoring is included only when the question has a settings row, so an
# export -> import round trip does not fabricate settings. Shared by
# rubric_export(), export_comments() and the mark app's download handlers.
#
# collection_path: Project root containing the grading database
# question_names: Character vector of question names to include

collect_rubric_data = function(collection_path, question_names) {
  # One connection for the whole export rather than three per question
  with_database(collection_path, function(conn) {
    rubric_data_from_conn(conn, question_names)
  })
}

# collect_rubric_data() on an open connection. With ids = TRUE each
# question's items keep their database ids as names (the snapshot writer
# stores them; the public format drops them).
#
# conn: DBI connection object
# question_names: Character vector of question names to include
# ids: Whether items are named by their database ids

rubric_data_from_conn = function(conn, question_names, ids = FALSE) {
  data = list(
    settings = load_all_settings(conn),
    items = load_all_items(conn),
    groups = load_all_item_groups(conn)
  )

  questions = lapply(question_names, function(question_name) {
    items = items_df_to_rubric_list(data$items, question_name)
    list(
      name = question_name,
      scoring = db_row_to_grade_state(
        data$settings[data$settings$question_name == question_name, , drop = FALSE]
      ),
      items = if (ids) items else unname(items),
      groups = groups_df_to_list(data$groups, data$items, question_name)
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
#' scoring setup and item groups) to a human-readable, schema-validatable
#' YAML file. Items are written in display order; order is meaningful, as
#' the first ten items are bound to keyboard hotkeys on import and any
#' further items (there is no limit) are toggled by clicking. A group's
#' items are nested under a group entry (`group`, optional `floor`,
#' `ceiling` and `exclusive`, then `items`). The file is written with the
#' lowest `format_version` that can represent it: `"1.2"` when a question's
#' scoring is optional, `"1.1"` when it contains a group, and `"1.0"`
#' otherwise.
#' No per-repository grading data is written.
#'
#' @param rubric A rubric list as returned by [read_rubric_yaml()] or
#'   [rubric_export()]: `format_version` plus a `questions` list whose entries
#'   have `name`, `items` (a list of `markermd_rubric_item` objects in
#'   display order, each item's `group` naming its group or `NA`),
#'   optionally `groups` (a named list of `markermd_rubric_group` objects
#'   keyed by group id) and optionally `scoring` (a `markermd_grade_state`
#'   object).
#' @param path Output file path (`.yaml`).
#'
#' @return The output `path`, invisibly.
#' @seealso [read_rubric_yaml()], [rubric_export()], [rubric_import()]
#' @export
write_rubric_yaml = function(rubric, path) {
  if (!is.list(rubric) || !is.list(rubric$questions)) {
    cli::cli_abort("`rubric` must be a list with a 'questions' list.")
  }
  if (is.null(rubric$format_version)) {
    rubric$format_version = markermd_rubric_version()
  }
  for (question in rubric$questions) {
    for (item in question$items) {
      if (!S7::S7_inherits(item, markermd_rubric_item)) {
        cli::cli_abort("Rubric items must be markermd_rubric_item objects.")
      }
    }
    for (group in question$groups) {
      if (!S7::S7_inherits(group, markermd_rubric_group)) {
        cli::cli_abort("Rubric item groups must be markermd_rubric_group objects.")
      }
    }
    if (!is.null(question$scoring) && !S7::S7_inherits(question$scoring, markermd_grade_state)) {
      cli::cli_abort("Question scoring must be a markermd_grade_state object.")
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
#' determines display order and keyboard hotkeys: the first ten items of a
#' question get the keys 1-9 and 0, and any further items have no hotkey and
#' are toggled by clicking in [mark()]. There is no limit on the number of
#' items. A group entry in the items list (format `"1.1"`) is flattened: its
#' members join the question's item list in place, each with its `group`
#' set, and the group itself is recorded in the question's `groups`.
#' Validation is performed by the S7 constructors as items and groups are
#' built.
#'
#' @param path Path to a rubric `.yaml`/`.yml` file.
#'
#' @return A rubric list: `format_version` plus `questions`, each with `name`,
#'   `items` (a list of `markermd_rubric_item` objects), `groups` (a named
#'   list of `markermd_rubric_group` objects, empty when the question has
#'   none) and `scoring` (a `markermd_grade_state` object, or `NULL` when the
#'   file omits it).
#' @seealso [write_rubric_yaml()], [rubric_import()], [validate_rubric_file()]
#' @export
read_rubric_yaml = function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Rubric file does not exist: {path}")
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
    cli::cli_abort("validate_rubric_file() requires the 'jsonvalidate' package.")
  }
  if (!file.exists(path)) {
    cli::cli_abort("Rubric file does not exist: {path}")
  }

  schema = system.file("schema/markermd-rubric.json", package = "markermd")
  x = yaml::read_yaml(path)

  json = jsonlite::toJSON(x, auto_unbox = TRUE)

  jsonvalidate::json_validate(json, schema, engine = "ajv", verbose = TRUE, error = FALSE)
}
