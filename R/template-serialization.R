# YAML serialization for markermd templates
#
# Templates are persisted as a human/LLM-friendly YAML file with a published
# JSON Schema (inst/schema/markermd-template.json). The heavy original_ast is
# never written: it is regenerated on load by re-parsing the assignment named
# in the file's `source.path`. Deserialization routes through the S7
# constructors so the existing validators (validate_rule_values, unique
# ids/names, non-empty node ids) are the authoritative validation layer.

# Format a whole number as an integer so YAML writes `1` rather than `1.0`,
# leaving genuine fractional values untouched.
#
# x: A length-one numeric

clean_number = function(x) {
  if (length(x) == 1 && is.finite(x) && x == round(x)) as.integer(x) else x
}

# markermd_rule -> plain list with verb-determined value keys
# (min/max for "has between", count for the count verbs, pattern otherwise).
#
# rule: markermd_rule S7 object

rule_to_list = function(rule) {
  base = list(node_type = rule@node_type, verb = rule@verb)
  values = switch(rule@verb,
    "has between" = list(min = clean_number(rule@values[1]), max = clean_number(rule@values[2])),
    "has at least" = ,
    "has at most" = list(count = clean_number(rule@values[1])),
    list(pattern = as.character(rule@values[1]))
  )
  c(base, values)
}

# plain list -> markermd_rule. The S7 constructor runs validate_rule_values,
# so malformed verb/value combinations raise an error here.
#
# x: List with node_type, verb and the verb-specific value key(s)

rule_from_list = function(x) {
  values = switch(x$verb,
    "has between" = as.numeric(c(x$min, x$max)),
    "has at least" = as.integer(x$count),
    "has at most" = as.integer(x$count),
    "has content" = as.character(x$pattern),
    "lacks content" = as.character(x$pattern),
    "has name" = as.character(x$pattern),
    as.character(x$pattern)
  )
  markermd_rule(node_type = as.character(unlist(x$node_type)), verb = x$verb, values = values)
}

# markermd_filter_condition -> plain list. negate is only emitted when set so
# files without negation are unchanged.
#
# condition: markermd_filter_condition S7 object

filter_condition_to_list = function(condition) {
  out = list(type = condition@type, value = condition@value)
  if (condition@negate) out$negate = TRUE
  out
}

# plain list -> markermd_filter_condition. The S7 constructor runs
# validate_filter_condition_value, so malformed type/value pairs raise here.
#
# x: List with type, value and optionally negate

filter_condition_from_list = function(x) {
  markermd_filter_condition(
    type = as.character(x$type),
    # value may be a scalar or (for "node type") a sequence of ORed kinds
    value = as.character(unlist(x$value)),
    negate = isTRUE(x$negate)
  )
}

# markermd_filter_group -> plain list. negate is only emitted when set.
#
# group: markermd_filter_group S7 object

filter_group_to_list = function(group) {
  out = list(conditions = lapply(group@conditions, filter_condition_to_list))
  if (group@negate) out$negate = TRUE
  out
}

# plain list -> markermd_filter_group
#
# x: List with a conditions list and optionally negate

filter_group_from_list = function(x) {
  conditions = if (is.null(x$conditions)) list() else lapply(x$conditions, filter_condition_from_list)
  markermd_filter_group(conditions = conditions, negate = isTRUE(x$negate))
}

# markermd_question -> plain list. node_ids is emitted as a list so a single
# id still serializes as a YAML sequence rather than a scalar. filters is only
# emitted when at least one group has conditions, so filter-less templates
# serialize exactly as before.
#
# question: markermd_question S7 object

question_to_list = function(question) {
  out = list(
    id = as.integer(question@id),
    name = question@name,
    node_ids = as.list(question@selected_nodes@node_ids),
    rules = lapply(question@rules, rule_to_list)
  )

  groups = Filter(function(g) length(g@conditions) > 0, question@filters)
  if (length(groups) > 0) {
    out$filters = lapply(groups, filter_group_to_list)
  }

  out
}

# plain list -> markermd_question
#
# x: List with id, name, node_ids, rules and optionally filters

question_from_list = function(x) {
  node_ids = if (is.null(x$node_ids)) character(0) else as.character(unlist(x$node_ids))
  rules = if (is.null(x$rules)) list() else lapply(x$rules, rule_from_list)
  filters = if (is.null(x$filters)) list() else lapply(x$filters, filter_group_from_list)

  # Templates written before scoring moved into the rubric carry a per-question
  # points field; it is intentionally ignored here.
  args = list(
    id = as.integer(x$id),
    name = as.character(x$name),
    selected_nodes = markermd_node_selection(node_ids = node_ids),
    rules = rules,
    filters = filters
  )

  do.call(markermd_question, args)
}

# markermd_metadata -> plain list. version is carried at the top level of the
# file as format_version; total_nodes is derived and recomputed on load, so
# neither is written here.
#
# metadata: markermd_metadata S7 object

metadata_to_list = function(metadata) {
  list(
    created_at = format(metadata@created_at, "%Y-%m-%dT%H:%M:%S%z"),
    created_by = metadata@created_by
  )
}

# plain list -> markermd_metadata
#
# x: List with created_at and created_by (both optional)
# version: Template format version taken from the file's format_version
# total_nodes: Node count recomputed from the re-parsed AST

metadata_from_list = function(x, version, total_nodes) {
  args = list(version = version, total_nodes = as.integer(total_nodes))

  if (!is.null(x$created_at)) {
    parsed = as.POSIXct(x$created_at, format = "%Y-%m-%dT%H:%M:%S%z")
    if (!is.na(parsed)) args$created_at = parsed
  }
  if (!is.null(x$created_by)) {
    args$created_by = as.character(x$created_by)
  }

  do.call(markermd_metadata, args)
}

# markermd_template -> plain list ready for yaml::write_yaml. The assignment
# document is referenced by source_path rather than embedded; the AST itself
# is not serialized.
#
# template: markermd_template S7 object
# source_path: Path to the assignment document, recorded under source.path

template_to_list = function(template, source_path = NULL) {
  out = list(format_version = template@metadata@version)

  if (!is.null(source_path)) {
    out$source = list(path = source_path)
  }

  out$metadata = metadata_to_list(template@metadata)
  out$questions = lapply(template@questions, question_to_list)
  out
}

# Resolve the assignment document referenced by a template file.
#
# Tries, in order: an explicit assignment override, the stored path as-is
# (absolute or relative to the working directory), then the stored path
# relative to the template file's directory. Returns a normalized path or NULL
# when none resolve.
#
# path: The source.path string from the file (may be NULL)
# base_dir: Directory of the template file, for resolving relative paths
# assignment: Explicit override path (may be NULL)

resolve_template_source = function(path, base_dir = ".", assignment = NULL) {
  candidates = character(0)
  if (!is.null(assignment)) candidates = c(candidates, assignment)
  if (!is.null(path)) candidates = c(candidates, path, file.path(base_dir, path))

  for (cand in candidates) {
    if (nzchar(cand) && file.exists(cand)) {
      return(normalizePath(cand, winslash = "/"))
    }
  }
  NULL
}

# An empty q2r pandoc AST, used as a placeholder when a template is loaded for
# grading and its source document cannot be resolved.

empty_pandoc_ast = function() {
  q2r::pandoc(blocks = q2r::pandoc_blocks(list()))
}

# plain list -> markermd_template, re-parsing the source document to rebuild
# original_ast. The resolved source path is attached as the
# "markermd_source_path" attribute, and the raw (unresolved) source.path string
# as "markermd_source_raw", so callers can preserve the source on re-save and
# locate the assignment by name even when the document is absent.
#
# x: List parsed from a template YAML file
# base_dir: Directory used to resolve a relative source.path
# assignment: Explicit assignment-path override (may be NULL)
# require_ast: When TRUE (the template() app) an unresolvable source is an
#   error; when FALSE (the mark() app) grading proceeds with an empty AST

template_from_list = function(x, base_dir = ".", assignment = NULL, require_ast = FALSE) {
  version = x$format_version
  if (is.null(version)) {
    stop("Template file is missing the required 'format_version' field.", call. = FALSE)
  }

  questions = if (is.null(x$questions)) list() else lapply(x$questions, question_from_list)

  source_path = resolve_template_source(x$source$path, base_dir = base_dir, assignment = assignment)

  if (!is.null(source_path)) {
    ast = parse_assignment_document(source_path)
  } else if (require_ast) {
    stop(
      "Could not locate the assignment document for this template",
      if (!is.null(x$source$path)) paste0(" (source.path: '", x$source$path, "')") else "",
      ".\nPass the assignment explicitly via the `assignment` argument, or fix source.path in the file.",
      call. = FALSE
    )
  } else {
    ast = empty_pandoc_ast()
  }

  metadata = metadata_from_list(
    if (is.null(x$metadata)) list() else x$metadata,
    version = as.character(version),
    total_nodes = length(q2r_flatten(ast))
  )

  template = markermd_template(
    original_ast = ast,
    questions = questions,
    metadata = metadata
  )
  attr(template, "markermd_source_path") = source_path
  attr(template, "markermd_source_raw") = x$source$path
  template
}

#' Write a markermd template to a YAML file
#'
#' Serializes a `markermd_template` to a human-readable, schema-validatable
#' YAML file. The assignment document is referenced by `source_path` (recorded
#' under `source.path`) rather than embedded; the parsed AST is regenerated on
#' load by re-parsing that document.
#'
#' @param template A `markermd_template` object.
#' @param path Output file path (`.yaml`).
#' @param source_path Optional path to the assignment document to record so the
#'   template can be re-opened and re-validated later.
#'
#' @return The output `path`, invisibly.
#' @export
write_template_yaml = function(template, path, source_path = NULL) {
  if (!S7::S7_inherits(template, markermd_template)) {
    stop("`template` must be a markermd_template object.", call. = FALSE)
  }
  yaml::write_yaml(template_to_list(template, source_path = source_path), path)
  invisible(path)
}

#' Read a markermd template from a YAML file
#'
#' Parses a template YAML file and rebuilds the `markermd_template` object,
#' re-parsing the referenced assignment document to regenerate its AST. All
#' validation is performed by the S7 constructors as objects are built.
#'
#' @param path Path to a template `.yaml`/`.yml` file.
#' @param assignment Optional path to the assignment document, overriding the
#'   `source.path` stored in the file (useful when the template has been moved).
#' @param require_ast When `TRUE`, an assignment document that cannot be located
#'   is an error (the template editor needs the AST to draw its tree). When
#'   `FALSE` (the default), grading proceeds with an empty AST.
#'
#' @return A `markermd_template` object.
#' @export
read_template_yaml = function(path, assignment = NULL, require_ast = FALSE) {
  if (!file.exists(path)) {
    stop("Template file does not exist: ", path, call. = FALSE)
  }
  x = yaml::read_yaml(path)
  template_from_list(x, base_dir = dirname(path), assignment = assignment, require_ast = require_ast)
}

#' Validate a template file against the markermd JSON Schema
#'
#' Structurally validates a template YAML (or JSON) file against the bundled
#' JSON Schema (`inst/schema/markermd-template.json`). This is an optional check
#' aimed at tooling and LLM-generated files; the authoritative validation
#' happens via the S7 constructors in [read_template_yaml()]. Requires the
#' suggested `jsonvalidate` package.
#'
#' @param path Path to a template `.yaml`/`.yml`/`.json` file.
#'
#' @return `TRUE` when the file conforms to the schema, otherwise `FALSE` with
#'   the validation errors attached as attributes (see [jsonvalidate::json_validate()]).
#' @export
validate_template_file = function(path) {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    stop("validate_template_file() requires the 'jsonvalidate' package.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Template file does not exist: ", path, call. = FALSE)
  }

  schema = system.file("schema/markermd-template.json", package = "markermd")
  x = yaml::read_yaml(path)

  # yaml::read_yaml collapses single-element sequences to length-1 vectors, which
  # jsonlite::toJSON(auto_unbox = TRUE) would then emit as JSON scalars. Keep
  # node_ids (and filter group/condition sequences) as arrays so single-element
  # cases still validate against the schema's array requirements.
  if (!is.null(x$questions)) {
    x$questions = lapply(x$questions, function(q) {
      if (!is.null(q$node_ids)) q$node_ids = as.list(q$node_ids)
      if (!is.null(q$filters)) {
        q$filters = lapply(as.list(q$filters), function(g) {
          if (!is.null(g$conditions)) g$conditions = as.list(g$conditions)
          g
        })
      }
      q
    })
  }

  json = jsonlite::toJSON(x, auto_unbox = TRUE)

  jsonvalidate::json_validate(json, schema, engine = "ajv", verbose = TRUE, error = FALSE)
}
