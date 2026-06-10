#' Helper Functions for Question Filters
#'
#' @description Centralized helper functions that define allowed values for
#' filter conditions and build the q2r predicate expressions they map onto.
#' These functions serve as the single source of truth for filter validation.
#'
#' @name filter_helpers
NULL

#' Get allowed filter condition types
#'
#' @description Returns the list of valid condition types that can be used in
#' question filters. Each type corresponds to a q2r predicate: "node type"
#' becomes an `is(<pandoc class>)` test while the others map directly onto the
#' q2r mask helpers of the same name.
#'
#' @return Character vector of allowed filter condition types
#' @export
get_allowed_filter_condition_types = function() {
  c(
    "node type",
    "has class",
    "has id",
    "has text",
    "has label",
    "has option",
    "has engine"
  )
}

#' Validate filter condition type
#'
#' @description Checks if a filter condition type is valid according to the
#' allowed types.
#'
#' @param type Character. The condition type to validate
#' @return Character error message if invalid, NULL if valid
#' @export
validate_filter_condition_type = function(type) {
  if (length(type) != 1) {
    return("Filter condition type must be a single character string")
  }

  if (!is.character(type)) {
    return("Filter condition type must be a character string")
  }

  if (is.na(type) || nchar(type) == 0) {
    return("Filter condition type cannot be empty or NA")
  }

  allowed_types = get_allowed_filter_condition_types()
  if (!type %in% allowed_types) {
    return(paste0("Filter condition type must be one of: ", paste(allowed_types, collapse = ", ")))
  }

  NULL
}

#' Validate filter condition value based on condition type
#'
#' @description Validates a filter condition value according to the
#' requirements of its condition type. "node type" values must be a node kind
#' from get_allowed_node_types() (excluding "Any node"); the other types take a
#' single character string (empty allowed).
#'
#' @param type Character. The condition type that determines validation requirements
#' @param value Character. The value to validate
#' @return Character error message if invalid, NULL if valid
#' @export
validate_filter_condition_value = function(type, value) {
  type_error = validate_filter_condition_type(type)
  if (!is.null(type_error)) {
    return(type_error)
  }

  if (length(value) != 1) {
    return("Filter condition value must be a single value")
  }

  if (!is.character(value)) {
    return("Filter condition value must be a character string")
  }

  if (is.na(value)) {
    return("Filter condition value cannot be NA")
  }

  if (type == "node type") {
    allowed_kinds = setdiff(get_allowed_node_types(), "Any node")
    if (!value %in% allowed_kinds) {
      return(paste0("Node type value must be one of: ", paste(allowed_kinds, collapse = ", ")))
    }
  }

  NULL
}

#' Get default value for a filter condition type
#'
#' @description Returns an appropriate default value for each filter condition
#' type. Used when creating new conditions or resetting after a type change.
#'
#' @param type Character. The condition type
#' @return Default value appropriate for the condition type
#' @export
get_default_filter_condition_value = function(type) {
  switch(type,
    "node type" = "Div",
    "has class" = ,
    "has id" = ,
    "has text" = ,
    "has label" = ,
    "has option" = ,
    "has engine" = "",
    NULL  # For unknown types
  )
}

# Parse a "has option" condition value into has_option() arguments. The syntax
# mirrors a Quarto "#|" option line: "eval" tests key presence, "eval: false"
# tests the key's value. The value part is converted to the YAML scalar type
# cell_options() produces (logical / integer / double / string) since q2r
# compares option values with identical().
#
# value: The condition's value string

parse_filter_option = function(value) {
  sep = regexpr(":", value, fixed = TRUE)
  if (sep == -1) {
    return(list(key = trimws(value), value = NULL))
  }

  key = trimws(substr(value, 1, sep - 1))
  raw = trimws(substr(value, sep + 1, nchar(value)))
  if (nchar(raw) == 0) {
    return(list(key = key, value = NULL))
  }

  parsed = if (tolower(raw) %in% c("true", "yes", "on")) {
    TRUE
  } else if (tolower(raw) %in% c("false", "no", "off")) {
    FALSE
  } else if (grepl("^-?[0-9]+$", raw)) {
    as.integer(raw)
  } else if (!is.na(suppressWarnings(as.numeric(raw)))) {
    as.numeric(raw)
  } else {
    raw
  }

  list(key = key, value = parsed)
}

# Quoted q2r predicate expressions for each friendly node kind, the reverse of
# q2r_node_kind(). "Chunk" / "Code block" both correspond to pandoc_code_block
# and are distinguished by is_code_cell(); "Markdown" covers paragraphs and
# plain blocks. is() and is_code_cell() resolve in q2r's predicate data mask.

filter_node_kind_exprs = list(
  "Heading" = quote(is(q2r::pandoc_header)),
  "Markdown" = quote(is(q2r::pandoc_paragraph) | is(q2r::pandoc_plain)),
  "Chunk" = quote(is_code_cell()),
  "Code block" = quote(is(q2r::pandoc_code_block) & !is_code_cell()),
  "Raw Block" = quote(is(q2r::pandoc_raw_block)),
  "Div" = quote(is(q2r::pandoc_div)),
  "Bullet list" = quote(is(q2r::pandoc_bullet_list)),
  "Ordered list" = quote(is(q2r::pandoc_ordered_list)),
  "Block quote" = quote(is(q2r::pandoc_block_quote)),
  "Table" = quote(is(q2r::pandoc_table)),
  "Figure" = quote(is(q2r::pandoc_figure)),
  "Horizontal rule" = quote(is(q2r::pandoc_horizontal_rule)),
  "Definition list" = quote(is(q2r::pandoc_definition_list)),
  "Line block" = quote(is(q2r::pandoc_line_block))
)

# Quoted q2r predicate expression for a friendly node kind, errors on unknown
# kinds so vocabulary drift from q2r_node_kind() is caught loudly
#
# kind: Single node kind string (a get_allowed_node_types() value, not "Any node")

filter_node_kind_expr = function(kind) {
  expr = filter_node_kind_exprs[[kind]]
  if (is.null(expr)) {
    stop("Unknown node kind for filter: ", kind)
  }
  expr
}

# Whether an expression is a call to a binary logical operator (& or |), used
# to decide when parentheses are needed while folding conditions and groups
#
# expr: An R call object or other language object

is_logical_op_call = function(expr) {
  is.call(expr) &&
    (identical(expr[[1]], as.name("&")) || identical(expr[[1]], as.name("|")))
}

# Negate a predicate expression, parenthesizing compound expressions (logical
# operators or an existing negation) so the result reads unambiguously:
# !has_class("x"), !(a & b), !(!has_class("x"))
#
# expr: An R call object

negate_expr = function(expr) {
  needs_parens = is_logical_op_call(expr) ||
    (is.call(expr) && identical(expr[[1]], as.name("!")))
  if (needs_parens) {
    expr = call("(", expr)
  }
  call("!", expr)
}

# Build the q2r predicate call for a single filter condition. User-entered
# values are inserted as literal call arguments (never pasted into a string),
# so quoting/escaping is handled by R itself.
#
# condition: markermd_filter_condition S7 object

filter_condition_expr = function(condition) {
  expr = switch(condition@type,
    "node type" = filter_node_kind_expr(condition@value),
    "has class" = call("has_class", condition@value),
    "has id" = call("has_id", condition@value),
    "has text" = call("has_text", condition@value),
    "has label" = call("has_label", condition@value),
    "has option" = {
      opt = parse_filter_option(condition@value)
      if (is.null(opt$value)) {
        call("has_option", opt$key)
      } else {
        call("has_option", opt$key, opt$value)
      }
    },
    "has engine" = call("has_engine", condition@value),
    stop("Unknown filter condition type: ", condition@type)
  )

  if (condition@negate) negate_expr(expr) else expr
}

# Build the AND-combined predicate expression for a filter group. Condition
# expressions containing a logical operator (e.g. the "Markdown" kind, which
# expands to an |) are parenthesized before AND-folding since & binds tighter
# than | (negated conditions are ! calls, which bind tighter than &, so they
# need no parens). A negated group wraps the whole fold in !(...). Returns
# NULL for an empty group.
#
# group: markermd_filter_group S7 object

filter_group_expr = function(group) {
  conditions = group@conditions
  if (length(conditions) == 0) {
    return(NULL)
  }

  exprs = lapply(conditions, filter_condition_expr)

  if (length(exprs) > 1) {
    exprs = lapply(exprs, function(e) {
      if (is_logical_op_call(e)) call("(", e) else e
    })
  }

  expr = Reduce(function(acc, e) call("&", acc, e), exprs)

  if (group@negate) negate_expr(expr) else expr
}

# Build the full OR-of-AND-groups predicate expression for a question's
# filters. Group expressions containing a logical operator are parenthesized
# when there are multiple groups so the rendered expression is unambiguous.
# Returns NULL when no group has any conditions (i.e. no filtering).
#
# filters: List of markermd_filter_group S7 objects

filters_expr = function(filters) {
  group_exprs = Filter(Negate(is.null), lapply(filters, filter_group_expr))
  if (length(group_exprs) == 0) {
    return(NULL)
  }

  if (length(group_exprs) > 1) {
    group_exprs = lapply(group_exprs, function(e) {
      if (is_logical_op_call(e)) call("(", e) else e
    })
  }

  Reduce(function(acc, e) call("|", acc, e), group_exprs)
}

# Deparse a question's filter expression as the q2r call shown in the UI
# preview. Returns NULL when the filters have no effect.
#
# filters: List of markermd_filter_group S7 objects

filters_expr_text = function(filters) {
  expr = filters_expr(filters)
  if (is.null(expr)) {
    return(NULL)
  }

  expr_text = paste(deparse(expr, width.cutoff = 500L), collapse = " ")
  paste0("select_children(nodes, ", expr_text, ")")
}
