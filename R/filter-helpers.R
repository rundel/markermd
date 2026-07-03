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

# Checks if a filter condition type is valid according to the allowed types.
# Returns a character error message if invalid, NULL if valid.
#
# type: Character. The condition type to validate

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

# Validates a filter condition value according to the requirements of its
# condition type. "node type" values are one or more node kinds from
# get_allowed_node_types() (excluding "Any node"), combined as a logical OR;
# the other types take a single character string (empty allowed). Returns a
# character error message if invalid, NULL if valid.
#
# type: Character. The condition type that determines validation requirements
# value: Character. The value to validate

validate_filter_condition_value = function(type, value) {
  type_error = validate_filter_condition_type(type)
  if (!is.null(type_error)) {
    return(type_error)
  }

  if (!is.character(value)) {
    return("Filter condition value must be a character string")
  }

  if (type == "node type") {
    # One or more kinds, ORed: the kinds are pairwise disjoint, so requiring a
    # single kind per condition would make a second ANDed node-type condition
    # silently unsatisfiable
    if (length(value) < 1) {
      return("Node type value must include at least one node kind")
    }
    if (any(is.na(value))) {
      return("Filter condition value cannot be NA")
    }
    allowed_kinds = setdiff(get_allowed_node_types(), "Any node")
    if (!all(value %in% allowed_kinds)) {
      return(paste0("Node type value must be one of: ", paste(allowed_kinds, collapse = ", ")))
    }
    return(NULL)
  }

  if (length(value) != 1) {
    return("Filter condition value must be a single value")
  }

  if (is.na(value)) {
    return("Filter condition value cannot be NA")
  }

  NULL
}

# Display choices for the filter condition-type select: the
# get_allowed_filter_condition_types() values with display labels annotating
# each type's matching semantics (exact, regex, glob, or the option
# mini-syntax), so the semantics stay visible after a value has been typed.
# Returns a named character vector suitable for shiny select choices.

filter_condition_type_choices = function() {
  c(
    "node type" = "node type",
    "has class (exact)" = "has class",
    "has id (exact)" = "has id",
    "has text (regex)" = "has text",
    "has label (glob)" = "has label",
    "has option (key: value)" = "has option",
    "has engine (exact)" = "has engine"
  )
}

# Returns an appropriate default value for each filter condition type. Used
# when creating new conditions or resetting after a type change.
#
# type: Character. The condition type

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
  } else if (grepl("^-?[0-9]+$", raw) && !is.na(suppressWarnings(as.integer(raw)))) {
    as.integer(raw)
  } else if (!is.na(suppressWarnings(as.numeric(raw)))) {
    # An integer literal too large for as.integer() (NA above) falls through to
    # a double here rather than silently becoming NA.
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
    cli::cli_abort("Unknown node kind for filter: {kind}")
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
    "node type" = {
      # Multiple kinds within one condition are ORed (the kinds are pairwise
      # disjoint, so AND would never match); the group-level fold
      # parenthesizes this compound expression before ANDing
      kind_exprs = lapply(condition@value, filter_node_kind_expr)
      Reduce(function(acc, e) call("|", acc, e), kind_exprs)
    },
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
    cli::cli_abort("Unknown filter condition type: {condition@type}")
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

# Whether a value compiles as a regular expression. Trying the regex is the
# only way to check it, hence the tryCatch.
#
# pattern: Character scalar

is_valid_regex = function(pattern) {
  probe = tryCatch(grepl(pattern, ""), error = function(e) e, warning = function(w) w)
  !inherits(probe, "condition")
}

# Human-readable warnings for filter condition values that silently misfire:
# uncompilable regexes, empty values (has_text("") matches every node while
# has_class("") matches none), and option tests without a key
#
# filters: List of markermd_filter_group S7 objects
# Returns: Character vector of warning messages (empty when all values are fine)

filter_value_warnings = function(filters) {
  msgs = character(0)
  for (gi in seq_along(filters)) {
    for (cond in filters[[gi]]@conditions) {
      value = cond@value
      if (cond@type == "has text") {
        if (nchar(value) == 0) {
          effect = if (cond@negate) "matches no nodes" else "matches every node"
          msgs = c(msgs, glue::glue("Group {gi}: an empty \"has text\" pattern {effect}."))
        } else if (!is_valid_regex(value)) {
          msgs = c(msgs, glue::glue("Group {gi}: \"{value}\" is not a valid regular expression."))
        }
      } else if (cond@type %in% c("has class", "has id", "has label", "has engine") &&
                 nchar(value) == 0) {
        effect = if (cond@negate) "matches every node" else "matches no nodes"
        msgs = c(msgs, glue::glue("Group {gi}: an empty \"{cond@type}\" value {effect}."))
      } else if (cond@type == "has option" && nchar(parse_filter_option(value)$key) == 0) {
        msgs = c(msgs, glue::glue("Group {gi}: \"has option\" needs an option key."))
      }
    }
  }
  msgs
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
