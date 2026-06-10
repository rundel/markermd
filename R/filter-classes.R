#' S7 Classes for Question Filters
#'
#' @description S7 classes representing per-question node filters. A question
#' holds a list of filter groups; conditions within a group are combined with
#' logical AND, and groups are combined with logical OR (disjunctive normal
#' form). Filters narrow the question's node set before its rules evaluate,
#' using q2r predicate filtering (e.g. `has_class()`, `has_text()`).
#'
#' @name filter_classes
NULL

#' @title Markermd Filter Condition
#' @description S7 class representing a single filter condition. Each condition
#' maps to one q2r predicate: a node-type test (`is(<pandoc class>)`) or one of
#' the attribute/content helpers (`has_class()`, `has_id()`, `has_text()`,
#' `has_label()`, `has_option()`, `has_engine()`).
#' @param type Character. The condition type. Must be one of the values
#'   returned by get_allowed_filter_condition_types().
#' @param value Character. The condition value. For "node type" this must be a
#'   node kind from get_allowed_node_types() (excluding "Any node"); for
#'   "has option" it is a Quarto cell option as written on a `#|` line
#'   ("eval" for key presence, "eval: false" for a key/value test); for
#'   "has engine" it is a cell engine name (e.g. "r"); for the other types it
#'   is the class name, id, regex pattern, or glob pattern.
#' @param negate Logical. When TRUE the predicate is negated (logical NOT).
#' @export
#' @examples
#' # Match divs
#' type_condition = markermd_filter_condition(
#'   type = "node type",
#'   value = "Div"
#' )
#'
#' # Match nodes carrying a class
#' class_condition = markermd_filter_condition(
#'   type = "has class",
#'   value = "hint"
#' )
markermd_filter_condition = S7::new_class(
  "markermd_filter_condition",
  properties = list(
    type = S7::new_property(
      S7::class_character,
      validator = function(value) {
        validate_filter_condition_type(value)
      }
    ),
    value = S7::new_property(
      S7::class_character
      # Note: value validation done in class validator since it depends on type
    ),
    negate = S7::new_property(
      S7::class_logical,
      default = quote(FALSE),
      validator = function(value) {
        if (length(value) != 1 || is.na(value)) {
          return("@negate must be a single non-NA logical")
        }
        NULL
      }
    )
  ),
  validator = function(self) {
    validate_filter_condition_value(self@type, self@value)
  },
  package = "markermd"
)

#' @title Markermd Filter Group
#' @description S7 class representing a group of filter conditions combined
#' with logical AND. A question's filter groups are combined with logical OR.
#' An empty group is allowed as transient editing state and is ignored during
#' evaluation and serialization.
#' @param conditions List of markermd_filter_condition objects
#' @param negate Logical. When TRUE the whole group's predicate is negated
#'   (logical NOT around the ANDed conditions).
#' @export
markermd_filter_group = S7::new_class(
  "markermd_filter_group",
  properties = list(
    conditions = S7::new_property(
      S7::class_list,
      default = quote(list()),
      validator = function(value) {
        for (i in seq_along(value)) {
          if (!S7::S7_inherits(value[[i]], markermd_filter_condition)) {
            return(paste0("conditions[[", i, "]] must be a markermd_filter_condition object"))
          }
        }
        NULL
      }
    ),
    negate = S7::new_property(
      S7::class_logical,
      default = quote(FALSE),
      validator = function(value) {
        if (length(value) != 1 || is.na(value)) {
          return("@negate must be a single non-NA logical")
        }
        NULL
      }
    )
  ),
  package = "markermd"
)

#' Create a new markermd filter condition with default values
#'
#' @description Convenience function to create a new filter condition with an
#' appropriate default value for the specified condition type.
#'
#' @param type Character. The condition type (defaults to "node type")
#' @param value Character. Custom value (if NULL, uses the default for the type)
#' @param negate Logical. Whether the condition is negated (defaults to FALSE)
#' @return markermd_filter_condition object
#' @export
new_markermd_filter_condition = function(type = "node type", value = NULL, negate = FALSE) {
  if (is.null(value)) {
    value = get_default_filter_condition_value(type)
  }

  markermd_filter_condition(
    type = type,
    value = value,
    negate = negate
  )
}

#' Create a new markermd filter group
#'
#' @description Convenience function to create a new filter group. By default
#' the group contains a single default condition.
#'
#' @param conditions List of markermd_filter_condition objects (if NULL, a
#'   single default condition is used)
#' @param negate Logical. Whether the group is negated (defaults to FALSE)
#' @return markermd_filter_group object
#' @export
new_markermd_filter_group = function(conditions = NULL, negate = FALSE) {
  if (is.null(conditions)) {
    conditions = list(new_markermd_filter_condition())
  }

  markermd_filter_group(conditions = conditions, negate = negate)
}
