#' S7 Class for Rule Validation
#'
#' @description S7 class representing a single validation rule with built-in validation
#' for node type, verb, and values based on allowed values from helper functions.
#'
#' @name rule_classes
NULL

#' @title Markermd Validation Rule
#' @description S7 class representing a single validation rule for template questions.
#' This class provides built-in validation for all rule properties and ensures
#' consistency across the application.
#' @param node_type Character. The type of AST node this rule applies to.
#'   Must be one of the values returned by get_allowed_node_types().
#' @param verb Character. The validation verb/action to perform.
#'   Must be one of the values returned by get_allowed_rule_verbs().
#' @param values Vector. The values/parameters for the validation.
#'   Type and format depend on the verb:
#'   - "has count of": numeric vector of length 2 (min, max)
#'   - "has content": character string (pattern)
#'   - "does not have content": character string (pattern)  
#'   - "has name": character string (pattern)
#' @export
#' @examples
#' \dontrun{
#' # Create a count rule
#' count_rule = markermd_rule(
#'   node_type = "rmd_heading",
#'   verb = "has count of", 
#'   values = c(1, 5)
#' )
#'
#' # Create a content rule
#' content_rule = markermd_rule(
#'   node_type = "rmd_chunk",
#'   verb = "has content",
#'   values = "*plot*"
#' )
#'
#' # Validate a rule
#' validation = validate_markermd_rule(count_rule)
#' if (!validation$valid) {
#'   print(validation$errors)
#' }
#' }
markermd_rule = S7::new_class(
  "markermd_rule",
  properties = list(
    node_type = S7::new_property(
      S7::class_character,
      validator = function(value) {
        validate_node_type(value)
      }
    ),
    verb = S7::new_property(
      S7::class_character,
      validator = function(value) {
        validate_rule_verb(value)
      }
    ),
    values = S7::new_property(
      S7::class_any  # Can be numeric vector or character based on verb
      # Note: values validation done in class validator since it depends on verb
    )
  ),
  validator = function(self) {
    # Cross-validate values against verb type
    verb_values_error = validate_rule_values(self@verb, self@values)
    if (!is.null(verb_values_error)) {
      return(verb_values_error)
    }
    
    NULL
  },
  package = "markermd"
)

#' Create a new markermd rule with default values
#'
#' @description Convenience function to create a new rule with appropriate default values
#' based on the specified verb type.
#'
#' @param node_type Character. The node type (defaults to "Any node")
#' @param verb Character. The validation verb (defaults to "has count of")
#' @param values Vector. Custom values (if NULL, uses defaults for the verb)
#' @return markermd_rule object
#' @export
new_markermd_rule = function(node_type = "Any node", verb = "has count of", values = NULL) {
  # Use default values if none provided
  if (is.null(values)) {
    values = get_default_rule_values(verb)
  }
  
  markermd_rule(
    node_type = node_type,
    verb = verb,
    values = values
  )
}

#' Validate a markermd rule object
#'
#' @description Manually validate a rule object and return detailed error information.
#' This is useful for debugging or providing user feedback.
#'
#' @param rule markermd_rule object to validate
#' @return List with 'valid' (logical) and 'errors' (character vector) components
#' @export
validate_markermd_rule = function(rule) {
  if (!S7::S7_inherits(rule, markermd_rule)) {
    return(list(valid = FALSE, errors = "Object is not a markermd_rule"))
  }
  
  errors = character(0)
  
  # Validate node type
  node_type_error = validate_node_type(rule@node_type)
  if (!is.null(node_type_error)) {
    errors = c(errors, paste("Node type:", node_type_error))
  }
  
  # Validate verb
  verb_error = validate_rule_verb(rule@verb)
  if (!is.null(verb_error)) {
    errors = c(errors, paste("Verb:", verb_error))
  }
  
  # Validate values (only if verb is valid)
  if (is.null(verb_error)) {
    values_error = validate_rule_values(rule@verb, rule@values)
    if (!is.null(values_error)) {
      errors = c(errors, paste("Values:", values_error))
    }
  }
  
  list(
    valid = length(errors) == 0,
    errors = errors
  )
}

#' Convert rule object to list format
#'
#' @description Converts a markermd_rule S7 object to the list format used by
#' the existing rules module for backward compatibility.
#'
#' @param rule markermd_rule object
#' @param rule_id Numeric. ID to assign to the rule in list format
#' @return Named list with rule data
#' @export
rule_to_list = function(rule, rule_id = 1) {
  if (!S7::S7_inherits(rule, markermd_rule)) {
    stop("Object must be a markermd_rule")
  }
  
  list(
    id = rule_id,
    node_types = rule@node_type,
    verb = rule@verb,
    verb_inputs = switch(rule@verb,
      "has count of" = list(count_range = rule@values),
      "has content" = list(content_pattern = rule@values),
      "does not have content" = list(content_pattern = rule@values),
      "has name" = list(name_pattern = rule@values),
      list()
    ),
    delete_observer = NULL  # Will be set by the module
  )
}

#' Convert list format to rule object
#'
#' @description Converts a rule from the existing module list format to a
#' markermd_rule S7 object for validation and standardization.
#'
#' @param rule_list Named list with rule data from existing module
#' @return markermd_rule object
#' @export
list_to_rule = function(rule_list) {
  if (!is.list(rule_list)) {
    stop("Input must be a list")
  }
  
  # Extract node type
  node_type = rule_list$node_types
  if (is.null(node_type)) {
    node_type = "Any node"
  }
  
  # Extract verb
  verb = rule_list$verb
  if (is.null(verb)) {
    verb = "has count of"
  }
  
  # Extract values based on verb type
  values = switch(verb,
    "has count of" = {
      if (!is.null(rule_list$verb_inputs$count_range)) {
        rule_list$verb_inputs$count_range
      } else {
        get_default_rule_values(verb)
      }
    },
    "has content" = {
      if (!is.null(rule_list$verb_inputs$content_pattern)) {
        rule_list$verb_inputs$content_pattern
      } else {
        get_default_rule_values(verb)
      }
    },
    "does not have content" = {
      if (!is.null(rule_list$verb_inputs$content_pattern)) {
        rule_list$verb_inputs$content_pattern
      } else {
        get_default_rule_values(verb)
      }
    },
    "has name" = {
      if (!is.null(rule_list$verb_inputs$name_pattern)) {
        rule_list$verb_inputs$name_pattern
      } else {
        get_default_rule_values(verb)
      }
    },
    get_default_rule_values(verb)
  )
  
  markermd_rule(
    node_type = node_type,
    verb = verb,
    values = values
  )
}