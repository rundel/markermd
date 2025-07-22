#' Helper Functions for Rule Validation
#'
#' @description Centralized helper functions that define allowed values for rule validation.
#' These functions serve as the single source of truth for rule validation values.
#'
#' @name rule_helpers
NULL

#' Get allowed node types for rules
#'
#' @description Returns the list of valid node types that can be used in validation rules.
#' This includes both a catch-all "Any node" option and specific parsermd node types.
#'
#' @return Character vector of allowed node types
#' @export
get_allowed_node_types = function() {
  c(
    "Any node",
    "rmd_yaml",
    "rmd_heading", 
    "rmd_chunk",
    "rmd_raw_chunk",
    "rmd_markdown",
    "rmd_code_block",
    "rmd_fenced_div_open",
    "rmd_fenced_div_close"
  )
}

#' Get allowed verbs for rules
#'
#' @description Returns the list of valid verification verbs that can be used in validation rules.
#' Each verb corresponds to a different type of validation check.
#'
#' @return Character vector of allowed rule verbs
#' @export
get_allowed_rule_verbs = function() {
  c(
    "has count of",
    "has content", 
    "does not have content",
    "has name"
  )
}

#' Validate node type value
#'
#' @description Checks if a node type is valid according to the allowed types.
#'
#' @param node_type Character. The node type to validate
#' @return Character error message if invalid, NULL if valid
#' @export
validate_node_type = function(node_type) {
  if (length(node_type) != 1) {
    return("Node type must be a single character string")
  }
  
  if (!is.character(node_type)) {
    return("Node type must be a character string")
  }
  
  if (is.na(node_type) || nchar(node_type) == 0) {
    return("Node type cannot be empty or NA")
  }
  
  allowed_types = get_allowed_node_types()
  if (!node_type %in% allowed_types) {
    return(paste0("Node type must be one of: ", paste(allowed_types, collapse = ", ")))
  }
  
  NULL
}

#' Validate rule verb value
#'
#' @description Checks if a rule verb is valid according to the allowed verbs.
#'
#' @param verb Character. The verb to validate
#' @return Character error message if invalid, NULL if valid
#' @export
validate_rule_verb = function(verb) {
  if (length(verb) != 1) {
    return("Rule verb must be a single character string")
  }
  
  if (!is.character(verb)) {
    return("Rule verb must be a character string")
  }
  
  if (is.na(verb) || nchar(verb) == 0) {
    return("Rule verb cannot be empty or NA")
  }
  
  allowed_verbs = get_allowed_rule_verbs()
  if (!verb %in% allowed_verbs) {
    return(paste0("Rule verb must be one of: ", paste(allowed_verbs, collapse = ", ")))
  }
  
  NULL
}

#' Validate rule values based on verb type
#'
#' @description Validates rule values according to the specific requirements of each verb type.
#' Different verbs have different value requirements (numeric ranges, text patterns, etc.).
#'
#' @param verb Character. The rule verb that determines validation requirements
#' @param values List or vector. The values to validate
#' @return Character error message if invalid, NULL if valid
#' @export
validate_rule_values = function(verb, values) {
  # First validate the verb itself
  verb_error = validate_rule_verb(verb)
  if (!is.null(verb_error)) {
    return(verb_error)
  }
  
  switch(verb,
    "has count of" = {
      # Should be a numeric vector of length 2 (min, max)
      if (!is.numeric(values) && !is.integer(values)) {
        return("Count range values must be numeric")
      }
      
      if (length(values) != 2) {
        return("Count range must have exactly 2 values (min, max)")
      }
      
      if (any(is.na(values)) || any(!is.finite(values))) {
        return("Count range values must be finite numbers")
      }
      
      if (any(values < 0)) {
        return("Count range values must be non-negative")
      }
      
      if (values[1] > values[2]) {
        return("Count range minimum must be <= maximum")
      }
      
      NULL
    },
    
    "has content" = ,
    "does not have content" = {
      # Should be a character string (pattern)
      if (length(values) != 1) {
        return("Content pattern must be a single value")
      }
      
      if (!is.character(values)) {
        return("Content pattern must be a character string")
      }
      
      if (is.na(values)) {
        return("Content pattern cannot be NA")
      }
      
      # Empty string is allowed for content patterns
      NULL
    },
    
    "has name" = {
      # Should be a character string (pattern)
      if (length(values) != 1) {
        return("Name pattern must be a single value")
      }
      
      if (!is.character(values)) {
        return("Name pattern must be a character string")
      }
      
      if (is.na(values)) {
        return("Name pattern cannot be NA")
      }
      
      # Empty string is allowed for name patterns
      NULL
    },
    
    # Default case for unknown verbs (should not happen with proper verb validation)
    paste0("Unknown verb type: ", verb)
  )
}

#' Get default values for a rule verb
#'
#' @description Returns appropriate default values for each rule verb type.
#' Used when creating new rules or resetting rule values.
#'
#' @param verb Character. The rule verb
#' @return Default values appropriate for the verb type
#' @export
get_default_rule_values = function(verb) {
  switch(verb,
    "has count of" = c(0, 10),
    "has content" = "",
    "does not have content" = "",
    "has name" = "",
    NULL  # For unknown verbs
  )
}

#' Convert S7 Rule to List Format
#'
#' @description Converts an S7 markermd_rule object to the list format expected by the rules module.
#' This is used when loading existing templates in the template interface.
#'
#' @param rule S7 markermd_rule object
#' @param rule_id Integer. ID to assign to the rule in list format
#' @return List with rule data formatted for the rules module
#' @export
rule_to_list = function(rule, rule_id) {
  if (!S7::S7_inherits(rule, markermd_rule)) {
    stop("rule must be a markermd_rule S7 object")
  }
  
  list(
    id = as.integer(rule_id),
    node_types = rule@node_type,  # Note: UI expects 'node_types' (plural)
    verb = rule@verb,
    values = rule@values
  )
}

