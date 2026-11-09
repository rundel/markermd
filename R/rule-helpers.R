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
#' These are the same friendly category names shown in the AST tree (see
#' `q2r_node_kind()`), plus a catch-all "Any node" option.
#'
#' @return Character vector of allowed node types
#' @export
get_allowed_node_types = function() {
  c(
    "Any node",
    "Heading",
    "Markdown",
    "Chunk",
    "Code block",
    "Raw Block",
    "HTML comment",
    "Div",
    "Bullet list",
    "Ordered list",
    "Block quote",
    "Table",
    "Figure",
    "Horizontal rule",
    "Definition list",
    "Line block"
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
    "has at least",
    "has at most",
    "has between",
    "has content", 
    "lacks content",
    "has name"
  )
}

# Checks if a rule's node types are valid. A rule targets one or more node
# types combined as a logical OR, so this accepts a non-empty character
# vector of unique allowed types. Returns a character error message if
# invalid, NULL if valid.
#
# node_type: Character vector. The node type(s) to validate

validate_node_type = function(node_type) {
  if (!is.character(node_type)) {
    return("Node type must be a character vector")
  }

  if (length(node_type) < 1) {
    return("Node type must include at least one type")
  }

  if (any(is.na(node_type)) || any(nchar(node_type) == 0)) {
    return("Node type cannot contain empty or NA values")
  }

  if (anyDuplicated(node_type) > 0) {
    return("Node type values must be unique")
  }

  allowed_types = get_allowed_node_types()
  if (!all(node_type %in% allowed_types)) {
    return(paste0("Node type must be one of: ", paste(allowed_types, collapse = ", ")))
  }

  NULL
}

# Checks if a rule verb is valid according to the allowed verbs. Returns a
# character error message if invalid, NULL if valid.
#
# verb: Character. The verb to validate

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

# Validates rule values according to the specific requirements of each verb
# type; different verbs have different value requirements (numeric ranges,
# text patterns, etc.). Returns a character error message if invalid, NULL if
# valid.
#
# verb: Character. The rule verb that determines validation requirements
# values: List or vector. The values to validate

validate_rule_values = function(verb, values) {
  # First validate the verb itself
  verb_error = validate_rule_verb(verb)
  if (!is.null(verb_error)) {
    return(verb_error)
  }
  
  switch(verb,
    "has between" = {
      # Should be a numeric vector of length 2 (min, max)
      if (!is.numeric(values)) {
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

      if (any(values != floor(values))) {
        return("Count range values must be whole numbers")
      }

      if (values[1] > values[2]) {
        return("Count range minimum must be <= maximum")
      }

      NULL
    },
    
    "has at least" = ,
    "has at most" = {
      # Should be a single non-negative integer
      if (length(values) != 1) {
        return("Count value must be a single value")
      }

      if (!is.numeric(values)) {
        return("Count value must be numeric")
      }
      
      if (is.na(values) || !is.finite(values)) {
        return("Count value must be a finite number")
      }
      
      if (values < 0) {
        return("Count value must be non-negative")
      }
      
      if (values != floor(values)) {
        return("Count value must be a whole number")
      }
      
      NULL
    },
    
    "has content" = ,
    "lacks content" = {
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

# Coerce a rule's values to the one representation each verb stores: doubles
# for a "has between" range, an integer for a count, a character scalar for a
# pattern. The editor's numeric inputs report integers while the defaults and
# YAML readers produce doubles (or the reverse), and identical() would call
# that a change; normalizing on every write keeps the stored rule, the
# serialized template and the editor's dirty flag stable across a no-op
# report. Values of the wrong type are returned as is for the class
# validator to reject.
#
# verb: Character. The rule verb
# values: The values to normalize

normalize_rule_values = function(verb, values) {
  if (verb == "has between" && is.numeric(values)) {
    as.numeric(values)
  } else if (verb %in% c("has at least", "has at most") && is.numeric(values) &&
             all(is.finite(values)) && all(values == floor(values))) {
    as.integer(values)
  } else if (verb %in% c("has content", "lacks content", "has name") && is.character(values)) {
    as.character(values)
  } else {
    values
  }
}

# Returns appropriate default values for each rule verb type. Used when
# creating new rules or resetting rule values.
#
# verb: Character. The rule verb

get_default_rule_values = function(verb) {
  switch(verb,
    "has between" = c(0, 10),
    "has at least" = 1L,
    "has at most" = 1L,
    "has content" = "",
    "lacks content" = "",
    "has name" = "",
    NULL  # For unknown verbs
  )
}

