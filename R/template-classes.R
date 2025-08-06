#' S7 Class Definitions for Markermd Templates
#'
#' @description S7 classes for representing markermd template structures with automatic validation.
#' These classes replace the previous list-based template format with type-safe objects
#' that provide built-in validation and better error messages.
#'
#' @name template_classes
NULL

#' @title Node Selection for Questions
#' @description S7 class representing selected AST nodes for a template question
#' @param indices Integer vector of 1-based AST node indices
#' @export
markermd_node_selection = S7::new_class(
  "markermd_node_selection",
  properties = list(
    indices = S7::new_property(
      S7::class_integer, 
      default = quote(integer(0))
    )
  ),
  validator = function(self) {
    if (length(self@indices) > 0 && (any(is.na(self@indices)) || !all(is.finite(self@indices)))) {
      return("All node indices must be finite integers")
    }
    if (any(self@indices < 1)) {
      return("All node indices must be >= 1 (1-based indexing)")
    }
    if (any(duplicated(self@indices))) {
      return("Node indices must be unique")
    }
    NULL
  },
  package = "markermd"
)

#' @title Template Question
#' @description S7 class representing a single template question with selected nodes and validation rules
#' @param id Integer. Unique question identifier
#' @param name Character. Question display name
#' @param selected_nodes node_selection. Selected AST nodes for this question
#' @param rules List. Validation rules for this question as markermd_rule objects
#' @export
markermd_question = S7::new_class(
  "markermd_question",
  properties = list(
    id = S7::new_property(
      S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) {
          "@id must be a single integer"
        } else if (value < 1) {
          "@id must be positive"
        }
      }
    ),
    name = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1) {
          "@name must be a single character string"
        } else if (nchar(value) == 0) {
          "@name cannot be empty"
        }
      }
    ),
    selected_nodes = S7::new_property(
      markermd_node_selection,
      default = quote(markermd_node_selection())
    ),
    rules = S7::new_property(
      S7::class_list,
      default = quote(list()),
      validator = function(value) {
        # Check all elements are markermd_rule objects
        for (i in seq_along(value)) {
          if (!S7::S7_inherits(value[[i]], markermd_rule)) {
            return(paste0("rules[[", i, "]] must be a markermd_rule object"))
          }
        }
        NULL
      }
    )
  ),
  package = "markermd"
)

#' @title Template Metadata
#' @description S7 class representing template creation and modification metadata
#' @param created_at POSIXct. Template creation timestamp
#' @param created_by Character. User who created the template
#' @param total_nodes Integer. Total number of nodes in original AST
#' @param version Character. Template format version
#' @export
markermd_metadata = S7::new_class(
  "markermd_metadata",
  properties = list(
    created_at = S7::new_property(
      S7::class_POSIXct,
      default = quote(Sys.time())
    ),
    created_by = S7::new_property(
      S7::class_character,
      default = quote(Sys.getenv("USER", "unknown")),
      validator = function(value) {
        if (length(value) != 1) {
          "@created_by must be a single character string"
        }
      }
    ),
    total_nodes = S7::new_property(
      S7::class_integer,
      default = quote(0L),
      validator = function(value) {
        if (length(value) != 1) {
          "@total_nodes must be a single integer"
        } else if (value < 0) {
          "@total_nodes must be non-negative"
        }
      }
    ),
    version = S7::new_property(
      S7::class_character,
      default = quote("1.0"),
      validator = function(value) {
        if (length(value) != 1) {
          "@version must be a single character string"
        }
      }
    )
  ),
  package = "markermd"
)

#' @title Markermd Template
#' @description S7 class representing a complete markermd template with questions and metadata
#' @param original_ast rmd_ast. The original parsed AST from parsermd
#' @param questions List of question objects
#' @param metadata template_metadata. Template metadata
#' @export
markermd_template = S7::new_class(
  "markermd_template",
  properties = list(
    original_ast = S7::new_property(
      S7::class_any,  # Will validate manually since we need parsermd classes
      validator = function(value) {
        if (!inherits(value, "rmd_ast")) {
          return("@original_ast must be an rmd_ast object from parsermd")
        }
        NULL
      }
    ),
    questions = S7::new_property(
      S7::class_list,
      default = quote(list()),
      validator = function(value) {
        # Check all elements are question objects
        for (i in seq_along(value)) {
          if (!S7::S7_inherits(value[[i]], markermd_question)) {
            return(paste0("questions[[", i, "]] must be a markermd_question object"))
          }
        }
        
        # Check question IDs are unique
        if (length(value) > 0) {
          ids = sapply(value, function(q) q@id)
          if (any(duplicated(ids))) {
            return("Question IDs must be unique")
          }
        }
        
        # Check question names are unique
        if (length(value) > 0) {
          names = sapply(value, function(q) q@name)
          if (any(duplicated(names))) {
            return("Question names must be unique")
          }
        }
        
        NULL
      }
    ),
    metadata = S7::new_property(
      markermd_metadata,
      default = quote(markermd_metadata())
    )
  ),
  validator = function(self) {
    # Validate that node selections are within bounds of the AST
    if (!is.null(self@original_ast) && length(self@questions) > 0) {
      n_nodes = length(self@original_ast@nodes)
      
      for (i in seq_along(self@questions)) {
        q = self@questions[[i]]
        indices = q@selected_nodes@indices
        
        if (length(indices) > 0 && any(indices > n_nodes)) {
          invalid_indices = indices[indices > n_nodes]
          return(paste0("Question '", q@name, "' has invalid node indices (> ", n_nodes, "): ", 
                       paste(invalid_indices, collapse = ", ")))
        }
      }
    }
    
    NULL
  },
  package = "markermd"
)

#' @title Rubric Item
#' @description S7 class representing a single rubric item with hotkey, points, description and selection state
#' @param hotkey Integer. Hotkey number (0-9) for keyboard selection
#' @param points Numeric. Point value for this rubric item
#' @param description Character. Description text for the rubric item
#' @param selected Logical. Whether this item is currently selected
#' @export
markermd_rubric_item = S7::new_class(
  "markermd_rubric_item",
  properties = list(
    hotkey = S7::new_property(
      S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) {
          "hotkey must be a single integer"
        } else if (value < 0 || value > 9) {
          "hotkey must be between 0 and 9"
        }
      }
    ),
    points = S7::new_property(
      S7::class_numeric,
      validator = function(value) {
        if (length(value) != 1) {
          "points must be a single numeric value"
        }
      }
    ),
    description = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1) {
          "description must be a single character string"
        }
      }
    ),
    selected = S7::new_property(
      S7::class_logical,
      default = quote(FALSE),
      validator = function(value) {
        if (length(value) != 1) {
          "selected must be a single logical value"
        }
      }
    )
  ),
  package = "markermd"
)