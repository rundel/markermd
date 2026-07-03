#' S7 Class Definitions for Markermd Templates
#'
#' @description S7 classes for representing markermd template structures with automatic validation.
#' These classes replace the previous list-based template format with type-safe objects
#' that provide built-in validation and better error messages.
#'
#' @name template_classes
NULL

# Current template format version. Bumped whenever the persisted template shape
# changes incompatibly (e.g. node-selection representation, rule vocabulary).
# assert_template_compatible() rejects templates older than this.

markermd_template_version = function() "3.0"

#' @title Node Selection for Questions
#' @description S7 class representing the headings and id'd divs a template
#'   question targets, identified by their q2r/Pandoc ids (header ids or div
#'   ids). An empty vector means no selection (the whole document).
#' @param node_ids Character vector of document-unique node ids (header or div ids)
#' @return A `markermd_node_selection` S7 object.
#' @export
markermd_node_selection = S7::new_class(
  "markermd_node_selection",
  properties = list(
    node_ids = S7::new_property(
      S7::class_character,
      default = quote(character(0))
    )
  ),
  validator = function(self) {
    if (anyNA(self@node_ids)) {
      return("Node ids must not be NA")
    }
    if (any(nchar(self@node_ids) == 0)) {
      return("Node ids must be non-empty strings")
    }
    if (any(duplicated(self@node_ids))) {
      return("Node ids must be unique")
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
#' @param filters List. Filter groups for this question as markermd_filter_group
#'   objects. Conditions within a group are ANDed, groups are ORed; filters
#'   narrow the question's node set before rules evaluate.
#' @return A `markermd_question` S7 object.
#' @export
markermd_question = S7::new_class(
  "markermd_question",
  properties = list(
    id = S7::new_property(
      S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) {
          return("@id must be a single integer")
        }
        if (is.na(value) || value < 1) {
          return("@id must be a positive integer")
        }
        return(NULL)
      }
    ),
    name = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1) {
          return("@name must be a single character string")
        }
        if (nchar(value) == 0) {
          return("@name cannot be empty")
        }
        return(NULL)
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
    ),
    filters = S7::new_property(
      S7::class_list,
      default = quote(list()),
      validator = function(value) {
        # Check all elements are markermd_filter_group objects
        for (i in seq_along(value)) {
          if (!S7::S7_inherits(value[[i]], markermd_filter_group)) {
            return(paste0("filters[[", i, "]] must be a markermd_filter_group object"))
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
#' @return A `markermd_metadata` S7 object.
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
          return("@created_by must be a single character string")
        }
        return(NULL)
      }
    ),
    total_nodes = S7::new_property(
      S7::class_integer,
      default = quote(0L),
      validator = function(value) {
        if (length(value) != 1) {
          return("@total_nodes must be a single integer")
        }
        if (is.na(value) || value < 0) {
          return("@total_nodes must be a non-negative integer")
        }
        return(NULL)
      }
    ),
    version = S7::new_property(
      S7::class_character,
      default = quote(markermd_template_version()),
      validator = function(value) {
        if (length(value) != 1) {
          return("@version must be a single character string")
        }
        return(NULL)
      }
    )
  ),
  package = "markermd"
)

#' @title Markermd Template
#' @description S7 class representing a complete markermd template with questions and metadata
#' @param original_ast pandoc. The original parsed AST from q2r
#' @param questions List of question objects
#' @param metadata template_metadata. Template metadata
#' @return A `markermd_template` S7 object.
#' @export
markermd_template = S7::new_class(
  "markermd_template",
  properties = list(
    original_ast = S7::new_property(
      S7::class_any,  # Will validate manually since we need q2r classes
      validator = function(value) {
        if (!S7::S7_inherits(value, q2r::pandoc)) {
          return("@original_ast must be a pandoc object from q2r")
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
  package = "markermd"
)

#' @title Rubric Item
#' @description S7 class representing a single rubric item with hotkey, points, description and selection state
#' @param hotkey Integer. Hotkey number (1-10) for keyboard selection, or NA for
#'   items past the first ten (which have no hotkey)
#' @param points Numeric. Point value for this rubric item
#' @param description Character. Description text for the rubric item
#' @param selected Logical. Whether this item is currently selected
#' @return A `markermd_rubric_item` S7 object.
#' @export
markermd_rubric_item = S7::new_class(
  "markermd_rubric_item",
  properties = list(
    hotkey = S7::new_property(
      S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) {
          return("hotkey must be a single integer")
        }
        if (!is.na(value) && (value < 1 || value > 10)) {
          return("hotkey must be between 1 and 10, or NA")
        }
        return(NULL)
      }
    ),
    points = S7::new_property(
      S7::class_numeric,
      validator = function(value) {
        if (length(value) != 1) {
          return("points must be a single numeric value")
        }
        return(NULL)
      }
    ),
    description = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1) {
          return("description must be a single character string")
        }
        return(NULL)
      }
    ),
    selected = S7::new_property(
      S7::class_logical,
      default = quote(FALSE),
      validator = function(value) {
        if (length(value) != 1) {
          return("selected must be a single logical value")
        }
        return(NULL)
      }
    )
  ),
  package = "markermd"
)

#' Grade State S7 Class
#'
#' Represents the grading state for a question including current score, total possible points,
#' and grading configuration options.
#'
#' @param current_score Numeric. Current points awarded
#' @param total_score Numeric. Maximum possible points  
#' @param grading_mode Character. Either "positive" or "negative" grading
#' @param bound_above_zero Logical. Whether to enforce score >= 0
#' @param bound_below_max Logical. Whether to enforce score <= maximum
#' @return A `markermd_grade_state` S7 object.
#' @export
markermd_grade_state = S7::new_class(
  "markermd_grade_state",
  properties = list(
    current_score = S7::new_property(
      S7::class_numeric,
      default = quote(0),
      validator = function(value) {
        if (length(value) != 1) {
          return("current_score must be a single numeric value")
        }
        return(NULL)
      }
    ),
    total_score = S7::new_property(
      S7::class_numeric,
      default = quote(0),
      validator = function(value) {
        if (length(value) != 1) {
          return("total_score must be a single numeric value")
        }
        if (is.na(value) || value < 0) {
          return("total_score must be a non-negative number")
        }
        return(NULL)
      }
    ),
    grading_mode = S7::new_property(
      S7::class_character,
      default = quote("positive"),
      validator = function(value) {
        if (length(value) != 1) {
          return("grading_mode must be a single character string")
        }
        if (!value %in% c("positive", "negative")) {
          return("grading_mode must be 'positive' or 'negative'")
        }
        return(NULL)
      }
    ),
    bound_above_zero = S7::new_property(
      S7::class_logical,
      default = quote(TRUE),
      validator = function(value) {
        if (length(value) != 1) {
          return("bound_above_zero must be a single logical value")
        }
        return(NULL)
      }
    ),
    bound_below_max = S7::new_property(
      S7::class_logical,
      default = quote(TRUE),
      validator = function(value) {
        if (length(value) != 1) {
          return("bound_below_max must be a single logical value")
        }
        return(NULL)
      }
    )
  ),
  package = "markermd"
)