#' Methods for Markermd Template Classes
#'
#' @description Methods for S7 template classes including validation, conversion,
#' printing, and serialization support.
#'
#' @name template_methods
NULL

# Print methods for better console output

#' @export
S7::method(print, markermd_node_selection) = function(x, ...) {
  cat("Node selection with", length(x@indices), "indices\n")
  if (length(x@indices) > 0) {
    if (length(x@indices) <= 10) {
      cat("Indices:", paste(x@indices, collapse = ", "), "\n")
    } else {
      cat("Indices:", paste(x@indices[1:10], collapse = ", "), "... (", length(x@indices) - 10, "more)\n")
    }
  }
  invisible(x)
}

#' @export
S7::method(print, markermd_question) = function(x, ...) {
  cat("Question:", x@name, "(ID:", x@id, ")\n")
  cat("Selected nodes:", length(x@selected_nodes@indices), "\n")
  cat("Rules:", length(x@rules), "\n")
  invisible(x)
}

#' @export
S7::method(print, markermd_metadata) = function(x, ...) {
  cat("Template metadata:\n")
  cat("  Created:", format(x@created_at), "\n")
  cat("  Created by:", x@created_by, "\n")
  cat("  Total nodes:", x@total_nodes, "\n")
  cat("  Version:", x@version, "\n")
  invisible(x)
}

#' @export
S7::method(print, markermd_template) = function(x, ...) {
  cat("Markermd template with", length(x@questions), "questions\n")
  if (length(x@questions) > 0) {
    cat("Questions:\n")
    for (q in x@questions) {
      cat("  -", q@name, "(", length(q@selected_nodes@indices), "nodes,", 
          length(q@rules), "rules)\n")
    }
  }
  cat("\nOriginal AST:", length(x@original_ast@nodes), "nodes\n")
  invisible(x)
}

# Length methods
#' @export
S7::method(length, markermd_node_selection) = function(x) {
  length(x@indices)
}

#' @export  
S7::method(length, markermd_template) = function(x) {
  length(x@questions)
}

# Conversion functions from list format

#' Convert List to node_selection
#' @param x Integer vector or list with indices
#' @export
as_markermd_node_selection = function(x) {
  if (S7::S7_inherits(x, markermd_node_selection)) {
    return(x)
  }
  
  if (is.list(x) && "indices" %in% names(x)) {
    indices = as.integer(x$indices)
  } else {
    indices = as.integer(x)
  }
  
  markermd_node_selection(indices = indices)
}

#' Convert List to question
#' @param x List with id, name, selected_nodes fields
#' @export
as_markermd_question = function(x) {
  if (S7::S7_inherits(x, markermd_question)) {
    return(x)
  }
  
  if (!is.list(x)) {
    stop("Input must be a list")
  }
  
  # Extract and validate required fields
  id = x$id
  if (is.null(id)) {
    stop("Question must have an id field")
  }
  
  name = x$name
  if (is.null(name)) {
    stop("Question must have a name field")
  }
  
  # Handle selected_nodes - could be a vector or node_selection object
  selected_nodes = if (!is.null(x$selected_nodes)) {
    as_markermd_node_selection(x$selected_nodes)
  } else {
    markermd_node_selection()
  }
  
  markermd_question(
    id = as.integer(id),
    name = as.character(name),
    selected_nodes = selected_nodes
  )
}

#' Convert List to template_metadata
#' @param x List with metadata fields
#' @export
as_markermd_metadata = function(x) {
  if (S7::S7_inherits(x, markermd_metadata)) {
    return(x)
  }
  
  if (is.null(x) || length(x) == 0) {
    return(template_metadata())
  }
  
  if (!is.list(x)) {
    stop("Input must be a list or NULL")
  }
  
  # Extract fields with defaults
  created_at = x$created_at %||% Sys.time()
  created_by = x$created_by %||% Sys.getenv("USER", "unknown")
  total_nodes = x$total_nodes %||% 0L
  version = x$version %||% "1.0"
  
  markermd_metadata(
    created_at = as.POSIXct(created_at),
    created_by = as.character(created_by),
    total_nodes = as.integer(total_nodes),
    version = as.character(version)
  )
}


# Conversion functions to list format (for backward compatibility)

#' Convert node_selection to List
#' @param x node_selection object
#' @export
as.list.markermd_node_selection = function(x, ...) {
  list(indices = x@indices)
}

#' Convert question to List  
#' @param x question object
#' @export
as.list.markermd_question = function(x, ...) {
  list(
    id = x@id,
    name = x@name,
    selected_nodes = x@selected_nodes@indices
  )
}

#' Convert template_metadata to List
#' @param x template_metadata object
#' @export
as.list.markermd_metadata = function(x, ...) {
  list(
    created_at = x@created_at,
    created_by = x@created_by,
    total_nodes = x@total_nodes,
    version = x@version
  )
}

#' Convert template to List
#' @param x template object
#' @export
as.list.markermd_template = function(x, ...) {
  list(
    original_ast = x@original_ast,
    questions = lapply(x@questions, as.list),
    metadata = as.list(x@metadata)
  )
}

# Utility functions

#' Add Question to Template
#' @param tmpl template object
#' @param q question object
#' @export
add_question = function(tmpl, q) {
  if (!S7::S7_inherits(tmpl, markermd_template)) {
    stop("tmpl must be a markermd_template object")
  }
  if (!S7::S7_inherits(q, markermd_question)) {
    stop("q must be a markermd_question object")
  }
  
  # Check for duplicate ID
  existing_ids = sapply(tmpl@questions, function(x) x@id)
  if (q@id %in% existing_ids) {
    stop("Question ID ", q@id, " already exists in template")
  }
  
  # Check for duplicate name
  existing_names = sapply(tmpl@questions, function(x) x@name)
  if (q@name %in% existing_names) {
    stop("Question name '", q@name, "' already exists in template")
  }
  
  tmpl@questions = c(tmpl@questions, list(q))
  tmpl
}

#' Remove Question from Template
#' @param tmpl template object
#' @param id Integer question ID to remove
#' @export
remove_question = function(tmpl, id) {
  if (!S7::S7_inherits(tmpl, markermd_template)) {
    stop("tmpl must be a markermd_template object")
  }
  
  question_ids = sapply(tmpl@questions, function(x) x@id)
  idx = which(question_ids == id)
  
  if (length(idx) == 0) {
    warning("Question with ID ", id, " not found")
    return(tmpl)
  }
  
  tmpl@questions = tmpl@questions[-idx]
  tmpl
}

#' Get Question by ID
#' @param tmpl template object
#' @param id Integer question ID
#' @export
get_question = function(tmpl, id) {
  if (!S7::S7_inherits(tmpl, markermd_template)) {
    stop("tmpl must be a markermd_template object")
  }
  
  question_ids = sapply(tmpl@questions, function(x) x@id)
  idx = which(question_ids == id)
  
  if (length(idx) == 0) {
    return(NULL)
  }
  
  tmpl@questions[[idx]]
}