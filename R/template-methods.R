#' Methods for Markermd Template Classes
#'
#' @description Methods for S7 template classes including validation, conversion,
#' printing, and serialization support.
#'
#' @name template_methods
NULL

# Print methods for better console output

S7::method(print, markermd_node_selection) = function(x, ...) {
  cat("Node selection with", length(x@node_ids), "node(s)\n")
  if (length(x@node_ids) > 0) {
    if (length(x@node_ids) <= 10) {
      cat("Nodes:", paste0("#", x@node_ids, collapse = ", "), "\n")
    } else {
      cat("Nodes:", paste0("#", x@node_ids[1:10], collapse = ", "), "... (", length(x@node_ids) - 10, "more)\n")
    }
  }
  invisible(x)
}

S7::method(print, markermd_question) = function(x, ...) {
  cat("Question:", x@name, "(ID:", x@id, ")\n")
  cat("Selected nodes:", length(x@selected_nodes@node_ids), "\n")
  cat("Rules:", length(x@rules), "\n")
  invisible(x)
}

S7::method(print, markermd_metadata) = function(x, ...) {
  cat("Template metadata:\n")
  cat("  Created:", format(x@created_at), "\n")
  cat("  Created by:", x@created_by, "\n")
  cat("  Total nodes:", x@total_nodes, "\n")
  cat("  Version:", x@version, "\n")
  invisible(x)
}

S7::method(print, markermd_template) = function(x, ...) {
  cat("Markermd template with", length(x@questions), "questions\n")
  if (length(x@questions) > 0) {
    cat("Questions:\n")
    for (q in x@questions) {
      section_count = length(q@selected_nodes@node_ids)
      rule_count = length(q@rules)
      section_text = if (section_count == 1) "section" else "sections"
      rule_text = if (rule_count == 1) "rule" else "rules"
      cat("  - ", q@name, " (", section_count, " ", section_text, ", ", rule_count, " ", rule_text, ")\n", sep = "")
    }
  }
  ast_node_count = length(q2r_flatten(x@original_ast))
  ast_node_text = if (ast_node_count == 1) "node" else "nodes"
  cat("\nOriginal AST:", ast_node_count, ast_node_text, "\n")
  invisible(x)
}

# Length methods

S7::method(length, markermd_node_selection) = function(x) {
  length(x@node_ids)
}

S7::method(length, markermd_template) = function(x) {
  length(x@questions)
}

# Utility functions

#' Add Question to Template
#' @param tmpl template object
#' @param q question object
#' @return The `markermd_template` object with `q` appended to its questions.
#' @export
add_question = function(tmpl, q) {
  if (!S7::S7_inherits(tmpl, markermd_template)) {
    cli::cli_abort("tmpl must be a markermd_template object")
  }
  if (!S7::S7_inherits(q, markermd_question)) {
    cli::cli_abort("q must be a markermd_question object")
  }
  
  # Check for duplicate ID
  existing_ids = sapply(tmpl@questions, function(x) x@id)
  if (q@id %in% existing_ids) {
    cli::cli_abort("Question ID {q@id} already exists in template")
  }
  
  # Check for duplicate name
  existing_names = sapply(tmpl@questions, function(x) x@name)
  if (q@name %in% existing_names) {
    cli::cli_abort("Question name '{q@name}' already exists in template")
  }
  
  tmpl@questions = c(tmpl@questions, list(q))
  tmpl
}

#' Remove Question from Template
#' @param tmpl template object
#' @param id Integer question ID to remove
#' @return The `markermd_template` object without the matching question
#'   (returned unchanged, with a warning, when `id` is not found).
#' @export
remove_question = function(tmpl, id) {
  if (!S7::S7_inherits(tmpl, markermd_template)) {
    cli::cli_abort("tmpl must be a markermd_template object")
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
#' @return The matching `markermd_question` object, or `NULL` when `id` is not found.
#' @export
get_question = function(tmpl, id) {
  if (!S7::S7_inherits(tmpl, markermd_template)) {
    cli::cli_abort("tmpl must be a markermd_template object")
  }
  
  question_ids = sapply(tmpl@questions, function(x) x@id)
  idx = which(question_ids == id)
  
  if (length(idx) == 0) {
    return(NULL)
  }
  
  tmpl@questions[[idx]]
}