#' Methods for Markermd Template Classes
#'
#' @description Console methods for the S7 template classes: `print()` for
#' templates, questions, node selections and template metadata, and
#' `length()` for templates (the number of questions) and node selections
#' (the number of selected node ids). Validation lives in the class
#' validators and serialization in [read_template_yaml()] /
#' [write_template_yaml()].
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
  if (is_file_question(x)) {
    cat("File:", x@file, "\n")
    return(invisible(x))
  }
  if (is_document_question(x)) {
    cat("Scope: whole document\n")
  } else {
    cat("Selected nodes:", length(x@selected_nodes@node_ids), "\n")
  }
  cat("Rules:", length(x@rules), "\n")
  filter_text = filters_expr_text(x@filters)
  if (!is.null(filter_text)) {
    cat("Filters:", filter_text, "\n")
  }
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
      section_text = if (is_document_question(q)) {
        "whole document"
      } else {
        paste(section_count, if (section_count == 1) "section" else "sections")
      }
      rule_text = if (rule_count == 1) "rule" else "rules"
      if (is_file_question(q)) {
        cat("  - ", q@name, " (file: ", q@file, ")\n", sep = "")
        next
      }
      cat("  - ", q@name, " (", section_text, ", ", rule_count, " ", rule_text, ")\n", sep = "")
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

#' Add a question to a template
#' @param template A `markermd_template` object.
#' @param question A `markermd_question` object.
#' @return The `markermd_template` object with `question` appended to its
#'   questions.
#' @export
template_add_question = function(template, question) {
  if (!S7::S7_inherits(template, markermd_template)) {
    cli::cli_abort("{.arg template} must be a markermd_template object.")
  }
  if (!S7::S7_inherits(question, markermd_question)) {
    cli::cli_abort("{.arg question} must be a markermd_question object.")
  }

  existing_ids = vapply(template@questions, function(x) x@id, integer(1))
  if (question@id %in% existing_ids) {
    cli::cli_abort("Question ID {question@id} already exists in the template.")
  }

  existing_names = vapply(template@questions, function(x) x@name, character(1))
  if (question@name %in% existing_names) {
    cli::cli_abort("Question name {.val {question@name}} already exists in the template.")
  }

  template@questions = c(template@questions, list(question))
  template
}

#' Remove a question from a template
#' @param template A `markermd_template` object.
#' @param id Integer question ID to remove.
#' @return The `markermd_template` object without the matching question
#'   (returned unchanged, with a warning, when `id` is not found).
#' @export
template_remove_question = function(template, id) {
  if (!S7::S7_inherits(template, markermd_template)) {
    cli::cli_abort("{.arg template} must be a markermd_template object.")
  }

  question_ids = vapply(template@questions, function(x) x@id, integer(1))
  idx = which(question_ids == id)

  if (length(idx) == 0) {
    cli::cli_warn("Question with ID {id} not found.")
    return(template)
  }

  template@questions = template@questions[-idx]
  template
}

#' Get a template's question by ID
#' @param template A `markermd_template` object.
#' @param id Integer question ID.
#' @return The matching `markermd_question` object, or `NULL` when `id` is not
#'   found.
#' @export
template_question = function(template, id) {
  if (!S7::S7_inherits(template, markermd_template)) {
    cli::cli_abort("{.arg template} must be a markermd_template object.")
  }

  question_ids = vapply(template@questions, function(x) x@id, integer(1))
  idx = which(question_ids == id)

  if (length(idx) == 0) {
    return(NULL)
  }

  template@questions[[idx]]
}
