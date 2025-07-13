#' Template Management Utilities
#'
#' Functions for saving, loading and managing assignment templates

#' Save Template to File
#'
#' Save a template with questions and node mappings to a JSON file
#'
#' @param questions List of question objects
#' @param node_selections List mapping question IDs to selected nodes
#' @param file_path Character. Path where to save the template
#' @param metadata List. Additional metadata about the template
#'
#' @return Logical indicating success
#'
save_template = function(questions, node_selections, file_path, metadata = list()) {
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required for template saving")
  }
  
  # Prepare template data
  template_data = list(
    version = "1.0",
    created = Sys.time(),
    metadata = metadata,
    questions = questions,
    node_selections = node_selections
  )
  
  # Save to file
  tryCatch({
    jsonlite::write_json(template_data, file_path, pretty = TRUE)
    return(TRUE)
  }, error = function(e) {
    warning("Failed to save template: ", e$message)
    return(FALSE)
  })
}

#' Load Template from File
#'
#' Load a previously saved template from a JSON file
#'
#' @param file_path Character. Path to the template file
#'
#' @return List with template data or NULL if failed
#'
load_template = function(file_path) {
  
  if (!file.exists(file_path)) {
    warning("Template file does not exist: ", file_path)
    return(NULL)
  }
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required for template loading")
  }
  
  tryCatch({
    template_data = jsonlite::read_json(file_path, simplifyVector = TRUE)
    
    # Basic validation
    if (is.null(template_data$questions) || is.null(template_data$node_selections)) {
      warning("Invalid template format")
      return(NULL)
    }
    
    return(template_data)
  }, error = function(e) {
    warning("Failed to load template: ", e$message)
    return(NULL)
  })
}

#' Validate Template
#'
#' Check if a template is valid and complete
#'
#' @param questions List of question objects
#' @param node_selections List mapping question IDs to selected nodes
#' @param total_nodes Integer. Total number of nodes in document
#'
#' @return List with validation results
#'
validate_template = function(questions, node_selections, total_nodes) {
  
  issues = character(0)
  warnings = character(0)
  
  # Check if we have questions
  if (length(questions) == 0) {
    issues = c(issues, "No questions defined")
    return(list(valid = FALSE, issues = issues, warnings = warnings))
  }
  
  # Check each question
  for (q in questions) {
    q_id_char = as.character(q$id)
    
    # Check question name
    if (is.null(q$name) || trimws(q$name) == "" || grepl("^Question [0-9]+$", q$name)) {
      warnings = c(warnings, paste("Question", q$id, "has default/empty name"))
    }
    
    # Check node selections
    selected_nodes = node_selections[[q_id_char]] %||% integer(0)
    if (length(selected_nodes) == 0) {
      issues = c(issues, paste("Question", q$id, "has no selected nodes"))
    }
    
    # Check for invalid node indices (selected_nodes use 1-based indexing with AST root offset)
    # Valid range is 2 to length(nodes)+1 since AST root is at index 1
    max_valid_index = total_nodes + 1  # total_nodes = length(nodes), max valid = length(nodes)+1
    invalid_nodes = selected_nodes[selected_nodes > max_valid_index | selected_nodes < 2]
    if (length(invalid_nodes) > 0) {
      issues = c(issues, paste("Question", q$id, "has invalid node indices:", paste(invalid_nodes - 1, collapse = ", ")))
    }
  }
  
  # Check for overlapping node selections
  all_selected = unlist(node_selections)
  duplicated_nodes = all_selected[duplicated(all_selected)]
  if (length(duplicated_nodes) > 0) {
    warnings = c(warnings, paste("Nodes selected in multiple questions:", paste(unique(duplicated_nodes - 1), collapse = ", ")))
  }
  
  # Check coverage (excluding AST root node from total count)
  covered_nodes = length(unique(all_selected))
  selectable_nodes = total_nodes  # total_nodes already excludes the AST root
  coverage_pct = round((covered_nodes / selectable_nodes) * 100, 1)
  if (coverage_pct < 50) {
    warnings = c(warnings, paste0("Low document coverage: only ", coverage_pct, "% of nodes selected"))
  }
  
  return(list(
    valid = length(issues) == 0,
    issues = issues,
    warnings = warnings,
    coverage_pct = coverage_pct,
    covered_nodes = covered_nodes,
    total_nodes = total_nodes
  ))
}

#' Generate Template Summary
#'
#' Create a human-readable summary of the template
#'
#' @param questions List of question objects  
#' @param node_selections List mapping question IDs to selected nodes
#' @param ast_nodes List of AST nodes for context
#'
#' @return Character vector with template summary
#'
generate_template_summary = function(questions, node_selections, ast_nodes = NULL) {
  
  if (length(questions) == 0) {
    return("No questions defined in template")
  }
  
  summary_lines = c(
    paste("Template Summary"),
    paste("==============="),
    paste("Questions:", length(questions)),
    ""
  )
  
  for (q in questions) {
    q_id_char = as.character(q$id)
    selected_nodes = node_selections[[q_id_char]] %||% integer(0)
    
    summary_lines = c(
      summary_lines,
      paste0("Question ", q$id, ": ", q$name),
      paste0("  Selected nodes: ", if (length(selected_nodes) == 0) "None" else paste(selected_nodes - 1, collapse = ", "))
    )
    
    # Add node details if AST is available
    if (!is.null(ast_nodes) && length(selected_nodes) > 0) {
      for (node_idx in selected_nodes) {
        # node_idx includes AST root offset, subtract 1 to access ast_nodes array
        ast_array_idx = node_idx - 1
        if (ast_array_idx > 0 && ast_array_idx <= length(ast_nodes)) {
          node = ast_nodes[[ast_array_idx]]
          node_type = class(node)[1]
          
          if (grepl("heading", node_type, ignore.case = TRUE)) {
            desc = paste0("Heading: ", node@name)
          } else if (grepl("chunk", node_type, ignore.case = TRUE)) {
            desc = paste0("Code chunk: ", node@name)
          } else if (grepl("markdown", node_type, ignore.case = TRUE)) {
            first_line = if (!is.null(node@lines) && length(node@lines) > 0) node@lines[1] else ""
            desc = paste0("Markdown: ", substr(first_line, 1, 30), "...")
          } else {
            desc = node_type
          }
          
          summary_lines = c(summary_lines, paste0("    ", node_idx - 1, ". ", desc))
        }
      }
    }
    
    summary_lines = c(summary_lines, "")
  }
  
  return(summary_lines)
}