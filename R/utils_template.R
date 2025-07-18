#' Template Management Utilities
#'
#' Functions for saving, loading and managing assignment templates

#' Null-coalescing operator
#'
#' Returns the right-hand side if the left-hand side is NULL
#'
#' @param x Left-hand side value
#' @param y Right-hand side value (returned if x is NULL)
#' @return x if not NULL, otherwise y
#'
'%||%' = function(x, y) if (is.null(x)) y else x

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

#' Create parsermd Templates from Saved Template Data
#'
#' Takes a saved template data structure and creates parsermd templates for each question
#' using the selected node subsets.
#'
#' @param template_data List containing original_ast, questions, and metadata components
#'   as returned by readRDS() from a saved template file
#'
#' @return Named list of parsermd templates, where names are question names and values
#'   are rmd_template objects created via parsermd::rmd_template()
#'
#' @examples
#' \dontrun{
#' template_data = readRDS("path/to/template.rds")
#' templates = create_question_templates(template_data)
#' }
#'
create_question_templates = function(template_data) {
  
  # Validate input structure
  if (!is.list(template_data)) {
    stop("template_data must be a list")
  }
  
  required_components = c("original_ast", "questions")
  missing = setdiff(required_components, names(template_data))
  if (length(missing) > 0) {
    stop("template_data missing required components: ", paste(missing, collapse = ", "))
  }
  
  if (!requireNamespace("parsermd", quietly = TRUE)) {
    stop("parsermd package is required for creating templates")
  }
  
  original_ast = template_data$original_ast
  questions = template_data$questions
  
  # Validate AST
  if (!inherits(original_ast, "rmd_ast")) {
    stop("original_ast must be an rmd_ast object")
  }
  
  if (length(questions) == 0) {
    return(list())
  }
  
  # Create templates for each question
  result_templates = list()
  
  for (q in questions) {
    # Validate question structure
    if (!is.list(q) || is.null(q$name) || is.null(q$selected_nodes)) {
      warning("Skipping invalid question: ", deparse(substitute(q)))
      next
    }
    
    selected_nodes = q$selected_nodes
    
    if (length(selected_nodes) == 0) {
      warning("Skipping question '", q$name, "' with no selected nodes")
      next
    }
    
    # Extract selected nodes from the original AST
    # selected_nodes are 1-based indices into the AST nodes list
    ast_nodes = original_ast@nodes
    
    # Validate node indices
    valid_indices = selected_nodes[selected_nodes >= 1 & selected_nodes <= length(ast_nodes)]
    if (length(valid_indices) != length(selected_nodes)) {
      invalid_indices = setdiff(selected_nodes, valid_indices)
      warning("Question '", q$name, "' has invalid node indices: ", paste(invalid_indices, collapse = ", "))
    }
    
    if (length(valid_indices) == 0) {
      warning("Skipping question '", q$name, "' with no valid node indices")
      next
    }
    
    # Create subset of nodes for this question
    question_nodes = ast_nodes[valid_indices]
    
    # Create new AST with just the selected nodes
    question_ast = original_ast
    question_ast@nodes = question_nodes
    
    # Create parsermd template from the subset AST
    tryCatch({
      template = parsermd::rmd_template(question_ast)
      result_templates[[q$name]] = template
    }, error = function(e) {
      warning("Failed to create template for question '", q$name, "': ", e$message)
    })
  }
  
  return(result_templates)
}

#' Process Templates for mark() Function
#'
#' Handles different template input types and converts them to a standardized format
#' for use in the marking interface.
#'
#' @param template Can be:
#'   - Character path to .rds file containing template data
#'   - List with raw template data (from readRDS)
#'   - List with transformed templates (from create_question_templates)
#'   - NULL (no template)
#'
#' @return Named list of parsermd templates for each question, or NULL if no template
#'
#' @examples
#' \dontrun{
#' # From file path
#' templates = process_mark_templates("path/to/template.rds")
#' 
#' # From raw template data
#' raw_data = readRDS("template.rds")
#' templates = process_mark_templates(raw_data)
#' }
#'
process_mark_templates = function(template) {
  
  # Handle NULL case
  if (is.null(template)) {
    return(NULL)
  }
  
  # Handle file path case
  if (is.character(template) && length(template) == 1) {
    if (!file.exists(template)) {
      stop("Template file does not exist: ", template)
    }
    if (!grepl("\\.rds$", template, ignore.case = TRUE)) {
      stop("Template file must have .rds extension: ", template)
    }
    
    template_data = readRDS(template)
    return(process_mark_templates(template_data))
  }
  
  # Handle list input
  if (!is.list(template)) {
    stop("Template must be a file path, list, or NULL")
  }
  
  # Check if this is already a transformed template list
  # Transformed templates have all elements as rmd_template objects
  if (length(template) > 0 && all(sapply(template, function(x) inherits(x, "rmd_template")))) {
    # Already transformed
    return(template)
  }
  
  # Check if this is raw template data (has original_ast and questions)
  if (all(c("original_ast", "questions") %in% names(template))) {
    # Raw template data - transform it
    return(create_question_templates(template))
  }
  
  # Unknown format
  stop("Template format not recognized. Expected file path, raw template data, or transformed template list.")
}

#' Validate Repository Against Templates
#'
#' Validates a parsed repository AST against question templates using parsermd::rmd_check_template
#'
#' @param ast rmd_ast object from parsing a repository document
#' @param templates Named list of rmd_template objects (from process_mark_templates)
#'
#' @return List with validation results for each question:
#'   - question_name: Question name
#'   - status: "pass", "fail", or "error"
#'   - messages: Character vector of validation messages
#'   - details: Full captured output for tooltip display
#'
validate_repo_against_templates = function(ast, templates) {
  
  if (is.null(ast) || is.null(templates) || length(templates) == 0) {
    return(list())
  }
  
  if (!requireNamespace("parsermd", quietly = TRUE)) {
    stop("parsermd package is required for template validation")
  }
  
  results = list()
  
  for (question_name in names(templates)) {
    template = templates[[question_name]]
    
    result = list(
      question_name = question_name,
      status = "error",
      messages = character(0),
      details = ""
    )
    
    tryCatch({
      # Capture all output from rmd_check_template
      captured_messages = capture.output({
        validation_result = parsermd::rmd_check_template(ast, template)
      }, type = "message")
      
      # Check validation result
      if (isTRUE(validation_result)) {
        result$status = "pass"
        result$messages = "Template validation passed"
        result$details = "All required elements are present and modified appropriately."
      } else {
        result$status = "fail"
        result$messages = captured_messages
        result$details = paste(captured_messages, collapse = "\n")
      }
      
    }, error = function(e) {
      result$status = "error"
      result$messages = paste("Validation error:", e$message)
      result$details = paste("Error during validation:", e$message)
    })
    
    results[[question_name]] = result
  }
  
  return(results)
}

#' Extract Question Content from AST
#'
#' Extracts the content for specific questions from a parsed AST based on template node selections
#'
#' @param ast rmd_ast object from parsing a repository document
#' @param template_data Raw template data containing original_ast and questions with node selections
#'
#' @return Named list where names are question names and values are the extracted content as text
#'
extract_question_content = function(ast, template_data) {
  
  if (is.null(ast) || is.null(template_data) || is.null(template_data$questions)) {
    return(list())
  }
  
  if (!requireNamespace("parsermd", quietly = TRUE)) {
    stop("parsermd package is required for content extraction")
  }
  
  # Get AST nodes
  ast_nodes = if (inherits(ast, "rmd_ast") && !is.null(ast@nodes)) {
    ast@nodes
  } else {
    ast
  }
  
  if (length(ast_nodes) == 0) {
    return(list())
  }
  
  results = list()
  
  for (q in template_data$questions) {
    question_name = q$name %||% paste("Question", q$id)
    selected_nodes = q$selected_nodes %||% integer(0)
    
    if (length(selected_nodes) == 0) {
      results[[question_name]] = "No content selected for this question."
      next
    }
    
    # Extract content from selected nodes
    question_content = tryCatch({
      # Validate node indices
      valid_indices = selected_nodes[selected_nodes >= 1 & selected_nodes <= length(ast_nodes)]
      
      if (length(valid_indices) == 0) {
        "Selected nodes are not available in this document."
      } else {
        # Extract content from each selected node
        node_contents = sapply(valid_indices, function(node_idx) {
          node = ast_nodes[[node_idx]]
          
          # Use as_document() to get the node content as text
          content = tryCatch({
            parsermd::as_document(node) |>
              as.character() |>
              paste(collapse = "\n")
          }, error = function(e) {
            paste("Error extracting content:", e$message)
          })
          
          return(content)
        })
        
        # Combine all node contents for this question
        paste(node_contents, collapse = "\n\n")
      }
    }, error = function(e) {
      paste("Error extracting question content:", e$message)
    })
    
    results[[question_name]] = question_content
  }
  
  return(results)
}