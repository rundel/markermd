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

# Legacy save_template() function removed - templates now use RDS format exclusively

# Legacy load_template() and generate_template_summary() functions removed - templates now use RDS format exclusively

#' Create Question Templates
#'
#' Creates parsermd templates from a template S7 object
#'
#' @param template_obj markermd_template S7 object
#' @return Named list of parsermd templates
#'
create_question_templates = function(template_obj) {
  if (!S7::S7_inherits(template_obj, markermd_template)) {
    stop("template_obj must be a markermd_template S7 object")
  }
  
  if (!requireNamespace("parsermd", quietly = TRUE)) {
    stop("parsermd package is required for creating templates")
  }
  
  if (length(template_obj@questions) == 0) {
    return(list())
  }
  
  result_templates = list()
  
  for (q in template_obj@questions) {
    if (length(q@selected_nodes@indices) == 0) {
      warning("Skipping question '", q@name, "' with no selected nodes")
      next
    }
      
    question_ast = template_obj@original_ast[q@selected_nodes@indices]
  
    if (!q@strict) {
      question_ast = question_ast |> parsermd::rmd_select(parsermd::has_type("rmd_heading"))
    }
    
    tryCatch({
      template = parsermd::rmd_template(question_ast, keep_headings=TRUE)
      result_templates[[q@name]] = template
    }, error = function(e) {
      warning("Failed to create template for question '", q@name, "': ", e$message)
    })
  }
  
  return(result_templates)
}

#' Process Templates for mark() Function
#'
#' Handles different template input types and converts them to a standardized format
#' for use in the marking interface using S7 template objects.
#'
#' @param template_input Can be:
#'   - Character path to .rds file containing template data
#'   - markermd_template S7 object
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
process_mark_templates = function(template_input) {
  
  if (is.null(template_input)) {
    return(NULL)
  }
  
  if (S7::S7_inherits(template_input, markermd_template)) {
    return(create_question_templates(template_input))
  }
  
  if (is.character(template_input) && length(template_input) == 1) {
    if (!file.exists(template_input)) {
      stop("Template file does not exist: ", template_input)
    }
    if (!grepl("\\.rds$", template_input, ignore.case = TRUE)) {
      stop("Template file must have .rds extension: ", template_input)
    }
    
    template_obj = readRDS(template_input)
    if (S7::S7_inherits(template_obj, markermd_template)) {
      return(create_question_templates(template_obj))
    } else {
      stop("Template file must contain a markermd_template S7 object")
    }
  }
  
  stop("Template must be a file path or markermd_template S7 object")
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
#' @param template_obj markermd_template S7 object containing questions with node selections
#'
#' @return Named list where names are question names and values are the extracted content as text
#'
extract_question_content = function(ast, template_obj) {
  
  if (is.null(ast) || is.null(template_obj) || length(template_obj@questions) == 0) {
    return(list())
  }
  
  if (!S7::S7_inherits(template_obj, markermd_template)) {
    stop("template_obj must be a markermd_template S7 object")
  }
  
  if (!requireNamespace("parsermd", quietly = TRUE)) {
    stop("parsermd package is required for content extraction")
  }
  
  ast_nodes = if (inherits(ast, "rmd_ast") && !is.null(ast@nodes)) {
    ast@nodes
  } else {
    ast
  }
  
  if (length(ast_nodes) == 0) {
    return(list())
  }
  
  results = list()
  
  for (q in template_obj@questions) {
    question_name = q@name
    selected_nodes = q@selected_nodes@indices
    
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