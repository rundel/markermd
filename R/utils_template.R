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
  
    # Filter to only headings for template creation
    question_ast = question_ast |> parsermd::rmd_select(parsermd::has_type("rmd_heading"))
    
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

#' Resolve Section Names from Selected Nodes
#'
#' Extract heading names/titles from originally selected nodes in template creation.
#' This is used to map template selections to sections in new documents.
#'
#' @param original_ast rmd_ast object from template creation
#' @param selected_node_indices Integer vector of originally selected node indices
#'
#' @return Character vector of section names for use with by_section()
#'
resolve_section_names = function(original_ast, selected_node_indices) {
  
  if (is.null(original_ast) || length(selected_node_indices) == 0) {
    return(character(0))
  }
  
  # Get AST nodes
  ast_nodes = if (inherits(original_ast, "rmd_ast") && !is.null(original_ast@nodes)) {
    original_ast@nodes
  } else {
    original_ast
  }
  
  if (length(ast_nodes) == 0) {
    return(character(0))
  }
  
  section_names = character(0)
  
  for (node_idx in selected_node_indices) {
    # Validate node index
    if (node_idx < 1 || node_idx > length(ast_nodes)) {
      warning("Node index ", node_idx, " is out of bounds")
      next
    }
    
    node = ast_nodes[[node_idx]]
    
    # If the selected node is a heading, use its name directly
    if (inherits(node, "rmd_heading")) {
      section_names = c(section_names, node@name)
    } else {
      # If not a heading, find the parent section heading
      # Look backwards for the most recent heading
      parent_heading = NULL
      for (i in (node_idx - 1):1) {
        if (i < 1) break
        parent_node = ast_nodes[[i]]
        if (inherits(parent_node, "rmd_heading")) {
          parent_heading = parent_node@name
          break
        }
      }
      
      if (!is.null(parent_heading)) {
        section_names = c(section_names, parent_heading)
      } else {
        warning("No parent heading found for node ", node_idx)
      }
    }
  }
  
  # Remove duplicates while preserving order
  unique(section_names)
}

#' Evaluate Rule Against AST Subset
#'
#' Apply individual rule to an AST subset and determine if the rule condition is met.
#'
#' @param ast_subset rmd_ast or list of nodes to evaluate
#' @param rule markermd_rule S7 object containing the rule definition
#' @param num_selected_nodes Integer number of originally selected heading nodes
#'
#' @return List with passed (logical) and message (character) components
#'
evaluate_rule = function(ast_subset, rule, num_selected_nodes = 0) {
  
  if (!S7::S7_inherits(rule, markermd_rule)) {
    return(list(passed = FALSE, message = "Invalid rule object"))
  }
  
  # Convert ast_subset to nodes if needed
  nodes = if (inherits(ast_subset, "rmd_ast") && !is.null(ast_subset@nodes)) {
    ast_subset@nodes
  } else if (is.list(ast_subset)) {
    ast_subset
  } else {
    return(list(passed = FALSE, message = "Invalid AST subset"))
  }
  
  # Apply node type filtering if not "Any node"
  if (rule@node_type != "Any node") {
    nodes = nodes[sapply(nodes, function(n) inherits(n, rule@node_type))]
  }
  
  # Evaluate based on verb
  switch(rule@verb,
    "has count of" = {
      # Count nodes - only subtract heading nodes if we're counting headings
      # For other node types, count them directly without subtracting
      actual_count = if (rule@node_type == "rmd_heading") {
        length(nodes) - num_selected_nodes
      } else {
        length(nodes)
      }
      min_count = rule@values[1]
      max_count = rule@values[2]
      
      passed = actual_count >= min_count && actual_count <= max_count
      message = if (passed) {
        paste0("Count check passed: ", actual_count, " nodes (range: ", min_count, "-", max_count, ")")
      } else {
        paste0("Count check failed: ", actual_count, " nodes (expected: ", min_count, "-", max_count, ")")
      }
      
      list(passed = passed, message = message)
    },
    
    "has content" = {
      pattern = rule@values[1]
      if (is.na(pattern) || nchar(pattern) == 0) {
        return(list(passed = TRUE, message = "Empty pattern always matches"))
      }
      
      # Extract content from all nodes
      content_found = FALSE
      for (node in nodes) {
        node_content = tryCatch({
          parsermd::as_document(node) |>
            as.character() |>
            paste(collapse = " ")
        }, error = function(e) "")
        
        # Use glob pattern matching
        if (length(grep(utils::glob2rx(pattern), node_content, ignore.case = TRUE)) > 0) {
          content_found = TRUE
          break
        }
      }
      
      message = if (content_found) {
        paste0("Content check passed: pattern '", pattern, "' found")
      } else {
        paste0("Content check failed: pattern '", pattern, "' not found")
      }
      
      list(passed = content_found, message = message)
    },
    
    "does not have content" = {
      pattern = rule@values[1]
      if (is.na(pattern) || nchar(pattern) == 0) {
        return(list(passed = TRUE, message = "Empty pattern never matches"))
      }
      
      # Extract content from all nodes
      content_found = FALSE
      for (node in nodes) {
        node_content = tryCatch({
          parsermd::as_document(node) |>
            as.character() |>
            paste(collapse = " ")
        }, error = function(e) "")
        
        # Use glob pattern matching
        if (length(grep(utils::glob2rx(pattern), node_content, ignore.case = TRUE)) > 0) {
          content_found = TRUE
          break
        }
      }
      
      # Inverse of "has content"
      passed = !content_found
      message = if (passed) {
        paste0("Negative content check passed: pattern '", pattern, "' not found")
      } else {
        paste0("Negative content check failed: pattern '", pattern, "' found")
      }
      
      list(passed = passed, message = message)
    },
    
    "has name" = {
      pattern = rule@values[1]
      if (is.na(pattern) || nchar(pattern) == 0) {
        return(list(passed = TRUE, message = "Empty pattern always matches"))
      }
      
      # Check node names/labels
      name_found = FALSE
      for (node in nodes) {
        node_name = ""
        
        # Extract name based on node type
        if (inherits(node, "rmd_heading") && !is.null(node@name)) {
          node_name = node@name
        } else if (inherits(node, "rmd_chunk") && !is.null(node@name)) {
          node_name = node@name
        }
        
        # Use glob pattern matching
        if (nchar(node_name) > 0 && length(grep(utils::glob2rx(pattern), node_name, ignore.case = TRUE)) > 0) {
          name_found = TRUE
          break
        }
      }
      
      message = if (name_found) {
        paste0("Name check passed: pattern '", pattern, "' found")
      } else {
        paste0("Name check failed: pattern '", pattern, "' not found")
      }
      
      list(passed = name_found, message = message)
    },
    
    # Default case for unknown verbs
    {
      list(passed = FALSE, message = paste0("Unknown rule verb: ", rule@verb))
    }
  )
}

#' Validate Question Rules Against New AST
#'
#' Validate all rules for a single question against a new AST using section-based matching.
#'
#' @param new_ast rmd_ast object from the document to validate
#' @param original_ast rmd_ast object from template creation  
#' @param question markermd_question S7 object containing rules and node selections
#'
#' @return List with question_name, status, messages, and details components
#'
validate_question_rules = function(new_ast, original_ast, question) {
  
  if (!S7::S7_inherits(question, markermd_question)) {
    return(list(
      question_name = "Unknown",
      status = "error",
      messages = "Invalid question object",
      details = "Question must be a markermd_question S7 object"
    ))
  }
  
  result = list(
    question_name = question@name,
    status = "pass",
    messages = character(0),
    details = ""
  )
  
  tryCatch({
    # Resolve section names from original selected nodes
    section_names = resolve_section_names(original_ast, question@selected_nodes@indices)
    
    if (length(section_names) == 0) {
      return(list(
        question_name = question@name,
        status = "error", 
        messages = "No section names could be resolved",
        details = "Could not determine sections from selected nodes"
      ))
    }
    
    # Get question content from new AST using by_section
    if (!requireNamespace("parsermd", quietly = TRUE)) {
      stop("parsermd package is required for section selection")
    }
    
    question_ast = tryCatch({
      parsermd::rmd_select(new_ast, parsermd::by_section(section_names), keep_yaml = FALSE)
    }, error = function(e) {
      # If by_section fails, return empty AST
      parsermd::rmd_ast(nodes = list())
    })
    
    # Count originally selected heading nodes for count adjustments
    num_selected_nodes = length(question@selected_nodes@indices)
    
    # If no rules, consider it a pass
    if (length(question@rules) == 0) {
      result$messages = "No rules defined - validation passed"
      result$details = paste0("Section(s) found: ", paste(section_names, collapse = ", "))
      return(result)
    }
    
    # Evaluate each rule
    rule_results = list()
    all_passed = TRUE
    
    for (i in seq_along(question@rules)) {
      rule = question@rules[[i]]
      rule_result = evaluate_rule(question_ast, rule, num_selected_nodes)
      
      rule_results[[i]] = rule_result
      if (!rule_result$passed) {
        all_passed = FALSE
      }
    }
    
    # Combine results
    result$status = if (all_passed) "pass" else "fail"
    result$messages = sapply(rule_results, function(r) r$message)
    result$details = paste(c(
      paste0("Section(s): ", paste(section_names, collapse = ", ")),
      sapply(rule_results, function(r) r$message)
    ), collapse = "\n")
    
  }, error = function(e) {
    result$status = "error"
    result$messages = paste("Validation error:", e$message)
    result$details = paste("Error during validation:", e$message)
  })
  
  return(result)
}

#' Validate Repository Against Rules
#'
#' Validates a parsed repository AST against template rules using section-based matching.
#' This replaces the parsermd template-based validation system.
#'
#' @param ast rmd_ast object from parsing a repository document
#' @param template_obj markermd_template S7 object with questions and rules
#'
#' @return List with validation results for each question:
#'   - question_name: Question name
#'   - status: "pass", "fail", or "error"
#'   - messages: Character vector of validation messages
#'   - details: Full captured output for tooltip display
#'
validate_repo_against_rules = function(ast, template_obj) {
  
  if (is.null(ast) || is.null(template_obj)) {
    return(list())
  }
  
  if (!S7::S7_inherits(template_obj, markermd_template)) {
    stop("template_obj must be a markermd_template S7 object")
  }
  
  if (length(template_obj@questions) == 0) {
    return(list())
  }
  
  results = list()
  
  for (question in template_obj@questions) {
    result = validate_question_rules(ast, template_obj@original_ast, question)
    results[[question@name]] = result
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