#' Template Management Utilities
#'
#' Functions for saving, loading and managing assignment templates

# Null-coalescing operator
# Returns the right-hand side if the left-hand side is NULL
#
# x: Left-hand side value
# y: Right-hand side value (returned if x is NULL)

'%||%' = function(x, y) if (is.null(x)) y else x

# Legacy save_template() function removed - templates now use RDS format exclusively

# Legacy load_template() and generate_template_summary() functions removed - templates now use RDS format exclusively

# Creates parsermd templates from a template S7 object
#
# template_obj: markermd_template S7 object

create_question_templates = function(template_obj) {
  stopifnot(S7::S7_inherits(template_obj, markermd_template))
  
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

# Handles different template input types and converts them to a standardized format
# for use in the marking interface using S7 template objects.
#
# template_input: Can be character path to .rds file, markermd_template S7 object, or NULL

process_mark_templates = function(template_input) {

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

# Validates a parsed repository AST against question templates using parsermd::rmd_check_template
#
# ast: rmd_ast object from parsing a repository document
# templates: Named list of rmd_template objects (from process_mark_templates)

validate_repo_against_templates = function(ast, templates) {
  
  results = list()
  
  for (question_name in names(templates)) {
    template = templates[[question_name]]
    
    # Capture all output from rmd_check_template - errors will propagate up
    captured_messages = capture.output({
      validation_result = parsermd::rmd_check_template(ast, template)
    }, type = "message")
    
    # Check validation result - only pass or fail
    if (isTRUE(validation_result)) {
      result = list(
        question_name = question_name,
        status = "pass",
        messages = "Template validation passed",
        details = "All required elements are present and modified appropriately."
      )
    } else {
      result = list(
        question_name = question_name,
        status = "fail",
        messages = captured_messages,
        details = paste(captured_messages, collapse = "\n")
      )
    }
    
    results[[question_name]] = result
  }
  
  return(results)
}

# Extract detailed heading hierarchies from originally selected nodes in template creation.
# Uses parsermd::rmd_node_sections() to get complete heading paths for more precise subsetting.
#
# original_ast: rmd_ast object from template creation
# selected_node_indices: Integer vector of originally selected node indices

resolve_section_hierarchies = function(original_ast, selected_node_indices) {
  
  all_sections = parsermd::rmd_node_sections(original_ast)

  selected_headings = all_sections[selected_node_indices] |>
    lapply(function(x) {
      x[!is.na(x)]
    })
  
  # Remove duplicates while preserving order
  unique(selected_headings)
}

# Apply individual rule to an AST subset and determine if the rule condition is met.
#
# ast_subset: rmd_ast or list of nodes to evaluate
# rule: markermd_rule S7 object containing the rule definition
# num_selected_nodes: Integer number of originally selected heading nodes

evaluate_rule = function(ast_subset, rule, num_selected_nodes = 0) {
  
  stopifnot(S7::S7_inherits(rule, markermd_rule))
  
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
    "has between" = {
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
    
    "has at least" = {
      # Count nodes - only subtract heading nodes if we're counting headings
      actual_count = if (rule@node_type == "rmd_heading") {
        length(nodes) - num_selected_nodes
      } else {
        length(nodes)
      }
      min_count = rule@values
      
      passed = actual_count >= min_count
      message = if (passed) {
        paste0("Minimum count check passed: ", actual_count, " nodes (minimum: ", min_count, ")")
      } else {
        paste0("Minimum count check failed: ", actual_count, " nodes (expected: at least ", min_count, ")")
      }
      
      list(passed = passed, message = message)
    },
    
    "has at most" = {
      # Count nodes - only subtract heading nodes if we're counting headings
      actual_count = if (rule@node_type == "rmd_heading") {
        length(nodes) - num_selected_nodes
      } else {
        length(nodes)
      }
      max_count = rule@values
      
      passed = actual_count <= max_count
      message = if (passed) {
        paste0("Maximum count check passed: ", actual_count, " nodes (maximum: ", max_count, ")")
      } else {
        paste0("Maximum count check failed: ", actual_count, " nodes (expected: at most ", max_count, ")")
      }
      
      list(passed = passed, message = message)
    },
    
    "has content" = {
      pattern = rule@values[1]
      if (is.na(pattern) || nchar(pattern) == 0) {
        return(list(passed = TRUE, message = "Empty pattern always matches"))
      }
      
      # Extract content from all nodes - errors will propagate up
      content_found = FALSE
      for (node in nodes) {
        node_content = parsermd::as_document(node) |>
          as.character() |>
          paste(collapse = " ")
        
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
    
    "lacks content" = {
      pattern = rule@values[1]
      if (is.na(pattern) || nchar(pattern) == 0) {
        return(list(passed = TRUE, message = "Empty pattern never matches"))
      }
      
      # Extract content from all nodes - errors will propagate up
      content_found = FALSE
      for (node in nodes) {
        node_content = parsermd::as_document(node) |>
          as.character() |>
          paste(collapse = " ")
        
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

# Validate all rules for a single question against a new AST using section-based matching.
#
# new_ast: rmd_ast object from the document to validate
# original_ast: rmd_ast object from template creation  
# question: markermd_question S7 object containing rules and node selections

validate_question_rules = function(new_ast, original_ast, question) {
  
  stopifnot(S7::S7_inherits(question, markermd_question))
  
  
  # Get question AST subset using the shared helper function
  question_ast = get_question_ast_subset(new_ast, original_ast, question)
  
  if (is.null(question_ast)) {
    stop("No section hierarchies could be resolved from selected nodes")
  }
  
  # Get section hierarchies for details display
  section_hierarchies = resolve_section_hierarchies(original_ast, question@selected_nodes@indices)
  
  # Format section hierarchies for display (convert list of character vectors to readable strings)
  formatted_hierarchies = if (length(section_hierarchies) > 0) {
    sapply(section_hierarchies, function(hierarchy) {
      if (is.character(hierarchy) && length(hierarchy) > 0) {
        paste(hierarchy, collapse = " > ")
      } else {
        "Unknown hierarchy"
      }
    })
  } else {
    character(0)
  }
  
  # Count originally selected heading nodes for count adjustments
  num_selected_nodes = length(question@selected_nodes@indices)
  
  # If no rules, consider it a pass
  if (length(question@rules) == 0) {
    return(list(
      question_name = question@name,
      status = "pass",
      messages = "No rules defined - validation passed",
      details = paste0("Section hierarchy(ies): ", paste(formatted_hierarchies, collapse = ", "))
    ))
  }
  
  # Evaluate each rule - any errors will propagate up
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
  
  # Return results - only pass or fail status
  list(
    question_name = question@name,
    status = if (all_passed) "pass" else "fail",
    messages = sapply(rule_results, function(r) r$message),
    details = paste(c(
      paste0("Section hierarchy(ies): ", paste(formatted_hierarchies, collapse = ", ")),
      sapply(rule_results, function(r) r$message)
    ), collapse = "\n")
  )
}

# Validates a parsed repository AST against template rules using section-based matching.
# This replaces the parsermd template-based validation system.
#
# ast: rmd_ast object from parsing a repository document
# template_obj: markermd_template S7 object with questions and rules

validate_repo_against_rules = function(ast, template_obj) {

  stopifnot(S7::S7_inherits(template_obj, markermd_template))
    
  results = list()
  
  for (question in template_obj@questions) {
    result = validate_question_rules(ast, template_obj@original_ast, question)
    results[[question@name]] = result
  }
  
  return(results)
}

# Extracts the AST subset for a specific question using hierarchical section matching
#
# current_ast: rmd_ast object from the document to analyze
# original_ast: rmd_ast object from template creation  
# question: markermd_question S7 object containing selected nodes

get_question_ast_subset = function(current_ast, original_ast, question) {
  
  stopifnot(S7::S7_inherits(question, markermd_question))
  
  section_hierarchies = resolve_section_hierarchies(original_ast, question@selected_nodes@indices)
  
  if (length(section_hierarchies) == 0) {
    return(current_ast)  # Return empty AST
  }
  
  # Create expressions for by_section() calls using !!
  args = lapply(section_hierarchies, function(x) {
    rlang::expr(parsermd::by_section(!!x, keep_parents = FALSE))
  })
  
  # Use splice operator to pass all selectors to rmd_select
  parsermd::rmd_select(current_ast, !!!args, keep_yaml = FALSE)
}

# Extracts the content for specific questions from a parsed AST based on template node selections
#
# ast: rmd_ast object from parsing a repository document
# template_obj: markermd_template S7 object containing questions with node selections

extract_question_content = function(ast, template_obj) {
  
  stopifnot(S7::S7_inherits(template_obj, markermd_template))

  results = list()
  
  for (q in template_obj@questions) {
    question_name = q@name
    selected_nodes = q@selected_nodes@indices
    
    if (length(selected_nodes) == 0) {
      results[[question_name]] = "No content selected for this question."
      next
    }
    
    # Extract content using the shared AST subset helper
    # Get the AST subset for this question
    question_ast = get_question_ast_subset(ast, template_obj@original_ast, q)
    
    question_content = if (is.null(question_ast) || length(question_ast@nodes) == 0) {
      "No matching content found in this document for the selected sections."
    } else {
      # Extract content from all nodes in the subset
      node_contents = sapply(question_ast@nodes, function(node) {
        # Use as_document() to get the node content as text
        content = parsermd::as_document(node) |>
          as.character() |>
          paste(collapse = "\n")
        
        return(content)
      })
      
      # Remove empty contents and combine
      node_contents = node_contents[nchar(node_contents) > 0]
      if (length(node_contents) > 0) {
        paste(node_contents, collapse = "\n\n")
      } else {
        "No content available for the matched sections."
      }
    }
    
    results[[question_name]] = question_content
  }
  
  return(results)
}