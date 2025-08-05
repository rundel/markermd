# Extract detailed heading hierarchies from selected nodes
#
# ast: rmd_ast object from template creation
# i: Integer vector of originally selected node indices

get_heading_selector = function(ast, i) {
  
  all_sections = parsermd::rmd_node_sections(ast)

  headings = all_sections[i] |>
    lapply(function(x) {
      x[!is.na(x)]
    })
  
  unique(headings) # Remove duplicates
}

# Calculate node count for rule evaluation
#
# nodes: List of AST nodes
# node_type: Rule node type
# num_selected_nodes: Number of originally selected heading nodes

calculate_node_count = function(nodes, node_type, num_selected_nodes) {
  if (node_type == "rmd_heading") {
    length(nodes) - num_selected_nodes
  } else {
    length(nodes)
  }
}

# Evaluate "has between" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object
# num_selected_nodes: Number of originally selected heading nodes

evaluate_rule_has_between = function(nodes, rule, num_selected_nodes) {
  actual_count = calculate_node_count(nodes, rule@node_type, num_selected_nodes)
  min_count = rule@values[1]
  max_count = rule@values[2]
  
  passed = actual_count >= min_count && actual_count <= max_count
  message = if (passed) {
    paste0("Count check passed: ", actual_count, " nodes (range: ", min_count, "-", max_count, ")")
  } else {
    paste0("Count check failed: ", actual_count, " nodes (expected: ", min_count, "-", max_count, ")")
  }
  
  list(passed = passed, message = message)
}

# Evaluate "has at least" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object
# num_selected_nodes: Number of originally selected heading nodes

evaluate_rule_has_at_least = function(nodes, rule, num_selected_nodes) {
  actual_count = calculate_node_count(nodes, rule@node_type, num_selected_nodes)
  min_count = rule@values
  
  passed = actual_count >= min_count
  message = if (passed) {
    paste0("Minimum count check passed: ", actual_count, " nodes (minimum: ", min_count, ")")
  } else {
    paste0("Minimum count check failed: ", actual_count, " nodes (expected: at least ", min_count, ")")
  }
  
  list(passed = passed, message = message)
}

# Evaluate "has at most" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object
# num_selected_nodes: Number of originally selected heading nodes

evaluate_rule_has_at_most = function(nodes, rule, num_selected_nodes) {
  actual_count = calculate_node_count(nodes, rule@node_type, num_selected_nodes)
  max_count = rule@values
  
  passed = actual_count <= max_count
  message = if (passed) {
    paste0("Maximum count check passed: ", actual_count, " nodes (maximum: ", max_count, ")")
  } else {
    paste0("Maximum count check failed: ", actual_count, " nodes (expected: at most ", max_count, ")")
  }
  
  list(passed = passed, message = message)
}

# Evaluate "has content" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_has_content = function(nodes, rule) {
  pattern = rule@values[1]
  if (is.na(pattern) || nchar(pattern) == 0) {
    return(list(passed = TRUE, message = "Empty pattern always matches"))
  }
  
  content_found = FALSE
  for (node in nodes) {
    node_content = parsermd::as_document(node) |>
      as.character() |>
      paste(collapse = " ")
    
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
}

# Evaluate "lacks content" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_lacks_content = function(nodes, rule) {
  pattern = rule@values[1]
  if (is.na(pattern) || nchar(pattern) == 0) {
    return(list(passed = TRUE, message = "Empty pattern never matches"))
  }
  
  content_found = FALSE
  for (node in nodes) {
    node_content = parsermd::as_document(node) |>
      as.character() |>
      paste(collapse = " ")
    
    if (length(grep(utils::glob2rx(pattern), node_content, ignore.case = TRUE)) > 0) {
      content_found = TRUE
      break
    }
  }
  
  passed = !content_found
  message = if (passed) {
    paste0("Negative content check passed: pattern '", pattern, "' not found")
  } else {
    paste0("Negative content check failed: pattern '", pattern, "' found")
  }
  
  list(passed = passed, message = message)
}

# Evaluate "has name" rule
#
# nodes: List of AST nodes
# rule: markermd_rule S7 object

evaluate_rule_has_name = function(nodes, rule) {
  pattern = rule@values[1]
  if (is.na(pattern) || nchar(pattern) == 0) {
    return(list(passed = TRUE, message = "Empty pattern always matches"))
  }
  
  name_found = FALSE
  for (node in nodes) {
    node_name = ""
    
    if (inherits(node, "rmd_heading") && !is.null(node@name)) {
      node_name = node@name
    } else if (inherits(node, "rmd_chunk") && !is.null(node@name)) {
      node_name = node@name
    }
    
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
}

# Apply individual rule to an AST subset and determine if the rule condition is met.
#
# ast: rmd_ast or list of nodes to evaluate
# rule: markermd_rule S7 object containing the rule definition
# num_selected_nodes: Integer number of originally selected heading nodes

evaluate_rule = function(ast, rule, num_selected_nodes = 0) {
  
  stopifnot(S7::S7_inherits(rule, markermd_rule))
  
  # Convert ast to nodes if needed
  nodes = if (inherits(ast, "rmd_ast")) {
    ast@nodes
  } else {
    ast
  }

  stopifnot(is.list(nodes))
  
  # Apply node type filtering if not "Any node"
  if (rule@node_type != "Any node") {
    nodes = nodes[sapply(nodes, function(n) inherits(n, rule@node_type))]
  }
  
  # Delegate to specific rule evaluation functions
  switch(rule@verb,
    "has between" = evaluate_rule_has_between(nodes, rule, num_selected_nodes),
    "has at least" = evaluate_rule_has_at_least(nodes, rule, num_selected_nodes),
    "has at most" = evaluate_rule_has_at_most(nodes, rule, num_selected_nodes),
    "has content" = evaluate_rule_has_content(nodes, rule),
    "lacks content" = evaluate_rule_lacks_content(nodes, rule),
    "has name" = evaluate_rule_has_name(nodes, rule),
    # Default case for unknown verbs
    list(passed = FALSE, message = paste0("Unknown rule verb: ", rule@verb))
  )
}

# Validate all rules for a single question against a new AST using section-based matching.
#
# repo_ast: rmd_ast object from the document to validate
# template_ast: rmd_ast object from template creation  
# question: markermd_question S7 object containing rules and node selections

validate_question_rules = function(repo_ast, template_ast, question) {
  
  # Get question AST subset using the shared helper function
  question_ast = get_question_ast_subset(repo_ast, template_ast, question)
  
  if (is.null(question_ast)) {
    stop("No section hierarchies could be resolved from selected nodes")
  }
  
  # Get section hierarchies for details display
  section_hierarchies = get_heading_selector(template_ast, question@selected_nodes@indices)
  
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
# template: markermd_template S7 object with questions and rules

validate_repo_against_rules = function(ast, template) {

  stopifnot(S7::S7_inherits(template, markermd_template))
    
  results = list()
  
  for (question in template@questions) {
    result = validate_question_rules(ast, template@original_ast, question)
    results[[question@name]] = result
  }
  
  return(results)
}

# Extracts the AST subset for a specific question using hierarchical section matching
#
# current_ast: rmd_ast object from the document to analyze
# template_ast: rmd_ast object from template creation  
# question: markermd_question S7 object containing selected nodes

get_question_ast_subset = function(current_ast, template_ast, question) {
  
  stopifnot(S7::S7_inherits(question, markermd_question))
  
  selectors = get_heading_selector(template_ast, question@selected_nodes@indices)
  
  if (length(selectors) == 0) {
    return(current_ast)  # Return empty AST
  }
  
  # Create expressions for by_section() calls using !!
  args = lapply(selectors, function(x) {
    rlang::expr(parsermd::by_section(!!x, keep_parents = FALSE))
  })
  
  # Use splice operator to pass all selectors to rmd_select
  parsermd::rmd_select(current_ast, !!!args, keep_yaml = FALSE)
}

# Extracts the content for specific questions from a parsed AST based on template node selections
#
# repo_ast: rmd_ast object from parsing a repository document
# template: markermd_template S7 object containing questions with node selections

extract_question_content = function(repo_ast, template) {
  
  stopifnot(S7::S7_inherits(template, markermd_template))

  results = list()
  
  for (q in template@questions) {
    if (length(q@selected_nodes@indices) == 0) {
      results[[q@name]] = "No content selected for this question."
      next
    }
    
    # Extract content using the shared AST subset helper
    # Get the AST subset for this question
    question_ast = get_question_ast_subset(repo_ast, template@original_ast, q)
    
    if (is.null(question_ast) || length(question_ast@nodes) == 0) {
      results[[q@name]] = "No matching content found in this document for the selected sections."
      next
    } 

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
    
    results[[q@name]] = if (length(node_contents) > 0) {
      paste(node_contents, collapse = "\n\n")
    } else {
      "No content available for the matched sections."
    }
  }
  
  return(results)
}