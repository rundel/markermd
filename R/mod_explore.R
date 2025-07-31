#' Explore Interface Module
#'
#' Shiny module for exploring assignment documents

#' Explore UI
#'
#' @param id Character. Module namespace ID
#'
explore_ui = function(id) {
  ns = shiny::NS(id)
  
  shiny::div(
    id = ns("template_validation"),
    style = "max-height: calc(100vh - 200px); overflow-y: auto;",
    shiny::uiOutput(ns("template_validation_ui"))
  )
}

#' Explore Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param current_repo_name Reactive. Current repository name
#' @param validation_results Reactive. Validation results for current repository
#' @param selected_question_name Reactive. Currently selected question name
#' @param template Reactive. Template object for reference
#'
explore_server = function(id, ast, current_repo_name = shiny::reactiveVal(NULL), validation_results = shiny::reactiveVal(NULL), selected_question_name = shiny::reactiveVal(NULL), template = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # All questions validation cards UI
    output$template_validation_ui = shiny::renderUI({
      current_validation = validation_results()
      current_template = template()
      
      if (is.null(current_template)) {
        return(shiny::p("No template loaded for validation.", 
                       style = "color: #6c757d; font-style: italic;"))
      }
      
      if (is.null(current_validation) || length(current_validation) == 0) {
        return(shiny::p("No validation data available for current repository.", 
                       style = "color: #6c757d; font-style: italic;"))
      }
      
      # Create cards for all questions
      question_cards = lapply(current_template@questions, function(template_question) {
        question_name = template_question@name
        
        # Get validation result for this question
        question_result = if (!is.null(current_validation[[question_name]])) {
          current_validation[[question_name]]
        } else {
          list(status = "unknown", messages = character(0))
        }
        
        # Overall status styling
        status_color = switch(question_result$status,
          "pass" = "#28a745",
          "fail" = "#dc3545", 
          "error" = "#ffc107",
          "#6c757d"
        )
        
        # Create HTML for solid circle icons with white symbols
        status_icon_html = switch(question_result$status,
          "pass" = '<i class="fas fa-circle" style="color: #28a745;"></i><i class="fas fa-check" style="color: white; position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%); font-size: 10px;"></i>',
          "fail" = '<i class="fas fa-circle" style="color: #dc3545;"></i><i class="fas fa-times" style="color: white; position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%); font-size: 10px;"></i>',
          "error" = '<i class="fas fa-circle" style="color: #ffc107;"></i><i class="fas fa-exclamation" style="color: white; position: absolute; left: 50%; top: 50%; transform: translate(-50%, -50%); font-size: 10px;"></i>',
          '<i class="fas fa-question-circle" style="color: #6c757d;"></i>'
        )
        
        # Get the document nodes for this question using section selection
        question_nodes_content = if (length(template_question@selected_nodes@indices) > 0) {
          tryCatch({
            # Resolve section hierarchies from the template's selected nodes
            section_hierarchies = resolve_section_hierarchies(current_template@original_ast, template_question@selected_nodes@indices)
            
            if (length(section_hierarchies) > 0) {
              # Get the question content from current AST using hierarchical matching
              current_ast = ast()
              if (!is.null(current_ast)) {
                question_ast = tryCatch({
                  # Get all node section hierarchies for the current AST
                  current_ast_sections = parsermd::rmd_node_sections(current_ast)
                  
                  # Find nodes that match our target hierarchies
                  matching_indices = c()
                  
                  for (target_hierarchy in section_hierarchies) {
                    # Split target hierarchy into parts
                    target_parts = strsplit(target_hierarchy, " > ")[[1]]
                    
                    # Find nodes whose hierarchy matches or is contained within the target
                    for (i in seq_along(current_ast_sections)) {
                      node_hierarchy = current_ast_sections[[i]]
                      if (is.character(node_hierarchy) && length(node_hierarchy) > 0) {
                        # Remove NAs from node hierarchy
                        clean_node_hierarchy = node_hierarchy[!is.na(node_hierarchy)]
                        
                        # Check if target hierarchy matches or is a prefix of node hierarchy
                        if (length(clean_node_hierarchy) >= length(target_parts)) {
                          # Check if the target parts match the beginning of the node hierarchy
                          if (all(target_parts == clean_node_hierarchy[1:length(target_parts)])) {
                            matching_indices = c(matching_indices, i)
                          }
                        }
                      }
                    }
                  }
                  
                  # Remove duplicates and sort
                  matching_indices = unique(sort(matching_indices))
                  
                  if (length(matching_indices) > 0) {
                    # Subset the AST using the matching indices
                    current_ast[matching_indices]
                  } else {
                    # No matches found, fallback to by_section
                    section_names = sapply(section_hierarchies, function(hierarchy) {
                      parts = strsplit(hierarchy, " > ")[[1]]
                      parts[length(parts)]
                    })
                    parsermd::rmd_select(current_ast, parsermd::by_section(section_names), keep_yaml = FALSE)
                  }
                }, error = function(e) {
                  # Fallback to by_section approach
                  section_names = sapply(section_hierarchies, function(hierarchy) {
                    parts = strsplit(hierarchy, " > ")[[1]]
                    parts[length(parts)]
                  })
                  parsermd::rmd_select(current_ast, parsermd::by_section(section_names), keep_yaml = FALSE)
                })
                
                if (!is.null(question_ast) && length(question_ast@nodes) > 0) {
                  # Build tree structure for the question AST
                  tree_items = build_ast_tree_structure(question_ast)
                  
                  if (length(tree_items) > 1) {  # Must have more than just document root
                    # Filter out document root (index 0) and adjust depths for question display
                    content_items = tree_items[sapply(tree_items, function(x) x$index != 0)]
                    
                    # Adjust depths to start from 1 for content nodes (so they display at visual level 1)
                    if (length(content_items) > 0) {
                      min_depth = min(sapply(content_items, function(x) x$depth))
                      content_items = lapply(content_items, function(x) {
                        x$depth = x$depth - min_depth + 1
                        # Update parent indices: if parent was document root (0), set to NULL
                        if (!is.null(x$parent_index) && x$parent_index == 0) {
                          x$parent_index = NULL
                        }
                        x
                      })
                      
                      # Create non-interactive tree display for this question
                      # Use unique ID prefix for each question to avoid conflicts
                      question_id = paste0("q_", gsub("[^A-Za-z0-9]", "", question_name))
                      # Start tree at depth 1 to match the adjusted node depths
                      simple_tree = create_simple_tree_readonly_at_depth(content_items, session$ns, question_id, start_depth = 1)
                      
                      shiny::div(
                        style = "margin-bottom: 3px; padding: 8px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; max-height: 120px; overflow-y: auto; font-size: 11px;",
                        simple_tree
                      )
                    } else {
                      shiny::p("No content found in selected sections.", style = "font-size: 11px; color: #6c757d; font-style: italic; margin: 8px 0;")
                    }
                  } else {
                    shiny::p("No content found in selected sections.", style = "font-size: 11px; color: #6c757d; font-style: italic; margin: 8px 0;")
                  }
                } else {
                  shiny::p("No matching sections found in current document.", style = "font-size: 11px; color: #6c757d; font-style: italic; margin: 8px 0;")
                }
              } else {
                shiny::p("No document loaded.", style = "font-size: 11px; color: #6c757d; font-style: italic; margin: 8px 0;")
              }
            } else {
              shiny::p("No sections resolved from template.", style = "font-size: 11px; color: #6c757d; font-style: italic; margin: 8px 0;")
            }
          }, error = function(e) {
            shiny::p(paste("Error loading content:", e$message), style = "font-size: 11px; color: #dc3545; font-style: italic; margin: 8px 0;")
          })
        } else {
          shiny::p("No nodes selected in template.", style = "font-size: 11px; color: #6c757d; font-style: italic; margin: 8px 0;")
        }
        
        # Create rule details
        rule_details = if (length(template_question@rules) > 0) {
          # Parse rule messages to understand individual rule results
          if (is.character(question_result$messages) && length(question_result$messages) > 0) {
            rule_items = lapply(seq_along(question_result$messages), function(i) {
              message = question_result$messages[i]
              rule = if (i <= length(template_question@rules)) template_question@rules[[i]] else NULL
              
              # Determine if this rule passed based on message content
              rule_passed = grepl("passed|check passed|always matches|never matches", message, ignore.case = TRUE)
              rule_color = if (rule_passed) "#28a745" else "#dc3545"
              rule_icon = if (rule_passed) "check" else "times"
              
              shiny::div(
                style = paste0("margin: 6px 0; padding: 8px; border-left: 3px solid ", rule_color, "; background-color: #f8f9fa; border-radius: 3px;"),
                shiny::div(
                  style = "display: flex; align-items: center; margin-bottom: 4px;",
                  shiny::icon(rule_icon, style = paste0("color: ", rule_color, "; margin-right: 6px; font-size: 14px;")),
                  shiny::strong(
                    if (!is.null(rule)) {
                      paste0(rule@node_type, " ", rule@verb, " \"", paste(rule@values, collapse = ", "), "\"")
                    } else {
                      paste0("Rule ", i)
                    },
                    style = "font-size: 13px;"
                  )
                )
              )
            })
            
            shiny::div(rule_items)
          } else {
            shiny::p("No detailed rule information available.", style = "font-size: 12px; color: #6c757d; font-style: italic; margin: 10px 0;")
          }
        } else {
          shiny::p("No rules defined for this question.", style = "font-size: 12px; color: #6c757d; font-style: italic; margin: 10px 0; text-align: center;")
        }
        
        # Create question card
        bslib::card(
          bslib::card_header(
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              shiny::h6(question_name, style = "margin: 0; color: #333; font-weight: 600;"),
              shiny::div(
                style = "position: relative; font-size: 18px;",
                shiny::HTML(status_icon_html)
              )
            )
          ),
          bslib::card_body(
            style = "padding: 12px;",
            question_nodes_content,
            rule_details
          )
        )
      })
      
      # Arrange question cards in two columns with wrapping
      if (length(question_cards) == 0) {
        shiny::p("No questions available.", style = "color: #6c757d; font-style: italic;")
      } else {
        # Split cards into two columns for better distribution
        left_column_cards = question_cards[seq(1, length(question_cards), 2)]
        right_column_cards = if (length(question_cards) > 1) {
          question_cards[seq(2, length(question_cards), 2)]
        } else {
          list()
        }
        
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::div(
            style = "display: flex; flex-direction: column; gap: 12px;",
            left_column_cards
          ),
          shiny::div(
            style = "display: flex; flex-direction: column; gap: 12px;",
            right_column_cards
          )
        )
      }
    })
    
    # Store created observers to avoid duplicates
    created_observers = shiny::reactiveValues()
    
    # Create preview button observers once when template and AST are available
    shiny::observe({
      current_template = template()
      current_ast = ast()
      
      if (!is.null(current_template) && !is.null(current_ast)) {
        # Create observers for all questions and their nodes
        for (template_question in current_template@questions) {
          question_id = paste0("q_", gsub("[^A-Za-z0-9]", "", template_question@name))
          
          # Skip if observers already created for this question
          if (!is.null(created_observers[[question_id]])) {
            next
          }
          
          # Get the question AST using hierarchical matching (same logic as in renderUI)
          question_ast = tryCatch({
            # Resolve section hierarchies from the template's selected nodes
            section_hierarchies = resolve_section_hierarchies(current_template@original_ast, template_question@selected_nodes@indices)
            
            if (length(section_hierarchies) > 0) {
              # Get matching nodes using hierarchical matching
              current_ast_sections = parsermd::rmd_node_sections(current_ast)
              matching_indices = c()
              
              for (target_hierarchy in section_hierarchies) {
                target_parts = strsplit(target_hierarchy, " > ")[[1]]
                
                for (i in seq_along(current_ast_sections)) {
                  node_hierarchy = current_ast_sections[[i]]
                  if (is.character(node_hierarchy) && length(node_hierarchy) > 0) {
                    clean_node_hierarchy = node_hierarchy[!is.na(node_hierarchy)]
                    
                    if (length(clean_node_hierarchy) >= length(target_parts)) {
                      if (all(target_parts == clean_node_hierarchy[1:length(target_parts)])) {
                        matching_indices = c(matching_indices, i)
                      }
                    }
                  }
                }
              }
              
              matching_indices = unique(sort(matching_indices))
              if (length(matching_indices) > 0) {
                current_ast[matching_indices]
              } else {
                NULL
              }
            } else {
              NULL
            }
          }, error = function(e) {
            NULL
          })
          
          if (!is.null(question_ast) && length(question_ast@nodes) > 0) {
            # Build tree structure and create observers for each node
            tree_items = build_ast_tree_structure(question_ast)
            
            if (length(tree_items) > 1) {  # Has more than just document root
              content_items = tree_items[sapply(tree_items, function(x) x$index != 0)]
              
              if (length(content_items) > 0) {
                # Create preview observers for each content node
                for (item in content_items) {
                  local({
                    local_node_index = item$index
                    local_question_ast = question_ast
                    button_id = paste0("preview_", question_id, "_", local_node_index)
                    
                    shiny::observeEvent(input[[button_id]], {
                      # Get the actual node from the question AST
                      if (local_node_index >= 1 && local_node_index <= length(local_question_ast@nodes)) {
                        node = local_question_ast@nodes[[local_node_index]]
                        
                        # Get node content for preview
                        content = parsermd::as_document(node) |>
                          as.character() |>
                          paste(collapse = "\n")
                        
                        # Remove leading whitespace from all lines to prevent Prism indentation issues
                        content_lines = strsplit(content, "\n")[[1]]
                        # Find the minimum indentation (excluding empty lines)
                        non_empty_lines = content_lines[nzchar(trimws(content_lines))]
                        if (length(non_empty_lines) > 0) {
                          min_indent = min(nchar(content_lines) - nchar(trimws(content_lines, which = "left")), na.rm = TRUE)
                          # Remove the minimum indentation from all lines
                          content_lines = sapply(content_lines, function(line) {
                            if (nzchar(trimws(line))) {
                              substr(line, min_indent + 1, nchar(line))
                            } else {
                              line  # Keep empty lines as-is
                            }
                          })
                        }
                        content = paste(content_lines, collapse = "\n")
                        
                        
                        # Get node type for title
                        node_type = class(node)[1]
                        
                        # Determine syntax highlighting language based on node type
                        syntax_language = switch(node_type,
                          "rmd_yaml" = "yaml",
                          "rmd_markdown" = "markdown", 
                          "rmd_chunk" = {
                            # Extract engine from chunk
                            if (inherits(node, "rmd_chunk") && !is.null(node@engine)) {
                              # Map common R Markdown engines to Prism languages
                              switch(node@engine,
                                "r" = "r",
                                "python" = "python",
                                "sql" = "sql",
                                "bash" = "bash",
                                "sh" = "bash",
                                "javascript" = "javascript",
                                "js" = "javascript",
                                "css" = "css",
                                "html" = "html",
                                "yaml" = "yaml",
                                "json" = "json",
                                # Default to R for unknown engines
                                "r"
                              )
                            } else {
                              "r"  # Default to R
                            }
                          },
                          "rmd_raw_chunk" = "text",
                          "rmd_code_block" = "text",
                          # Default to text for other node types
                          "text"
                        )
                        
                        shiny::showModal(
                          shiny::modalDialog(
                            title = shiny::span(node_type, style = "font-size: 16px; font-weight: bold;"),
                            size = "l",
                            shiny::div(
                              style = "max-height: 500px; overflow-y: auto;",
                              # Pre element with soft wrapping for long lines
                              shiny::tags$pre(
                                id = paste0("syntax-content-", local_node_index),
                                style = "margin: 0; font-size: 12px; line-height: 1.4; background: #f5f2f0; padding: 15px; border-radius: 3px; white-space: pre-wrap; word-wrap: break-word; overflow-wrap: break-word;",
                                content  # Raw content without HTML escaping for now
                              )
                            ),
                            footer = NULL,
                            easyClose = TRUE
                          )
                        )
                        
                        # Apply syntax highlighting manually to avoid Prism's auto-formatting
                        shinyjs::runjs(paste0("
                          setTimeout(function() {
                            var preElement = document.getElementById('syntax-content-", local_node_index, "');
                            if (preElement && typeof Prism !== 'undefined') {
                              // Create a temporary code element with the language class
                              var codeElement = document.createElement('code');
                              codeElement.className = 'language-", syntax_language, "';
                              codeElement.textContent = preElement.textContent;
                              
                              // Clear the pre element and append the code element
                              preElement.innerHTML = '';
                              preElement.appendChild(codeElement);
                              
                              // Highlight just this element
                              Prism.highlightElement(codeElement);
                              console.log('Manually highlighted element');
                            } else {
                              console.log('Element or Prism not found');
                            }
                          }, 200);
                        "))
                      }
                    }, ignoreInit = TRUE)
                  })
                }
                
                # Mark observers as created for this question
                created_observers[[question_id]] = TRUE
              }
            }
          }
        }
      }
    })
    
    # Return empty list for now
    return(list())
  })
}