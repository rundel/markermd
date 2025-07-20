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
    style = "height: calc(100vh - 150px); min-height: 600px; padding: 20px;",
    
    shiny::div(
      style = "border: 1px solid #ddd; padding: 15px; background-color: #f9f9f9; min-height: 400px;",
      shiny::h4("Validation Results"),
      shiny::div(
        id = ns("template_validation"),
        style = "max-height: calc(100vh - 250px); overflow-y: auto;",
        shiny::uiOutput(ns("template_validation_ui"))
      )
    )
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
            # Resolve section names from the template's selected nodes
            section_names = resolve_section_names(current_template@original_ast, template_question@selected_nodes@indices)
            
            if (length(section_names) > 0) {
              # Get the question content from current AST using by_section
              current_ast = ast()
              if (!is.null(current_ast)) {
                question_ast = parsermd::rmd_select(current_ast, parsermd::by_section(section_names), keep_yaml = FALSE)
                
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
                      simple_tree = create_simple_tree_readonly_at_depth(content_items, function(x) paste0("question_", question_id, "_", x), question_id, start_depth = 1)
                      
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
    
    # Return empty list for now
    return(list())
  })
}