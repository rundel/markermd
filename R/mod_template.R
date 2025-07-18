#' Template Interface Module
#'
#' Shiny module for creating assignment templates

#' Template UI
#'
#' @param id Character. Module namespace ID
#'
template_ui = function(id) {
  ns = shiny::NS(id)
  
  shiny::div(
    style = "height: calc(100vh - 150px); min-height: 600px;",
    bslib::layout_columns(
      col_widths = c(6, 6),
      style = "height: 100%;",
      shiny::div(
        style = "height: 100%; display: flex; flex-direction: column;",
        shiny::h3("Document Structure", style = "flex-shrink: 0;"),
        shiny::div(
          id = ns("ast_tree_container"),
          style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; flex: 1; overflow-y: auto; min-height: 400px;",
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          shiny::helpText("Click node text to select, click circle to toggle with children, ", shiny::icon("search"), " to preview content"),
          shiny::actionButton(ns("clear_selections"), "Clear Selections", class = "btn-secondary btn-sm")
        ),
        shiny::uiOutput(ns("ast_tree_ui"))
      )
      ),
      shiny::div(
        style = "height: 100%; display: flex; flex-direction: column;",
        shiny::h3("Questions", style = "flex-shrink: 0;"),
        shiny::div(
          id = ns("questions_container"),
          style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; flex: 1; overflow-y: auto; min-height: 300px; margin-bottom: 10px;",
          shiny::uiOutput(ns("questions_ui"))
        ),
        shiny::div(
          style = "flex-shrink: 0; margin-top: 10px;",
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::uiOutput(ns("save_button_ui")),
            shiny::actionButton(ns("load_template"), "Load Template", class = "btn-warning btn-sm", style = "width: 100%;")
          )
        ),
      )
    )
  )
}

#' Template Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param template_path Character. Optional path to template RDS file to load on startup
#'
template_server = function(id, ast, template_path = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Centralized state management
    state = shiny::reactiveVal(list(
      questions = list(),           # List of question objects: list(id=1, name="Question 1", selected_nodes=c(...))
      current_question_id = 1,      # Currently active question ID
      last_question_id = 0          # Last created question ID (incremented for each new question)
    ))
    
    # Helper function to create delete observer for a question
    create_delete_observer = function(question_id) {
      shiny::observeEvent(input[[paste0("delete_question_", question_id)]], {
        current_state = state()
        current_questions = current_state$questions
        
        # Find and remove this question
        question_to_remove_idx = which(sapply(current_questions, function(q) q$id == question_id))
        if (length(question_to_remove_idx) > 0) {
          # Destroy the observer before removing the question
          if (!is.null(current_questions[[question_to_remove_idx]]$delete_observer)) {
            current_questions[[question_to_remove_idx]]$delete_observer$destroy()
          }
          
          # Remove the question
          updated_questions = current_questions[-question_to_remove_idx]
          
          # Update current question if needed
          if (current_state$current_question_id == question_id) {
            if (length(updated_questions) > 0) {
              current_state$current_question_id = updated_questions[[1]]$id
            } else {
              current_state$current_question_id = 1
            }
          }
          
          # Update state
          current_state$questions = updated_questions
          state(current_state)
        }
      })
    }
    
    # Load template on startup if provided
    shiny::observe({
      if (!is.null(template_path)) {
        # Load the template file automatically
        template_data = tryCatch({
          readRDS(template_path)
        }, error = function(e) {
          shiny::showNotification(paste("Error loading template:", e$message), type = "error")
          return(NULL)
        })
        
        if (!is.null(template_data) && is.list(template_data) && !is.null(template_data$questions)) {
          current_state = state()
          loaded_questions = template_data$questions
          
          # Convert loaded questions to state structure with delete observers
          if (length(loaded_questions) > 0) {
            for (i in seq_along(loaded_questions)) {
              # Ensure selected_nodes exists and sort them
              if (is.null(loaded_questions[[i]]$selected_nodes)) {
                loaded_questions[[i]]$selected_nodes = integer(0)
              } else {
                # Use nodes directly without offset
                loaded_questions[[i]]$selected_nodes = sort(loaded_questions[[i]]$selected_nodes)
              }
              
              # Ensure strict field exists
              if (is.null(loaded_questions[[i]]$strict)) {
                loaded_questions[[i]]$strict = FALSE
              }
              
              # Create delete observer for this loaded question
              loaded_questions[[i]]$delete_observer = create_delete_observer(loaded_questions[[i]]$id)
            }
            
            # Update state
            current_state$questions = loaded_questions
            current_state$current_question_id = loaded_questions[[1]]$id
            
            # Set last_question_id to the highest ID among loaded questions
            max_id = max(sapply(loaded_questions, function(q) q$id))
            current_state$last_question_id = max_id
            
            state(current_state)
            
            shiny::showNotification(
              paste("Successfully loaded template with", length(loaded_questions), "questions"), 
              type = "message"
            )
          }
        }
      }
    })
    
    # Get AST nodes for easier handling
    ast_nodes = shiny::reactive({
      if (is.null(ast())) return(NULL)
      
      # Handle new parsermd structure with nodes slot
      if (inherits(ast(), "rmd_ast") && !is.null(ast()@nodes)) {
        ast()@nodes
      } else {
        ast()
      }
    })
    
    # Create interactive AST tree display
    output$ast_tree_ui = shiny::renderUI({
      if (is.null(ast()) || is.null(ast_nodes())) {
        return(shiny::p("No document loaded"))
      }
      
      current_state = state()
      current_q_id = current_state$current_question_id
      
      # Get selected nodes for current question
      selected_nodes = integer(0)
      if (length(current_state$questions) > 0) {
        question_matches = sapply(current_state$questions, function(q) q$id == current_q_id)
        question_idx = which(question_matches)
        if (length(question_idx) > 0) {
          current_question = current_state$questions[[question_idx]]
          selected_nodes = current_question$selected_nodes %||% integer(0)
        }
      }
      
      # Build tree structure
      tree_items = build_ast_tree_structure(ast())
      
      if (length(tree_items) == 0) {
        return(shiny::p("No document structure available"))
      }
      
      # Create the simple tree
      simple_tree = create_simple_tree(tree_items, selected_nodes, session$ns)
      
      shiny::div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #dee2e6;",
        simple_tree
      )
    })
    
    # Helper function to update node selection in state
    update_node_selection = function(node_index, add = TRUE) {
      current_state = state()
      current_q_id = current_state$current_question_id
      
      # If no questions exist, create a default one
      if (length(current_state$questions) == 0) {
        next_id = current_state$last_question_id + 1
        current_state$last_question_id = next_id
        
        delete_observer = create_delete_observer(next_id)
        new_question = list(
          id = next_id,
          name = paste("Question", next_id),
          selected_nodes = integer(0),
          strict = FALSE,
          delete_observer = delete_observer
        )
        current_state$questions = list(new_question)
        current_state$current_question_id = next_id
        state(current_state)
      }
      
      # Find current question
      question_matches = sapply(current_state$questions, function(q) q$id == current_q_id)
      question_idx = which(question_matches)
      if (length(question_idx) > 0) {
        current_question = current_state$questions[[question_idx]]
        current_selections = current_question$selected_nodes %||% integer(0)
        
        if (add) {
          # Add to selection if not already selected
          if (!node_index %in% current_selections) {
            current_selections = c(current_selections, node_index)
          }
        } else {
          # Remove from selection
          current_selections = setdiff(current_selections, node_index)
        }
        
        # Sort the selected nodes vector
        current_selections = sort(current_selections)
        
        # Update the question in state
        current_state$questions[[question_idx]]$selected_nodes = current_selections
        state(current_state)
      }
    }
    
    # Handle node action clicks
    shiny::observe({
      if (is.null(ast_nodes())) return()
      
      nodes = ast_nodes()
      tree_items = build_ast_tree_structure(ast())
      
      # Iterate through nodes to match tree UI creation
      for (i in seq_along(nodes)) {
        local({
          node_index = i  # Use direct node index to match tree UI
          
          # Handle "Select" button
          shiny::observeEvent(input[[paste0("select_", node_index)]], {
            current_state = state()
            current_q_id = current_state$current_question_id
            
            # Get current selections (update_node_selection will handle empty questions)
            current_selections = integer(0)
            if (length(current_state$questions) > 0) {
              question_matches = sapply(current_state$questions, function(q) q$id == current_q_id)
              question_idx = which(question_matches)
              if (length(question_idx) > 0) {
                current_question = current_state$questions[[question_idx]]
                current_selections = current_question$selected_nodes %||% integer(0)
              }
            }
            
            # Toggle selection
            if (node_index %in% current_selections) {
              update_node_selection(node_index, add = FALSE)
            } else {
              update_node_selection(node_index, add = TRUE)
            }
          })
          
          # Handle "Select +" button (toggle node + children)
          shiny::observeEvent(input[[paste0("select_children_", node_index)]], {
            # Find all children first to determine behavior
            children = find_node_children(tree_items, node_index)
            
            if (length(children) == 0) {
              # Leaf node: behave exactly like clicking the node text (simple toggle)
              current_state = state()
              current_q_id = current_state$current_question_id
              
              # Get current selections (update_node_selection will handle empty questions)
              current_selections = integer(0)
              if (length(current_state$questions) > 0) {
                question_matches = sapply(current_state$questions, function(q) q$id == current_q_id)
                question_idx = which(question_matches)
                if (length(question_idx) > 0) {
                  current_question = current_state$questions[[question_idx]]
                  current_selections = current_question$selected_nodes %||% integer(0)
                }
              }
              
              # Toggle selection - same logic as the select_ button
              if (node_index %in% current_selections) {
                update_node_selection(node_index, add = FALSE)
              } else {
                update_node_selection(node_index, add = TRUE)
              }
            } else {
              # Non-leaf node: use the complex toggle logic for node + children
              # Ensure we have at least one question
              update_node_selection(node_index, add = TRUE)  # This will create question if needed
              
              current_state = state()
              current_q_id = current_state$current_question_id
              
              # Find current question
              question_matches = sapply(current_state$questions, function(q) q$id == current_q_id)
              question_idx = which(question_matches)
              if (length(question_idx) > 0) {
                current_question = current_state$questions[[question_idx]]
                current_selections = current_question$selected_nodes %||% integer(0)
                
                # Find all children
                nodes_to_toggle = c(node_index, children)
                
                # Determine current state - check if any nodes in the group are unselected
                currently_unselected = setdiff(nodes_to_toggle, current_selections)
                
                if (length(currently_unselected) > 0) {
                  # If any nodes are unselected, select ALL nodes in the group
                  new_selections = unique(c(current_selections, nodes_to_toggle))
                } else {
                  # If all nodes are selected, unselect ALL nodes in the group
                  new_selections = setdiff(current_selections, nodes_to_toggle)
                }
                
                # Sort the selected nodes vector
                new_selections = sort(new_selections)
                
                # Update the question in state
                current_state$questions[[question_idx]]$selected_nodes = new_selections
                state(current_state)
              }
            }
          })
          
          # Handle "Preview" button
          shiny::observeEvent(input[[paste0("preview_", node_index)]], {
            # Use node_index directly
            if (node_index >= 1 && node_index <= length(nodes)) {
              node = nodes[[node_index]]
              
              # Use as_document() to get raw node content
              content = tryCatch({
                result = parsermd::as_document(node) |>
                  as.character() |>
                  paste(collapse="\n")
              }, error = function(e) {
                paste("Error:", e$message)
              })

              # Get node type for title
              node_type = class(node)[1]
              
              shiny::showModal(
                shiny::modalDialog(
                  title = shiny::div(
                    style = "display: flex; justify-content: space-between; align-items: center; margin: 0; padding: 0;",
                    shiny::span(node_type, style = "font-weight: bold;"),
                    shiny::tags$button(
                      type = "button",
                      class = "close",
                      `data-dismiss` = "modal",
                      `aria-label` = "Close",
                      style = "background: none; border: none; font-size: 24px; font-weight: bold; color: #000; opacity: 0.5; cursor: pointer;",
                      shiny::icon("times")
                    )
                  ),
                  size = "l",
                  shiny::div(
                    style = "max-height: 500px; overflow-y: auto;",
                    shiny::pre(
                      content,
                      style = "font-family: 'Courier New', Courier, monospace; font-size: 12px; white-space: pre-wrap; margin: 0; background: #f8f9fa; padding: 15px; border: 1px solid #e9ecef; border-radius: 3px; line-height: 1.4;"
                    )
                  ),
                  footer = NULL,
                  easyClose = TRUE
                )
              )
            }
          })
        })
      }
    })
    
    # Display questions
    output$questions_ui = shiny::renderUI({
      current_state = state()
      current_questions = current_state$questions
      current_q_id = current_state$current_question_id

      # Add question button - positioned in upper middle
      add_button = shiny::div(
        style = "text-align: center; margin-bottom: 15px;",
        shiny::actionButton(
          session$ns("add_question"), 
          shiny::icon("plus"),
          class = "btn-primary btn-sm",
          style = "font-size: 12px; padding: 4px 8px; border-radius: 50%; width: 30px; height: 30px;",
          title = "Add Question"
        )
      )
      
      if (length(current_questions) == 0) {
        return(shiny::div(
          add_button,
          shiny::p("No questions created yet.", style = "text-align: center; color: #6c757d; margin-top: 20px;")
        ))
      }
      
      question_items = lapply(current_questions, function(q) {
        selected_nodes = q$selected_nodes %||% integer(0)
        is_current = (q$id == current_q_id)
        
        border_color = if (is_current) "#007bff" else "#ccc"
        bg_color = if (is_current) "#f8f9fa" else "white"
        
        shiny::div(
          class = "question-item",
          style = paste0(
            "border: 2px solid ", border_color, "; ",
            "margin-bottom: 10px; padding: 10px; ",
            "background-color: ", bg_color, "; ",
            "cursor: pointer; position: relative;"
          ),
          onclick = paste0("Shiny.setInputValue('", session$ns("select_question"), "', ", q$id, ");"),
          
          # Delete button in upper right corner
          shiny::actionButton(
            session$ns(paste0("delete_question_", q$id)),
            shiny::icon("times"),
            class = "btn-danger btn-sm",
            style = "position: absolute; top: 5px; right: 5px; font-size: 12px; padding: 0; border-radius: 50%; width: 20px; height: 20px; z-index: 10; display: flex; align-items: center; justify-content: center; line-height: 1;",
            title = "Delete Question",
            onclick = "event.stopPropagation();"  # Prevent triggering the question selection
          ),
          
          shiny::div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; padding-right: 30px;",
            shiny::div(
              style = "flex-grow: 1; margin-right: 10px;",
              shiny::textInput(
                session$ns(paste0("question_name_", q$id)),
                NULL,
                value = q$name,
                placeholder = "Question name",
                width = "100%"
              )
            ),
            shiny::div(
              style = "flex-shrink: 0; margin-right: 10px;",
              onclick = "event.stopPropagation();",
              shiny::checkboxInput(
                session$ns(paste0("question_strict_", q$id)),
                "Strict",
                value = q$strict %||% FALSE,
                width = "auto"
              )
            ),
            shiny::div(
              style = "flex-shrink: 0; width: 80px; text-align: right;",
              if (is_current) shiny::span("← Current", class = "badge badge-primary") else shiny::span(style = "display: inline-block; width: 1px;")
            )
          ),
          
          shiny::p(
            shiny::strong("Selected nodes: "),
            if (length(selected_nodes) == 0) {
              shiny::span("None", style = "color: #6c757d;")
            } else {
              # Display nodes directly without offset
              shiny::span(paste(selected_nodes, collapse = ", "), style = "color: #28a745;")
            }
          )
        )
      })
      
      shiny::tagList(
        question_items,
        add_button
      )
    })
    
    # Add new question
    shiny::observeEvent(input$add_question, {
      current_state = state()
      current_questions = current_state$questions
      
      # Use and increment last_question_id from state
      next_id = current_state$last_question_id + 1
      current_state$last_question_id = next_id

      # Create delete observer for this question
      delete_observer = create_delete_observer(next_id)
      
      new_question = list(
        id = next_id,
        name = paste("Question", next_id),
        selected_nodes = integer(0),
        strict = FALSE,
        delete_observer = delete_observer
      )
      
      # Update state
      current_state$questions = c(current_questions, list(new_question))
      current_state$current_question_id = new_question$id
      state(current_state)
    })
    
    
    # Clear all selections
    shiny::observeEvent(input$clear_selections, {
      current_state = state()
      
      # Clear selected_nodes for all questions
      for (i in seq_along(current_state$questions)) {
        current_state$questions[[i]]$selected_nodes = integer(0)
      }
      
      state(current_state)
    })
    
    # Handle question selection
    shiny::observeEvent(input$select_question, {
      current_state = state()
      current_state$current_question_id = input$select_question
      state(current_state)
    })
    
    # Reactive value for the current save filename
    save_filename = shiny::reactiveVal("template")
    
    # Dynamic save button UI
    output$save_button_ui = shiny::renderUI({
      current_state = state()
      current_questions = current_state$questions
      
      if (length(current_questions) == 0) {
        # Show disabled button when no questions
        shiny::actionButton(
          session$ns("save_disabled"), 
          "Save Template", 
          class = "btn-secondary btn-sm", 
          style = "width: 100%;",
          disabled = TRUE
        )
      } else {
        # Show working download button
        shiny::downloadButton(
          session$ns("save_template"), 
          "Save Template", 
          class = "btn-success btn-sm", 
          style = "width: 100%;"
        )
      }
    })
    
    # Save template functionality using downloadHandler  
    output$save_template = shiny::downloadHandler(
      filename = function() {
        # Always prompt for filename when download starts
        timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("template_", timestamp, ".rds")
      },
      content = function(file) {
        current_state = state()
        current_questions = current_state$questions
        
        if (length(current_questions) == 0) {
          # Create empty template if no questions
          template_data = list(
            original_ast = ast(),
            questions = list(),
            metadata = list(
              created_at = Sys.time(),
              created_by = Sys.getenv("USER", "unknown"),
              total_nodes = if (!is.null(ast_nodes())) length(ast_nodes()) else 0
            )
          )
        } else {
          # Prepare questions data (remove delete_observer for saving)
          clean_questions = list()
          for (q in current_questions) {
            # Get current values from inputs
            name_value = input[[paste0("question_name_", q$id)]] %||% q$name
            strict_value = input[[paste0("question_strict_", q$id)]] %||% q$strict %||% FALSE
            
            # Use nodes directly without offset
            clean_q = list(
              id = q$id,
              name = name_value,
              selected_nodes = q$selected_nodes %||% integer(0),
              strict = strict_value
            )
            clean_questions = c(clean_questions, list(clean_q))
          }
          
          # Create template data structure
          template_data = list(
            original_ast = ast(),
            questions = clean_questions,
            metadata = list(
              created_at = Sys.time(),
              created_by = Sys.getenv("USER", "unknown"),
              total_nodes = if (!is.null(ast_nodes())) length(ast_nodes()) else 0
            )
          )
        }
        
        # Save as RDS file
        saveRDS(template_data, file)
      },
      contentType = "application/rds"
    )
    
    # Load template functionality
    shiny::observeEvent(input$load_template, {
      # Show file upload modal
      shiny::showModal(
        shiny::modalDialog(
          title = "Load Template",
          shiny::fileInput(
            session$ns("template_file"),
            "Select template file (.rds):",
            accept = c(".rds"),
            width = "100%"
          ),
          shiny::uiOutput(session$ns("load_button_ui")),
          footer = shiny::tagList(
            shiny::modalButton("Cancel")
          )
        )
      )
    })
    
    # Dynamic Load button that only appears when file is selected
    output$load_button_ui = shiny::renderUI({
      if (!is.null(input$template_file) && !is.null(input$template_file$datapath)) {
        shiny::div(
          style = "text-align: right; margin-top: 10px;",
          shiny::actionButton(session$ns("load_confirm"), "Load", class = "btn-primary")
        )
      } else {
        shiny::div(
          style = "text-align: center; margin-top: 10px; color: #6c757d;",
          "Please select a file to enable the Load button"
        )
      }
    })
    
    # Confirm template loading
    shiny::observeEvent(input$load_confirm, {
      file_info = input$template_file
      
      if (is.null(file_info) || is.null(file_info$datapath)) {
        shiny::showNotification("Please select a template file", type = "warning")
        return()
      }
      
      # Load the RDS file
      template_data = tryCatch({
        readRDS(file_info$datapath)
      }, error = function(e) {
        shiny::showNotification(paste("Error loading template:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(template_data)) {
        return()
      }
      
      # Validate template structure
      if (!is.list(template_data) || is.null(template_data$questions)) {
        shiny::showNotification("Invalid template format", type = "error")
        return()
      }
      
      current_state = state()
      loaded_questions = template_data$questions
      
      # Convert loaded questions to state structure with delete observers
      if (length(loaded_questions) > 0) {
        for (i in seq_along(loaded_questions)) {
          # Ensure selected_nodes exists and sort them
          if (is.null(loaded_questions[[i]]$selected_nodes)) {
            loaded_questions[[i]]$selected_nodes = integer(0)
          } else {
            # Use nodes directly without offset
            loaded_questions[[i]]$selected_nodes = sort(loaded_questions[[i]]$selected_nodes)
          }
          
          # Ensure strict field exists
          if (is.null(loaded_questions[[i]]$strict)) {
            loaded_questions[[i]]$strict = FALSE
          }
          
          # Create delete observer for this loaded question
          loaded_questions[[i]]$delete_observer = create_delete_observer(loaded_questions[[i]]$id)
        }
        
        # Update state
        current_state$questions = loaded_questions
        current_state$current_question_id = loaded_questions[[1]]$id
        
        # Set last_question_id to the highest ID among loaded questions
        max_id = max(sapply(loaded_questions, function(q) q$id))
        current_state$last_question_id = max_id
        
        shiny::showNotification(
          paste("Successfully loaded", length(loaded_questions), "questions"), 
          type = "message"
        )
      } else {
        # Empty template
        current_state$questions = list()
        current_state$current_question_id = 1
        current_state$last_question_id = 0
        
        shiny::showNotification("Loaded empty template", type = "message")
      }
      
      state(current_state)
      shiny::removeModal()
    })
    
    # Return reactive values for parent module
    return(list(
      state = state
    ))
  })
}