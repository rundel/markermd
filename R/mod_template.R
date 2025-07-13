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
    shiny::fluidRow(
      style = "height: 100%;",
      shiny::column(
        width = 6,
        style = "height: 100%; display: flex; flex-direction: column;",
        shiny::h3("Document Structure", style = "flex-shrink: 0;"),
        shiny::div(
          id = ns("ast_tree_container"),
          style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; flex: 1; overflow-y: auto; min-height: 400px;",
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          shiny::helpText("Use ", shiny::icon("check"), " to toggle selection, ", shiny::icon("check-double"), " to toggle with children, ", shiny::icon("search"), " to preview content"),
          shiny::actionButton(ns("clear_selections"), "Clear Selections", class = "btn-secondary btn-sm")
        ),
        shiny::uiOutput(ns("ast_tree_ui"))
      )
      ),
      shiny::column(
        width = 6,
        style = "height: 100%; display: flex; flex-direction: column;",
        shiny::h3("Questions", style = "flex-shrink: 0;"),
        shiny::div(
          id = ns("questions_container"),
          style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; flex: 1; overflow-y: auto; min-height: 300px; margin-bottom: 10px;",
          shiny::uiOutput(ns("questions_ui"))
        ),
        shiny::div(
          style = "flex-shrink: 0; margin-top: 10px;",
          shiny::fluidRow(
            shiny::column(4, shiny::actionButton(ns("save_template"), "Save Template", class = "btn-success btn-sm", style = "width: 100%;")),
            shiny::column(4, shiny::actionButton(ns("load_template"), "Load Template", class = "btn-warning btn-sm", style = "width: 100%;")),
            shiny::column(4, shiny::actionButton(ns("preview_template"), "Preview", class = "btn-info btn-sm", style = "width: 100%;"))
          )
        ),
        shiny::div(
          id = ns("validation_results"),
          style = "flex-shrink: 0; margin-top: 10px;",
          shiny::uiOutput(ns("validation_ui"))
        )
      )
    )
  )
}

#' Template Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#'
template_server = function(id, ast) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Centralized state management
    state = shiny::reactiveVal(list(
      questions = list(),           # List of question objects: list(id=1, name="Question 1", selected_nodes=c(...))
      current_question_id = 1,      # Currently active question ID
      last_question_id = 0          # Last created question ID (incremented for each new question)
    ))
    
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
      
      for (i in seq_along(nodes)) {
        local({
          node_index = i + 1  # Adjust for AST root at index 1
          
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
              children = find_node_children(tree_items, node_index)
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
          })
          
          # Handle "Preview" button
          shiny::observeEvent(input[[paste0("preview_", node_index)]], {
            # node_index = i + 1, so original i = node_index - 1
            original_i = node_index - 1
            if (original_i >= 1 && original_i <= length(nodes)) {
              node = nodes[[original_i]]  # Use calculated original_i for nodes access
              
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
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            shiny::textInput(
              session$ns(paste0("question_name_", q$id)),
              NULL,
              value = q$name,
              placeholder = "Question name",
              width = "100%"
            ),
            if (is_current) shiny::span("← Current", class = "badge badge-primary", style = "margin-left: 10px;") else NULL
          ),
          
          shiny::p(
            shiny::strong("Selected nodes: "),
            if (length(selected_nodes) == 0) {
              shiny::span("None", style = "color: #6c757d;")
            } else {
              # Subtract 1 from each index to account for AST root node not being selectable
              display_nodes = selected_nodes - 1
              shiny::span(paste(display_nodes, collapse = ", "), style = "color: #28a745;")
            }
          )
        )
      })
      
      shiny::tagList(
        question_items,
        add_button
      )
    })
    
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
    
    
    # Validation display
    output$validation_ui = shiny::renderUI({
      current_state = state()
      current_questions = current_state$questions
      nodes = ast_nodes()
      
      if (length(current_questions) == 0 || is.null(nodes)) {
        return(NULL)
      }
      
      # Convert questions to old format for validation function
      # TODO: Update validate_template to work directly with new state structure
      selections = list()
      for (q in current_questions) {
        selections[[as.character(q$id)]] = q$selected_nodes %||% integer(0)
      }
      
      validation = validate_template(current_questions, selections, length(nodes))
      
      if (validation$valid) {
        status_class = "alert-success"
        status_icon = "check-circle"
        status_text = "Template is valid"
      } else {
        status_class = "alert-danger" 
        status_icon = "exclamation-triangle"
        status_text = "Template has issues"
      }
      
      content = list(
        shiny::div(
          class = paste("alert", status_class),
          style = "padding: 8px; margin-top: 10px;",
          shiny::icon(status_icon), " ",
          status_text, " (", validation$covered_nodes, "/", validation$total_nodes, " nodes, ",
          validation$coverage_pct, "% coverage)"
        )
      )
      
      if (length(validation$issues) > 0) {
        content = c(content, list(
          shiny::div(
            class = "alert alert-danger",
            style = "padding: 8px; margin-top: 5px;",
            shiny::strong("Issues:"),
            shiny::tags$ul(lapply(validation$issues, shiny::tags$li))
          )
        ))
      }
      
      if (length(validation$warnings) > 0) {
        content = c(content, list(
          shiny::div(
            class = "alert alert-warning",
            style = "padding: 8px; margin-top: 5px;",
            shiny::strong("Warnings:"),
            shiny::tags$ul(lapply(validation$warnings, shiny::tags$li))
          )
        ))
      }
      
      shiny::tagList(content)
    })
    
    # Save template functionality
    shiny::observeEvent(input$save_template, {
      current_state = state()
      current_questions = current_state$questions
      
      if (length(current_questions) == 0) {
        return()
      }
      
      # Create a unique filename
      timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
      filename = paste0("template_", timestamp, ".json")
      file_path = file.path(tempdir(), filename)
      
      # Prepare metadata
      metadata = list(
        document_path = "unknown", # Could be passed from parent
        total_nodes = if (!is.null(ast_nodes())) length(ast_nodes()) else 0,
        created_by = Sys.getenv("USER", "unknown")
      )
      
      # Convert questions to old format for save function
      # TODO: Update save_template to work directly with new state structure
      selections = list()
      clean_questions = list()
      for (q in current_questions) {
        selections[[as.character(q$id)]] = q$selected_nodes %||% integer(0)
        # Remove delete_observer from question for saving
        clean_q = list(
          id = q$id,
          name = q$name
        )
        clean_questions = c(clean_questions, list(clean_q))
      }
      
      # Save template
      success = save_template(clean_questions, selections, file_path, metadata)
      
      # Template save completed
    })
    
    # Load template functionality
    shiny::observeEvent(input$load_template, {
      # For now, just show a file path input modal
      shiny::showModal(
        shiny::modalDialog(
          title = "Load Template",
          shiny::textInput(
            session$ns("template_path"),
            "Template file path:",
            placeholder = "/path/to/template.json"
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(session$ns("load_confirm"), "Load", class = "btn-primary")
          )
        )
      )
    })
    
    # Confirm template loading
    shiny::observeEvent(input$load_confirm, {
      file_path = input$template_path
      
      if (is.null(file_path) || trimws(file_path) == "") {
        return()
      }
      
      template_data = load_template(file_path)
      
      if (is.null(template_data)) {
        return()
      }
      
      # Convert loaded data to new state structure
      # TODO: Update load_template to work directly with new state structure
      current_state = state()
      loaded_questions = template_data$questions
      
      # Add selected_nodes and delete_observers to each question from node_selections
      for (i in seq_along(loaded_questions)) {
        q_id = as.character(loaded_questions[[i]]$id)
        loaded_selections = template_data$node_selections[[q_id]] %||% integer(0)
        # Sort the loaded selections in case they weren't sorted when saved
        loaded_questions[[i]]$selected_nodes = sort(loaded_selections)
        # Create delete observer for this loaded question
        loaded_questions[[i]]$delete_observer = create_delete_observer(loaded_questions[[i]]$id)
      }
      
      # Update state
      current_state$questions = loaded_questions
      if (length(loaded_questions) > 0) {
        current_state$current_question_id = loaded_questions[[1]]$id
        # Set last_question_id to the highest ID among loaded questions
        max_id = max(sapply(loaded_questions, function(q) q$id))
        current_state$last_question_id = max_id
      }
      state(current_state)
      
      shiny::removeModal()
      # Template loaded successfully
    })
    
    # Preview template functionality  
    shiny::observeEvent(input$preview_template, {
      current_state = state()
      current_questions = current_state$questions
      nodes = ast_nodes()
      
      if (length(current_questions) == 0) {
        return()
      }
      
      # Convert questions to old format for preview function
      # TODO: Update generate_template_summary to work directly with new state structure
      selections = list()
      for (q in current_questions) {
        selections[[as.character(q$id)]] = q$selected_nodes %||% integer(0)
      }
      
      # Generate template summary
      summary_lines = generate_template_summary(current_questions, selections, nodes)
      summary_text = paste(summary_lines, collapse = "\n")
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Template Preview",
          size = "l",
          shiny::div(
            style = "max-height: 400px; overflow-y: auto;",
            shiny::pre(summary_text)
          ),
          footer = shiny::modalButton("Close")
        )
      )
    })
    
    # Return reactive values for parent module
    return(list(
      state = state
    ))
  })
}