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
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #dee2e6; flex: 1; overflow-y: auto;",
          shiny::uiOutput(ns("ast_tree_ui"))
        ),
        shiny::div(
          style = "flex-shrink: 0; margin-top: 10px; margin-bottom: 10px; height: 50px; display: flex; align-items: center; justify-content: center;",
          shiny::actionButton(ns("clear_selections"), "Clear Current Question", class = "btn-secondary btn-sm")
        )
      ),
      shiny::div(
        style = "height: 100%; display: flex; flex-direction: column;",
        shiny::h3("Questions", style = "flex-shrink: 0;"),
        shiny::div(
          id = ns("questions_container"),
          style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; flex: 1; overflow-y: auto;",
          shiny::uiOutput(ns("questions_ui"))
        ),
        shiny::div(
          style = "flex-shrink: 0; margin-top: 10px; margin-bottom: 10px; height: 50px; display: flex; align-items: center; justify-content: center;",
          shiny::uiOutput(ns("save_button_ui"))
        )
      )
    )
  )
}

#' Template Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param template_obj markermd_template S7 object. Optional template to load on startup
#'
template_server = function(id, ast, template_obj = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values for add rule triggers  
    add_rule_triggers = shiny::reactiveValues()
    
    # Centralized state management
    state = shiny::reactiveVal(list(
      questions = list(),           # List of question objects: list(id=1, selected_nodes=c(...), rules=...)
      current_question_id = 1,      # Currently active question ID
      last_question_id = 0          # Last created question ID (incremented for each new question)
    ))
    
    # Helper function to create delete observer for a question
    create_delete_observer = function(question_id) {
      shiny::observeEvent(input[[paste0("delete_question_", question_id)]], {
        # Find and remove this question
        question_to_remove_idx = which(sapply(state()$questions, function(q) q$id == question_id))
        if (length(question_to_remove_idx) > 0) {
          # Destroy the observer before removing the question
          if (!is.null(state()$questions[[question_to_remove_idx]]$delete_observer)) {
            state()$questions[[question_to_remove_idx]]$delete_observer$destroy()
          }
          
          # Remove the question
          updated_questions = state()$questions[-question_to_remove_idx]
          
          # Update current question if needed
          updated_state = state()
          if (updated_state$current_question_id == question_id) {
            if (length(updated_questions) > 0) {
              updated_state$current_question_id = updated_questions[[1]]$id
            } else {
              updated_state$current_question_id = 1
            }
          }
          
          # Update state
          updated_state$questions = updated_questions
          state(updated_state)
        }
      })
    }
    
    # Load template on startup if provided (use observeEvent to run only once)
    template_loaded = shiny::reactiveVal(FALSE)
    
    shiny::observeEvent(template_obj, {
      if (!is.null(template_obj) && !template_loaded()) {
        # Convert S7 template to state structure (names handled separately)
        loaded_questions = lapply(template_obj@questions, function(q) {
          list(
            id = q@id,
            selected_nodes = sort(q@selected_nodes@indices),
            delete_observer = create_delete_observer(q@id),
            rules = q@rules  # Store S7 rules for later restoration
          )
        })
        
        # Update state if we have questions
        if (length(loaded_questions) > 0) {
          updated_state = state()
          updated_state$questions = loaded_questions
          updated_state$current_question_id = loaded_questions[[1]]$id
          
          # Set last_question_id to the highest ID among loaded questions
          max_id = max(sapply(loaded_questions, function(q) q$id))
          updated_state$last_question_id = max_id
          
          state(updated_state)
          
          # Initialize text inputs with loaded question names
          for (i in seq_along(template_obj@questions)) {
            q = template_obj@questions[[i]]
            shiny::updateTextInput(
              session = session,
              inputId = paste0("question_name_", q@id),
              value = q@name
            )
          }
          
          shiny::showNotification(
            paste("Successfully loaded template with", length(loaded_questions), "questions"), 
            type = "message"
          )
        } else {
          # Empty template
          shiny::showNotification("Loaded empty template", type = "message")
        }
        
        # Mark template as loaded to prevent re-loading
        template_loaded(TRUE)
      }
    }, once = TRUE, ignoreNULL = TRUE)
    
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
      
      current_q_id = state()$current_question_id
      
      # Get selected nodes for current question
      selected_nodes = integer(0)
      if (length(state()$questions) > 0) {
        question_matches = sapply(state()$questions, function(q) q$id == current_q_id)
        question_idx = which(question_matches)
        if (length(question_idx) > 0) {
          current_question = state()$questions[[question_idx]]
          selected_nodes = current_question$selected_nodes %||% integer(0)
        }
      }
      
      # Build tree structure
      tree_items = build_ast_tree_structure(ast())
      
      if (length(tree_items) == 0) {
        return(shiny::p("No document structure available"))
      }
      
      # Create the simple tree
      create_simple_tree(tree_items, selected_nodes, session$ns)
    })
    
    # Helper function to update node selection in state
    update_node_selection = function(node_index, add = TRUE) {
      current_q_id = state()$current_question_id
      
      # If no questions exist, create a default one
      if (length(state()$questions) == 0) {
        updated_state = state()
        next_id = updated_state$last_question_id + 1
        updated_state$last_question_id = next_id
        
        delete_observer = create_delete_observer(next_id)
        new_question = list(
          id = next_id,
          name = paste("Question", next_id),
          selected_nodes = integer(0),
          delete_observer = delete_observer
        )
        updated_state$questions = list(new_question)
        updated_state$current_question_id = next_id
        state(updated_state)
      }
      
      # Find current question
      question_matches = sapply(state()$questions, function(q) q$id == current_q_id)
      question_idx = which(question_matches)
      if (length(question_idx) > 0) {
        current_question = state()$questions[[question_idx]]
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
        updated_state = state()
        updated_state$questions[[question_idx]]$selected_nodes = current_selections
        state(updated_state)
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
          node = ast_nodes()[[node_index]]
          node_type = class(node)[1]
          
          # Only create selection observers for heading nodes that don't have selected ancestors
          if (node_type == "rmd_heading") {
            # Handle "Select" button - only toggle directly selected nodes
            shiny::observeEvent(input[[paste0("select_", node_index)]], {
              # Check if this node is selectable (no selected ancestors)
              current_q_id = state()$current_question_id
              
              # Find current question or create one if needed
              if (length(state()$questions) == 0) {
                updated_state = state()
                next_id = updated_state$last_question_id + 1
                updated_state$last_question_id = next_id
                
                delete_observer = create_delete_observer(next_id)
                new_question = list(
                  id = next_id,
                  name = paste("Question", next_id),
                  selected_nodes = integer(0),
                  delete_observer = delete_observer
                )
                updated_state$questions = list(new_question)
                updated_state$current_question_id = next_id
                state(updated_state)
              }
              
              question_matches = sapply(state()$questions, function(q) q$id == current_q_id)
              question_idx = which(question_matches)
              if (length(question_idx) > 0) {
                current_question = state()$questions[[question_idx]]
                current_selected = current_question$selected_nodes %||% integer(0)
                
                # Check if node has selected ancestors - if so, don't allow selection
                if (has_selected_ancestor(tree_items, node_index, current_selected)) {
                  return()  # Silently ignore click
                }
                
                # Toggle this node in selected list
                if (node_index %in% current_selected) {
                  # Remove from selected
                  new_selected = setdiff(current_selected, node_index)
                } else {
                  # Add to selected, but first remove any descendants that are currently selected
                  descendants_to_remove = find_selected_descendants(tree_items, node_index, current_selected)
                  new_selected = setdiff(current_selected, descendants_to_remove)
                  new_selected = c(new_selected, node_index)
                }
                new_selected = sort(new_selected)
                
                # Update the question in state
                updated_state = state()
                updated_state$questions[[question_idx]]$selected_nodes = new_selected
                state(updated_state)
              }
            })
            
            # Handle "Select +" button - same as text click
            shiny::observeEvent(input[[paste0("select_children_", node_index)]], {
              # Check if this node is selectable (no selected ancestors)
              current_q_id = state()$current_question_id
              
              # Find current question or create one if needed
              if (length(state()$questions) == 0) {
                updated_state = state()
                next_id = updated_state$last_question_id + 1
                updated_state$last_question_id = next_id
                
                delete_observer = create_delete_observer(next_id)
                new_question = list(
                  id = next_id,
                  name = paste("Question", next_id),
                  selected_nodes = integer(0),
                  delete_observer = delete_observer
                )
                updated_state$questions = list(new_question)
                updated_state$current_question_id = next_id
                state(updated_state)
              }
              
              question_matches = sapply(state()$questions, function(q) q$id == current_q_id)
              question_idx = which(question_matches)
              if (length(question_idx) > 0) {
                current_question = state()$questions[[question_idx]]
                current_selected = current_question$selected_nodes %||% integer(0)
                
                # Check if node has selected ancestors - if so, don't allow selection
                if (has_selected_ancestor(tree_items, node_index, current_selected)) {
                  return()  # Silently ignore click
                }
                
                # Toggle this node in selected list
                if (node_index %in% current_selected) {
                  # Remove from selected
                  new_selected = setdiff(current_selected, node_index)
                } else {
                  # Add to selected, but first remove any descendants that are currently selected
                  descendants_to_remove = find_selected_descendants(tree_items, node_index, current_selected)
                  new_selected = setdiff(current_selected, descendants_to_remove)
                  new_selected = c(new_selected, node_index)
                }
                new_selected = sort(new_selected)
                
                # Update the question in state
                updated_state = state()
                updated_state$questions[[question_idx]]$selected_nodes = new_selected
                state(updated_state)
              }
            })
          }
          
          # Handle "Preview" button (available for all nodes)
          shiny::observeEvent(input[[paste0("preview_", node_index)]], {
            # Use node_index directly
            if (node_index >= 1 && node_index <= length(ast_nodes())) {
              node = ast_nodes()[[node_index]]
              
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
      current_q_id = state()$current_question_id

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
      
      if (length(state()$questions) == 0) {
        return(shiny::div(
          add_button,
          shiny::p("No questions created yet.", style = "text-align: center; color: #6c757d; margin-top: 20px;")
        ))
      }
      
      # Preserve existing input values before re-rendering
      existing_values = list()
      for (q in state()$questions) {
        input_id = paste0("question_name_", q$id)
        existing_values[[as.character(q$id)]] = input[[input_id]]
      }
      
      question_items = lapply(state()$questions, function(q) {
        selected_nodes = q$selected_nodes %||% integer(0)
        is_current = (q$id == current_q_id)
        
        # Use existing input value if available, otherwise use default
        question_name_value = existing_values[[as.character(q$id)]] %||% paste("Question", q$id)
        
        # Create card class with current question styling
        card_class = if (is_current) "border-primary" else ""
        
        bslib::card(
          class = card_class,
          style = if (is_current) "border-color: #007bff; border-width: 2px;" else "",
          onclick = paste0("Shiny.setInputValue('", session$ns("select_question"), "', ", q$id, ");"),
          
          # Card header with question name and delete button
          bslib::card_header(
            style = "position: relative; cursor: pointer;",
            shiny::div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              
              # Question name input
              shiny::div(
                style = "flex-grow: 1; margin-right: 10px;",
                shiny::textInput(
                  session$ns(paste0("question_name_", q$id)),
                  NULL,
                  value = question_name_value,
                  width = "100%"
                )
              ),
              
              # Current indicator and delete button
              shiny::div(
                style = "flex-shrink: 0; display: flex; align-items: center; gap: 10px;",
                if (is_current) shiny::span("← Current", class = "badge badge-primary") else NULL,
                shiny::actionButton(
                  session$ns(paste0("delete_question_", q$id)),
                  shiny::icon("times"),
                  class = "btn-danger btn-sm",
                  style = "font-size: 12px; padding: 2px 6px; border-radius: 50%; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; line-height: 1;",
                  title = "Delete Question",
                  onclick = "event.stopPropagation();"
                )
              )
            )
          ),
          
          # Selected nodes card body
          bslib::card_body(
            style = "padding: 6px 12px;",
            shiny::p(
              shiny::strong("Selected nodes: "),
              if (length(selected_nodes) == 0) {
                shiny::span("None", style = "color: #6c757d;")
              } else {
                # Build tree structure to compute children
                tree_items = build_ast_tree_structure(ast())
                
                # Create display format: parent [child1,child2,child3]
                node_displays = sapply(selected_nodes, function(node_index) {
                  children = find_all_descendants(tree_items, node_index)
                  if (length(children) > 0) {
                    paste0(node_index, " [", paste(children, collapse = ","), "]")
                  } else {
                    as.character(node_index)
                  }
                })
                
                shiny::span(paste(node_displays, collapse = ", "), style = "color: #28a745;")
              }
            )
          ),
          
          # Rules collapsible card body
          bslib::card_body(
            style = "padding: 6px 12px;",
            shiny::tagList(
              # CSS for chevron rotation
              shiny::tags$style(shiny::HTML(glue::glue("
                #<<session$ns(paste0('rules_header_', q$id))>>.expanded .fa-chevron-down {
                  transform: rotate(180deg);
                }
              ", .open = "<<", .close = ">>"))),
              
              shiny::div(
                # Rules header with toggle
                shiny::div(
                  id = session$ns(paste0("rules_header_", q$id)),
                  style = "cursor: pointer; display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px;",
                  onclick = glue::glue("
                    var content = document.getElementById('<<session$ns(paste0('rules_content_', q$id))>>');
                    var header = document.getElementById('<<session$ns(paste0('rules_header_', q$id))>>');
                    if (content.style.display === 'none' || content.style.display === '') {
                      content.style.display = 'block';
                      header.classList.add('expanded');
                    } else {
                      content.style.display = 'none';
                      header.classList.remove('expanded');
                    }
                  ", .open = "<<", .close = ">>"),
                  shiny::strong("Validaton rules:"),
                  shiny::icon("chevron-down", style = "transition: transform 0.3s;")
                ),
                
                # Rules content (initially hidden)
                shiny::div(
                  id = session$ns(paste0("rules_content_", q$id)),
                  style = "display: block;",
                  rules_ui(session$ns(paste0("rules_", q$id))),
                  
                  # Add rule button (inside collapsible content)
                  shiny::div(
                    style = "margin-top: 10px; text-align: center;",
                    shiny::actionButton(
                      session$ns(paste0("add_rule_", q$id)),
                      shiny::icon("plus"),
                      class = "btn-success btn-sm",
                      style = "font-size: 10px; padding: 2px 6px;",
                      title = "Add Rule"
                    ),
                    shiny::span("Add Rule", style = "margin-left: 6px; font-size: 12px;")
                  )
                )
              )
            )
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
      updated_state = state()
      
      # Use and increment last_question_id from state
      next_id = updated_state$last_question_id + 1
      updated_state$last_question_id = next_id

      # Create delete observer for this question
      delete_observer = create_delete_observer(next_id)
      
      new_question = list(
        id = next_id,
        name = paste("Question", next_id),
        selected_nodes = integer(0),
        delete_observer = delete_observer
      )
      
      # Update state
      updated_state$questions = c(updated_state$questions, list(new_question))
      updated_state$current_question_id = new_question$id
      state(updated_state)
    })
    
    
    # Clear selections for current question only
    shiny::observeEvent(input$clear_selections, {
      current_q_id = state()$current_question_id
      
      # Find and clear selected_nodes for current question only
      if (length(state()$questions) > 0) {
        question_matches = sapply(state()$questions, function(q) q$id == current_q_id)
        question_idx = which(question_matches)
        if (length(question_idx) > 0) {
          updated_state = state()
          updated_state$questions[[question_idx]]$selected_nodes = integer(0)
          state(updated_state)
        }
      }
    })
    
    # Handle question selection
    shiny::observeEvent(input$select_question, {
      updated_state = state()
      updated_state$current_question_id = input$select_question
      state(updated_state)
    })
    
    # Reactive value for the current save filename
    save_filename = shiny::reactiveVal("template")
    
    # Dynamic save button UI
    output$save_button_ui = shiny::renderUI({
      if (length(state()$questions) == 0) {
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
        if (length(state()$questions) == 0) {
          template_s7 = markermd_template(
            original_ast = ast(),
            questions = list(),
            metadata = markermd_metadata(
              created_at = Sys.time(),
              created_by = Sys.getenv("USER", "unknown"),
              total_nodes = if (!is.null(ast_nodes())) length(ast_nodes()) else 0L
            )
          )
        } else {
          s7_questions = list()
          for (q in state()$questions) {
            name_value = input[[paste0("question_name_", q$id)]] %||% paste("Question", q$id)
            
            # Get rules for this question from rules servers
            q_rules_list = list()
            
            # Convert rules to S7 objects if they exist
            if (!is.null(question_rules()[[as.character(q$id)]])) {
              current_q_rules = question_rules()[[as.character(q$id)]]
              if (length(current_q_rules) > 0) {
                q_rules_list = tryCatch({
                  lapply(current_q_rules, function(rule) {
                    list_to_rule(rule)
                  })
                }, error = function(e) {
                  # Log warning but continue - save template without problematic rules
                  warning("Failed to convert rules for question ", q$id, ": ", e$message)
                  list()
                })
              }
            }
            
            s7_q = markermd_question(
              id = as.integer(q$id),
              name = name_value,
              selected_nodes = markermd_node_selection(indices = as.integer(q$selected_nodes %||% integer(0))),
              rules = q_rules_list
            )
            s7_questions = c(s7_questions, list(s7_q))
          }
          
          template_s7 = markermd_template(
            original_ast = ast(),
            questions = s7_questions,
            metadata = markermd_metadata(
              created_at = Sys.time(),
              created_by = Sys.getenv("USER", "unknown"),
              total_nodes = if (!is.null(ast_nodes())) length(ast_nodes()) else 0L
            )
          )
        }
        
        saveRDS(template_s7, file)
      },
      contentType = "application/rds"
    )
    
    # Initialize rules servers for questions
    question_rules = shiny::reactiveVal(list())
    
    # Function to create rules server for a question
    create_rules_server_for_question = function(q_id, initial_rules = NULL) {
      # Initialize trigger for this question
      add_rule_triggers[[q_id]] = 0
      
      # Create a local reactive for this specific question
      local_trigger = shiny::reactive({
        add_rule_triggers[[q_id]]
      })
      
      updated_q_rules = question_rules()
      updated_q_rules[[q_id]] = rules_server(
        paste0("rules_", q_id),
        shiny::reactive(as.numeric(q_id)),
        add_rule_trigger = local_trigger,
        initial_rules = initial_rules
      )
      question_rules(updated_q_rules)
      
      # Create observer for the add rule button
      shiny::observeEvent(input[[paste0("add_rule_", q_id)]], {
        add_rule_triggers[[q_id]] = add_rule_triggers[[q_id]] + 1
      })
    }
    
    # Create rules servers when questions are added (only for new questions)
    last_question_count = shiny::reactiveVal(0)
    shiny::observe({
      current_count = length(state()$questions)
      
      if (current_count > last_question_count()) {
        # New questions were added
        new_questions = state()$questions[(last_question_count() + 1):current_count]
        for (q in new_questions) {
          q_id = as.character(q$id)
          
          # Convert S7 rules to list format if they exist
          initial_rules_list = NULL
          if (!is.null(q$rules) && length(q$rules) > 0) {
            initial_rules_list = tryCatch({
              lapply(seq_along(q$rules), function(i) {
                rule_to_list(q$rules[[i]], rule_id = i)
              })
            }, error = function(e) {
              # Log warning but continue - don't fail template loading due to rule issues
              warning("Failed to convert rules for question ", q$id, ": ", e$message)
              NULL
            })
          }
          
          create_rules_server_for_question(q_id, initial_rules = initial_rules_list)
        }
      }
      
      last_question_count(current_count)
    })
    
    # Return reactive values for parent module
    return(list(
      state = state,
      question_rules = question_rules
    ))
  })
}