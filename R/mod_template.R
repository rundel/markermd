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
        
        new_question = list(
          id = next_id,
          name = paste("Question", next_id),
          selected_nodes = integer(0),
          delete_observer = create_delete_observer(next_id),
          rules = list()
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
                
                new_question = list(
                  id = next_id,
                  name = paste("Question", next_id),
                  selected_nodes = integer(0),
                  delete_observer = create_delete_observer(next_id),
                  rules = list()
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
                
                new_question = list(
                  id = next_id,
                  name = paste("Question", next_id),
                  selected_nodes = integer(0),
                  delete_observer = create_delete_observer(next_id),
                  rules = list()
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
              content = parsermd::as_document(node) |>
                as.character() |>
                paste(collapse="\n")

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
    
    # Reactive for tracking question structure changes (not content changes)
    questions_structure = shiny::reactive({
      list(
        question_ids = sapply(state()$questions, function(q) q$id),
        current_question_id = state()$current_question_id
      )
    })
    
    # Display questions
    output$questions_ui = shiny::renderUI({
      # Only react to structural changes, not content changes
      structure = questions_structure()
      current_q_id = structure$current_question_id

      # Add question button - positioned in upper middle
      add_button = shiny::div(
        style = "text-align: center; margin-bottom: 15px;",
        shiny::actionButton(
          session$ns("add_question"), 
          shiny::icon("plus"),
          class = "btn-primary btn-sm",
          style = "font-size: 12px; padding: 4px 8px; border-radius: 20%; width: 30px; height: 30px;",
          title = "Add Question"
        ),
        shiny::span("Add Question", style = "margin-left: 8px; font-size: 14px; color: #333;")
      )
      
      # Get current questions snapshot (not reactive)
      current_questions = isolate(state()$questions)
      
      if (length(current_questions) == 0) {
        return(shiny::div(
          add_button
        ))
      }
      
      # Get existing input values using isolate to prevent reactive dependency
      existing_values = list()
      for (q in current_questions) {
        existing_values[[as.character(q$id)]] = isolate( input[[ paste0("question_name_", q$id) ]] )
      }
      
      question_items = lapply(current_questions, function(q) {
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
                  style = "font-size: 12px; padding: 2px 6px; border-radius: 20%; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; line-height: 1;",
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
                tree_items = build_ast_tree_structure(isolate(ast()))
                
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
          
          # Rules card body
          bslib::card_body(
            style = "padding: 6px 12px;",
            shiny::div(
              # Rules header with add button on same line
              shiny::div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
                shiny::div(
                  shiny::strong("Validation rules: "),
                  # Check if this question has rules
                  if (is.null(question_rules()[[as.character(q$id)]]) || 
                      length(question_rules()[[as.character(q$id)]]()) == 0) {
                    shiny::span("None", style = "color: #6c757d;")
                  }
                ),
                shiny::div(
                  style = "display: flex; align-items: center;",
                  shiny::actionButton(
                    session$ns(paste0("add_rule_", q$id)),
                    shiny::icon("plus"),
                    class = "btn-outline-primary btn-sm",
                    style = "font-size: 10px; padding: 2px 6px;",
                    title = "Add Rule"
                  ),
                  shiny::span("Add Rule", style = "margin-left: 6px; font-size: 12px;")
                )
              ),
              
              # Rules content (always visible)
              shiny::div(
                rules_ui(session$ns(paste0("rules_", q$id)))
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

      new_question = list(
        id = next_id,
        name = paste("Question", next_id),
        selected_nodes = integer(0),
        delete_observer = create_delete_observer(next_id),
        rules = list()
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
    
    # Helper function to capture a single question and build S7 question from current state
    capture_question = function(question_data) {
      isolate({
        # Get question name from input
        question_name = input[[paste0("question_name_", question_data$id)]] %||% paste("Question", question_data$id)
        
        # Get current rules from the rules server for this question
        q_rules_list = list()
        if (!is.null(question_rules()[[as.character(question_data$id)]])) {
          rules_server_fn = question_rules()[[as.character(question_data$id)]]
          current_rules = rules_server_fn()
          
          # Convert each rule from list format to S7 format
          for (rule in current_rules) {
            if (!is.null(rule$node_types) && !is.null(rule$verb) && !is.null(rule$values)) {
              # Create S7 rule from the rule's stored values
              s7_rule = markermd_rule(
                node_type = rule$node_types,
                verb = rule$verb,
                values = rule$values
              )
              q_rules_list = c(q_rules_list, list(s7_rule))
            }
          }
        }
        
        # Create and return S7 question object
        return(markermd_question(
          id = as.integer(question_data$id),
          name = question_name,
          selected_nodes = markermd_node_selection(indices = as.integer(question_data$selected_nodes %||% integer(0))),
          rules = q_rules_list
        ))
      })
    }
    
    # Helper function to capture all questions and build list of S7 questions from input values
    capture_questions = function() {
        current_questions = isolate(state()$questions)
      
        return(lapply(current_questions, capture_question))
    }
    
    # Save template functionality using downloadHandler  
    output$save_template = shiny::downloadHandler(
      filename = function() {
        # Always prompt for filename when download starts
        timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("template_", timestamp, ".rds")
      },
      content = function(file) {
        isolate({
          current_questions = state()$questions
          
          if (length(current_questions) == 0) {
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
            # Use capture_questions to get all questions
            s7_questions = capture_questions()
            
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
        })
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
          
          # Convert S7 rules to list format for the rules server (but keep S7 in state)
          initial_rules_list = if (!is.null(q$rules) && length(q$rules) > 0) {
            lapply(seq_along(q$rules), function(i) {
              rule_to_list(q$rules[[i]], rule_id = i)
            })
          } else {
            NULL
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