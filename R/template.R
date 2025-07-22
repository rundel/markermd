#' Template App
#'
#' Main Shiny application for creating assignment templates

#' Template App UI
#'
#' @param ast Reactive. The parsed AST object
#' @param template_obj markermd_template S7 object. Optional template to load on startup
#'
template_app = function(ast, template_obj = NULL) {
  
  # UI
  ui = shiny::div(
    # Initialize shinyjs
    shinyjs::useShinyjs(),
    style = "height: calc(100vh - 150px); min-height: 600px;",
    bslib::layout_columns(
      col_widths = c(6, 6),
      style = "height: 100%;",
      shiny::div(
        style = "height: 100%; display: flex; flex-direction: column;",
        shiny::h3("Document Structure", style = "flex-shrink: 0;"),
        shiny::div(
          id = "ast_tree_container",
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #dee2e6; flex: 1; overflow-y: auto;",
          shiny::uiOutput("ast_tree_ui")
        ),
        shiny::div(
          style = "flex-shrink: 0; margin-top: 10px; margin-bottom: 10px; height: 50px; display: flex; align-items: center; justify-content: center;",
          shiny::actionButton("clear_selections", "Clear Current Question", class = "btn-secondary btn-sm")
        )
      ),
      shiny::div(
        style = "height: 100%; display: flex; flex-direction: column;",
        shiny::h3("Questions", style = "flex-shrink: 0;"),
        shiny::div(
          id = "questions_container",
          style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; flex: 1; overflow-y: auto;",
          shiny::uiOutput("questions_ui")
        ),
        shiny::div(
          style = "flex-shrink: 0; margin-top: 10px; margin-bottom: 10px; height: 50px; display: flex; align-items: center; justify-content: center;",
          shiny::uiOutput("save_button_ui")
        )
      )
    )
  )
  
  # Server
  server = function(input, output, session) {
    
    # Global state management
    current_question_id = shiny::reactiveVal(1)
    last_question_id = shiny::reactiveVal(0)
    
    # Question modules storage
    question_modules = shiny::reactiveVal(list())
    
    # Load template on startup if provided (disabled for now)
    # template_loaded = shiny::reactiveVal(FALSE)
    # 
    # shiny::observeEvent(template_obj, {
    #   if (!is.null(template_obj) && !template_loaded()) {
    #     # TODO: Implement template loading
    #     template_loaded(TRUE)
    #   }
    # }, once = TRUE, ignoreNULL = TRUE)
    
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
      
      current_q_id = current_question_id()
      
      # Get selected nodes for current question
      selected_nodes = integer(0)
      modules_list = question_modules()
      if (length(modules_list) > 0 && !is.null(modules_list[[as.character(current_q_id)]])) {
        current_module = modules_list[[as.character(current_q_id)]]
        if (!is.null(current_module$server)) {
          nodes_result = current_module$server$get_selected_nodes()
          if (!is.null(nodes_result)) {
            selected_nodes = nodes_result
          }
        }
      }
      
      # Build tree structure
      tree_items = build_ast_tree_structure(ast())
      
      if (length(tree_items) == 0) {
        return(shiny::p("No document structure available"))
      }
      
      # Create the simple tree
      create_simple_tree(tree_items, selected_nodes, identity)
    })
    
    # Handle node action clicks
    shiny::observe({
      if (is.null(ast_nodes())) return()
      
      nodes = ast_nodes()
      tree_items = build_ast_tree_structure(ast())
      
      # Iterate through nodes to match tree UI creation
      for (i in seq_along(nodes)) {
        local({
          node_index = i
          node = ast_nodes()[[node_index]]
          node_type = class(node)[1]
          
          # Only create selection observers for heading nodes
          if (node_type == "rmd_heading") {
            # Handle "Select" button
            shiny::observeEvent(input[[paste0("select_", node_index)]], {
              current_q_id = current_question_id()
              
              # Ensure question exists
              ensure_current_question_exists()
              
              modules_list = question_modules()
              current_module = modules_list[[as.character(current_q_id)]]
              
              if (!is.null(current_module$server)) {
                current_selected = current_module$server$get_selected_nodes()
                
                # Check if node has selected ancestors
                if (has_selected_ancestor(tree_items, node_index, current_selected)) {
                  return()
                }
                
                # Toggle this node
                if (node_index %in% current_selected) {
                  current_module$server$remove_node(node_index)
                } else {
                  # Remove descendants first
                  descendants_to_remove = find_selected_descendants(tree_items, node_index, current_selected)
                  for (desc in descendants_to_remove) {
                    current_module$server$remove_node(desc)
                  }
                  current_module$server$add_node(node_index)
                }
              }
            })
            
            # Handle "Select +" button
            shiny::observeEvent(input[[paste0("select_children_", node_index)]], {
              current_q_id = current_question_id()
              
              # Ensure question exists
              ensure_current_question_exists()
              
              modules_list = question_modules()
              current_module = modules_list[[as.character(current_q_id)]]
              
              if (!is.null(current_module$server)) {
                current_selected = current_module$server$get_selected_nodes()
                
                # Check if node has selected ancestors
                if (has_selected_ancestor(tree_items, node_index, current_selected)) {
                  return()
                }
                
                # Toggle this node (same as select for now)
                if (node_index %in% current_selected) {
                  current_module$server$remove_node(node_index)
                } else {
                  # Remove descendants first
                  descendants_to_remove = find_selected_descendants(tree_items, node_index, current_selected)
                  for (desc in descendants_to_remove) {
                    current_module$server$remove_node(desc)
                  }
                  current_module$server$add_node(node_index)
                }
              }
            })
          }
          
          # Handle "Preview" button (available for all nodes)
          shiny::observeEvent(input[[paste0("preview_", node_index)]], {
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
    
    # Helper function to ensure current question exists
    ensure_current_question_exists = function() {
      current_q_id = current_question_id()
      modules_list = question_modules()
      
      if (is.null(modules_list[[as.character(current_q_id)]])) {
        # Create new question
        add_new_question()
      }
    }
    
    # Helper function to add new question
    add_new_question = function() {
      next_id = last_question_id() + 1
      last_question_id(next_id)
      
      # Create initial question object
      initial_question = markermd_question(
        id = as.integer(next_id),
        name = paste("Question", next_id),
        selected_nodes = markermd_node_selection(indices = integer()),
        rules = list()
      )
      
      # Create question module
      module_id = paste0("question_", next_id)
      
      # Update question modules (store the initial question, server will be created when UI is inserted)
      modules_list = question_modules()
      modules_list[[as.character(next_id)]] = list(
        id = next_id,
        module_id = module_id,
        initial_question = initial_question,
        server = NULL  # Will be created when UI is inserted
      )
      question_modules(modules_list)
      
      # Set as current question
      current_question_id(next_id)
      
      return(next_id)
    }
    
    # Create persistent question container
    output$questions_ui = shiny::renderUI({
      shiny::div(
        id = "questions_container_content",
        shiny::uiOutput("question_items"),
        shiny::uiOutput("add_question_button")
      )
    })
    
    # Render add question button
    output$add_question_button = shiny::renderUI({
      shiny::div(
        style = "text-align: center; margin-bottom: 15px;",
        shiny::actionButton(
          "add_question", 
          shiny::icon("plus"),
          class = "btn-primary btn-sm",
          style = "font-size: 12px; padding: 4px 8px; border-radius: 20%; width: 30px; height: 30px;",
          title = "Add Question"
        ),
        shiny::span("Add Question", style = "margin-left: 8px; font-size: 14px; color: #333;")
      )
    })
    
    # Use insertUI/removeUI approach for stable question modules
    output$question_items = shiny::renderUI({
      # Just render empty container - questions will be inserted dynamically
      shiny::div(id = "dynamic_questions_container")
    })
    
    # Track which questions have been inserted
    inserted_questions = shiny::reactiveVal(character(0))
    
    
    # Insert new questions when they are added
    shiny::observe({
      modules_list = question_modules()
      current_inserted = inserted_questions()
      
      # Find new questions that need to be inserted
      new_questions = setdiff(names(modules_list), current_inserted)
      
      modules_updated = FALSE
      
      for (q_id_str in new_questions) {
        q_id = as.numeric(q_id_str)
        question_module = modules_list[[q_id_str]]
        
        if (!is.null(question_module)) {
          # Create the server for this question if it doesn't exist
          if (is.null(question_module$server)) {
            question_server_fn = question_server(
              question_module$module_id, 
              ast
            )
            
            # Store the server in the modules list
            modules_list[[q_id_str]]$server = question_server_fn
            modules_updated = TRUE
          }
          
          # Create question wrapper
          question_wrapper = shiny::div(
            id = paste0("question_wrapper_", q_id),
            style = "margin-bottom: 15px;",
            onclick = paste0("Shiny.setInputValue('select_question', ", q_id, ");"),
            
            # Question module UI - styling will be updated by separate observer
            shiny::div(
              id = paste0("question_card_", q_id),
              class = "border",
              style = "border-radius: 0.375rem;",
              question_ui(modules_list[[q_id_str]]$module_id, q_id)
            )
          )
          
          # Insert the question UI
          shiny::insertUI(
            selector = "#dynamic_questions_container",
            where = "beforeEnd",
            ui = question_wrapper
          )
        }
      }
      
      # Update question_modules reactive only once if we made changes
      if (modules_updated) {
        question_modules(modules_list)
      }
      
      # Update inserted questions list only with successfully inserted questions
      if (length(new_questions) > 0) {
        successfully_inserted = c(current_inserted, new_questions)
        inserted_questions(successfully_inserted)
      }
    })
    
    # Remove questions when they are deleted
    shiny::observe({
      modules_list = question_modules()
      current_inserted = inserted_questions()
      
      # Find questions that need to be removed
      removed_questions = setdiff(current_inserted, names(modules_list))
      
      for (q_id_str in removed_questions) {
        q_id = as.numeric(q_id_str)
        # Remove the question UI
        shiny::removeUI(
          selector = paste0("#question_wrapper_", q_id)
        )
      }
      
      # Only update inserted_questions if we actually removed something
      if (length(removed_questions) > 0) {
        new_inserted = setdiff(current_inserted, removed_questions)
        inserted_questions(new_inserted)
      }
    })
    
    # Update question styling when current question changes (without recreating UI)
    shiny::observe({
      current_q_id = current_question_id()
      modules_list = question_modules()
      
      for (q_id_str in names(modules_list)) {
        q_id = as.numeric(q_id_str)
        is_current = (q_id == current_q_id)
        
        # Update card styling via JavaScript
        if (is_current) {
          shinyjs::runjs(paste0("
            $('#question_card_", q_id, "').addClass('border-primary');
            $('#question_card_", q_id, "').css({
              'border-color': '#007bff !important',
              'border-width': '2px !important'
            });
          "))
        } else {
          shinyjs::runjs(paste0("
            $('#question_card_", q_id, "').removeClass('border-primary');
            $('#question_card_", q_id, "').css({
              'border-color': '',
              'border-width': ''
            });
          "))
        }
      }
    })
    
    # Add new question handler
    shiny::observeEvent(input$add_question, {
      add_new_question()
    })
    
    # Clear selections for current question
    shiny::observeEvent(input$clear_selections, {
      current_q_id = current_question_id()
      modules_list = question_modules()
      current_module = modules_list[[as.character(current_q_id)]]
      
      if (!is.null(current_module$server)) {
        current_module$server$clear_nodes()
      }
    })
    
    # Handle question selection
    shiny::observeEvent(input$select_question, {
      current_question_id(input$select_question)
    })
    
    
    # Handle question deletion
    shiny::observe({
      modules_list = question_modules()
      questions_to_remove = c()
      
      for (q_id_str in names(modules_list)) {
        question_module = modules_list[[q_id_str]]
        if (!is.null(question_module$server)) {
          delete_count = question_module$server$delete_clicked()
          
          if (!is.null(delete_count) && delete_count > 0) {
            questions_to_remove = c(questions_to_remove, q_id_str)
          }
        }
      }
      
      if (length(questions_to_remove) > 0) {
        # Remove questions
        modules_list = question_modules()
        
        for (q_id_str in questions_to_remove) {
          modules_list[[q_id_str]] = NULL
        }
        
        question_modules(modules_list)
        
        # Update current question if needed
        current_q_id = current_question_id()
        if (as.character(current_q_id) %in% questions_to_remove) {
          if (length(modules_list) > 0) {
            current_question_id(as.numeric(names(modules_list)[1]))
          } else {
            current_question_id(1)
          }
        }
      }
    })
    
    # Dynamic save button UI
    output$save_button_ui = shiny::renderUI({
      if (length(question_modules()) == 0) {
        shiny::actionButton(
          "save_disabled", 
          "Save Template", 
          class = "btn-secondary btn-sm", 
          style = "width: 100%;",
          disabled = TRUE
        )
      } else {
        shiny::downloadButton(
          "save_template", 
          "Save Template", 
          class = "btn-success btn-sm", 
          style = "width: 100%;"
        )
      }
    })
    
    # Save template functionality (disabled for now)
    # output$save_template = shiny::downloadHandler(
    #   filename = function() {
    #     timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
    #     paste0("template_", timestamp, ".rds")
    #   },
    #   content = function(file) {
    #     # TODO: Implement save functionality
    #   },
    #   contentType = "application/rds"
    # )
  }
  
  return(list(ui = ui, server = server))
}