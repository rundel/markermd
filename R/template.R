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
    
    # Include Prism.js for syntax highlighting
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism.min.css"),
      shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-core.min.js"),
      shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/plugins/autoloader/prism-autoloader.min.js")
    ),
    
    shiny::tags$style(shiny::HTML("
      /* Rule form controls */
      .rule-item select,
      .rule-item .form-control,
      .rule-item input[type='number'],
      .rule-item .input-group-addon {
        font-size: 12px !important;
        height: 32px !important;
        padding: 4px 8px !important;
      }
      
      .rule-item {
        position: relative;
        z-index: 100;
        width: 100% !important;
        max-width: 100% !important;
        overflow: visible !important;
      }
      
      .rule-item .form-group { margin-bottom: 0 !important; }
      .rule-item .input-group-addon { line-height: 1.2 !important; }
      .rule-item select:focus { z-index: 1000; }
      
      /* Modal styling */
      .modal-header { padding: 8px 15px !important; }
      .modal-title { margin: 0 !important; padding: 0 !important; line-height: 1.2 !important; }
      
      /* Code syntax highlighting */
      .modal-content pre[class*='language-'],
      .modal-content pre#syntax-content {
        margin: 0 !important;
        padding: 15px !important;
        text-indent: 0 !important;
        background: #f5f2f0 !important;
        white-space: pre-wrap !important;
        word-wrap: break-word !important;
        overflow-wrap: break-word !important;
      }
      
      .modal-content code[class*='language-'],
      .modal-content .token {
        text-indent: 0 !important;
        padding: 0 !important;
        margin: 0 !important;
        white-space: pre-wrap !important;
        word-wrap: break-word !important;
        overflow-wrap: break-word !important;
      }
      
      .modal-content code[class*='language-'] {
        display: block !important;
        padding-left: 0 !important;
        margin-left: 0 !important;
      }
      
      /* Questions container layout */
      #questions_container {
        overflow-y: auto !important;
        overflow-x: hidden !important;
        max-height: calc(100vh - 300px) !important;
      }
      
      #dynamic_questions_container,
      #questions_container .card {
        width: 100% !important;
        max-width: 100% !important;
        box-sizing: border-box !important;
      }
      
      #questions_container .card {
        margin-bottom: 10px !important;
        overflow: visible !important;
      }
      
      #questions_container .bslib-card {
        margin-bottom: 0 !important;
      }
      
      #questions_container .bslib-card .card-body {
        padding-bottom: 8px !important;
      }
      
      #questions_container .bslib-card .card-body > div:last-child {
        margin-bottom: 0 !important;
      }
      
      #questions_container .card * {
        box-sizing: border-box !important;
      }
    ")),
    
    style = "height: calc(100vh - 150px); min-height: 600px; max-height: calc(100vh - 150px);",
    bslib::layout_columns(
      col_widths = c(6, 6),
      class = "h-100",
      ast_selectable_ui("ast_panel"),
      shiny::div(
        class = "h-100 d-flex flex-column",
        shiny::h3("Questions", class = "flex-shrink-0 mb-2"),
        shiny::div(
          id = "questions_container",
          class = "border p-2 pb-4 bg-light flex-fill overflow-auto w-100",
          shiny::uiOutput("questions_ui")
        ),
        shiny::div(
          class = "flex-shrink-0 mt-2 mb-2 d-flex align-items-center justify-content-center",
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
    
    # Reactive value for pending node clicks
    pending_node_click = shiny::reactiveVal(NULL)
    
    # Trigger for forcing styling updates
    styling_trigger = shiny::reactiveVal(0)
    
    # Load template on startup if provided
    template_loaded = shiny::reactiveVal(FALSE)
    
    shiny::observeEvent(template_obj, {
      if (!is.null(template_obj) && !template_loaded()) {
        # Load questions from template
        for (question in template_obj@questions) {
          id = question@id
          
          # Create initial question object in the same format as add_new_question
          module_id = paste0("question_", id)
          
          # Store the module data (server will be created when UI is inserted)
          modules = question_modules()
          modules[[as.character(id)]] = list(
            id = id,
            module_id = module_id,
            initial_question = question,
            server = NULL  # Will be created when UI is inserted
          )
          question_modules(modules)
          
          # Update last question ID if this is higher
          if (id > last_question_id()) {
            last_question_id(id)
          }
        }
        
        # Set current question to first question if any exist
        if (length(template_obj@questions) > 0) {
          current_question_id(template_obj@questions[[1]]@id)
        }
        
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
    
    # Get selected nodes for current question
    selected_nodes = shiny::reactive({
      current_q_id = current_question_id()
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
      
      return(selected_nodes)
    })
    
    # Initialize AST selectable module
    ast_result = ast_selectable_server("ast_panel", ast, selected_nodes)
    
    # Handle pending node clicks when question modules change
    shiny::observeEvent(list(question_modules(), pending_node_click()), {
      click_data = pending_node_click()
      if (is.null(click_data)) return()
      
      current_q_id = current_question_id()
      modules_list = question_modules()
      current_module = modules_list[[as.character(current_q_id)]]
      
      # Only process if we now have a server
      if (!is.null(current_module) && !is.null(current_module$server)) {
        # Clear the pending click
        pending_node_click(NULL)
        
        # Apply the node selection
        node_index = click_data$node_index
        current_selected = current_module$server$get_selected_nodes()
        tree_items = build_ast_tree_structure(ast())
        
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
    
    # Handle node selection events from AST module
    shiny::observeEvent(ast_result$node_clicked(), {
      click_data = ast_result$node_clicked()
      if (is.null(click_data)) return()
      
      current_q_id = current_question_id()
      
      # Check if question exists
      modules_list = question_modules()
      current_module = modules_list[[as.character(current_q_id)]]
      question_exists = !is.null(current_module)
      
      # If question doesn't exist, create it and store the pending click
      if (!question_exists) {
        ensure_current_question_exists()
        pending_node_click(click_data)
        return()
      }
      
      # If question exists but server doesn't exist yet, store as pending
      if (is.null(current_module$server)) {
        pending_node_click(click_data)
        return()
      }
      
      # Process the click immediately
      node_index = click_data$node_index
      current_selected = current_module$server$get_selected_nodes()
      tree_items = build_ast_tree_structure(ast())
      
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
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Handle clear selections from AST module
    shiny::observeEvent(ast_result$clear_clicked(), {
      current_q_id = current_question_id()
      modules_list = question_modules()
      current_module = modules_list[[as.character(current_q_id)]]
      
      if (!is.null(current_module$server)) {
        current_module$server$clear_nodes()
      }
    })
    
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
        class = "text-center mb-3",
        shiny::actionButton(
          "add_question", 
          shiny::icon("plus"),
          class = "btn-primary btn-sm rounded-circle",
          style = "width: 30px; height: 30px;",
          title = "Add Question"
        ),
        shiny::span("Add Question", class = "ms-2 text-dark")
      )
    })
    
    # Use insertUI/removeUI approach for stable question modules
    output$question_items = shiny::renderUI({
      modules_list = question_modules()
      
      if (length(modules_list) == 0) {
        shiny::div(id = "dynamic_questions_container")
      } else {
        shiny::div(
          id = "dynamic_questions_container",
          lapply(names(modules_list), function(q_id_str) {
            q_id = as.numeric(q_id_str)
            question_module = modules_list[[q_id_str]]
            
            # Create the server for this question if it doesn't exist
            if (is.null(question_module$server)) {
              question_server_fn = question_server(
                question_module$module_id, 
                ast_nodes,
                initial_question = question_module$initial_question
              )
              
              # Store the server in the modules list
              modules_list[[q_id_str]]$server = question_server_fn
              question_modules(modules_list)
            }
            
            # Create question wrapper
            shiny::div(
              id = paste0("question_wrapper_", q_id),
              style = "margin-bottom: 15px; width: 100%; max-width: 100%; box-sizing: border-box;",
              onclick = paste0("Shiny.setInputValue('select_question', ", q_id, ");"),
              
              # Question module UI
              shiny::div(
                id = paste0("question_card_", q_id),
                class = "border",
                style = "border-radius: 0.375rem;",
                question_ui(question_module$module_id, q_id)
              )
            )
          })
        )
      }
    })
    
    # Track which questions have been inserted
    inserted_questions = shiny::reactiveVal(character(0))
    
    
    # Insert new questions when they are added (DISABLED - using renderUI approach instead)
    # shiny::observe({
    #   modules_list = question_modules()
    #   current_inserted = inserted_questions()
    #   
    #   cat("Debug UI: modules_list has", length(modules_list), "questions\n")
    #   cat("Debug UI: current_inserted has", length(current_inserted), "questions\n")
    #   cat("Debug UI: modules_list keys:", paste(names(modules_list), collapse = ", "), "\n")
    #   cat("Debug UI: current_inserted keys:", paste(current_inserted, collapse = ", "), "\n")
    #   
    #   # Find new questions that need to be inserted
    #   new_questions = setdiff(names(modules_list), current_inserted)
    #   cat("Debug UI: new_questions to insert:", paste(new_questions, collapse = ", "), "\n")
    #   
    #   modules_updated = FALSE
    #   
    #   for (q_id_str in new_questions) {
    #     q_id = as.numeric(q_id_str)
    #     question_module = modules_list[[q_id_str]]
    #     
    #     cat("Debug UI: Processing question", q_id_str, "\n")
    #     
    #     if (!is.null(question_module)) {
    #       cat("Debug UI: Question module exists\n")
    #       # Create the server for this question if it doesn't exist
    #       if (is.null(question_module$server)) {
    #         question_server_fn = question_server(
    #           question_module$module_id, 
    #           ast,
    #           initial_question = question_module$initial_question
    #         )
    #         
    #         # Store the server in the modules list
    #         modules_list[[q_id_str]]$server = question_server_fn
    #         modules_updated = TRUE
    #       }
    #       
    #       # Create question wrapper
    #       question_wrapper = shiny::div(
    #         id = paste0("question_wrapper_", q_id),
    #         style = "margin-bottom: 15px; width: 100%; max-width: 100%; box-sizing: border-box;",
    #         onclick = paste0("Shiny.setInputValue('select_question', ", q_id, ");"),
    #         
    #         # Question module UI - styling will be updated by separate observer
    #         shiny::div(
    #           id = paste0("question_card_", q_id),
    #           class = "border",
    #           style = "border-radius: 0.375rem;",
    #           {
    #             cat("Debug UI: Creating question_ui for module_id:", modules_list[[q_id_str]]$module_id, "q_id:", q_id, "\n")
    #             question_ui(modules_list[[q_id_str]]$module_id, q_id)
    #           }
    #         )
    #       )
    #       
    #       # Insert the question UI
    #       cat("Debug UI: Inserting UI for question", q_id, "\n")
    #       cat("Debug UI: question_wrapper created successfully\n")
    #       
    #       # Check if container exists
    #       shiny::insertUI(
    #         selector = "#dynamic_questions_container",
    #         where = "beforeEnd",
    #         ui = question_wrapper,
    #         immediate = TRUE
    #       )
    #       
    #       cat("Debug UI: UI inserted for question", q_id, "\n")
    #       
    #       # Trigger styling update after UI insertion
    #       styling_trigger(styling_trigger() + 1)
    #     }
    #   }
    #   
    #   # Update question_modules reactive only once if we made changes
    #   if (modules_updated) {
    #     question_modules(modules_list)
    #   }
    #   
    #   # Update inserted questions list only with successfully inserted questions
    #   if (length(new_questions) > 0) {
    #     successfully_inserted = c(current_inserted, new_questions)
    #     inserted_questions(successfully_inserted)
    #   }
    # })
    
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
    
    # Update question styling when current question changes or styling trigger fires
    shiny::observe({
      current_q_id = current_question_id()
      modules_list = question_modules()
      styling_trigger()  # React to styling trigger
      
      # Apply styling immediately without setTimeout first, then with delay as backup
      for (q_id_str in names(modules_list)) {
        q_id = as.numeric(q_id_str)
        is_current = (q_id == current_q_id)
        
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
      
      # Backup with delay for newly inserted elements
      shinyjs::runjs(paste0("
        setTimeout(function() {
          $('#question_card_", current_q_id, "').addClass('border-primary');
          $('#question_card_", current_q_id, "').css({
            'border-color': '#007bff !important',
            'border-width': '2px !important'
          });
        }, 100);
      "))
    })
    
    # Add new question handler
    shiny::observeEvent(input$add_question, {
      add_new_question()
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
          class = "btn-secondary btn-sm w-100",
          disabled = TRUE
        )
      } else {
        shiny::downloadButton(
          "save_template", 
          "Save Template", 
          class = "btn-success btn-sm w-100"
        )
      }
    })
    
    # Save template functionality
    output$save_template = shiny::downloadHandler(
      filename = function() {
        timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0("template_", timestamp, ".rds")
      },
      content = function(file) {
        # Build template object from current questions
        questions_list = list()
        
        modules = question_modules()
        for (i in seq_along(modules)) {
          module_data = modules[[i]]
          
          if (!is.null(module_data) && !is.null(module_data$server) && is.list(module_data$server)) {
            if (!is.null(module_data$server$question) && is.function(module_data$server$question)) {
              question_obj = module_data$server$question()
              
              if (!is.null(question_obj)) {
                # Ensure question ID matches the module ID for uniqueness
                if (question_obj@id != module_data$id) {
                  question_obj@id = as.integer(module_data$id)
                }
                
                questions_list[[length(questions_list) + 1]] = question_obj
              }
            }
          }
        }
        
        # Create template object
        template_obj = markermd_template(
          original_ast = ast(),
          questions = questions_list
        )
        
        # Save to file
        saveRDS(template_obj, file)
      },
      contentType = "application/rds"
    )
  }
  
  return(list(ui = ui, server = server))
}