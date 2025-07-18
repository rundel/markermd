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
    
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::h3("Document Explorer"),
        shiny::p("This is a placeholder for the document exploration interface.", 
               style = "color: #6c757d; font-style: italic;")
      )
    ),
    
    shiny::hr(),
    
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::div(
          style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; min-height: 400px;",
          shiny::h4("Document Structure"),
          shiny::div(
            id = ns("ast_tree_container"),
            style = "flex: 1; overflow-y: auto; min-height: 350px;",
            shiny::uiOutput(ns("ast_tree_ui"))
          )
        )
      ),
      shiny::column(
        width = 6,
        shiny::div(
          style = "border: 1px solid #ddd; padding: 15px; background-color: #f9f9f9; min-height: 200px;",
          shiny::h4("Template Validation"),
          shiny::div(
            id = ns("template_validation"),
            shiny::uiOutput(ns("template_validation_ui"))
          )
        ),
        shiny::br(),
        shiny::div(
          style = "border: 1px solid #ddd; padding: 15px; background-color: #f9f9f9; min-height: 200px;",
          shiny::h4("Repository Summary"),
          shiny::verbatimTextOutput(ns("repo_summary"))
        )
      )
    ),
    
    shiny::br(),
    
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          style = "border: 1px solid #ddd; padding: 15px; background-color: #f8f9fa; min-height: 200px;",
          shiny::h4("Analysis Tools"),
          shiny::p("Document analysis and exploration tools will be added here in the future."),
          shiny::tags$ul(
            shiny::tags$li("Code chunk analysis"),
            shiny::tags$li("Markdown section overview"),
            shiny::tags$li("YAML metadata inspection"),
            shiny::tags$li("Document statistics and insights")
          )
        )
      )
    )
  )
}

#' Explore Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param current_repo_name Reactive. Current repository name
#' @param template Reactive. Template data for validation
#'
explore_server = function(id, ast, current_repo_name = shiny::reactiveVal(NULL), template = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Track preview observers for cleanup
    preview_observers = shiny::reactiveVal(list())
    
    # Reactive value for validation results
    validation_data = shiny::reactiveVal(list())
    
    # Function to destroy all existing preview observers
    destroy_preview_observers = function() {
      # Remove any existing modal dialogs
      shiny::removeModal()
      
      current_observers = preview_observers()
      if (length(current_observers) > 0) {
        for (observer in current_observers) {
          if (!is.null(observer)) {
            observer$destroy()
          }
        }
      }
      preview_observers(list())
    }
    
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
    
    # Create interactive AST tree display (read-only version)
    output$ast_tree_ui = shiny::renderUI({
      if (is.null(ast()) || is.null(ast_nodes())) {
        return(shiny::p("No document loaded", style = "color: #6c757d; font-style: italic;"))
      }
      
      # Build tree structure
      tree_items = build_ast_tree_structure(ast())
      
      if (length(tree_items) == 0) {
        return(shiny::p("No document structure available", style = "color: #6c757d; font-style: italic;"))
      }
      
      # Create the simple tree (read-only version with no selection)
      # Use current repo name as ID to avoid button collisions
      repo_id = if (!is.null(current_repo_name()) && current_repo_name() != "") {
        # Clean repo name for use as ID (remove special characters)
        gsub("[^A-Za-z0-9]", "", current_repo_name())
      } else {
        NULL
      }
      simple_tree = create_simple_tree_readonly(tree_items, session$ns, repo_id)
      
      shiny::div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #dee2e6;",
        simple_tree
      )
    })
    
    # Track the current repository to detect changes
    last_repo_name = shiny::reactiveVal(NULL)
    
    # Handle node preview clicks with observer tracking
    shiny::observe({
      current_repo = current_repo_name()
      
      # Clean up observers when repo changes or becomes null
      if (!identical(last_repo_name(), current_repo)) {
        destroy_preview_observers()
        last_repo_name(current_repo)
      }
      
      if (is.null(ast_nodes()) || is.null(current_repo)) {
        return()
      }
      
      # Only create observers if we don't already have them for this repo
      if (length(preview_observers()) == 0) {
        nodes = ast_nodes()
        tree_items = build_ast_tree_structure(ast())
        
        # Create repo_id for button identification
        repo_id = if (!is.null(current_repo) && current_repo != "") {
          gsub("[^A-Za-z0-9]", "", current_repo)
        } else {
          NULL
        }
        
        # Track new observers
        new_observers = list()
        
        for (i in seq_along(nodes)) {
          local({
            node_index = i  # Use direct node index
            
            # Create unique button ID matching the one created in the tree
            button_id = if (!is.null(repo_id)) {
              paste0("preview_", repo_id, "_", node_index)
            } else {
              paste0("preview_", node_index)
            }
            
            # Handle "Preview" button and track the observer
            observer = shiny::observeEvent(input[[button_id]], {
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
            }, ignoreInit = TRUE)
            
            # Store the observer in our tracking list
            new_observers[[length(new_observers) + 1]] = observer
          })
        }
        
        # Update the tracked observers
        preview_observers(new_observers)
      }
    })
    
    # Display repository summary
    output$repo_summary = shiny::renderText({
      if (is.null(ast())) {
        return("No repository selected")
      }
      
      summary = get_document_summary(ast())
      paste(
        paste("Total nodes:", summary$total_nodes),
        paste("Code chunks:", summary$code_chunks),
        paste("Markdown sections:", summary$markdown_sections),
        paste("YAML headers:", if(summary$has_yaml) "Yes" else "No"),
        "",
        "This view shows the parsed AST structure",
        "of the currently selected repository.",
        sep = "\n"
      )
    })
    
    # Perform validation when AST or template changes
    shiny::observe({
      current_ast = ast()
      current_template = template()
      
      if (!is.null(current_ast) && !is.null(current_template) && length(current_template) > 0) {
        # Run validation
        validation_results = validate_repo_against_templates(current_ast, current_template)
        validation_data(validation_results)
      } else {
        validation_data(list())
      }
    })
    
    # Template validation UI
    output$template_validation_ui = shiny::renderUI({
      validation_results = validation_data()
      current_template = template()
      
      if (is.null(current_template) || length(current_template) == 0) {
        return(shiny::p("No template loaded for validation.", 
                       style = "color: #6c757d; font-style: italic;"))
      }
      
      if (length(validation_results) == 0) {
        return(shiny::p("Validation not available.", 
                       style = "color: #6c757d; font-style: italic;"))
      }
      
      # Create validation results display with icons and bslib tooltips
      validation_items = lapply(names(validation_results), function(question_name) {
        validation = validation_results[[question_name]]
        
        # Determine status icon and create tooltip content for failures/errors only
        if (validation$status == "pass") {
          # No tooltip for passing validations
          icon_element = shiny::div(
            style = "font-size: 18px;",
            shiny::icon("check-circle", class = "text-success")
          )
        } else {
          # Create formatted tooltip content for failures and errors
          if (validation$status == "fail") {
            status_icon = shiny::icon("times-circle", class = "text-danger")
          } else {
            status_icon = shiny::icon("exclamation-circle", class = "text-warning")
          }
          
          # Format tooltip content - messages already have prefix symbols (✗, *)
          message_text = if (length(validation$messages) > 0) {
            paste(validation$messages, collapse = "\n")
          } else {
            validation$details
          }
          
          tooltip_content = shiny::div(
            style = "text-align: left; white-space: pre-line;",
            message_text
          )
          
          icon_element = bslib::tooltip(
            shiny::div(
              style = "font-size: 18px; cursor: help;",
              status_icon
            ),
            tooltip_content,
            placement = "top"
          )
        }
        
        shiny::div(
          style = "display: flex; align-items: center; gap: 12px; padding: 8px 0; border-bottom: 1px solid #eee;",
          icon_element,
          shiny::span(
            question_name,
            style = "font-weight: 500;"
          )
        )
      })
      
      shiny::div(
        style = "padding: 10px 0;",
        validation_items
      )
    })
    
    # Clean up observers when module is destroyed
    session$onSessionEnded(function() {
      destroy_preview_observers()
    })
    
    # Return empty list for now
    return(list())
  })
}