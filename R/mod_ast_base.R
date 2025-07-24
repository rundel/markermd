#' Base AST Module
#'
#' Common functionality for AST tree display modules

#' Base AST UI
#'
#' @param id Character. Module namespace ID
#' @param title Character. Panel title (default: "Document Structure")
#' @param show_clear_button Logical. Whether to show clear selections button
#'
ast_base_ui = function(id, title = "Document Structure", show_clear_button = FALSE) {
  ns = shiny::NS(id)
  
  shiny::div(
    style = "height: 100%; max-height: 100%; display: flex; flex-direction: column;",
    shiny::h3(title, style = "flex-shrink: 0; margin-bottom: 10px;"),
    shiny::div(
      id = ns("ast_tree_container"),
      style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; border: 1px solid #dee2e6; flex: 1; overflow-y: auto; min-height: 0;",
      shiny::uiOutput(ns("ast_tree_ui"))
    ),
    if (show_clear_button) {
      shiny::div(
        style = "flex-shrink: 0; margin-top: 10px; margin-bottom: 10px; height: 50px; display: flex; align-items: center; justify-content: center;",
        shiny::actionButton(ns("clear_selections"), "Clear Current Question", class = "btn-secondary btn-sm")
      )
    }
  )
}

#' Base AST Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param selected_nodes Reactive. Currently selected node indices (optional)
#' @param enable_preview Logical. Whether to enable preview functionality
#'
ast_base_server = function(id, ast, selected_nodes = shiny::reactive(integer(0)), enable_preview = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    
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
    
    # Create AST tree display
    output$ast_tree_ui = shiny::renderUI({
      if (is.null(ast()) || is.null(ast_nodes())) {
        return(shiny::p("No document loaded"))
      }
      
      # Build tree structure
      tree_items = build_ast_tree_structure(ast())
      
      if (length(tree_items) == 0) {
        return(shiny::p("No document structure available"))
      }
      
      # Get current selected nodes
      current_selected = selected_nodes()
      
      # Create the tree
      create_simple_tree(tree_items, current_selected, identity)
    })
    
    # Handle preview functionality if enabled
    if (enable_preview) {
      shiny::observe({
        if (is.null(ast_nodes())) return()
        
        nodes = ast_nodes()
        
        # Create preview observers for all nodes
        for (i in seq_along(nodes)) {
          local({
            node_index = i
            
            # Handle "Preview" button
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
                    title = shiny::span(node_type, style = "font-size: 16px; font-weight: bold;"),
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
    }
    
    # Return reactive values and methods
    return(list(
      ast_nodes = ast_nodes,
      clear_clicked = shiny::reactive({
        input$clear_selections
      })
    ))
  })
}