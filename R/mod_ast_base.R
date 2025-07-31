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
    class = "h-100 d-flex flex-column",
    shiny::h3(title, class = "flex-shrink-0 mb-2"),
    shiny::div(
      id = ns("ast_tree_container"),
      class = "bg-light p-3 rounded border flex-fill overflow-auto",
      shiny::uiOutput(ns("ast_tree_ui"))
    ),
    if (show_clear_button) {
      shiny::div(
        class = "flex-shrink-0 mt-2 mb-2 d-flex align-items-center justify-content-center",
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
#' @param selection_mode Character. Selection mode ("interactive", "readonly", "highlight_only")
#' @param id_prefix Character. Optional prefix for button IDs to avoid collisions
#'
ast_base_server = function(id, ast, selected_nodes = shiny::reactive(integer(0)), enable_preview = TRUE, selection_mode = "readonly", id_prefix = NULL) {
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
      
      # Create the tree using unified function
      create_unified_tree(tree_items, current_selected, session$ns, selection_mode, id_prefix)
    })
    
    # Reactive value to store node click events (for interactive mode)
    node_clicked = shiny::reactiveVal(NULL)
    
    # Handle preview functionality and node interactions
    shiny::observe({
      if (is.null(ast_nodes())) return()
      
      nodes = ast_nodes()
      tree_items = build_ast_tree_structure(ast())
      
      # Create observers for all nodes
      for (i in seq_along(nodes)) {
        local({
          node_index = i
          node = ast_nodes()[[node_index]]
          node_type = class(node)[1]
          
          # Create button IDs with optional prefix
          preview_id = if (!is.null(id_prefix)) {
            paste0("preview_", id_prefix, "_", node_index)
          } else {
            paste0("preview_", node_index)
          }
          
          # Handle "Preview" button if enabled
          if (enable_preview) {
            shiny::observeEvent(input[[preview_id]], {
              if (node_index >= 1 && node_index <= length(ast_nodes())) {
                node = ast_nodes()[[node_index]]
                
                # Use enhanced preview logic from selectable module
                raw_content = parsermd::as_document(node) |>
                  as.character() |>
                  paste(collapse="\n")
                
                # Remove leading whitespace from all lines to prevent Prism indentation issues
                content_lines = strsplit(raw_content, "\n")[[1]]
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
                        id = paste0("syntax-content-", node_index),
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
                    var preElement = document.getElementById('syntax-content-", node_index, "');
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
            })
          }
          
          # Handle node selection functionality for interactive mode
          if (selection_mode == "interactive" && node_type == "rmd_heading") {
            # Handle node selection (both text and circle button)
            select_id = paste0("select_", node_index)
            select_children_id = paste0("select_children_", node_index)
            
            shiny::observeEvent(input[[select_id]], {
              # Store the click event with timestamp to ensure uniqueness
              node_clicked(list(
                node_index = node_index,
                action = "toggle",
                timestamp = Sys.time()
              ))
            })
            
            # Handle circle button selection (same behavior as text)
            shiny::observeEvent(input[[select_children_id]], {
              # Store the click event with timestamp to ensure uniqueness
              node_clicked(list(
                node_index = node_index,
                action = "toggle",
                timestamp = Sys.time()
              ))
            })
          }
        })
      }
    })
    
    # Return reactive values and methods
    result = list(
      ast_nodes = ast_nodes,
      clear_clicked = shiny::reactive({
        input$clear_selections
      })
    )
    
    # Add node_clicked for interactive mode
    if (selection_mode == "interactive") {
      result$node_clicked = shiny::reactive({
        node_clicked()
      })
    }
    
    return(result)
  })
}