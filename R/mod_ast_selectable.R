#' Selectable AST Module
#'
#' AST tree display with node selection capabilities

#' Selectable AST UI
#'
#' @param id Character. Module namespace ID
#' @param title Character. Panel title (default: "Document Structure")
#'
ast_selectable_ui = function(id, title = "Document Structure") {
  ast_base_ui(id, title = title, show_clear_button = TRUE)
}

#' Selectable AST Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param selected_nodes Reactive. Currently selected node indices
#'
ast_selectable_server = function(id, ast, selected_nodes = shiny::reactive(integer(0))) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Create namespace function
    ns = session$ns
    
    # Reactive value to store node click events
    node_clicked = shiny::reactiveVal(NULL)
    
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
      create_simple_tree(tree_items, current_selected, ns)
    })
    
    # Handle node selection functionality
    shiny::observe({
      if (is.null(ast_nodes())) return()
      
      nodes = ast_nodes()
      tree_items = build_ast_tree_structure(ast())
      
      # Iterate through nodes to create selection observers
      for (i in seq_along(nodes)) {
        local({
          node_index = i
          node = ast_nodes()[[node_index]]
          node_type = class(node)[1]
          
          # Only create selection observers for heading nodes
          if (node_type == "rmd_heading") {
            # Handle node selection (both text and circle button)
            shiny::observeEvent(input[[paste0("select_", node_index)]], {
              # Store the click event with timestamp to ensure uniqueness
              node_clicked(list(
                node_index = node_index,
                action = "toggle",
                timestamp = Sys.time()
              ))
            })
            
            # Handle circle button selection (same behavior as text)
            shiny::observeEvent(input[[paste0("select_children_", node_index)]], {
              # Store the click event with timestamp to ensure uniqueness
              node_clicked(list(
                node_index = node_index,
                action = "toggle",
                timestamp = Sys.time()
              ))
            })
          }
          
          # Handle "Preview" button (available for all nodes)
          shiny::observeEvent(input[[paste0("preview_", node_index)]], {
            if (node_index >= 1 && node_index <= length(ast_nodes())) {
              node = ast_nodes()[[node_index]]
              
              # Use as_document() to get raw node content
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
        })
      }
    })
    
    # Return reactive values and methods
    return(list(
      ast_nodes = ast_nodes,
      node_clicked = shiny::reactive({
        node_clicked()
      }),
      clear_clicked = shiny::reactive({
        input$clear_selections
      })
    ))
  })
}