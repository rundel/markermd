#' AST Module
#'
#' Shiny modules for displaying AST tree structures with various interaction modes

# Base AST Module ----------------------------------------------------------------

#' Base AST UI
#'
#' @param id Character. Module namespace ID
#' @param title Character. Panel title (default: "Document Structure")
#' @param show_clear_button Logical. Whether to show clear selections button
#'
ast_base_ui = function(id, title = "Document Structure", show_clear_button = FALSE) {
  ns = shiny::NS(id)
  
  bslib::card(
    class = "h-100",
    bslib::card_header(title, class = "bg-light"),
    bslib::card_body(
      class = "flex-fill overflow-auto p-0",
      shiny::div(
        id = ns("ast_tree_container"),
        class = "bg-light p-3 h-100 overflow-auto",
        shiny::uiOutput(ns("ast_tree_ui"))
      )
    ),
    if (show_clear_button) {
      bslib::card_footer(
        class = "text-center",
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
                
                # Monaco Editor handles indentation properly, so we keep original content
                content = raw_content

                # Get node type for title
                node_type = class(node)[1]
                
                # Determine Monaco Editor language based on node type
                monaco_language = switch(node_type,
                  "rmd_yaml" = "yaml",
                  "rmd_markdown" = "markdown", 
                  "rmd_chunk" = {
                    # Extract engine from chunk
                    if (inherits(node, "rmd_chunk") && !is.null(node@engine)) {
                      # Map common R Markdown engines to Monaco languages
                      switch(node@engine,
                        "r" = "r",
                        "python" = "python",
                        "sql" = "sql",
                        "bash" = "shell",
                        "sh" = "shell",
                        "javascript" = "javascript",
                        "js" = "javascript",
                        "css" = "css",
                        "html" = "html",
                        "yaml" = "yaml",
                        "json" = "json",
                        # Default to markdown for unknown engines
                        "markdown"
                      )
                    } else {
                      "markdown"  # Default to markdown
                    }
                  },
                  "rmd_raw_chunk" = "markdown",
                  "rmd_code_block" = "markdown",
                  # Default to markdown for other node types
                  "markdown"
                )
                
                # Create unique editor ID
                editor_id = paste0("monaco-editor-ast-", node_index)
                
                shiny::showModal(
                  markermd_modal(
                    title = shiny::span(node_type, style = "font-size: 16px; font-weight: bold;"),
                    size = "l",
                    shiny::div(
                      style = "height: 400px;",
                      shiny::div(
                        id = editor_id,
                        style = "height: 100%; width: 100%; border: 1px solid #e1e5e9;"
                      )
                    ),
                    footer = NULL
                  )
                )
                
                # Initialize Monaco Editor
                shinyjs::runjs(paste0("
                  (function() {
                    // Load Monaco Editor if not already loaded
                    if (typeof monaco === 'undefined') {
                      var script = document.createElement('script');
                      script.src = 'https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs/loader.js';
                      script.onload = function() {
                        require.config({ paths: { vs: 'https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs' } });
                        require(['vs/editor/editor.main'], function() {
                          createEditor();
                        });
                      };
                      document.head.appendChild(script);
                    } else {
                      createEditor();
                    }
                    
                    function createEditor() {
                      // Clean up any existing editor
                      var existingContainer = document.getElementById('", editor_id, "');
                      if (existingContainer && existingContainer.editor) {
                        existingContainer.editor.dispose();
                      }
                      
                      // Create the editor
                      var editor = monaco.editor.create(document.getElementById('", editor_id, "'), {
                        value: ", jsonlite::toJSON(content, auto_unbox = TRUE), ",
                        language: '", monaco_language, "',
                        theme: 'vs',
                        readOnly: true,
                        wordWrap: 'on',
                        wrappingIndent: 'indent',
                        fontSize: 12,
                        lineNumbers: 'on',
                        minimap: { enabled: false },
                        scrollBeyondLastLine: false,
                        automaticLayout: true,
                        contextmenu: false,
                        selectOnLineNumbers: false
                      });
                      
                      // Store reference for cleanup
                      document.getElementById('", editor_id, "').editor = editor;
                    }
                  })();
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

# Selectable AST Module ----------------------------------------------------------

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
  
  # Use base AST server with interactive mode
  ast_base_server(id, ast, selected_nodes, enable_preview = TRUE, selection_mode = "interactive")
}

# Read-Only AST Module -----------------------------------------------------------

#' Read-Only AST UI
#'
#' @param id Character. Module namespace ID
#' @param title Character. Panel title (default: "Document Structure")
#'
ast_readonly_ui = function(id, title = "Document Structure") {
  ast_base_ui(id, title = title, show_clear_button = FALSE)
}

#' Read-Only AST Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param selected_nodes Reactive. Optional selected nodes to highlight (default: none)
#' @param enable_preview Logical. Whether to enable preview functionality (default: TRUE)
#' @param id_prefix Character. Optional prefix for button IDs to avoid collisions
#'
ast_readonly_server = function(id, ast, selected_nodes = shiny::reactive(integer(0)), enable_preview = TRUE, id_prefix = NULL) {
  
  # Determine selection mode based on whether nodes are provided for highlighting
  selection_mode = if (length(selected_nodes()) > 0) "highlight_only" else "readonly"
  
  # Use base AST server with read-only functionality  
  ast_base_server(id, ast, selected_nodes, enable_preview, selection_mode, id_prefix)
}