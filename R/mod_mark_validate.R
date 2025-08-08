#' Mark Validation Interface Module
#'
#' Shiny module for displaying validation results during marking

#' Get Question Content Display
#'
#' Creates the content display for a question including tree structure
#'
#' @param repo_ast The current document AST
#' @param template_ast The original template AST
#' @param question The question object with selected nodes
#' @param session Shiny session object
#'
#' @return Shiny UI element or paragraph with message
#'
get_question_content = function(repo_ast, template_ast, question, session) {
  
  question_ast = get_question_ast(repo_ast, template_ast, question)
  tree_items = build_ast_tree_structure(question_ast)
  
  # Filter out document root (index 0) and adjust depths for question display
  content_items = tree_items[sapply(tree_items, function(x) x$index != 0)]
  
  
  # Adjust depths to start from 1 for content nodes
  min_depth = min(sapply(content_items, function(x) x$depth))
  content_items = lapply(content_items, function(x) {
    x$depth = x$depth - min_depth + 1
    # Update parent indices: if parent was document root (0), set to NULL
    if (!is.null(x$parent_index) && x$parent_index == 0) {
      x$parent_index = NULL
    }
    x
  })
  
  # Create non-interactive tree display for this question
  question_id = paste0("q_", gsub("[^A-Za-z0-9]", "", question@name))
  simple_tree = create_simple_tree_readonly_at_depth(content_items, session$ns, question_id, start_depth = 1)
  
  shiny::div(
    class = "mb-1 p-2 bg-light border rounded overflow-auto small",
    style = "max-height: 120px;",
    simple_tree
  )
}

#' Create Rule Details Display
#'
#' Creates the rule details section for a question card
#'
#' @param question The question object with rules
#' @param question_result The validation result for this question
#'
#' @return Shiny UI element with rule details
#'
create_rule_details = function(question, question_result) {

  # Parse rule messages to understand individual rule results
  rule_items = lapply(seq_along(question_result$messages), function(i) {
    message = question_result$messages[i]
    rule = question@rules[[i]]
    
    # Determine if this rule passed based on message content
    rule_passed = question_result$passed[i]
    rule_color = if (rule_passed) "#28a745" else "#dc3545"
    rule_icon = if (rule_passed) "check" else "times"
    
    shiny::div(
      style = paste0("margin: 6px 0; padding: 8px; border-left: 3px solid ", rule_color, "; background-color: #f8f9fa; border-radius: 3px;"),
      shiny::div(
        style = "display: flex; align-items: center;",
        shiny::icon(rule_icon, style = paste0("color: ", rule_color, "; margin-right: 6px; font-size: 14px;")),
        if (!is.null(rule)) {
          shiny::span(
            rule@node_type,
            class = "text-muted fw-medium me-2",
            style = "font-size: 11px; padding: 2px 6px; border: 1px solid #dee2e6; border-radius: 6px;"
          )
        },
        shiny::span(
          message,
          style = "font-size: 13px;"
        )
      )
    )
  })
  
  shiny::div(rule_items)
}

#' Create Question Card
#'
#' Creates a single question validation card
#'
#' @param question The question object
#' @param question_result The validation result for this question
#' @param current_ast The current document AST
#' @param original_ast The original template AST
#' @param session Shiny session object
#'
#' @return bslib card element
#'
create_question_card = function(question, question_result, current_ast, original_ast, session) {
  question_name = question@name
  
  # Overall status styling - only pass/fail states
  status_color = switch(question_result$status,
    "pass" = "#28a745",
    "fail" = "#dc3545",
    "#6c757d"  # Default for unknown status
  )
  
  # Create HTML for solid circle icons with white symbols - only pass/fail states
  status_icon_html = switch(question_result$status,
    "pass" = '<i class="fas fa-circle text-success"></i><i class="fas fa-check text-white position-absolute top-50 start-50 translate-middle" style="font-size: 10px;"></i>',
    "fail" = '<i class="fas fa-circle text-danger"></i><i class="fas fa-times text-white position-absolute top-50 start-50 translate-middle" style="font-size: 10px;"></i>',
    '<i class="fas fa-question-circle text-muted"></i>'  # Default for unknown status
  )
  
  # Get the document nodes for this question using section selection
  question_nodes_content = get_question_content(current_ast, original_ast, question, session)
  
  # Create rule details
  rule_details = create_rule_details(question, question_result)
  
  # Create question card
  bslib::card(
    bslib::card_header(
      class = "bg-light",
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        shiny::span(question_name, class = "my-0 text-dark fw-semibold"),
        shiny::div(
          style = "position: relative; font-size: 18px;",
          shiny::HTML(status_icon_html)
        )
      )
    ),
    bslib::card_body(
      class = "pt-2 pb-1",
      question_nodes_content
    ),
    bslib::card_body(
      class = "pt-0 pb-1",
      rule_details
    )
  )
}

#' Mark Validation UI
#'
#' @param id Character. Module namespace ID
#'
mark_validate_ui = function(id) {
  ns = shiny::NS(id)
  
  shiny::div(
    id = ns("template_validation"),
    style = "max-height: calc(100vh - 200px); overflow-y: auto;",
    shiny::uiOutput(ns("template_validation_ui"))
  )
}

#' Mark Validation Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param current_repo_name Reactive. Current repository name
#' @param validation_results Reactive. Validation results for current repository
#' @param selected_question_name Reactive. Currently selected question name
#' @param template Reactive. Template object for reference
#'
mark_validate_server = function(id, ast, current_repo_name = shiny::reactiveVal(NULL), validation_results = shiny::reactiveVal(NULL), selected_question_name = shiny::reactiveVal(NULL), template = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    
    
    output$template_validation_ui = shiny::renderUI({
      shiny::req(template(), validation_results(), ast())
      
      current_validation = validation_results()
      current_template = template()
      current_ast = ast()
      
      question_cards = lapply(current_template@questions, function(question) {
        create_question_card(
          question, current_validation[[question@name]], 
          current_ast, current_template@original_ast, session
        )
      })
      
      if (length(question_cards) == 0) {
        shiny::p("No questions available.", class = "text-muted fst-italic")
      } else {
        shiny::div(
          style = "display: flex; flex-direction: column; gap: 3px;",
          question_cards
        )
      }
    })
    
    # Store created observers to avoid duplicates
    created_observers = shiny::reactiveValues()
    
    # Create preview button observers once when template and AST are available
    shiny::observe({
      # Use req() for cleaner validation 
      shiny::req(template(), ast())
      
      current_template = template()
      current_ast = ast()
      
      # Create observers for all questions and their nodes
      for (question in current_template@questions) {
        question_id = paste0("q_", gsub("[^A-Za-z0-9]", "", question@name))
        
        # Skip if observers already created for this question
        if (!is.null(created_observers[[question_id]])) {
          next
        }
        
        # Get the question AST using the shared function from utils_template.R
        question_ast = get_question_ast(current_ast, current_template@original_ast, question)
        
        if (!is.null(question_ast) && length(question_ast@nodes) > 0) {
          # Build tree structure and create observers for each node
          tree_items = build_ast_tree_structure(question_ast)
          
          if (length(tree_items) > 1) {  # Has more than just document root
            content_items = tree_items[sapply(tree_items, function(x) x$index != 0)]
            
            if (length(content_items) > 0) {
              # Create preview observers for each content node
              for (item in content_items) {
                local({
                  local_node_index = item$index
                  local_question_ast = question_ast
                  button_id = paste0("preview_", question_id, "_", local_node_index)
                  
                  shiny::observeEvent(input[[button_id]], {
                    # Get the actual node from the question AST
                    if (local_node_index >= 1 && local_node_index <= length(local_question_ast@nodes)) {
                      node = local_question_ast@nodes[[local_node_index]]
                      
                      # Get node content for preview
                      content = parsermd::as_document(node) |>
                        as.character() |>
                        paste(collapse = "\n")
                      
                      # Monaco Editor handles indentation properly, so we keep original content
                      
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
                      editor_id = paste0("monaco-editor-validate-", local_node_index)
                      
                      shiny::showModal(
                        shiny::modalDialog(
                          title = shiny::span(node_type, style = "font-size: 16px; font-weight: bold;"),
                          size = "l",
                          shiny::div(
                            style = "height: 400px;",
                            shiny::div(
                              id = editor_id,
                              style = "height: 100%; width: 100%; border: 1px solid #e1e5e9;"
                            )
                          ),
                          footer = NULL,
                          easyClose = TRUE
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
                  }, ignoreInit = TRUE)
                })
              }
              
              # Mark observers as created for this question
              created_observers[[question_id]] = TRUE
            }
          }
        }
      }
    })
    
    # Return empty list for now
    return(list())
  })
}