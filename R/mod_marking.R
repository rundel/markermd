#' Marking Interface Module
#'
#' Shiny module for grading assignments

#' Marking UI
#'
#' @param id Character. Module namespace ID
#'
marking_ui = function(id) {
  ns = shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = c(4, 8),
    shiny::div(
      shiny::h3("Questions"),
      shiny::div(
        style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; height: 500px; overflow-y: auto;",
        shiny::div(
          id = ns("questions_list"),
          shiny::p("No template loaded. Please create a template first.")
        )
      )
    ),
    shiny::div(
      shiny::h3("Assignment Content"),
      shiny::tabsetPanel(
        id = ns("content_tabs"),
        shiny::tabPanel(
          "Document View",
          shiny::div(
            style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; height: 450px; overflow-y: auto;",
            shiny::verbatimTextOutput(ns("document_content"))
          )
        ),
        shiny::tabPanel(
          "Grading",
          shiny::div(
            style = "padding: 10px;",
            shiny::h4("Assignment Grading"),
            shiny::br(),
            shiny::numericInput(
              ns("score"),
              "Score:",
              value = 0,
              min = 0,
              max = 100,
              step = 1
            ),
            shiny::textAreaInput(
              ns("feedback"),
              "Feedback:",
              rows = 6,
              placeholder = "Enter feedback for this question..."
            ),
            shiny::br(),
            shiny::actionButton(ns("save_grade"), "Save Grade", class = "btn-success")
          )
        )
      )
    )
  )
}

#' Marking Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param template_obj Reactive. markermd_template S7 object with questions and rules
#' @param validation_results Reactive. Optional validation results for current repository
#'
marking_server = function(id, ast, template_obj, validation_results = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values for managing grading
    current_question = shiny::reactiveVal(1)
    grades = shiny::reactiveVal(list())
    
    # Display document content
    output$document_content = shiny::renderText({
      if (is.null(ast())) {
        return("No document loaded")
      }
      
      # This is a placeholder - in a real implementation we would
      # display the actual document content, possibly with highlighting
      # for the currently selected question nodes
      
      summary = get_document_summary(ast())
      
      paste(
        "Document Summary:",
        paste("Total nodes:", summary$total_nodes),
        paste("Code chunks:", summary$code_chunks),
        paste("Markdown sections:", summary$markdown_sections),
        paste("Has YAML header:", summary$has_yaml),
        "",
        "Full document content would be displayed here...",
        sep = "\n"
      )
    })
    
    # Update questions list based on template
    shiny::observe({
      current_template_obj = template_obj()
      if (is.null(current_template_obj) || length(current_template_obj@questions) == 0) {
        content = shiny::p("No template loaded. Please create a template first.")
      } else {
        current_ast = ast()
        
        # Extract question content for tooltips
        question_contents = if (!is.null(current_ast)) {
          extract_question_content(current_ast, current_template_obj)
        } else {
          list()
        }
        
        content = shiny::div(
          style = "padding: 10px;",
          shiny::h5("Questions:", style = "margin-bottom: 10px;"),
          
          shiny::tags$ul(
            style = "list-style-type: none; padding-left: 0; margin: 0;",
            lapply(seq_along(current_template_obj@questions), function(i) {
              question_name = current_template_obj@questions[[i]]@name
              # Get tooltip content
              tooltip_content = question_contents[[question_name]] %||% "No content available for this question."
              
              # Get validation status if available
              current_validation = validation_results()
              validation_status = if (!is.null(current_validation) && !is.null(current_validation[[question_name]])) {
                status = current_validation[[question_name]]$status
                if (status == "pass") {
                  '<span style="color: #28a745; margin-left: 10px;"><i class="fas fa-check-circle"></i></span>'
                } else if (status == "fail") {
                  '<span style="color: #dc3545; margin-left: 10px;"><i class="fas fa-times-circle"></i></span>'
                } else if (status == "error") {
                  '<span style="color: #ffc107; margin-left: 10px;"><i class="fas fa-exclamation-triangle"></i></span>'
                } else {
                  ""
                }
              } else {
                ""
              }
              
              # Create a unique ID for this question item
              item_id = session$ns(paste0("question_item_", i))
              
              # Escape content for JavaScript (simple approach)
              escaped_content = gsub("'", "\\\\'", tooltip_content)
              escaped_content = gsub("\"", "\\\\\"", escaped_content)
              escaped_content = gsub("\n", "\\\\n", escaped_content)
              escaped_content = substr(escaped_content, 1, 800)  # Limit length
              
              # Create question item with both hover tooltip and click modal
              shiny::tags$li(
                id = item_id,
                style = "padding: 8px 0; border-bottom: 1px solid #eee; cursor: help;",
                
                # Hover tooltip (simple version)
                title = paste(substr(tooltip_content, 1, 200), if(nchar(tooltip_content) > 200) "..." else ""),
                
                # Click handler for detailed modal
                onclick = paste0("Shiny.setInputValue('", session$ns("show_question_content"), "', {question: '", question_name, "', content: ", jsonlite::toJSON(tooltip_content, auto_unbox = TRUE), "});"),
                
                # Also add hover effect with CSS
                onmouseover = "this.style.backgroundColor = '#f0f0f0';",
                onmouseout = "this.style.backgroundColor = 'transparent';",
                
                shiny::HTML(paste0(question_name, validation_status))
              )
            })
          )
        )
      }
      
      shiny::removeUI(selector = paste0("#", session$ns("questions_list"), " > *"))
      shiny::insertUI(
        selector = paste0("#", session$ns("questions_list")),
        ui = content
      )
    })
    
    # Handle question content modal display
    shiny::observeEvent(input$show_question_content, {
      if (!is.null(input$show_question_content)) {
        question_name = input$show_question_content$question
        content = input$show_question_content$content
        
        # Remove leading whitespace from all lines to prevent Prism indentation issues
        content_lines = strsplit(content, "\n")[[1]]
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
        
        
        # Determine syntax highlighting language based on content type
        # Try to detect the content type from the content itself
        syntax_language = if (grepl("^---\\s*$", content, perl = TRUE)) {
          "yaml"  # YAML front matter
        } else if (grepl("^```\\{r", content, perl = TRUE)) {
          "r"  # R chunk
        } else if (grepl("^```\\{python", content, perl = TRUE)) {
          "python"  # Python chunk
        } else if (grepl("^```\\{sql", content, perl = TRUE)) {
          "sql"  # SQL chunk
        } else if (grepl("^```\\{bash", content, perl = TRUE)) {
          "bash"  # Bash chunk
        } else if (grepl("^```", content, perl = TRUE)) {
          "text"  # Generic code block
        } else {
          "markdown"  # Default to markdown
        }
        
        # Generate unique modal ID for this question content
        modal_id = paste0("syntax-content-", gsub("[^A-Za-z0-9]", "", question_name))
        
        shiny::showModal(
          shiny::modalDialog(
            title = shiny::span(paste("Content for", question_name), style = "font-size: 16px; font-weight: bold;"),
            size = "l",
            shiny::div(
              style = "max-height: 500px; overflow-y: auto;",
              # Pre element with soft wrapping for long lines
              shiny::tags$pre(
                id = modal_id,
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
            var preElement = document.getElementById('", modal_id, "');
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
    
    # Question selection is no longer interactive - questions are just displayed as a list
    
    # Save grade (note: with non-interactive questions list, this may need to be reworked)
    shiny::observeEvent(input$save_grade, {
      current_grades = grades()
      question_id = as.character(current_question())
      
      current_grades[[question_id]] = list(
        score = input$score,
        feedback = input$feedback,
        timestamp = Sys.time()
      )
      
      grades(current_grades)
    })
    
    # Next question functionality removed since questions are no longer interactive
    
    # Return reactive values
    return(list(
      current_question = current_question,
      grades = grades
    ))
  })
}