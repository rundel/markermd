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
#' @param template Reactive. The template object with questions
#' @param template_obj Reactive. markermd_template S7 object with node selections for content extraction
#'
marking_server = function(id, ast, template, template_obj = shiny::reactiveVal(NULL)) {
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
      if (is.null(template()) || length(template()) == 0) {
        content = shiny::p("No template loaded. Please create a template first.")
      } else {
        templates_list = template()
        current_ast = ast()
        current_template_obj = template_obj()
        
        # Extract question content for tooltips
        question_contents = if (!is.null(current_ast) && !is.null(current_template_obj)) {
          extract_question_content(current_ast, current_template_obj)
        } else {
          list()
        }
        
        content = shiny::div(
          style = "padding: 10px;",
          shiny::h5("Questions:", style = "margin-bottom: 10px;"),
          
          shiny::tags$ul(
            style = "list-style-type: none; padding-left: 0; margin: 0;",
            lapply(seq_along(names(templates_list)), function(i) {
              question_name = names(templates_list)[i]
              # Get tooltip content
              tooltip_content = question_contents[[question_name]] %||% "No content available for this question."
              
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
                
                question_name
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
        
        shiny::showModal(
          shiny::modalDialog(
            title = paste("Content for", question_name),
            shiny::div(
              style = "max-height: 500px; overflow-y: auto;",
              shiny::pre(
                content,
                style = "font-family: 'Courier New', Courier, monospace; font-size: 12px; white-space: pre-wrap; margin: 0; background: #f8f9fa; padding: 15px; border: 1px solid #e9ecef; border-radius: 3px; line-height: 1.4;"
              )
            ),
            footer = shiny::modalButton("Close"),
            easyClose = TRUE,
            size = "l"
          )
        )
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