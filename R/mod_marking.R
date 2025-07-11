#' Marking Interface Module
#'
#' Shiny module for grading assignments

#' Marking UI
#'
#' @param id Character. Module namespace ID
#'
marking_ui = function(id) {
  ns = shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 4,
      shiny::h3("Questions"),
      shiny::div(
        style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; height: 500px; overflow-y: auto;",
        shiny::div(
          id = ns("questions_list"),
          shiny::p("No template loaded. Please create a template first.")
        )
      )
    ),
    shiny::column(
      width = 8,
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
            shiny::h4("Current Question: [No question selected]"),
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
            shiny::actionButton(ns("save_grade"), "Save Grade", class = "btn-success"),
            shiny::actionButton(ns("next_question"), "Next Question", class = "btn-primary")
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
#'
marking_server = function(id, ast, template) {
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
        questions_list = template()
        content = shiny::tagList(
          lapply(seq_along(questions_list), function(i) {
            q = questions_list[[i]]
            is_current = (i == current_question())
            
            shiny::div(
              class = if (is_current) "question-item active" else "question-item",
              style = paste(
                "border: 1px solid #ccc; margin-bottom: 5px; padding: 10px; cursor: pointer;",
                if (is_current) "background-color: #e3f2fd;" else "background-color: white;"
              ),
              onclick = paste0("Shiny.setInputValue('", session$ns("select_question"), "', ", i, ");"),
              shiny::h6(paste("Question", i)),
              shiny::p(q$name %||% paste("Question", i)),
              shiny::tags$small(paste("Nodes:", if (length(q$selected_nodes) == 0) "None" else paste(q$selected_nodes, collapse = ", ")))
            )
          })
        )
      }
      
      shiny::removeUI(selector = paste0("#", session$ns("questions_list"), " > *"))
      shiny::insertUI(
        selector = paste0("#", session$ns("questions_list")),
        ui = content
      )
    })
    
    # Handle question selection
    shiny::observeEvent(input$select_question, {
      current_question(input$select_question)
      
      # Load existing grade if available
      existing_grade = grades()[[as.character(input$select_question)]]
      if (!is.null(existing_grade)) {
        shiny::updateNumericInput(session, "score", value = existing_grade$score)
        shiny::updateTextAreaInput(session, "feedback", value = existing_grade$feedback)
      } else {
        shiny::updateNumericInput(session, "score", value = 0)
        shiny::updateTextAreaInput(session, "feedback", value = "")
      }
    })
    
    # Save grade
    shiny::observeEvent(input$save_grade, {
      current_grades = grades()
      question_id = as.character(current_question())
      
      current_grades[[question_id]] = list(
        score = input$score,
        feedback = input$feedback,
        timestamp = Sys.time()
      )
      
      grades(current_grades)
      
      shiny::showNotification("Grade saved successfully!", type = "message")
    })
    
    # Next question
    shiny::observeEvent(input$next_question, {
      if (!is.null(template()) && current_question() < length(template())) {
        current_question(current_question() + 1)
      }
    })
    
    # Return reactive values
    return(list(
      current_question = current_question,
      grades = grades
    ))
  })
}