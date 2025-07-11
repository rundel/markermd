#' Template Interface Module
#'
#' Shiny module for creating assignment templates

#' Template UI
#'
#' @param id Character. Module namespace ID
#'
template_ui = function(id) {
  ns = shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 6,
      shiny::h3("Document Structure"),
      shiny::div(
        style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; height: 400px; overflow-y: auto;",
        shiny::verbatimTextOutput(ns("ast_tree"))
      )
    ),
    shiny::column(
      width = 6,
      shiny::h3("Questions"),
      shiny::div(
        style = "margin-bottom: 10px;",
        shiny::actionButton(ns("add_question"), "Add Question", class = "btn-primary"),
        shiny::actionButton(ns("remove_question"), "Remove Question", class = "btn-warning")
      ),
      shiny::div(
        id = ns("questions_container"),
        style = "border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9; height: 350px; overflow-y: auto;",
        shiny::p("No questions created yet. Click 'Add Question' to start.")
      )
    )
  )
}

#' Template Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#'
template_server = function(id, ast) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values for managing questions
    questions = shiny::reactiveVal(list())
    selected_question = shiny::reactiveVal(NULL)
    
    # Display the AST tree
    output$ast_tree = shiny::renderText({
      if (is.null(ast())) {
        return("No document loaded")
      }
      
      tree_display = create_ast_tree_display(ast())
      paste(tree_display, collapse = "\n")
    })
    
    # Add new question
    shiny::observeEvent(input$add_question, {
      current_questions = questions()
      new_question = list(
        id = length(current_questions) + 1,
        name = paste("Question", length(current_questions) + 1),
        selected_nodes = integer(0)
      )
      questions(c(current_questions, list(new_question)))
      
      update_questions_display()
    })
    
    # Remove question
    shiny::observeEvent(input$remove_question, {
      current_questions = questions()
      if (length(current_questions) > 0) {
        questions(current_questions[-length(current_questions)])
        update_questions_display()
      }
    })
    
    # Update questions display
    update_questions_display = function() {
      current_questions = questions()
      
      if (length(current_questions) == 0) {
        content = shiny::tags$p("No questions created yet. Click 'Add Question' to start.")
      } else {
        content = shiny::tagList(
          lapply(current_questions, function(q) {
            shiny::div(
              class = "question-item",
              style = "border: 1px solid #ccc; margin-bottom: 10px; padding: 10px; background-color: white;",
              shiny::h5(paste("Question", q$id)),
              shiny::textInput(
                session$ns(paste0("question_name_", q$id)),
                "Question Name:",
                value = q$name,
                placeholder = "Enter question description..."
              ),
              shiny::p(shiny::strong("Selected nodes:"), 
                if (length(q$selected_nodes) == 0) "None" else paste(q$selected_nodes, collapse = ", ")
              ),
              shiny::actionButton(
                session$ns(paste0("select_nodes_", q$id)),
                "Select Nodes",
                class = "btn-sm btn-outline-primary"
              )
            )
          })
        )
      }
      
      shiny::removeUI(selector = paste0("#", session$ns("questions_container"), " > *"))
      shiny::insertUI(
        selector = paste0("#", session$ns("questions_container")),
        ui = content
      )
    }
    
    # Return reactive values for parent module
    return(list(
      questions = questions,
      selected_question = selected_question
    ))
  })
}