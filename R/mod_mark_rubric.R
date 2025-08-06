#' Mark Rubric Interface Module
#'
#' Shiny module for displaying and navigating rubric questions during marking

#' Mark Rubric UI
#'
#' @param id Character. Module namespace ID
#'
mark_rubric_ui = function(id) {
  ns = shiny::NS(id)
  
  bslib::card(
    class = "h-100",
    bslib::card_header(
      class = "bg-light",
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        shiny::span("Rubric"),
        shiny::div(
          style = "min-width: 150px;",
          shiny::selectInput(
            ns("question_select"),
            NULL,
            choices = NULL,
            width = "100%",
            selectize = TRUE
          )
        )
      )
    ),
    bslib::card_body(
      class = "overflow-auto small",
      shiny::uiOutput(ns("rubric_display"))
    )
  )
}

#' Mark Rubric Server
#'
#' @param id Character. Module namespace ID
#' @param template Reactive. Template object containing questions
#' @param on_question_change Reactive function. Callback when question selection changes
#'
mark_rubric_server = function(id, template, on_question_change = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Populate select input with template questions
    shiny::observe({
      req(template())
      
      question_names = sapply(template()@questions, function(q) q@name)
      choices = setNames(question_names, question_names)
      shiny::updateSelectInput(session, "question_select", choices = choices)
    })
    
    # Rubric items display
    output$rubric_display = shiny::renderUI({
      req(template())
      ns = session$ns
      
      shiny::div(
        mark_rubric_item_ui(
          ns("item1"), 
          markermd_rubric_item(
            hotkey = 1L,
            points = 5.0,
            description = "Correctly implements the main logic with proper variable naming",
            selected = FALSE
          )
        ),
        mark_rubric_item_ui(
          ns("item2"), 
          markermd_rubric_item(
            hotkey = 2L,
            points = 3.0,
            description = "Includes appropriate comments explaining the solution",
            selected = FALSE
          )
        ),
        mark_rubric_item_ui(
          ns("item3"), 
          markermd_rubric_item(
            hotkey = 3L,
            points = -2.0,
            description = "Code contains syntax errors or runtime issues",
            selected = FALSE
          )
        )
      )
    })
    
    # Initialize server components for the hardcoded rubric items
    item1_reactive = mark_rubric_item_server(
      "item1", 
      markermd_rubric_item(
        hotkey = 1L,
        points = 5.0,
        description = "Correctly implements the main logic with proper variable naming",
        selected = FALSE
      ),
      function(updated_item) {
        cat("Item 1 selection changed:", updated_item@selected, "\n")
      }
    )
    
    item2_reactive = mark_rubric_item_server(
      "item2", 
      markermd_rubric_item(
        hotkey = 2L,
        points = 3.0,
        description = "Includes appropriate comments explaining the solution",
        selected = FALSE
      ),
      function(updated_item) {
        cat("Item 2 selection changed:", updated_item@selected, "\n")
      }
    )
    
    item3_reactive = mark_rubric_item_server(
      "item3", 
      markermd_rubric_item(
        hotkey = 3L,
        points = -2.0,
        description = "Code contains syntax errors or runtime issues",
        selected = FALSE
      ),
      function(updated_item) {
        cat("Item 3 selection changed:", updated_item@selected, "\n")
      }
    )
    
    # Notify when question selection changes
    shiny::observe({
      req(input$question_select)
      if (!is.null(on_question_change)) {
        on_question_change(input$question_select)
      }
    })
    
    # Return reactive values for external use
    return(list(
      selected_question = shiny::reactive(input$question_select),
      item1 = item1_reactive,
      item2 = item2_reactive,
      item3 = item3_reactive
    ))
  })
}