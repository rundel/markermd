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
      shiny::div(
        id = ns("rubric_items_container"),
        # Dynamic container for items
        shiny::uiOutput(ns("rubric_items_ui"))
      ),
      shiny::div(
        class = "text-center mb-3",
        shiny::actionButton(
          ns("add_item"), 
          shiny::icon("plus"),
          class = "btn-primary btn-sm rounded-circle",
          style = "width: 30px; height: 30px;",
          title = "Add Question"
        ),
        shiny::span("Add Item", class = "ms-2 text-dark")
      )
    )
  )
}

#' Mark Rubric Server
#'
#' @param id Character. Module namespace ID
#' @param template markermd_template. Static template object containing questions
#' @param on_question_change Reactive function. Callback when question selection changes
#'
mark_rubric_server = function(id, template, on_question_change = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Global id index
    id_idx = 0

    question_names = purrr::map_chr(template@questions, "name")

    question_item_servers = shiny::reactiveValues()
    for(name in question_names) {
      question_item_servers[[name]] = list()
    }

    redraw_ui = shiny::reactiveVal(0)

    shiny::observe({
      shiny::updateSelectInput(
        session, "question_select", 
        choices = question_names, selected = question_names[1]
      )
    })
    
    output$rubric_items_ui = shiny::renderUI({
      req(input$question_select)

      uis = lapply(question_item_servers[[input$question_select]], function(server) {
        mark_rubric_item_ui(session$ns(server$id), server$item())
      })
      
      return(shiny::tagList(uis))
    }) |>
      bindEvent(redraw_ui(), input$question_select)
    
    shiny::observe({
      on_question_change(input$question_select)
    }) |> bindEvent(input$question_select)

    # Handle add item button
    shiny::observe({
      server_id = paste0("item_", id_idx)

      hotkey = max( 0L, purrr::map_int(question_item_servers[[input$question_select]], ~ .x$item()@hotkey) )+1L
      hotkey = if (hotkey > 10) NA_integer_ else hotkey

      server = mark_rubric_item_server(
          server_id,
          markermd_rubric_item(hotkey, 0, "")
      )
      question_item_servers[[input$question_select]][[server_id]] = server

      shiny::observe({
        question_item_servers[[input$question_select]][[server$id]] = NULL
      }) |>
        bindEvent(server$delete_signal(), ignoreInit = TRUE)

      id_idx <<- id_idx + 1
      redraw_ui(redraw_ui()+1)
    }) |>
      bindEvent(input$add_item, ignoreInit = TRUE)
    
    # Return reactive values for external use
    return(list(
      selected_question = shiny::reactive(input$question_select)
    ))
  })
}