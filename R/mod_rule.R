#' Rule Module
#'
#' Shiny module for managing a validation rule

#' Rule UI
#'
#' @param id Character. Module namespace ID
#'
rule_ui = function(id) {
  ns = shiny::NS(id)
  
  shiny::div(
    class = "rule-item",
    style = "margin: 2px 0; padding: 2px 8px;",
    
    # Rule configuration with flexbox layout
    shiny::div(
      style = "display: flex; align-items: center; gap: 4px; width: 100%;",
      
      # Node types selection
      shiny::div(
        style = "flex: 0 0 35%;",
        shiny::selectInput(
          ns("node_types"),
          NULL,
          choices = get_allowed_node_types(),
          selected = get_allowed_node_types()[1],
          multiple = FALSE,
          width = "100%"
        )
      ),
      
      # Verb selection
      shiny::div(
        style = "flex: 0 0 25%;",
        shiny::selectInput(
          ns("verb"),
          NULL,
          choices = get_allowed_rule_verbs(),
          selected = get_allowed_rule_verbs()[1],
          width = "100%"
        )
      ),
      
      # Dynamic verb inputs
      shiny::div(
        style = "flex: 1;",
        shiny::uiOutput(
          ns("verb_inputs")
        )
      ),
      
      # Delete button
      shiny::div(
        style = "flex: 0 0 auto;",
        shiny::actionButton(
          ns("delete"),
          shiny::icon("trash-alt"),
          class = "btn-outline-danger btn-sm",
          style = "font-size: 10px; padding: 0; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center;",
          title = "Delete Rule"
        )
      )
    )
  )
}

#' Rule Server
#'
#' @param id Character. Module namespace ID
#' @param initial_rule List. Initial rule data (optional)
#'
rule_server = function(id, initial_rule = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive for current rule values
    state = shiny::reactiveVal({
      node = get_allowed_node_types()[1]
      verb = get_allowed_rule_verbs()[1]
      value = get_default_rule_values(verb)

      markermd_rule(node, verb, value)
    })


    # Render dynamic verb inputs
    output$verb_inputs = shiny::renderUI({
      create_rule_verb_inputs(state()@verb, state()@values, session$ns)
    }) |>
      bindEvent(input$verb)
    
    # Handle verb changes - reset values when verb changes
    shiny::observe({
      current_state = state()
      current_state@verb = input$verb
      current_state@values = get_default_rule_values(input$verb)
      state(current_state)
    }) |>
      bindEvent(input$verb)
    
    # Handle value input changes for different verb types
    shiny::observe({
      current_state = state()
      current_state@values = input$values
      state(current_state)
    }) |>
      bindEvent(input$values)
    
    # Return reactive rule data and delete signal
    return(list(
      rule = shiny::reactive({
        list(
          node_types = input$node_types,
          verb = input$verb,
          values = input$values
        )
      }),
      delete_clicked = shiny::reactive({
        input$delete
      })
    ))
  })
}

create_rule_verb_inputs = function(verb, value, ns) {
  
  
  switch(verb,
    "has count of" = {
      shinyWidgets::numericRangeInput(
        ns("values"),
        NULL,
        value = value,
        min = 0,
        max = 100,
        step = 1,
        width = "100%"
      )
    },
    
    "has content" = {
      shiny::textInput(
        ns("values"),
        NULL,
        value = value,
        width = "100%"
      )
    },
    
    "does not have content" = {
      shiny::textInput(
        ns("values"),
        NULL,
        value = value,
        width = "100%"
      )
    },
    
    "has name" = {
      shiny::textInput(
        ns("values"),
        NULL,
        value = value,
        width = "100%"
      )
    },

    # Default case
    NULL
  )
}