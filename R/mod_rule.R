#' Rule Module
#'
#' Shiny module for managing a validation rule

#' Rule UI
#'
#' @param id Character. Module namespace ID
#' @param initial_rule markermd_rule object. Initial rule for setting default values (optional)
#'
rule_ui = function(id, initial_rule = NULL) {
  ns = shiny::NS(id)
  
  # Extract initial values if rule provided
  if (!is.null(initial_rule) && S7::S7_inherits(initial_rule, markermd_rule)) {
    initial_node_type = initial_rule@node_type
    initial_verb = initial_rule@verb
  } else {
    initial_node_type = get_allowed_node_types()[1]
    initial_verb = get_allowed_rule_verbs()[1]
  }
  
  shiny::div(
    class = "rule-item",
    style = "margin: 2px 0; padding: 2px 8px; font-size: 12px;",
    
    # Rule configuration with flexbox layout
    shiny::div(
      style = "display: flex; align-items: center; gap: 4px; width: 100%; position: relative; z-index: 1000;",
      
      # Node types selection
      shiny::div(
        style = "flex: 0 0 35%; position: relative;",
        shiny::selectInput(
          ns("node_types"),
          NULL,
          choices = get_allowed_node_types(),
          selected = initial_node_type,
          multiple = FALSE,
          width = "100%",
          selectize = FALSE
        )
      ),
      
      # Verb selection
      shiny::div(
        style = "flex: 0 0 25%; position: relative;",
        shiny::selectInput(
          ns("verb"),
          NULL,
          choices = get_allowed_rule_verbs(),
          selected = initial_verb,
          width = "100%",
          selectize = FALSE
        )
      ),
      
      # Dynamic verb inputs
      shiny::div(
        style = "flex: 1; position: relative;",
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

    # Rule state using S7 object
    state = shiny::reactiveVal({
      if (!is.null(initial_rule) && S7::S7_inherits(initial_rule, markermd_rule)) {
        initial_rule
      } else {
        new_markermd_rule()
      }
    })


    # Render dynamic verb inputs - trigger on state changes
    output$verb_inputs = shiny::renderUI({
      current_state = state()
      create_rule_verb_inputs(current_state@verb, current_state@values, session$ns)
    })
    
    # Handle node type changes
    shiny::observe({
      req(input$node_types)
      current_state = state()
      current_state@node_type = input$node_types
      state(current_state)
    }) |>
      bindEvent(input$node_types)
    
    # Handle verb changes - reset values when verb changes
    shiny::observe({
      req(input$verb)
      current_state = state()
      current_state@verb = input$verb
      current_state@values = get_default_rule_values(input$verb)
      state(current_state)
    }) |>
      bindEvent(input$verb)
    
    # Handle value input changes for different verb types
    shiny::observe({
      req(input$values)
      current_state = state()
      current_state@values = input$values
      state(current_state)
    }) |>
      bindEvent(input$values)
    
    # Return reactive rule data and delete signal
    return(list(
      rule = shiny::reactive({
        state()
      }),
      delete_clicked = shiny::reactive({
        input$delete
      })
    ))
  })
}

create_rule_verb_inputs = function(verb, value, ns) {
  
  # Wrap all inputs in a div with smaller text styling
  input_wrapper = function(input_element) {
    shiny::div(
      style = "font-size: 12px;",
      input_element
    )
  }
  
  switch(verb,
    "has between" = {
      input_wrapper(
        shinyWidgets::numericRangeInput(
          ns("values"),
          NULL,
          value = value,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        )
      )
    },
    
    "has at least" = ,
    "has at most" = {
      input_wrapper(
        shiny::numericInput(
          ns("values"),
          NULL,
          value = value,
          min = 0,
          max = 1000,
          step = 1,
          width = "100%"
        )
      )
    },
    
    "has content" = {
      input_wrapper(
        shiny::textInput(
          ns("values"),
          NULL,
          value = value,
          width = "100%"
        )
      )
    },
    
    "lacks content" = {
      input_wrapper(
        shiny::textInput(
          ns("values"),
          NULL,
          value = value,
          width = "100%"
        )
      )
    },
    
    "has name" = {
      input_wrapper(
        shiny::textInput(
          ns("values"),
          NULL,
          value = value,
          width = "100%"
        )
      )
    },

    # Default case
    NULL
  )
}

#' Rule UI without server module (direct input handling)
#'
#' @param id Character. The rule ID for input naming
#' @param rule markermd_rule S7 object. The rule to display
#' @param ns Function. Namespace function for proper input scoping
#'
rule_ui_direct = function(id, rule, ns = NULL) {
  # Use namespace function if provided, otherwise identity
  ns_func = if (is.null(ns)) function(x) x else ns
  
  shiny::div(
    class = "rule-item",
    style = "margin: 2px 0; padding: 2px 8px; font-size: 12px;",
    
    # Rule configuration with flexbox layout
    shiny::div(
      style = "display: flex; align-items: center; gap: 4px; width: 100%; position: relative; z-index: 1000;",
      
      # Node types selection
      shiny::div(
        style = "flex: 0 0 35%; position: relative;",
        shiny::selectInput(
          ns_func(paste0(id, "-node_types")),
          NULL,
          choices = get_allowed_node_types(),
          selected = rule@node_type,
          multiple = FALSE,
          width = "100%",
          selectize = FALSE
        )
      ),
      
      # Verb selection
      shiny::div(
        style = "flex: 0 0 25%; position: relative;",
        shiny::selectInput(
          ns_func(paste0(id, "-verb")),
          NULL,
          choices = get_allowed_rule_verbs(),
          selected = rule@verb,
          width = "100%",
          selectize = FALSE
        )
      ),
      
      # Dynamic verb inputs - render statically for now
      shiny::div(
        style = "flex: 1; position: relative;",
        create_rule_verb_inputs_direct(rule@verb, rule@values, id, ns_func)
      ),
      
      # Delete button
      shiny::div(
        style = "flex: 0 0 auto;",
        shiny::actionButton(
          ns_func(paste0(id, "-delete")),
          shiny::icon("trash-alt"),
          class = "btn-outline-danger btn-sm",
          style = "font-size: 10px; padding: 0; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center;",
          title = "Delete Rule"
        )
      )
    )
  )
}

#' Create rule verb inputs directly (without reactive UI)
#'
#' @param verb Character. The rule verb
#' @param value Default values for the verb
#' @param id Character. The rule ID for input naming
#' @param ns_func Function. Namespace function for proper input scoping
#'
create_rule_verb_inputs_direct = function(verb, value, id, ns_func = NULL) {
  # Use namespace function if provided, otherwise identity
  if (is.null(ns_func)) ns_func = function(x) x
  
  # Wrap all inputs in a div with smaller text styling
  input_wrapper = function(input_element) {
    shiny::div(
      style = "font-size: 12px;",
      input_element
    )
  }
  
  switch(verb,
    "has between" = {
      input_wrapper(
        shinyWidgets::numericRangeInput(
          ns_func(paste0(id, "-values")),
          NULL,
          value = value,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        )
      )
    },
    
    "has at least" = ,
    "has at most" = {
      input_wrapper(
        shiny::numericInput(
          ns_func(paste0(id, "-values")),
          NULL,
          value = value,
          min = 0,
          max = 1000,
          step = 1,
          width = "100%"
        )
      )
    },
    
    "has content" = {
      input_wrapper(
        shiny::textInput(
          ns_func(paste0(id, "-values")),
          NULL,
          value = value,
          width = "100%"
        )
      )
    },
    
    "lacks content" = {
      input_wrapper(
        shiny::textInput(
          ns_func(paste0(id, "-values")),
          NULL,
          value = value,
          width = "100%"
        )
      )
    },
    
    "has name" = {
      input_wrapper(
        shiny::textInput(
          ns_func(paste0(id, "-values")),
          NULL,
          value = value,
          width = "100%"
        )
      )
    },
    
    # Default case
    NULL
  )
}