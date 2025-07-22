#' Rules Module
#'
#' Shiny module for creating and managing validation rules for questions

#' Rules UI
#'
#' @param id Character. Module namespace ID
#'
rules_ui = function(id) {
  ns = shiny::NS(id)
  
  shiny::tagList(
    # CSS to override Shiny input margins and scale down elements
    shiny::tags$style(shiny::HTML(glue::glue("
      #<<ns('rules_container')>> .form-group {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
        font-size: 12px;
      }
      #<<ns('rules_container')>> .shiny-input-container {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
      #<<ns('rules_container')>> .rule-item {
        font-size: 12px;
      }
      #<<ns('rules_container')>> .btn {
        font-size: 10px;
        padding: 1px 3px;
        height: 24px;
      }
      #<<ns('rules_container')>> select,
      #<<ns('rules_container')>> input[type='text'],
      #<<ns('rules_container')>> input[type='number'] {
        font-size: 11px !important;
        height: 26px !important;
        padding: 2px 6px !important;
        line-height: 1.2 !important;
      }
      #<<ns('rules_container')>> .selectize-input {
        font-size: 11px !important;
        min-height: 26px !important;
        padding: 2px 6px !important;
        line-height: 1.2 !important;
      }
      #<<ns('rules_container')>> .selectize-input.items {
        padding-top: 2px !important;
        padding-bottom: 2px !important;
      }
      #<<ns('rules_container')>> .selectize-input > input {
        font-size: 11px !important;
        height: 22px !important;
      }
      #<<ns('rules_container')>> .numeric-range-input .form-control {
        font-size: 11px !important;
        height: 26px !important;
        padding: 2px 6px !important;
      }
      #<<ns('rules_container')>> .numeric-range-input .input-group-text {
        font-size: 11px !important;
        padding: 2px 6px !important;
        height: 26px !important;
        line-height: 1.2 !important;
      }
      #<<ns('rules_container')>> .input-group-text {
        font-size: 11px !important;
        padding: 2px 6px !important;
        height: 26px !important;
      }
      /* High z-index for all select dropdowns */
      #<<ns('rules_container')>> .selectize-dropdown,
      #<<ns('rules_container')>> .bootstrap-select .dropdown-menu {
        z-index: 99999 !important;
        position: absolute !important;
        font-size: 11px !important;
      }
      #<<ns('rules_container')>> .selectize-dropdown .option,
      #<<ns('rules_container')>> .selectize-dropdown .optgroup-header,
      #<<ns('rules_container')>> .bootstrap-select .dropdown-item {
        font-size: 11px !important;
        padding: 2px 6px !important;
        line-height: 1.2 !important;
      }
      
      /* Base z-index for select containers */
      #<<ns('rules_container')>> .selectize-control,
      #<<ns('rules_container')>> .shiny-input-container,
      #<<ns('rules_container')>> select {
        position: relative !important;
        z-index: auto !important;
      }
      
      /* Boost z-index for active/focused selects */
      #<<ns('rules_container')>> .selectize-control.focus,
      #<<ns('rules_container')>> .selectize-control.dropdown-active,
      #<<ns('rules_container')>> .shiny-input-container:has(select:focus),
      #<<ns('rules_container')>> .shiny-input-container:focus-within {
        z-index: 100000 !important;
        position: relative !important;
      }
      
      /* Ensure rule items have low z-index when not active */
      #<<ns('rules_container')>> .rule-item {
        position: relative !important;
        z-index: 1 !important;
      }
      
      /* When a rule item contains an active select, boost its z-index */
      #<<ns('rules_container')>> .rule-item:has(.selectize-control.dropdown-active),
      #<<ns('rules_container')>> .rule-item:has(.shiny-input-container:focus-within) {
        z-index: 100000 !important;
      }
      
      /* Override any transform that might create stacking context */
      #<<ns('rules_container')>> * {
        transform: none !important;
      }
      /* Override card overflow to allow dropdowns to escape */
      .card-body {
        overflow: visible !important;
      }
      .card {
        overflow: visible !important;
      }
      /* Ensure all containers allow overflow */
      #<<ns('rules_container')>> {
        overflow: visible !important;
      }
      .rule-item {
        overflow: visible !important;
      }
    ", .open = "<<", .close = ">>"))),
    
    shiny::div(
      id = ns("rules_container"),
      # Rules list container
      shiny::div(
        id = ns("rules_list"),
        shiny::uiOutput(ns("rules_ui"))
      )
    )
  )
}

#' Rules Server
#'
#' @param id Character. Module namespace ID
#' @param question_id Reactive. The current question ID
#' @param add_rule_trigger Reactive. External trigger for adding rules
#' @param initial_rules List. Initial rules to load (optional)
#'
rules_server = function(id, question_id, add_rule_trigger = NULL, initial_rules = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # State management for rules
    rules_state = shiny::reactiveVal(list())
    rule_counter = shiny::reactiveVal(0)
    
    # Helper function to create delete observer for a rule (defined early for scoping)
    create_rule_delete_observer = function(rule_id) {
      shiny::observeEvent(input[[paste0("delete_rule_", rule_id)]], {
        current_rules = rules_state()
        updated_rules = current_rules[sapply(current_rules, function(r) r$id != rule_id)]
        rules_state(updated_rules)
      })
    }
    
    # Initialize with provided rules if any
    if (!is.null(initial_rules) && length(initial_rules) > 0) {
      # Set up initial rules with delete observers
      initialized_rules = lapply(initial_rules, function(rule) {
        # Validate rule structure
        if (!is.list(rule) || is.null(rule$id)) {
          warning("Invalid rule structure, skipping")
          return(NULL)
        }
        
        # Create delete observer for this rule
        delete_observer = create_rule_delete_observer(rule$id)
        rule$delete_observer = delete_observer
        rule
      })
      
      # Remove any NULL entries from failed rules
      initialized_rules = initialized_rules[!sapply(initialized_rules, is.null)]
      
      if (length(initialized_rules) > 0) {
        rules_state(initialized_rules)
        
        # Set rule counter to max ID from initial rules
        max_id = max(sapply(initialized_rules, function(r) r$id))
        rule_counter(max_id)
      }
    }
    
    # Get allowed values from helper functions
    available_node_types = get_allowed_node_types()
    available_verbs = get_allowed_rule_verbs()
    
    # Add new rule - use external trigger if provided, otherwise use internal button
    if (!is.null(add_rule_trigger)) {
      # Use external trigger
      shiny::observeEvent(add_rule_trigger(), ignoreInit = TRUE, {
        shiny::req(add_rule_trigger() > 0)
        current_rules = rules_state()
        rule_counter(rule_counter() + 1)
        new_rule_id = rule_counter()
        
        # Create delete observer for this rule
        delete_observer = create_rule_delete_observer(new_rule_id)
        
        new_rule = list(
          id = new_rule_id,
          node_types = available_node_types[1],  # First option is "Any node"
          verb = available_verbs[1],             # First option is "has count of"
          verb_inputs = list(),
          values = get_default_rule_values(available_verbs[1]),
          delete_observer = delete_observer
        )
        
        updated_rules = c(current_rules, list(new_rule))
        rules_state(updated_rules)
    })
    } else {
      # Use internal button
      shiny::observeEvent(input$add_rule, {
        current_rules = rules_state()
        rule_counter(rule_counter() + 1)
        new_rule_id = rule_counter()
        
        # Create delete observer for this rule
        delete_observer = create_rule_delete_observer(new_rule_id)
        
        new_rule = list(
          id = new_rule_id,
          node_types = available_node_types[1],  # First option is "Any node"
          verb = available_verbs[1],             # First option is "has count of"
          verb_inputs = list(),
          values = get_default_rule_values(available_verbs[1]),
          delete_observer = delete_observer
        )
        
        updated_rules = c(current_rules, list(new_rule))
        rules_state(updated_rules)
      })
    }
    
    # Render rules UI
    output$rules_ui = shiny::renderUI({
      current_rules = rules_state()
      
      if (length(current_rules) == 0) {
        return(NULL)
      }
      
      rule_items = lapply(current_rules, function(rule) {
        create_rule_ui(rule, session$ns, available_node_types, available_verbs)
      })
      
      shiny::tagList(rule_items)
    })
    
    # Handle node type changes for all rules
    shiny::observe({
      current_rules = rules_state()
      for (rule in current_rules) {
        local({
          rule_id = rule$id
          shiny::observeEvent(input[[paste0("rule_node_types_", rule_id)]], {
            current_rules = isolate(rules_state())
            rule_idx = which(sapply(current_rules, function(r) r$id == rule_id))
            if (length(rule_idx) > 0) {
              current_rules[[rule_idx]]$node_types = input[[paste0("rule_node_types_", rule_id)]] %||% character(0)
              isolate(rules_state(current_rules))
            }
          })
        })
      }
    })
    
    # Handle verb changes for all rules
    shiny::observe({
      current_rules = rules_state()
      for (rule in current_rules) {
        local({
          rule_id = rule$id
          shiny::observeEvent(input[[paste0("rule_verb_", rule_id)]], {
            current_rules = isolate(rules_state())
            rule_idx = which(sapply(current_rules, function(r) r$id == rule_id))
            if (length(rule_idx) > 0) {
              new_verb = input[[paste0("rule_verb_", rule_id)]]
              current_rules[[rule_idx]]$verb = new_verb
              # Reset verb inputs and values when verb changes
              current_rules[[rule_idx]]$verb_inputs = list()
              current_rules[[rule_idx]]$values = get_default_rule_values(new_verb)
              isolate(rules_state(current_rules))
            }
          })
        })
      }
    })
    
    # Track created UI renderers to prevent re-creation on value changes  
    created_ui_renderers = shiny::reactiveValues()
    
    # Render dynamic verb inputs for all rules (only create once per rule)
    shiny::observe({
      current_rules = rules_state()  # Keep reactive to see new rules
      for (rule in current_rules) {
        rule_id = rule$id
        
        # Only create UI renderer once per rule
        if (is.null(created_ui_renderers[[paste0("ui_", rule_id)]])) {
          local({
            local_rule_id = rule_id
            output[[paste0("rule_verb_inputs_", local_rule_id)]] = shiny::renderUI({
              # Get current rule state for verb and values
              current_rules = isolate(rules_state())
              current_rule = NULL
              for (r in current_rules) {
                if (r$id == local_rule_id) {
                  current_rule = r
                  break
                }
              }
              
              if (!is.null(current_rule)) {
                current_verb = input[[paste0("rule_verb_", local_rule_id)]] %||% current_rule$verb
                create_verb_inputs_ui(current_verb, local_rule_id, session$ns, current_rule)
              } else {
                NULL
              }
            })
            
            # Mark UI renderer as created
            created_ui_renderers[[paste0("ui_", local_rule_id)]] = TRUE
          })
        }
      }
    })
    
    # Track created value input observers to prevent infinite loops
    created_value_observers = shiny::reactiveValues()
    
    # Create value input observers separately from structural changes
    shiny::observe({
      current_rules = rules_state()
      for (rule in current_rules) {
        rule_id = rule$id
        
        # Only create value observers once per rule
        if (is.null(created_value_observers[[paste0("values_", rule_id)]])) {
          local({
            local_rule_id = rule_id
            
            # Create observers for all possible value input types
            # Count range observer
            shiny::observeEvent(input[[paste0("rule_count_range_", local_rule_id)]], {
              current_rules = isolate(rules_state())
              rule_idx = which(sapply(current_rules, function(r) r$id == local_rule_id))
              if (length(rule_idx) > 0) {
                current_rules[[rule_idx]]$values = input[[paste0("rule_count_range_", local_rule_id)]]
                isolate(rules_state(current_rules))
              }
            })
            
            # Content pattern observer
            shiny::observeEvent(input[[paste0("rule_content_pattern_", local_rule_id)]], {
              current_rules = isolate(rules_state())
              rule_idx = which(sapply(current_rules, function(r) r$id == local_rule_id))
              if (length(rule_idx) > 0) {
                current_rules[[rule_idx]]$values = input[[paste0("rule_content_pattern_", local_rule_id)]]
                isolate(rules_state(current_rules))
              }
            })
            
            # Name pattern observer
            shiny::observeEvent(input[[paste0("rule_name_pattern_", local_rule_id)]], {
              current_rules = isolate(rules_state())
              rule_idx = which(sapply(current_rules, function(r) r$id == local_rule_id))
              if (length(rule_idx) > 0) {
                current_rules[[rule_idx]]$values = input[[paste0("rule_name_pattern_", local_rule_id)]]
                isolate(rules_state(current_rules))
              }
            })
          })
          
          # Mark value observers as created for this rule
          created_value_observers[[paste0("values_", rule_id)]] = TRUE
        }
      }
    })
    
    # Return reactive rules list
    return(shiny::reactive({
      rules_state()
    }))
  })
}

#' Create Rule UI
#'
#' Helper function to create UI for a single rule
#'
#' @param rule List containing rule data
#' @param ns Namespace function
#' @param available_node_types Vector of available node types
#' @param available_verbs Vector of available verbs
#'
create_rule_ui = function(rule, ns, available_node_types, available_verbs) {
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
          ns(paste0("rule_node_types_", rule$id)),
          NULL,
          choices = available_node_types,
          selected = rule$node_types,
          multiple = FALSE,
          width = "100%"
        )
      ),
      
      # Verb selection
      shiny::div(
        style = "flex: 0 0 25%;",
        shiny::selectInput(
          ns(paste0("rule_verb_", rule$id)),
          NULL,
          choices = available_verbs,
          selected = rule$verb,
          width = "100%"
        )
      ),
      
      # Dynamic verb inputs
      shiny::div(
        style = "flex: 1;",
        shiny::uiOutput(ns(paste0("rule_verb_inputs_", rule$id)))
      ),
      
      # Delete button
      shiny::div(
        style = "flex: 0 0 auto;",
        shiny::actionButton(
          ns(paste0("delete_rule_", rule$id)),
          shiny::icon("trash-alt"),
          class = "btn-outline-danger btn-sm",
          style = "font-size: 10px; padding: 0; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center;",
          title = "Delete Rule"
        )
      )
    )
  )
}

#' Create Verb Inputs UI
#'
#' Helper function to create dynamic inputs based on selected verb
#'
#' @param verb Character. The selected verb
#' @param rule_id Numeric. The rule ID
#' @param ns Namespace function
#'
create_verb_inputs_ui = function(verb, rule_id, ns, rule = NULL) {
  
  if (is.null(verb) || verb == "") {
    return(NULL)
  }
  
  # Get default values from rule if available
  rule_values = if (!is.null(rule) && !is.null(rule$values)) {
    rule$values
  } else {
    get_default_rule_values(verb)
  }
  
  switch(verb,
    "has count of" = {
      # Ensure we have numeric values for count
      count_values = if (is.numeric(rule_values) && length(rule_values) == 2) {
        rule_values
      } else {
        c(0, 10)
      }
      
      shinyWidgets::numericRangeInput(
        ns(paste0("rule_count_range_", rule_id)),
        NULL,
        value = count_values,
        min = 0,
        max = 100,
        step = 1,
        width = "100%"
      )
    },
    
    "has content" = {
      # Ensure we have character value for content
      content_value = if (is.character(rule_values) && length(rule_values) == 1) {
        rule_values
      } else {
        ""
      }
      
      shiny::textInput(
        ns(paste0("rule_content_pattern_", rule_id)),
        NULL,
        value = content_value,
        width = "100%"
      )
    },
    
    "does not have content" = {
      # Ensure we have character value for content
      content_value = if (is.character(rule_values) && length(rule_values) == 1) {
        rule_values
      } else {
        ""
      }
      
      shiny::textInput(
        ns(paste0("rule_content_pattern_", rule_id)),
        NULL,
        value = content_value,
        width = "100%"
      )
    },
    
    "has name" = {
      # Ensure we have character value for name
      name_value = if (is.character(rule_values) && length(rule_values) == 1) {
        rule_values
      } else {
        ""
      }
      
      shiny::textInput(
        ns(paste0("rule_name_pattern_", rule_id)),
        NULL,
        value = name_value,
        width = "100%"
      )
    },
    
    # Default case
    NULL
  )
}