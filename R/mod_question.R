#' Question Module
#'
#' Shiny module for managing a single question with its rules

#' Question UI
#'
#' @param id Character. Module namespace ID
#' @param name_id Integer. The question ID used for default naming
#'
question_ui = function(id, name_id) {
  ns = shiny::NS(id)
  
  bslib::card(
    style = "margin: 0; width: 100%; max-width: 100%; box-sizing: border-box;",
    bslib::card_header(
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        
        # Question name input
        shiny::div(
          style = "flex-grow: 1; margin-right: 10px;",
          shiny::textInput(
            ns("question_name"),
            NULL,
            value = paste("Question", name_id),
            width = "100%"
          )
        ),
        
        # Delete question button
        shiny::div(
          style = "flex-shrink: 0;",
          shiny::actionButton(
            ns("delete_question"),
            shiny::icon("times"),
            class = "btn-danger btn-sm",
            style = "font-size: 12px; padding: 2px 6px; border-radius: 20%; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; line-height: 1;",
            title = "Delete Question"
          )
        )
      )
    ),
    
    # Question content
    bslib::card_body(
      style = "padding: 12px;",
      
      # Selected nodes display
      shiny::div(
        style = "margin-bottom: 2px;",
        shiny::strong("Selected nodes: "),
        shiny::uiOutput(ns("selected_nodes_display"))
      ),
      
      # Rules section
      shiny::div(
        # Rules header with add button
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-top: -2px; margin-bottom: 8px;",
          shiny::div(
            shiny::strong("Validation rules: "),
            shiny::uiOutput(ns("rules_status"))
          ),
          shiny::div(
            style = "display: flex; align-items: center;",
            shiny::actionButton(
              ns("add_rule"),
              shiny::icon("plus"),
              class = "btn-outline-primary btn-sm",
              style = "font-size: 10px; padding: 2px 6px;",
              title = "Add Rule"
            ),
            shiny::span("Add Rule", style = "margin-left: 6px; font-size: 12px;")
          )
        ),
        
        # Rules container
        shiny::div(
          id = ns("rules_container"),
          style = "position: relative; z-index: 1; margin-bottom: 0;",
          shiny::uiOutput(ns("rules_ui"))
        )
      )
    )
  )
}

#' Question Server
#'
#' @param id Character. Module namespace ID
#' @param question_id Integer. The question ID
#' @param ast Reactive. The parsed AST object for building tree structure
#'
question_server = function(id, ast, initial_question = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Validate AST in a reactive context (disabled for testing)
    # shiny::observe({
    #   if (!is.null(ast())) {
    #     stopifnot(S7::S7_inherits(ast(), parsermd::rmd_ast))
    #   }
    # })

    # Question state - use initial_question if provided, otherwise default
    state = shiny::reactiveVal({
      if (!is.null(initial_question)) {
        initial_question
      } else {
        markermd_question(1L, "default", markermd_node_selection(), list())
      }
    })
    
    # Render selected nodes display
    output$selected_nodes_display = shiny::renderUI({
      nodes = state()@selected_nodes@indices
      if (length(nodes) == 0) {
        shiny::span("None", class = "text-muted")
      } else {
        tree_items = build_ast_tree_structure(ast())
        
        node_displays = sapply(nodes, function(node_index) {
          children = find_all_descendants(tree_items, node_index)
          if (length(children) > 0) {
            paste0(node_index, " [", paste(children, collapse = ","), "]")
          } else {
            as.character(node_index)
          }
        })
        
        shiny::span(
          paste(node_displays, collapse = ", "), 
          class = "text-success"
        )
      }
    })
        
    observe({
      req(input$question_name)
      cur_state = state()
      cur_state@name = input$question_name
      state(cur_state)
    }) |>
      bindEvent(input$question_name)
    
    # Rule management - simplified working approach
    rules_list = shiny::reactiveVal(list())
    next_rule_id = shiny::reactiveVal(1L)
    
    # Initialize rules_list from loaded question state
    shiny::observe({
      current_state = state()
      if (length(current_state@rules) > 0 && length(rules_list()) == 0) {
        # Convert S7 rules to the rules_list format
        loaded_rules = list()
        for (i in seq_along(current_state@rules)) {
          rule = current_state@rules[[i]]
          loaded_rules[[as.character(i)]] = rule
        }
        rules_list(loaded_rules)
        next_rule_id(length(loaded_rules) + 1L)
      }
    }, priority = 1000)  # High priority to run before other observers
    
    # Add rule button observer
    shiny::observe({
      rule_id = next_rule_id()
      
      
      # Before adding new rule, capture current input values for existing rules
      # This mirrors the question name handling approach
      current_rules = rules_list()
      for (existing_rule_id in names(current_rules)) {
        rule = current_rules[[existing_rule_id]]
        
        # Get current input values
        node_types_input = paste0("rule_", existing_rule_id, "-node_types")
        verb_input = paste0("rule_", existing_rule_id, "-verb") 
        values_input = paste0("rule_", existing_rule_id, "-values")
        
        # Update rule with current input values if available
        if (!is.null(input[[node_types_input]]) && !is.null(input[[verb_input]])) {
          final_node_type = input[[node_types_input]]
          final_verb = input[[verb_input]]
          final_values = if (!is.null(input[[values_input]])) {
            values_value = input[[values_input]]
            if (is.null(validate_rule_values(final_verb, values_value))) {
              values_value
            } else {
              get_default_rule_values(final_verb)
            }
          } else {
            get_default_rule_values(final_verb)
          }
          
          # Update the rule if anything changed
          if (rule@node_type != final_node_type || rule@verb != final_verb || !identical(rule@values, final_values)) {
            current_rules[[existing_rule_id]] = new_markermd_rule(
              node_type = final_node_type,
              verb = final_verb,
              values = final_values
            )
          }
        }
      }
      
      # Create new rule with default values
      new_rule = new_markermd_rule()
      
      # Add rule to rules list
      current_rules[[as.character(rule_id)]] = new_rule
      rules_list(current_rules)
      
      # Update question state
      cur_state = state()
      cur_state@rules = current_rules
      state(cur_state)
      
      # Increment rule ID for next rule
      next_rule_id(rule_id + 1L)
    }) |>
      bindEvent(input$add_rule)
    
    # Dynamic delete observer management
    delete_observers = shiny::reactiveVal(list())
    
    # Function to create a delete observer for a specific rule
    create_delete_observer = function(rule_id) {
      delete_input_id = paste0("rule_", rule_id, "-delete")
      
      observer = shiny::observeEvent(input[[delete_input_id]], {
        current_rules = rules_list()
        
        # Before deletion, capture current input values for all remaining rules
        preserved_rules = list()
        for (preserve_rule_id in names(current_rules)) {
          if (preserve_rule_id != rule_id) {  # Skip the rule being deleted
            rule = current_rules[[preserve_rule_id]]
            
            # Get current input values
            node_types_input = paste0("rule_", preserve_rule_id, "-node_types")
            verb_input = paste0("rule_", preserve_rule_id, "-verb")
            values_input = paste0("rule_", preserve_rule_id, "-values")
            
            # Use current input values if available, otherwise keep existing
            final_node_type = if (!is.null(input[[node_types_input]])) input[[node_types_input]] else rule@node_type
            final_verb = if (!is.null(input[[verb_input]])) input[[verb_input]] else rule@verb
            final_values = if (!is.null(input[[values_input]])) {
              values_value = input[[values_input]]
              if (is.null(validate_rule_values(final_verb, values_value))) {
                values_value
              } else {
                rule@values
              }
            } else {
              rule@values
            }
            
            # Create updated rule with current input values
            preserved_rules[[preserve_rule_id]] = new_markermd_rule(
              node_type = final_node_type,
              verb = final_verb,
              values = final_values
            )
          }
        }
        
        # Re-index preserved rules to maintain sequential numbering
        if (length(preserved_rules) > 0) {
          reindexed_rules = list()
          rule_objects = unname(preserved_rules)
          for (i in seq_along(rule_objects)) {
            reindexed_rules[[as.character(i)]] = rule_objects[[i]]
          }
          current_rules = reindexed_rules
        } else {
          current_rules = list()
        }
        
        rules_list(current_rules)
        
        # Update question state
        cur_state = state()
        cur_state@rules = current_rules
        state(cur_state)
        
        # Reset next rule ID for sequential numbering
        next_rule_id(length(current_rules) + 1L)
        
        # The monitor observer will handle creating new observers for the updated rules_list
        # No need for manual cleanup here since re-indexing changes rule IDs anyway
        
      }, ignoreInit = TRUE)
      
      return(observer)
    }
    
    # Monitor rules_list changes to create/destroy delete observers
    shiny::observe({
      current_rules = rules_list()
      current_observers = delete_observers()
      
      # Create observers for new rules
      new_observers = current_observers
      for (rule_id in names(current_rules)) {
        if (!rule_id %in% names(current_observers)) {
          new_observers[[rule_id]] = create_delete_observer(rule_id)
        }
      }
      
      # Remove observers for deleted rules (though this is handled in deletion logic too)
      for (obs_id in names(current_observers)) {
        if (!obs_id %in% names(current_rules)) {
          current_observers[[obs_id]]$destroy()
          new_observers[[obs_id]] = NULL
        }
      }
      
      delete_observers(new_observers)
    })
    
    # Handle rule input updates
    shiny::observe({
      current_rules = rules_list()
      rules_changed = FALSE
      
      for (rule_id in names(current_rules)) {
        node_types_input = paste0("rule_", rule_id, "-node_types")
        verb_input = paste0("rule_", rule_id, "-verb")
        values_input = paste0("rule_", rule_id, "-values")
        
        # Update rule if inputs have changed
        req(input[[node_types_input]], input[[verb_input]])
        
        rule = current_rules[[rule_id]]
        new_node_type = input[[node_types_input]]
        new_verb = input[[verb_input]]
        
        # Determine new values
        new_values = if (!is.null(input[[values_input]])) {
          values_value = input[[values_input]]
          
          # Validate the values for the current verb
          if (is.null(validate_rule_values(new_verb, values_value))) {
            values_value
          } else {
            get_default_rule_values(new_verb)
          }
        } else {
          get_default_rule_values(new_verb)
        }
        
        # Check if anything changed (including values)
        values_changed = !identical(rule@values, new_values)
        
        if (rule@node_type != new_node_type || rule@verb != new_verb || values_changed) {
          # Create new rule to avoid S7 validation issues
          new_rule = new_markermd_rule(
            node_type = new_node_type,
            verb = new_verb,
            values = new_values
          )
          
          current_rules[[rule_id]] = new_rule
          rules_changed = TRUE
        }
      }
      
      # Update reactive values if there were changes
      if (rules_changed) {
        rules_list(current_rules)
        
        # Update question state
        cur_state = state()
        cur_state@rules = current_rules
        state(cur_state)
      }
    })
    
    # Render rules UI
    output$rules_ui = shiny::renderUI({
      current_rules = rules_list()
      
      if (length(current_rules) == 0) {
        # Don't show any text when no rules are present
        NULL
      } else {
        rule_uis = lapply(names(current_rules), function(rule_id) {
          rule = current_rules[[rule_id]]
          rule_ui_direct(paste0("rule_", rule_id), rule, session$ns)
        })
        
        shiny::tagList(rule_uis)
      }
    })
    
    # Render rules status
    output$rules_status = shiny::renderUI({
      current_rules = rules_list()
      rule_count = length(current_rules)
      
      if (rule_count == 0) {
        shiny::span("None", class = "text-muted")
      } else {
        shiny::span(
          paste0("(", rule_count, " rule", if (rule_count != 1) "s" else "", ")"),
          class = "text-success"
        )
      }
    })
    
    # Handle verb changes - monitor verb inputs and update rules
    shiny::observe({
      current_rules = rules_list()
      rules_changed = FALSE
      
      # Get all current input values
      all_inputs = shiny::reactiveValuesToList(input)
      
      for (rule_id in names(current_rules)) {
        verb_input_id = paste0("rule_", rule_id, "-verb")
        current_verb_value = all_inputs[[verb_input_id]]
        
        
        req(current_verb_value)
        
        rule = current_rules[[rule_id]]
        if (rule@verb != current_verb_value) {
          # Verb changed - create new rule with updated values
          
          # Create completely new rule to avoid S7 validation issues
          new_rule = new_markermd_rule(
            node_type = rule@node_type,
            verb = current_verb_value,
            values = get_default_rule_values(current_verb_value)
          )
          
          current_rules[[rule_id]] = new_rule
          rules_changed = TRUE
        }
      }
      
      if (rules_changed) {
        rules_list(current_rules)
        
        # Update question state
        cur_state = state()
        cur_state@rules = current_rules
        state(cur_state)
      }
    })
    
    # Return reactive question data and methods
    return(list(
      # Reactive data
      question = shiny::reactive({
        state()
      }),
      
      # Node management methods
      add_node = function(node_index) {
        cur_state = state()
        cur_nodes = cur_state@selected_nodes@indices
        if (!node_index %in% cur_nodes) {
          cur_state@selected_nodes = markermd_node_selection(indices = sort(c(cur_nodes, node_index)))
          state(cur_state)
        }
      },

      remove_node = function(node_index) {
        cur_state = state()
        cur_state@selected_nodes = markermd_node_selection(indices = setdiff(cur_state@selected_nodes@indices, node_index))
        state(cur_state)
      },
    
      clear_nodes = function() {
        cur_state = state()
        cur_state@selected_nodes = markermd_node_selection(indices = integer())
        state(cur_state)
      },
        
      get_selected_nodes = shiny::reactive({
        state()@selected_nodes@indices
      }),
      
      delete_clicked = shiny::reactive({
        input$delete_question
      })
    ))
    })
}
