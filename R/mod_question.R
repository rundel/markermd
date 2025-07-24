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
question_server = function(id, ast) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Validate AST in a reactive context (disabled for testing)
    # shiny::observe({
    #   if (!is.null(ast())) {
    #     stopifnot(S7::S7_inherits(ast(), parsermd::rmd_ast))
    #   }
    # })

    # Question state
    state = shiny::reactiveVal({
      markermd_question(1L, "default", markermd_node_selection(), list())
    })
    
    # Render selected nodes display
    output$selected_nodes_display = shiny::renderUI({
      nodes = state()@selected_nodes@indices
      if (length(nodes) == 0) {
        shiny::span("None", style = "color: #6c757d;")
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
          style = "color: #28a745;"
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
    
    # Add rule button observer
    shiny::observe({
      rule_id = next_rule_id()
      
      # Create new rule with default values
      new_rule = new_markermd_rule()
      
      # Add rule to rules list
      current_rules = rules_list()
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
    
    # Handle rule deletion using a different approach - single observer watching all inputs
    delete_observers = shiny::reactiveVal(list())
    
    # Single observer that checks reactively for delete clicks
    shiny::observe({
      current_rules = rules_list()
      
      # Get all input values as a reactive dependency
      all_inputs = shiny::reactiveValuesToList(input)
      
      # Check each rule's delete button (with proper namespacing)
      for (rule_id in names(current_rules)) {
        delete_input_id = paste0("rule_", rule_id, "-delete")
        delete_value = all_inputs[[delete_input_id]]
        
        if (!is.null(delete_value) && delete_value > 0) {
          # Remove rule from rules list
          current_rules[[rule_id]] = NULL
          rules_list(current_rules)
          
          # Update question state
          cur_state = state()
          cur_state@rules = current_rules
          state(cur_state)
          
          break  # Only handle one deletion at a time
        }
      }
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
        
        # Check if anything changed
        if (rule@node_type != new_node_type || rule@verb != new_verb) {
          # Determine new values
          new_values = if (!is.null(input[[values_input]])) {
            values_value = input[[values_input]]
            
            # Validate the values for the new verb
            if (is.null(validate_rule_values(new_verb, values_value))) {
              values_value
            } else {
              get_default_rule_values(new_verb)
            }
          } else {
            get_default_rule_values(new_verb)
          }
          
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
        shiny::span("None", style = "color: #6c757d;")
      } else {
        shiny::span(
          paste0("(", rule_count, " rule", if (rule_count != 1) "s" else "", ")"),
          style = "color: #28a745;"
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
