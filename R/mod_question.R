# Question Module
#
# Shiny module for managing a single question with its rules

# Question UI
#
# id: Character. Module namespace ID
# name_id: Integer. The question ID used for default naming

question_ui = function(id, name_id) {
  ns = shiny::NS(id)
  
  bslib::card(
    style = "margin: 0; width: 100%; max-width: 100%; box-sizing: border-box;",
    bslib::card_header(
      class = "bg-light",
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

# Question Server
#
# id: Character. Module namespace ID
# ast: Reactive. The parsed AST object for building tree structure
# initial_question: S7 markermd_question object. Optional initial question state

question_server = function(id, ast, initial_question = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Question state - use initial_question if provided, otherwise default
    state = shiny::reactiveVal({
      if (!is.null(initial_question)) {
        initial_question
      } else {
        markermd_question(1L, "default", markermd_node_selection(), list())
      }
    })
    
    # Memoised flattened tree. ast() is write-once, so this is built once and
    # reused instead of re-flattening the whole document on every selection change.
    question_tree_items = shiny::reactive({
      if (is.null(ast())) return(list())
      build_ast_tree_structure(ast())
    })

    # Render selected nodes display: each node-id selector followed by the
    # number of nodes it covers (the selected heading/div and its descendants).
    output$selected_nodes_display = shiny::renderUI({
      ids = state()@selected_nodes@node_ids
      if (length(ids) == 0) {
        shiny::span("None", class = "text-muted")
      } else {
        tree_items = question_tree_items()
        labels = vapply(ids, function(id) {
          n = length(compute_all_selected_nodes(tree_items, node_ids_to_indices(ast(), id)))
          paste0("#", id, " (", n, " node", if (n != 1) "s" else "", ")")
        }, character(1))

        shiny::span(paste(labels, collapse = ", "), class = "text-success")
      }
    })
        
    shiny::observe({
      shiny::req(input$question_name)
      cur_state = state()
      cur_state@name = input$question_name
      state(cur_state)
    }) |>
      shiny::bindEvent(input$question_name)
    
    # Rule management - simplified working approach
    rules_list = shiny::reactiveVal(list())
    next_rule_id = shiny::reactiveVal(1L)

    # rule_ids whose node-type multiselect has reported a (non-NULL) value at
    # least once. An empty selectize multiselect reports NULL, which is also the
    # not-yet-initialised state, so this lets capture_rule_inputs() tell a
    # deliberate clear (the catch-all "Any node") apart from a loaded rule whose
    # widget has not reported yet (keep its stored value). Bookkeeping only, so
    # reads/writes are isolated from reactivity.
    node_types_seen = shiny::reactiveVal(character(0))
    note_node_types_seen = function(rule_id) {
      seen = shiny::isolate(node_types_seen())
      if (!(rule_id %in% seen)) {
        node_types_seen(c(seen, rule_id))
      }
    }

    # Bumped only when the set of rules changes (load/add/delete) so the rules UI
    # re-renders on structural changes but not on every value edit. Re-rendering on
    # a value edit would rebuild the node-type multiselect mid-interaction; its
    # menu lives on <body> (dropdownParent = "body"), so the rebuild orphans the
    # open menu and subsequent clicks are lost.
    rules_render_trigger = shiny::reactiveVal(0L)
    trigger_rules_render = function() {
      rules_render_trigger(shiny::isolate(rules_render_trigger()) + 1L)
    }

    # Live evaluation of every rule against the template's own document. Keyed by
    # rule_id ("1".."k", matching the rules_list ordering) so per-rule status
    # outputs can look up their result. Empty when no document/rules yet. When no
    # section is selected, get_question_ast() evaluates against the whole document.
    rule_status = shiny::reactive({
      q = state()
      rules = q@rules
      if (is.null(ast()) || length(rules) == 0) {
        return(list())
      }
      # A question with rules but no selected nodes targets nothing, so its rules
      # cannot pass. Report them as failing rather than evaluating against the
      # whole document (get_question_ast()'s empty-selection fallback).
      if (length(q@selected_nodes@node_ids) == 0) {
        return(stats::setNames(
          lapply(rules, function(rule) list(passed = FALSE, message = "No nodes selected")),
          as.character(seq_along(rules))
        ))
      }
      question_ast = get_question_ast(ast(), q)
      stats::setNames(
        lapply(rules, function(rule) evaluate_rule(question_ast, rule)),
        as.character(seq_along(rules))
      )
    })

    # Small pass/fail badge from an evaluate_rule() result (or NULL)
    rule_status_badge = function(status) {
      if (is.null(status)) {
        return(NULL)
      }
      if (isTRUE(status$passed)) {
        shiny::icon("check", style = "color: #28a745; font-size: 16px;", title = status$message)
      } else {
        shiny::icon("times", style = "color: #dc3545; font-size: 16px;", title = status$message)
      }
    }
    
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
        trigger_rules_render()
      }
    }, priority = 1000)  # High priority to run before other observers
    
    # Capture the current input values for a single rule into a new
    # markermd_rule. The values input is validated for the active verb; when it
    # is absent or invalid the supplied fallback is used instead.
    #
    # rule_id: Character. The rule ID whose inputs should be read
    # rule: markermd_rule. The currently stored rule, used for fallbacks
    # invalid_values_fallback: Function(verb, rule). Produces the values to use
    #   when the values input is missing or fails validation
    # node_type_seen: Logical. Whether the node-type multiselect has reported a
    #   value before, which disambiguates an empty (NULL) selection.

    capture_rule_inputs = function(rule_id, rule, invalid_values_fallback, node_type_seen = FALSE) {
      node_types_input = paste0("rule_", rule_id, "-node_types")
      verb_input = paste0("rule_", rule_id, "-verb")
      values_input = paste0("rule_", rule_id, "-values")

      # An empty selectize multiselect reports NULL. If it has reported before,
      # an empty box is a deliberate clear meaning the catch-all "Any node";
      # otherwise it has not initialised yet, so keep the stored value rather than
      # clobbering a loaded rule before its widget reports.
      picked_node_types = input[[node_types_input]]
      final_node_type = if (is.null(picked_node_types)) {
        if (node_type_seen) "Any node" else rule@node_type
      } else {
        picked_node_types
      }
      final_verb = if (!is.null(input[[verb_input]])) input[[verb_input]] else rule@verb
      final_values = if (!is.null(input[[values_input]])) {
        values_value = input[[values_input]]
        if (is.null(validate_rule_values(final_verb, values_value))) {
          values_value
        } else {
          invalid_values_fallback(final_verb, rule)
        }
      } else {
        invalid_values_fallback(final_verb, rule)
      }

      new_markermd_rule(
        node_type = final_node_type,
        verb = final_verb,
        values = final_values
      )
    }

    # Add rule button observer
    shiny::observe({
      rule_id = next_rule_id()

      # Before adding new rule, capture current input values for existing rules
      # This mirrors the question name handling approach
      current_rules = rules_list()
      for (existing_rule_id in names(current_rules)) {
        rule = current_rules[[existing_rule_id]]

        node_types_input = paste0("rule_", existing_rule_id, "-node_types")
        verb_input = paste0("rule_", existing_rule_id, "-verb")

        # Update rule with current input values if available
        if (!is.null(input[[node_types_input]]) && !is.null(input[[verb_input]])) {
          note_node_types_seen(existing_rule_id)
          updated_rule = capture_rule_inputs(
            existing_rule_id, rule,
            function(verb, rule) get_default_rule_values(verb),
            node_type_seen = TRUE
          )

          # Update the rule if anything changed
          if (!setequal(rule@node_type, updated_rule@node_type) || rule@verb != updated_rule@verb || !identical(rule@values, updated_rule@values)) {
            current_rules[[existing_rule_id]] = updated_rule
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
      trigger_rules_render()
    }) |>
      shiny::bindEvent(input$add_rule)
    
    # Dynamic delete observer management
    delete_observers = shiny::reactiveVal(list())

    # Dynamic per-rule live-status output management (ids already wired)
    status_outputs = shiny::reactiveVal(character(0))

    # Wire the live-status uiOutput for a rule. Reads rule_status() by rule_id
    # reactively, so re-indexing on delete is transparent (id "1" always shows
    # whichever rule now occupies slot 1).
    #
    # rule_id: Character. The rule ID to wire a status output for

    create_status_output = function(rule_id) {
      output[[paste0("rule_", rule_id, "-status")]] = shiny::renderUI({
        rule_status_badge(rule_status()[[rule_id]])
      })
    }
    
    # Create a delete observer for a specific rule
    #
    # rule_id: Character. The rule ID to create observer for

    create_delete_observer = function(rule_id) {
      delete_input_id = paste0("rule_", rule_id, "-delete")

      observer = shiny::observe({
        current_rules = rules_list()

        # Before deletion, capture current input values for all remaining rules
        preserved_rules = list()
        for (preserve_rule_id in names(current_rules)) {
          if (preserve_rule_id != rule_id) {  # Skip the rule being deleted
            rule = current_rules[[preserve_rule_id]]

            # Create updated rule with current input values, keeping the stored
            # values when the values input is absent or invalid
            preserved_rules[[preserve_rule_id]] = capture_rule_inputs(
              preserve_rule_id, rule,
              function(verb, rule) rule@values,
              node_type_seen = preserve_rule_id %in% shiny::isolate(node_types_seen())
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
        trigger_rules_render()

        # The monitor observer will handle creating new observers for the updated rules_list
        # No need for manual cleanup here since re-indexing changes rule IDs anyway

      }) |>
        shiny::bindEvent(input[[delete_input_id]], ignoreInit = TRUE)

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

      # Wire a live-status output for any newly seen rule id; keep the tracked
      # set mirroring the current rules (outputs persist but read by id).
      wired = status_outputs()
      for (rule_id in names(current_rules)) {
        if (!rule_id %in% wired) {
          create_status_output(rule_id)
          wired = c(wired, rule_id)
        }
      }
      status_outputs(wired[wired %in% names(current_rules)])
    })
    
    # Handle rule input updates
    shiny::observe({
      current_rules = rules_list()
      rules_changed = FALSE
      verb_changed = FALSE

      for (rule_id in names(current_rules)) {
        node_types_input = paste0("rule_", rule_id, "-node_types")
        verb_input = paste0("rule_", rule_id, "-verb")

        # Only require the verb; an empty node-type multiselect reports NULL and
        # is handled (clear -> "Any node") in capture_rule_inputs.
        shiny::req(input[[verb_input]])

        if (!is.null(input[[node_types_input]])) note_node_types_seen(rule_id)

        rule = current_rules[[rule_id]]
        new_rule = capture_rule_inputs(
          rule_id, rule,
          function(verb, rule) get_default_rule_values(verb),
          node_type_seen = rule_id %in% shiny::isolate(node_types_seen())
        )

        # Check if anything changed (including values)
        values_changed = !identical(rule@values, new_rule@values)

        if (!setequal(rule@node_type, new_rule@node_type) || rule@verb != new_rule@verb || values_changed) {
          if (rule@verb != new_rule@verb) verb_changed = TRUE
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

        # A verb change swaps the values control, which is part of the statically
        # rendered rule UI, so it must re-render. Node-type and value edits do not.
        if (verb_changed) trigger_rules_render()
      }
    })
    
    # Render rules UI. Depends only on the structural trigger; rule values are read
    # via isolate() so editing a rule's inputs does not rebuild the rule controls.
    output$rules_ui = shiny::renderUI({
      rules_render_trigger()
      current_rules = shiny::isolate(rules_list())

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
    
    # Render rules status: rule count plus an aggregate pass/fail badge from the
    # live evaluation against the current document.
    output$rules_status = shiny::renderUI({
      current_rules = rules_list()
      rule_count = length(current_rules)

      if (rule_count == 0) {
        shiny::span("None", class = "text-muted")
      } else {
        statuses = rule_status()
        badge = if (length(statuses) > 0) {
          if (all(vapply(statuses, function(s) isTRUE(s$passed), logical(1)))) {
            shiny::icon("check", style = "color: #28a745; margin-left: 4px;", title = "All rules pass on the current document")
          } else {
            shiny::icon("times", style = "color: #dc3545; margin-left: 4px;", title = "Some rules fail on the current document")
          }
        } else {
          NULL
        }

        shiny::span(
          paste0("(", rule_count, " rule", if (rule_count != 1) "s" else "", ")"),
          badge,
          class = "text-success"
        )
      }
    })
    
    # Verb changes are handled by the "Handle rule input updates" observer above
    # (capture_rule_inputs resets incompatible values and bumps the render
    # trigger). A separate verb observer depending on reactiveValuesToList(input)
    # used to live here; it re-ran on every input change and only ever no-opped
    # because the observer above had already applied the verb change, so it was
    # removed.

    # Return reactive question data and methods
    return(list(
      # Reactive data
      question = shiny::reactive({
        state()
      }),
      
      # Node management methods. The public interface stays index-based (the tree
      # interaction works in index space), but selections are stored as the
      # nodes' q2r ids (heading or div ids), converted at this boundary.
      add_node = function(node_index) {
        id = node_id_for_index(ast(), node_index)
        if (nchar(id) == 0) {
          return(invisible(NULL))
        }
        cur_state = state()
        cur_ids = cur_state@selected_nodes@node_ids
        if (!id %in% cur_ids) {
          cur_state@selected_nodes = markermd_node_selection(node_ids = c(cur_ids, id))
          state(cur_state)
        }
      },

      remove_node = function(node_index) {
        id = node_id_for_index(ast(), node_index)
        cur_state = state()
        cur_state@selected_nodes = markermd_node_selection(
          node_ids = setdiff(cur_state@selected_nodes@node_ids, id)
        )
        state(cur_state)
      },

      clear_nodes = function() {
        cur_state = state()
        cur_state@selected_nodes = markermd_node_selection(node_ids = character(0))
        state(cur_state)
      },

      get_selected_nodes = shiny::reactive({
        node_ids_to_indices(ast(), state()@selected_nodes@node_ids)
      }),
      
      delete_clicked = shiny::reactive({
        input$delete_question
      })
    ))
    })
}
