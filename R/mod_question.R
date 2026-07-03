# Question Module
#
# Shiny module for managing a single question with its rules

# Question UI
#
# id: Character. Module namespace ID
# name_id: Integer. The question ID used to build the fallback default name
# name: Character. The question's name to show; defaults to "Question <name_id>"
#   so a freshly added question keeps its positional default, while a loaded or
#   imported question shows its own saved name

question_ui = function(id, name_id, name = paste("Question", name_id)) {
  ns = shiny::NS(id)

  bslib::card(
    style = "margin: 0; width: 100%; max-width: 100%; box-sizing: border-box;",
    # The bslib card-header is itself a horizontal flex row, so these controls are
    # added directly as its flex children. space-between spreads the title and
    # delete button evenly, keeping the delete button hard-right; the title has
    # a modest, shrinkable width so it does not stretch across the whole header.
    bslib::card_header(
      class = "bg-light",
      style = "gap: 0.75rem; justify-content: space-between;",

      # Question name input
      shiny::div(
        style = "flex: 0 1 14rem; min-width: 0;",
        shiny::textInput(
          ns("question_name"),
          NULL,
          value = name,
          width = "100%"
        )
      ),

      # Reorder and delete controls (right aligned)
      shiny::div(
        style = "flex: 0 0 auto;",
        class = "d-flex align-items-center gap-1",
        shiny::actionButton(
          ns("move_question_up"),
          shiny::icon("chevron-up"),
          class = "btn btn-sm p-1 border-0 text-secondary",
          style = "line-height: 1;",
          title = "Move question up"
        ),
        shiny::actionButton(
          ns("move_question_down"),
          shiny::icon("chevron-down"),
          class = "btn btn-sm p-1 border-0 text-secondary",
          style = "line-height: 1;",
          title = "Move question down"
        ),
        shiny::actionButton(
          ns("delete_question"),
          shiny::icon("times"),
          class = "btn-danger btn-sm",
          style = "font-size: 12px; padding: 2px 6px; border-radius: 20%; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; line-height: 1;",
          title = "Delete Question"
        )
      )
    ),
    
    # One-line summary shown while the card is collapsed (inactive); the
    # question-summary / question-body visibility swap is CSS-driven off the
    # wrapper's question-active class (see template-app.R)
    bslib::card_body(
      class = "question-summary py-1 small text-muted",
      shiny::uiOutput(ns("summary_line"))
    ),

    # Question content (full body, shown when the card is active)
    bslib::card_body(
      class = "question-body",
      style = "padding: 12px;",

      # Selected nodes display with an adjacent clear control
      shiny::div(
        style = "margin-bottom: 2px;",
        class = "d-flex align-items-baseline justify-content-between gap-2",
        shiny::div(
          shiny::strong("Selected nodes: "),
          shiny::uiOutput(ns("selected_nodes_display"), inline = TRUE)
        ),
        shiny::uiOutput(ns("clear_nodes_button"), inline = TRUE)
      ),

      # Filters section
      shiny::div(
        # Filters header with matching-semantics help and add button
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-top: -2px; margin-bottom: 8px;",
          shiny::div(
            class = "d-flex align-items-center gap-1",
            shiny::strong("Filters: "),
            shiny::uiOutput(ns("filters_status"), inline = TRUE),
            bslib::popover(
              shiny::icon("circle-question", class = "text-muted"),
              title = "Filter matching",
              shiny::tags$ul(
                class = "small mb-0 ps-3",
                shiny::tags$li(shiny::strong("node type"), ": matches any of the selected kinds"),
                shiny::tags$li(shiny::strong("has class / has id / has engine"), ": exact match"),
                shiny::tags$li(shiny::strong("has text"), ": regular expression"),
                shiny::tags$li(shiny::strong("has label"), ": glob pattern (e.g. fig-*)"),
                shiny::tags$li(shiny::strong("has option"), ": \"eval\" tests key presence, \"eval: false\" tests its value")
              )
            )
          ),
          shiny::actionButton(
            ns("add_filter_group"),
            "Add Filter",
            icon = shiny::icon("plus"),
            class = "btn-outline-primary btn-sm",
            style = "font-size: 11px; padding: 2px 8px;"
          )
        ),

        # Filter groups container
        shiny::div(
          id = ns("filters_container"),
          style = "margin-bottom: 0;",
          shiny::uiOutput(ns("filters_ui"))
        ),

        # Live preview of the constructed q2r filter expression, plus
        # warnings for values that would silently misfire
        shiny::uiOutput(ns("filter_preview")),
        shiny::uiOutput(ns("filter_warnings"))
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
          shiny::actionButton(
            ns("add_rule"),
            "Add Rule",
            icon = shiny::icon("plus"),
            class = "btn-outline-primary btn-sm",
            style = "font-size: 11px; padding: 2px 8px;"
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

    # One-line summary shown while the card is collapsed
    output$summary_line = shiny::renderUI({
      q = state()
      n_nodes = length(q@selected_nodes@node_ids)
      n_filters = length(q@filters)
      n_rules = length(q@rules)
      shiny::span(glue::glue(
        "{n_nodes} node{if (n_nodes == 1) '' else 's'} selected \u00b7 ",
        "{n_filters} filter{if (n_filters == 1) '' else 's'} \u00b7 ",
        "{n_rules} rule{if (n_rules == 1) '' else 's'}"
      ))
    })

    # Clear control next to the selected-nodes display, shown only when there
    # is a selection to clear
    output$clear_nodes_button = shiny::renderUI({
      if (length(state()@selected_nodes@node_ids) == 0) {
        return(NULL)
      }
      shiny::actionButton(
        session$ns("clear_nodes_btn"),
        "Clear",
        class = "btn-link btn-sm p-0 text-decoration-none",
        title = "Clear this question's selected nodes"
      )
    })

    shiny::observe({
      cur_state = state()
      cur_state@selected_nodes = markermd_node_selection(node_ids = character(0))
      state(cur_state)
    }) |>
      shiny::bindEvent(input$clear_nodes_btn)

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

    # Confirm before deleting a question so a stray click does not lose its rules.
    # The actual removal is driven off the confirm button via delete_clicked().
    shiny::observe({
      shiny::showModal(shiny::modalDialog(
        title = "Delete question?",
        glue::glue("Delete \"{state()@name}\" and its validation rules? This cannot be undone."),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_delete_question"), "Delete", class = "btn-danger")
        )
      ))
    }) |>
      shiny::bindEvent(input$delete_question)

    shiny::observe({
      shiny::removeModal()
    }) |>
      shiny::bindEvent(input$confirm_delete_question)
    
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

    # Filter management. filters_list mirrors rules_list but is two-level:
    # group ids ("1".."g") each holding list(negate = logical, conditions =
    # named list of markermd_filter_condition keyed "1".."k").
    filters_list = shiny::reactiveVal(list())
    next_group_id = shiny::reactiveVal(1L)

    # Bumped only on structural filter changes (load/add/delete and condition
    # type changes, which swap the value control) so value edits do not rebuild
    # the filter controls mid-interaction.
    filters_render_trigger = shiny::reactiveVal(0L)
    trigger_filters_render = function() {
      filters_render_trigger(shiny::isolate(filters_render_trigger()) + 1L)
    }

    # Convert the keyed filters_list structure into the question's list of
    # markermd_filter_group objects
    filter_groups_from_keyed = function(groups) {
      lapply(unname(groups), function(group) {
        markermd_filter_group(conditions = unname(group$conditions), negate = group$negate)
      })
    }

    # Write a keyed filters structure into both filters_list and the question
    # state so consumers (preview, rule_status via get_question_ast) update
    set_filters_state = function(groups) {
      filters_list(groups)
      cur_state = state()
      cur_state@filters = filter_groups_from_keyed(groups)
      state(cur_state)
    }

    # Re-key a list sequentially as "1".."n"
    reindex_keys = function(x) {
      stats::setNames(unname(x), as.character(seq_along(x)))
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
        lapply(rules, function(rule) {
          res = evaluate_rule(question_ast, rule)
          # An empty pattern passes trivially (see evaluate_rule_has_content
          # and friends); surface that as a warning rather than a green check
          # so the author notices the rule is not testing anything yet
          if (rule@verb %in% c("has content", "lacks content", "has name") &&
              (is.na(rule@values[1]) || nchar(rule@values[1]) == 0)) {
            res = list(
              passed = NA,
              message = "Empty pattern: this rule passes trivially. Enter a pattern."
            )
          }
          res
        }),
        as.character(seq_along(rules))
      )
    })

    # Small status badge from an evaluate_rule() result (or NULL). The message
    # rides in a bslib tooltip so it is reachable by keyboard and touch, not
    # just on a precise hover.
    rule_status_badge = function(status) {
      if (is.null(status)) {
        return(NULL)
      }
      icon_tag = if (is.na(status$passed)) {
        shiny::icon("triangle-exclamation", style = "color: #ffc107; font-size: 14px;")
      } else if (isTRUE(status$passed)) {
        shiny::icon("check", style = "color: #28a745; font-size: 16px;")
      } else {
        shiny::icon("times", style = "color: #dc3545; font-size: 16px;")
      }
      bslib::tooltip(shiny::span(icon_tag, tabindex = "0"), status$message)
    }
    
    # Initialize rules_list and filters_list from loaded question state
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
      if (length(current_state@filters) > 0 && length(filters_list()) == 0) {
        # Convert S7 filter groups to the keyed filters_list format
        loaded_groups = list()
        for (i in seq_along(current_state@filters)) {
          group = current_state@filters[[i]]
          loaded_conditions = list()
          for (j in seq_along(group@conditions)) {
            loaded_conditions[[as.character(j)]] = group@conditions[[j]]
          }
          loaded_groups[[as.character(i)]] = list(
            negate = group@negate,
            conditions = loaded_conditions
          )
        }
        filters_list(loaded_groups)
        next_group_id(length(loaded_groups) + 1L)
        trigger_filters_render()
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

        # Remap the seen-widget bookkeeping through the same re-indexing: the
        # k-th preserved rule takes id k. Stale ids would otherwise mark
        # whichever rule now holds an old id as already seen, turning its
        # not-yet-rendered widget's NULL into a deliberate clear ("Any node").
        seen = shiny::isolate(node_types_seen())
        node_types_seen(as.character(which(names(preserved_rules) %in% seen)))

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
    
    # Render rules status: "None" when the question has no rules, otherwise
    # nothing (the rules are listed below, each with its own live pass/fail icon).
    output$rules_status = shiny::renderUI({
      if (length(rules_list()) == 0) {
        shiny::span("None", class = "text-muted")
      } else {
        NULL
      }
    })
    
    # Verb changes are handled by the "Handle rule input updates" observer above
    # (capture_rule_inputs resets incompatible values and bumps the render
    # trigger). A separate verb observer depending on reactiveValuesToList(input)
    # used to live here; it re-ran on every input change and only ever no-opped
    # because the observer above had already applied the verb change, so it was
    # removed.

    # Filter node-type multiselects that have reported a (non-NULL) value at
    # least once. The node-type value control is a selectize multiselect, so
    # an empty selection reports NULL, which is also the not-yet-initialised
    # state; this disambiguates the two (same scheme as node_types_seen for
    # rule rows). Stale keys after re-indexing are harmless here: the
    # deliberate-clear branch keeps the stored value and merely re-syncs the
    # widget to it.
    filter_values_seen = shiny::reactiveVal(character(0))
    note_filter_value_seen = function(key) {
      seen = shiny::isolate(filter_values_seen())
      if (!(key %in% seen)) {
        filter_values_seen(c(seen, key))
      }
    }

    # Capture the current input values for a single filter condition into a new
    # markermd_filter_condition. The type and negate controls always report a
    # value, so missing inputs just keep the stored condition; the node-type
    # multiselect's NULL is disambiguated via filter_values_seen. When the
    # value is invalid for the active type (e.g. stale text after switching to
    # "node type") the type's default is used instead.
    #
    # group_id: Character. The group ID whose condition should be read
    # cond_id: Character. The condition ID within the group
    # condition: markermd_filter_condition. The currently stored condition

    capture_filter_inputs = function(group_id, cond_id, condition) {
      type_input = paste0("filter_", group_id, "_", cond_id, "-type")
      value_input = paste0("filter_", group_id, "_", cond_id, "-value")
      negate_input = paste0("filter_", group_id, "_", cond_id, "-negate")
      seen_key = paste0(group_id, "_", cond_id)

      final_type = if (!is.null(input[[type_input]])) input[[type_input]] else condition@type
      final_negate = if (!is.null(input[[negate_input]])) input[[negate_input]] else condition@negate

      if (!is.null(input[[value_input]])) {
        note_filter_value_seen(seen_key)
      }

      # A type change always resets the value to the new type's default so a
      # stale value (e.g. a class name in a label field) never carries over.
      # The value input is frozen until the swapped-in control reports, since
      # observers re-run before the re-render and would otherwise read the old
      # widget's value back into the freshly reset state.
      final_value = if (final_type != condition@type) {
        shiny::freezeReactiveValue(input, value_input)
        get_default_filter_condition_value(final_type)
      } else if (!is.null(input[[value_input]])) {
        input[[value_input]]
      } else if (final_type == "node type" && seen_key %in% shiny::isolate(filter_values_seen())) {
        # The multiselect was deliberately emptied. An empty kind set is not a
        # valid condition (it would match nothing), so keep the stored kinds
        # and snap the widget back to them rather than leaving the UI showing
        # an empty control while a hidden filter stays active.
        shiny::updateSelectizeInput(session, value_input, selected = condition@value)
        condition@value
      } else {
        condition@value
      }

      if (!is.null(validate_filter_condition_value(final_type, final_value))) {
        final_value = get_default_filter_condition_value(final_type)
      }

      markermd_filter_condition(type = final_type, value = final_value, negate = final_negate)
    }

    # Capture current inputs for every condition (and each group's negate
    # toggle) across all groups, used before structural changes so pending
    # edits are not lost on re-render
    capture_all_filter_inputs = function(groups) {
      for (group_id in names(groups)) {
        negate_input = input[[paste0("filter_group_", group_id, "-negate")]]
        if (!is.null(negate_input)) {
          groups[[group_id]]$negate = negate_input
        }
        for (cond_id in names(groups[[group_id]]$conditions)) {
          groups[[group_id]]$conditions[[cond_id]] = capture_filter_inputs(
            group_id, cond_id, groups[[group_id]]$conditions[[cond_id]]
          )
        }
      }
      groups
    }

    # Add filter group button observer
    shiny::observe({
      groups = capture_all_filter_inputs(filters_list())

      group_id = next_group_id()
      groups[[as.character(group_id)]] = list(
        negate = FALSE,
        conditions = list("1" = new_markermd_filter_condition())
      )

      set_filters_state(groups)
      next_group_id(group_id + 1L)
      trigger_filters_render()
    }) |>
      shiny::bindEvent(input$add_filter_group)

    # Dynamic filter observer management, keyed "g<id>" for group-level
    # observers and "c<gid>_<cid>" for per-condition delete observers
    filter_observers = shiny::reactiveVal(list())

    # Create the add-condition and delete observers for a filter group
    #
    # group_id: Character. The group ID to create observers for

    create_filter_group_observers = function(group_id) {
      add_observer = shiny::observe({
        groups = capture_all_filter_inputs(filters_list())

        conditions = groups[[group_id]]$conditions
        conditions[[as.character(length(conditions) + 1L)]] = new_markermd_filter_condition()
        groups[[group_id]]$conditions = conditions

        set_filters_state(groups)
        trigger_filters_render()
      }) |>
        shiny::bindEvent(input[[paste0("filter_group_", group_id, "-add_condition")]], ignoreInit = TRUE)

      delete_observer = shiny::observe({
        groups = capture_all_filter_inputs(filters_list())

        groups[[group_id]] = NULL
        groups = reindex_keys(groups)

        set_filters_state(groups)
        next_group_id(length(groups) + 1L)
        trigger_filters_render()
      }) |>
        shiny::bindEvent(input[[paste0("filter_group_", group_id, "-delete")]], ignoreInit = TRUE)

      list(add_observer, delete_observer)
    }

    # Create a delete observer for a specific filter condition. Deleting a
    # group's last condition deletes the group.
    #
    # group_id: Character. The group ID containing the condition
    # cond_id: Character. The condition ID to create the observer for

    create_filter_condition_observer = function(group_id, cond_id) {
      shiny::observe({
        groups = capture_all_filter_inputs(filters_list())

        conditions = groups[[group_id]]$conditions
        conditions[[cond_id]] = NULL

        if (length(conditions) == 0) {
          groups[[group_id]] = NULL
          groups = reindex_keys(groups)
          next_group_id(length(groups) + 1L)
        } else {
          groups[[group_id]]$conditions = reindex_keys(conditions)
        }

        set_filters_state(groups)
        trigger_filters_render()
      }) |>
        shiny::bindEvent(input[[paste0("filter_", group_id, "_", cond_id, "-delete")]], ignoreInit = TRUE)
    }

    # Monitor filters_list changes to create/destroy filter observers
    shiny::observe({
      groups = filters_list()
      current_observers = filter_observers()

      active_keys = character(0)
      for (group_id in names(groups)) {
        active_keys = c(active_keys, paste0("g", group_id))
        for (cond_id in names(groups[[group_id]]$conditions)) {
          active_keys = c(active_keys, paste0("c", group_id, "_", cond_id))
        }
      }

      new_observers = current_observers
      for (stale_key in setdiff(names(current_observers), active_keys)) {
        observers = current_observers[[stale_key]]
        if (!is.list(observers)) observers = list(observers)
        for (observer in observers) observer$destroy()
        new_observers[[stale_key]] = NULL
      }

      for (group_id in names(groups)) {
        group_key = paste0("g", group_id)
        if (!group_key %in% names(new_observers)) {
          new_observers[[group_key]] = create_filter_group_observers(group_id)
        }
        for (cond_id in names(groups[[group_id]]$conditions)) {
          cond_key = paste0("c", group_id, "_", cond_id)
          if (!cond_key %in% names(new_observers)) {
            new_observers[[cond_key]] = create_filter_condition_observer(group_id, cond_id)
          }
        }
      }

      filter_observers(new_observers)
    })

    # Handle filter condition input updates (type, value, and negate edits)
    shiny::observe({
      groups = filters_list()
      filters_changed = FALSE
      type_changed = FALSE

      for (group_id in names(groups)) {
        group_negate = input[[paste0("filter_group_", group_id, "-negate")]]
        if (!is.null(group_negate) && !identical(group_negate, groups[[group_id]]$negate)) {
          groups[[group_id]]$negate = group_negate
          filters_changed = TRUE
        }

        for (cond_id in names(groups[[group_id]]$conditions)) {
          type_input = paste0("filter_", group_id, "_", cond_id, "-type")
          shiny::req(input[[type_input]])

          condition = groups[[group_id]]$conditions[[cond_id]]
          new_condition = capture_filter_inputs(group_id, cond_id, condition)

          if (condition@type != new_condition@type ||
              !identical(condition@value, new_condition@value) ||
              condition@negate != new_condition@negate) {
            # A type change swaps the value control, which is part of the
            # statically rendered condition UI, so it must re-render; negate
            # and value edits do not
            if (condition@type != new_condition@type) type_changed = TRUE
            groups[[group_id]]$conditions[[cond_id]] = new_condition
            filters_changed = TRUE
          }
        }
      }

      if (filters_changed) {
        set_filters_state(groups)
        if (type_changed) trigger_filters_render()
      }
    })

    # Render filter groups UI. Depends only on the structural trigger; condition
    # values are read via isolate() so editing inputs does not rebuild controls.
    output$filters_ui = shiny::renderUI({
      filters_render_trigger()
      groups = shiny::isolate(filters_list())

      if (length(groups) == 0) {
        NULL
      } else {
        filters_ui_direct(groups, session$ns)
      }
    })

    # Render filters status: "None" when the question has no filter groups,
    # otherwise a live count of the nodes the filters exclude
    output$filters_status = shiny::renderUI({
      if (length(filters_list()) == 0) {
        return(shiny::span("None", class = "text-muted"))
      }
      excluded = question_filtered_indices(ast(), state())
      n = length(excluded)
      shiny::span(
        class = "badge text-bg-secondary fw-normal",
        glue::glue("excludes {n} node{if (n == 1) '' else 's'}")
      )
    })

    # Live preview of the q2r predicate the current filters construct. Reads
    # state() directly so value edits update the preview without a re-render.
    output$filter_preview = shiny::renderUI({
      preview_text = filters_expr_text(state()@filters)
      if (is.null(preview_text)) {
        NULL
      } else {
        shiny::div(
          class = "small text-muted mb-2",
          shiny::span("q2r predicate: ", class = "fst-italic"),
          shiny::tags$code(preview_text, style = "white-space: pre-wrap;")
        )
      }
    })

    # Inline warnings for filter values that silently misfire (bad regexes,
    # empty patterns, option tests without a key)
    output$filter_warnings = shiny::renderUI({
      msgs = filter_value_warnings(state()@filters)
      if (length(msgs) == 0) {
        return(NULL)
      }
      shiny::div(
        class = "small text-danger mb-2",
        lapply(msgs, function(msg) {
          shiny::div(shiny::icon("triangle-exclamation"), " ", msg)
        })
      )
    })

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

      get_selected_nodes = shiny::reactive({
        node_ids_to_indices(ast(), state()@selected_nodes@node_ids)
      }),

      delete_clicked = shiny::reactive({
        input$confirm_delete_question
      }),

      move_up_clicked = shiny::reactive({
        input$move_question_up
      }),

      move_down_clicked = shiny::reactive({
        input$move_question_down
      })
    ))
    })
}
