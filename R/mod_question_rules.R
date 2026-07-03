# The validation-rules row machinery for the question module: a dynamic
# collection of rule rows (rule_<id>-<field> inputs) kept in sync with the
# question state. Like ast_preview_observers(), this is a plain wiring
# function operating on the question module's own session, not a nested Shiny
# module: the structured input ids are built by rule_ui_direct() and read
# directly off the module's input.

# Small status badge from an evaluate_rule() result (or NULL). The message
# rides in a bslib tooltip so it is reachable by keyboard and touch, not
# just on a precise hover.
#
# status: List with passed/message from evaluate_rule(), or NULL

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

# Wire the validation-rules row machinery onto a question module's session.
# Owns the rules_list/next_rule_id/node_types_seen bookkeeping internally;
# all cross-machine communication flows through the shared question state.
#
# input, output, session: The question module's server objects
# state: reactiveVal holding the markermd_question S7 object (shared source
#   of truth; @rules is written by this machine)
# ast: Reactive. The parsed AST object
#
# Returns list(rule_status = reactive of per-rule evaluation results)

question_rules_server = function(input, output, session, state, ast) {

  # Rule management - simplified working approach
  rules_list = shiny::reactiveVal(list())
  next_rule_id = shiny::reactiveVal(1L)

  # Seen registry for the per-rule node-type multiselects: a NULL from a
  # noted widget is a deliberate clear meaning the catch-all "Any node", so
  # the delete path MUST remap the keys through its re-indexing (see the
  # policy note on make_seen_registry in mod_question_rows.R)
  node_types_seen = make_seen_registry()

  # Structural render trigger: bumped on load/add/delete and verb changes
  # (which swap the values control), never on value edits (see
  # make_render_trigger in mod_question_rows.R)
  rules_render = make_render_trigger()

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
      rules_render$bump()
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

  # Write a keyed rules structure into both rules_list and the question state
  # so consumers (per-rule status, serialization) update together (the
  # rules-side twin of set_filters_state)
  set_rules_state = function(rules) {
    rules_list(rules)
    cur_state = state()
    cur_state@rules = rules
    state(cur_state)
  }

  # Capture current inputs for every rule, used before structural changes so
  # pending edits are not lost on re-render (the rules-side twin of
  # capture_all_filter_inputs). Values inputs that are absent or invalid keep
  # the stored rule's values.
  capture_all_rule_inputs = function(rules) {
    for (rule_id in names(rules)) {
      if (!is.null(input[[paste0("rule_", rule_id, "-node_types")]])) {
        node_types_seen$note(rule_id)
      }
      rules[[rule_id]] = capture_rule_inputs(
        rule_id, rules[[rule_id]],
        function(verb, rule) rule@values,
        node_type_seen = node_types_seen$has(rule_id)
      )
    }
    rules
  }

  # Freeze every row input for the given rule ids, called by the structural
  # observers after they capture pending edits and before the re-render.
  # Structural changes remap row ids (deletes shift survivors down; adds may
  # reuse a previously vacated slot), so the input-updates observer would
  # otherwise read the pre-change widgets' stale inputs under the new ids in
  # this same flush, clobbering the re-mapped rules. Freezing silences the
  # stale reads for the rest of the flush, and the client re-reports every
  # frozen input once the re-rendered widgets bind, even when the value is
  # unchanged.
  freeze_rule_inputs = function(rule_ids) {
    for (rule_id in rule_ids) {
      for (field in c("node_types", "verb", "values")) {
        shiny::freezeReactiveValue(input, paste0("rule_", rule_id, "-", field))
      }
    }
  }

  # Add rule button observer
  shiny::observe({
    rule_id = next_rule_id()

    # Before adding the new rule, capture pending input edits for the
    # existing rules so the structural re-render does not lose them
    current_rules = capture_all_rule_inputs(rules_list())

    # Create new rule with default values
    current_rules[[as.character(rule_id)]] = new_markermd_rule()

    freeze_rule_inputs(names(current_rules))
    set_rules_state(current_rules)

    # Increment rule ID for next rule
    next_rule_id(rule_id + 1L)
    rules_render$bump()
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
      all_ids = names(current_rules)

      # Before deletion, capture current input values for the surviving
      # rules, keeping stored values when a values input is absent or invalid
      preserved_rules = capture_all_rule_inputs(
        current_rules[setdiff(all_ids, rule_id)]
      )

      # Re-index preserved rules to maintain sequential numbering; state
      # never sees a zero-length named list
      reindexed = if (length(preserved_rules) > 0) reindex_keys(preserved_rules) else list()

      # Remap the seen-widget bookkeeping through the same re-indexing: the
      # k-th preserved rule takes id k. Stale ids would otherwise mark
      # whichever rule now holds an old id as already seen, turning its
      # not-yet-rendered widget's NULL into a deliberate clear ("Any node").
      node_types_seen$remap(names(preserved_rules))

      freeze_rule_inputs(all_ids)
      set_rules_state(reindexed)

      # Reset next rule ID for sequential numbering
      next_rule_id(length(reindexed) + 1L)
      rules_render$bump()

      # The monitor observer will handle creating new observers for the updated rules_list
      # No need for manual cleanup here since re-indexing changes rule IDs anyway

    }) |>
      shiny::bindEvent(input[[delete_input_id]], ignoreInit = TRUE)

    return(observer)
  }

  # Monitor rules_list changes to create/destroy delete observers
  shiny::observe({
    current_rules = rules_list()

    delete_observers(sync_keyed_observers(
      delete_observers(), names(current_rules), create_delete_observer
    ))

    # Wire a live-status output for any newly seen rule id; keep the tracked
    # set mirroring the current rules. Outputs are not observers and are
    # never destroyed (they read rule_status() by id), so they stay outside
    # the sync_keyed_observers lifecycle.
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

      if (!is.null(input[[node_types_input]])) node_types_seen$note(rule_id)

      rule = current_rules[[rule_id]]
      new_rule = capture_rule_inputs(
        rule_id, rule,
        function(verb, rule) get_default_rule_values(verb),
        node_type_seen = node_types_seen$has(rule_id)
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
      set_rules_state(current_rules)

      # A verb change swaps the values control, which is part of the statically
      # rendered rule UI, so it must re-render. Node-type and value edits do not.
      if (verb_changed) rules_render$bump()
    }
  })

  # Render rules UI. Depends only on the structural trigger; rule values are read
  # via isolate() so editing a rule's inputs does not rebuild the rule controls.
  output$rules_ui = shiny::renderUI({
    rules_render$depend()
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

  invisible(list(rule_status = rule_status))
}
