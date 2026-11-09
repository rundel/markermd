# The validation-rules row machinery for the question module: a dynamic
# collection of rule rows (rule_<key>-<field> inputs) kept in sync with the
# question state. Row keys are stable (minted once, never reused or
# renumbered; see mod_question_rows.R), so a row's inputs always belong to
# that row and the question state holds the rules as an unnamed list in row
# order. Like ast_preview_observers(), this is a plain wiring function
# operating on the question module's own session, not a nested Shiny module:
# the structured input ids are built by rule_ui_direct() and read directly
# off the module's input.

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
    fa_icon("triangle-exclamation", style = "color: #ffc107; font-size: 14px;")
  } else if (isTRUE(status$passed)) {
    fa_icon("check", style = "color: #28a745; font-size: 16px;")
  } else {
    fa_icon("times", style = "color: #dc3545; font-size: 16px;")
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

  # The rows, keyed by stable rule key in display order; next_rule_id only
  # ever grows, so a deleted row's key (and its input ids) is never reused
  rules_list = shiny::reactiveVal(list())
  next_rule_id = shiny::reactiveVal(1L)

  # Seen registry for the per-rule node-type multiselects: a NULL from a
  # noted widget is a deliberate clear meaning the catch-all "Any node" (see
  # make_seen_registry in mod_question_rows.R)
  node_types_seen = make_seen_registry()

  # Structural render trigger: bumped on load/add/delete and verb changes
  # (which swap the values control), never on value edits (see
  # make_render_trigger in mod_question_rows.R)
  rules_render = make_render_trigger()

  # Live evaluation of every rule against the template's own document, through
  # the same evaluate_question_rules() grading uses (so a document-scope
  # question's rules evaluate against the whole document here as there, and a
  # sections question without nodes fails here as there). Keyed by the stable
  # rule key (rules_list and state@rules are written together, in the same
  # order) so per-rule status outputs can look up their result. Empty when no
  # document/rules yet.
  rule_status = shiny::reactive({
    q = state()
    rules = q@rules
    keys = names(rules_list())
    if (is.null(ast()) || length(rules) == 0) {
      return(list())
    }
    results = evaluate_question_rules(ast(), q)
    stats::setNames(
      lapply(seq_along(rules), function(i) {
        rule = rules[[i]]
        res = results[[i]]
        # Grading treats these as plain results (a trivial pass, a failure);
        # the author should instead see a warning that the rule is not
        # testing what they think yet
        if (rule@verb %in% c("has content", "lacks content", "has name")) {
          pattern = rule@values[1]
          if (is.na(pattern) || nchar(pattern) == 0) {
            res = list(
              passed = NA,
              message = "Empty pattern: this rule passes trivially. Enter a pattern."
            )
          } else if (!is_valid_regex(pattern)) {
            res = list(
              passed = NA,
              message = "Invalid regular expression: this rule fails until the pattern is fixed."
            )
          }
        }
        res
      }),
      keys
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
    # "has between" is two numeric inputs (see create_rule_verb_inputs()),
    # read back together; a half-filled pair falls back like a missing input
    values_value = if (identical(final_verb, "has between")) {
      lo = input[[paste0(values_input, "_min")]]
      hi = input[[paste0(values_input, "_max")]]
      if (!is.null(lo) && !is.null(hi) && !is.na(lo) && !is.na(hi)) c(lo, hi) else NULL
    } else {
      input[[values_input]]
    }
    final_values = if (!is.null(values_value) && is.null(validate_rule_values(final_verb, values_value))) {
      values_value
    } else {
      invalid_values_fallback(final_verb, rule)
    }

    new_markermd_rule(
      node_type = final_node_type,
      verb = final_verb,
      values = final_values
    )
  }

  # Write the keyed rows into rules_list and, unnamed, into the question
  # state so consumers (per-rule status, serialization) update together (the
  # rules-side twin of set_filters_state). The row keys are the editor's
  # own; a named list would serialize as a mapping instead of the array the
  # template schema requires.
  set_rules_state = function(rules) {
    rules_list(rules)
    cur_state = state()
    cur_state@rules = unname(rules)
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

  # Add rule button observer. No input is frozen around the re-render: the
  # surviving rows keep their keys, so the inputs the re-rendered widgets
  # report are the ones already stored under those keys.
  shiny::observe({
    rule_id = next_rule_id()

    # Before adding the new rule, capture pending input edits for the
    # existing rules so the structural re-render does not lose them
    current_rules = capture_all_rule_inputs(rules_list())

    # Create new rule with default values
    current_rules[[as.character(rule_id)]] = new_markermd_rule()

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

  # Wire the live-status uiOutput for a rule. Reads rule_status() by its
  # stable key reactively.
  #
  # rule_id: Character. The rule ID to wire a status output for

  create_status_output = function(rule_id) {
    output[[paste0("rule_", rule_id, "-status")]] = shiny::renderUI({
      rule_status_badge(rule_status()[[rule_id]])
    })
  }

  # Create a delete observer for a specific rule. A click for a key that is
  # no longer in the list (a second click on a row already deleted in this
  # session) is dropped, since the key can only ever have meant that row.
  #
  # rule_id: Character. The rule key to create the observer for

  create_delete_observer = function(rule_id) {
    delete_input_id = paste0("rule_", rule_id, "-delete")

    observer = shiny::observe({
      current_rules = rules_list()
      if (!rule_id %in% names(current_rules)) {
        return()
      }

      # Before deletion, capture current input values for the surviving
      # rules, keeping stored values when a values input is absent or invalid
      preserved_rules = capture_all_rule_inputs(
        current_rules[setdiff(names(current_rules), rule_id)]
      )

      # State never sees a zero-length named list
      if (length(preserved_rules) == 0) preserved_rules = list()

      node_types_seen$forget(rule_id)
      set_rules_state(preserved_rules)
      rules_render$bump()
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

      # An invalid or half-typed value keeps the stored values (the warning
      # below tells the author why); only a verb change, whose old values
      # cannot fit the new control, falls back to the verb's defaults
      rule = current_rules[[rule_id]]
      new_rule = capture_rule_inputs(
        rule_id, rule,
        function(verb, rule) if (identical(verb, rule@verb)) rule@values else get_default_rule_values(verb),
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

  # The values each rule's inputs currently hold that the rule cannot take
  # (a minimum above the maximum, a negative or fractional count, a cleared
  # bound). The stored rule keeps its last valid values while this shows the
  # author why the input and the badge disagree. Read from the inputs, not
  # the state, since the state never holds an invalid value.
  rule_input_problems = shiny::reactive({
    msgs = character(0)
    rules = rules_list()
    for (k in seq_along(rules)) {
      rule_id = names(rules)[k]
      verb = input[[paste0("rule_", rule_id, "-verb")]]
      if (is.null(verb) || !identical(verb, rules[[rule_id]]@verb)) next
      values_input = paste0("rule_", rule_id, "-values")
      problem = if (identical(verb, "has between")) {
        lo = input[[paste0(values_input, "_min")]]
        hi = input[[paste0(values_input, "_max")]]
        if (is.null(lo) || is.null(hi)) {
          NULL
        } else if (is.na(lo) || is.na(hi)) {
          "Count range needs both a minimum and a maximum"
        } else {
          validate_rule_values(verb, c(lo, hi))
        }
      } else {
        value = input[[values_input]]
        if (is.null(value)) NULL else validate_rule_values(verb, value)
      }
      if (!is.null(problem)) {
        msgs = c(msgs, paste0("Rule ", k, ": ", problem, ". The rule keeps its previous value."))
      }
    }
    msgs
  })

  output$rule_warnings = shiny::renderUI({
    msgs = rule_input_problems()
    if (length(msgs) == 0) {
      return(NULL)
    }
    shiny::div(
      class = "small text-danger mt-1",
      lapply(msgs, function(msg) {
        shiny::div(fa_icon("triangle-exclamation"), " ", msg)
      })
    )
  })

  # Verb changes are handled by the "Handle rule input updates" observer above
  # (capture_rule_inputs resets incompatible values and bumps the render
  # trigger). A separate verb observer depending on reactiveValuesToList(input)
  # used to live here; it re-ran on every input change and only ever no-opped
  # because the observer above had already applied the verb change, so it was
  # removed.

  invisible(list(rule_status = rule_status))
}
