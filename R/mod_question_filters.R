# The filter-groups row machinery for the question module: a two-level
# dynamic collection (groups keyed "1".."g", each holding conditions keyed
# "1".."k") kept in sync with the question state. Group controls use
# filter_group_<gid>-<field> ids and conditions filter_<gid>_<cid>-<field>
# ids, built by filter_group_ui()/filters_ui_direct(). Like the rules machine
# in mod_question_rules.R (its structural twin), this is a plain wiring
# function on the question module's own session, not a nested Shiny module.

# Convert the keyed filters_list structure into the question's list of
# markermd_filter_group objects
#
# groups: Named list keyed "1".."g" of list(negate =, conditions =) entries

filter_groups_from_keyed = function(groups) {
  lapply(unname(groups), function(group) {
    markermd_filter_group(conditions = unname(group$conditions), negate = group$negate)
  })
}

# Wire the filter-groups row machinery onto a question module's session.
# Owns the filters_list/next_group_id/filter_values_seen bookkeeping
# internally; all cross-machine communication flows through the shared
# question state.
#
# input, output, session: The question module's server objects
# state: reactiveVal holding the markermd_question S7 object (shared source
#   of truth; @filters is written by this machine)
# ast: Reactive. The parsed AST object

question_filters_server = function(input, output, session, state, ast) {

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

  # Write a keyed filters structure into both filters_list and the question
  # state so consumers (preview, rule_status via get_question_ast) update
  set_filters_state = function(groups) {
    filters_list(groups)
    cur_state = state()
    cur_state@filters = filter_groups_from_keyed(groups)
    state(cur_state)
  }

  # Initialize filters_list from loaded question state
  shiny::observe({
    current_state = state()
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

  # Freeze every group and condition input for the given keyed groups, called
  # by the structural observers after they capture pending edits and before
  # the re-render (the filters-side twin of freeze_rule_inputs). Structural
  # changes remap group/condition ids (deletes shift survivors down; adds may
  # reuse a previously vacated slot), so the input-updates observer would
  # otherwise read the pre-change widgets' stale inputs under the new ids in
  # this same flush, clobbering the re-mapped groups. Freezing silences the
  # stale reads for the rest of the flush, and the client re-reports every
  # frozen input once the re-rendered widgets bind, even when unchanged.
  freeze_filter_inputs = function(groups) {
    for (group_id in names(groups)) {
      shiny::freezeReactiveValue(input, paste0("filter_group_", group_id, "-negate"))
      for (cond_id in names(groups[[group_id]]$conditions)) {
        for (field in c("type", "value", "negate")) {
          shiny::freezeReactiveValue(input, paste0("filter_", group_id, "_", cond_id, "-", field))
        }
      }
    }
  }

  # Add filter group button observer
  shiny::observe({
    groups = capture_all_filter_inputs(filters_list())

    group_id = next_group_id()
    groups[[as.character(group_id)]] = list(
      negate = FALSE,
      conditions = list("1" = new_markermd_filter_condition())
    )

    freeze_filter_inputs(groups)
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

      freeze_filter_inputs(groups)
      set_filters_state(groups)
      trigger_filters_render()
    }) |>
      shiny::bindEvent(input[[paste0("filter_group_", group_id, "-add_condition")]], ignoreInit = TRUE)

    delete_observer = shiny::observe({
      pre_groups = capture_all_filter_inputs(filters_list())

      groups = pre_groups
      groups[[group_id]] = NULL
      groups = reindex_keys(groups)

      freeze_filter_inputs(pre_groups)
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
      pre_groups = capture_all_filter_inputs(filters_list())

      groups = pre_groups
      conditions = groups[[group_id]]$conditions
      conditions[[cond_id]] = NULL

      if (length(conditions) == 0) {
        groups[[group_id]] = NULL
        groups = reindex_keys(groups)
        next_group_id(length(groups) + 1L)
      } else {
        groups[[group_id]]$conditions = reindex_keys(conditions)
      }

      freeze_filter_inputs(pre_groups)
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

  invisible(NULL)
}
