# The filter-groups row machinery for the question module: a two-level
# dynamic collection (groups keyed by a stable group key, each holding
# conditions keyed by a stable condition key) kept in sync with the question
# state. Keys are minted from monotonic counters and never reused or
# renumbered (see mod_question_rows.R); the list order is the display order
# and the question state holds the groups and conditions as unnamed lists in
# that order. Group controls use filter_group_<gid>-<field> ids and
# conditions filter_<gid>_<cid>-<field> ids, built by
# filter_group_ui()/filters_ui_direct(). Like the rules machine in
# mod_question_rules.R (its structural twin), this is a plain wiring function
# on the question module's own session, not a nested Shiny module.

# Convert the keyed filters_list structure into the question's list of
# markermd_filter_group objects
#
# groups: Named list keyed by group key of list(negate =, conditions =,
#   next_cond =) entries

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
  # group keys each holding list(negate = logical, conditions = named list of
  # markermd_filter_condition keyed by condition key, next_cond = the next
  # condition key to mint in this group). next_group_id only ever grows.
  filters_list = shiny::reactiveVal(list())
  next_group_id = shiny::reactiveVal(1L)
  observers = make_observer_tracker()

  # Structural render trigger: bumped on load/add/delete and condition type
  # changes (which swap the value control), never on value edits (see
  # make_render_trigger in mod_question_rows.R)
  filters_render = make_render_trigger()

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
          conditions = loaded_conditions,
          next_cond = length(loaded_conditions) + 1L
        )
      }
      filters_list(loaded_groups)
      next_group_id(length(loaded_groups) + 1L)
      filters_render$bump()
    }
  }, priority = 1000) |>  # High priority to run before other observers
    observers$track()

  # Seen registry for the per-condition node-type multiselects, keyed
  # "<gid>_<cid>" (see make_seen_registry in mod_question_rows.R)
  filter_values_seen = make_seen_registry()

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
      filter_values_seen$note(seen_key)
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
    } else if (final_type == "node type" && filter_values_seen$has(seen_key)) {
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

  # The seen-registry keys of every condition in a group
  #
  # group_id: Character. The group key
  # group: The group's filters_list entry

  group_seen_keys = function(group_id, group) {
    paste0(group_id, "_", names(group$conditions))
  }

  # Add filter group button observer. No input is frozen around the
  # structural re-renders in this machine (apart from the type-change freeze
  # in capture_filter_inputs): surviving rows keep their keys, so the inputs
  # the re-rendered widgets report are the ones already stored under them.
  shiny::observe({
    groups = capture_all_filter_inputs(filters_list())

    group_id = next_group_id()
    groups[[as.character(group_id)]] = list(
      negate = FALSE,
      conditions = list("1" = new_markermd_filter_condition()),
      next_cond = 2L
    )

    set_filters_state(groups)
    next_group_id(group_id + 1L)
    filters_render$bump()
  }) |>
    shiny::bindEvent(input$add_filter_group) |>
    observers$track()

  # Dynamic filter observer management, keyed "g<id>" for group-level
  # observers and "c<gid>_<cid>" for per-condition delete observers
  filter_observers = shiny::reactiveVal(list())

  # Create the add-condition and delete observers for a filter group. An
  # event for a group key no longer in the list (a second click on a group
  # already deleted) is dropped, since the key can only ever have meant that
  # group.
  #
  # group_id: Character. The group key to create observers for

  create_filter_group_observers = function(group_id) {
    add_observer = shiny::observe({
      if (!group_id %in% names(filters_list())) {
        return()
      }
      groups = capture_all_filter_inputs(filters_list())

      group = groups[[group_id]]
      group$conditions[[as.character(group$next_cond)]] = new_markermd_filter_condition()
      group$next_cond = group$next_cond + 1L
      groups[[group_id]] = group

      set_filters_state(groups)
      filters_render$bump()
    }) |>
      shiny::bindEvent(input[[paste0("filter_group_", group_id, "-add_condition")]], ignoreInit = TRUE)

    delete_observer = shiny::observe({
      if (!group_id %in% names(filters_list())) {
        return()
      }
      groups = capture_all_filter_inputs(filters_list())

      filter_values_seen$forget(group_seen_keys(group_id, groups[[group_id]]))
      groups[[group_id]] = NULL
      if (length(groups) == 0) groups = list()

      set_filters_state(groups)
      filters_render$bump()
    }) |>
      shiny::bindEvent(input[[paste0("filter_group_", group_id, "-delete")]], ignoreInit = TRUE)

    list(add_observer, delete_observer)
  }

  # Create a delete observer for a specific filter condition. Deleting a
  # group's last condition deletes the group. An event for a condition key no
  # longer in its group is dropped.
  #
  # group_id: Character. The group key containing the condition
  # cond_id: Character. The condition key to create the observer for

  create_filter_condition_observer = function(group_id, cond_id) {
    shiny::observe({
      current = filters_list()
      if (!group_id %in% names(current) || !cond_id %in% names(current[[group_id]]$conditions)) {
        return()
      }
      groups = capture_all_filter_inputs(current)

      filter_values_seen$forget(paste0(group_id, "_", cond_id))
      conditions = groups[[group_id]]$conditions
      conditions[[cond_id]] = NULL

      if (length(conditions) == 0) {
        groups[[group_id]] = NULL
        if (length(groups) == 0) groups = list()
      } else {
        groups[[group_id]]$conditions = conditions
      }

      set_filters_state(groups)
      filters_render$bump()
    }) |>
      shiny::bindEvent(input[[paste0("filter_", group_id, "_", cond_id, "-delete")]], ignoreInit = TRUE)
  }

  # Monitor filters_list changes to create/destroy filter observers. Group
  # keys ("g<id>") map to add/delete observer pairs; condition keys
  # ("c<gid>_<cid>") to a single delete observer.
  shiny::observe({
    groups = filters_list()

    active_keys = character(0)
    for (group_id in names(groups)) {
      active_keys = c(active_keys, paste0("g", group_id))
      for (cond_id in names(groups[[group_id]]$conditions)) {
        active_keys = c(active_keys, paste0("c", group_id, "_", cond_id))
      }
    }

    filter_observers(sync_keyed_observers(
      filter_observers(), active_keys,
      function(key) {
        if (startsWith(key, "g")) {
          create_filter_group_observers(substring(key, 2))
        } else {
          ids = strsplit(substring(key, 2), "_", fixed = TRUE)[[1]]
          create_filter_condition_observer(ids[1], ids[2])
        }
      }
    ))
  }) |>
    observers$track()

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
      if (type_changed) filters_render$bump()
    }
  }) |>
    observers$track()

  # Render filter groups UI. Depends only on the structural trigger; condition
  # values are read via isolate() so editing inputs does not rebuild controls.
  output$filters_ui = shiny::renderUI({
    filters_render$depend()
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
        shiny::div(fa_icon("triangle-exclamation"), " ", msg)
      })
    )
  })

  invisible(list(
    destroy = function() observers$destroy(shiny::isolate(filter_observers()))
  ))
}
