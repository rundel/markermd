# Rubric module: item and group actions
#
# Every interaction with a rubric item row arrives as one item_event
# ({item, action, value}; see rubric_item_editing_head()) and every
# interaction with a group as one group_event ({group, action, value}; see
# rubric_groups_js()), from either tab's view. rubric_actions_wiring() routes
# them to the handlers here, which change the handles (mod_mark_rubric_state.R),
# write the database and patch or redraw the views. It also wires the add
# item / add group buttons of both tabs. A plain wiring function like
# rubric_state_wiring(): it fills the module's shared context rx.
#
# rx: The rubric module's shared context, after rubric_state_wiring()

rubric_actions_wiring = function(rx) {
  session = rx$session
  input = rx$input
  root = rx$root
  question_names = rx$question_names
  app_action = rx$app_action

  # --- Items -----------------------------------------------------------------

  # Move an item one position in the given direction (-1 up, 1 down) within
  # its group, or among the top-level blocks when ungrouped, wrapping at the
  # ends (see move_in_blocks()), then renumber
  move_block = function(question_name, id, direction) {
    handles = rx$items[[question_name]]
    ids = names(handles)
    new_ids = move_in_blocks(ids, item_groups_of(handles), id, direction)
    if (!identical(new_ids, ids)) {
      rx$reorder_items(question_name, new_ids)
    }
  }

  # The group menu: move the item to the end of the chosen group, or out of
  # its group (see move_item_to_group()). Moving into an exclusive group is
  # refused while any repository would end up with two of its items
  # selected, like turning the exclusive switch on.
  #
  # choice: The chosen group id, or "" / NULL for no group
  set_item_group = function(question_name, handle, choice) {
    target = if (is.null(choice) || !nzchar(choice)) NA_character_ else choice
    item = handle$item()
    if (identical(item@group, target)) {
      return()
    }
    if (!is.na(target)) {
      group_handle = rx$groups[[question_name]][[target]]
      if (is.null(group_handle)) {
        return()
      }
      if (isTRUE(group_handle$group()@exclusive)) {
        conflicts = rx$selection_conflicts(
          question_name, c(rx$group_members(question_name, target), handle$id)
        )
        if (nrow(conflicts) > 0) {
          shiny::showModal(rubric_group_conflict_modal(
            glue::glue("Cannot move this item into \"{group_handle$group()@label}\""),
            conflicts
          ))
          return()
        }
      }
    }

    handles = rx$items[[question_name]]
    moved = move_item_to_group(names(handles), item_groups_of(handles), handle$id, target)
    item@group = target
    handle$update_item(item)
    rx$reorder_items(question_name, moved$ids)
  }

  # A user toggle (button click or hotkey) for the repository on screen: the
  # item's new state, and in an exclusive group the deselection of its
  # selected siblings, are written as grade rows at once, here where the
  # action happens. Loading a repository's stored selections goes through
  # update_item() instead (apply_saved_selections()), so a load never writes.
  toggle_item = function(question_name, handle) {
    repo = shiny::isolate(rx$selected_repo())
    if (is.null(repo) || !identical(question_name, shiny::isolate(input$question_select))) {
      return()
    }
    item = handle$item()
    item@selected = !item@selected
    handle$update_item(item)
    writes = stats::setNames(list(item@selected), handle$id)
    if (item@selected && rx$group_is_exclusive(question_name, item@group)) {
      for (sibling in rx$items[[question_name]]) {
        if (identical(sibling$id, handle$id)) {
          next
        }
        other = sibling$item()
        if (identical(other@group, item@group) && other@selected) {
          other@selected = FALSE
          sibling$update_item(other)
          writes[[sibling$id]] = FALSE
        }
      }
    }
    save_grade_selections(root, question_name, repo, writes)
    rx$bump_grading_version()
  }

  # Put a row's description text back to what is stored, in both views,
  # after an edit was refused
  reset_description = function(handle) {
    session$sendCustomMessage("markermd_item_text", list(
      ids = as.list(rx$all_views_ids(handle$id, "description_text")),
      text = handle$item()@description
    ))
  }

  # A points or description edit: persist it when it changes the item, as
  # a recorded action. Descriptions identify items in the exchange formats,
  # so a blank one or one another item of the question already uses is
  # refused and the row put back. The hotkey, group and selection are
  # carried over untouched.
  edit_item = function(question_name, handle, description = NULL, points = NULL) {
    item = handle$item()
    if (!is.null(description)) {
      description = trimws(description)
      if (!nzchar(description)) {
        shiny::showNotification("A rubric item needs a description.", type = "warning")
        reset_description(handle)
        return()
      }
      others = rx$items[[question_name]]
      others = others[setdiff(names(others), handle$id)]
      if (description %in% vapply(others, function(other) other$item()@description, character(1))) {
        shiny::showNotification(
          glue::glue("Another item of {question_name} is already described \"{description}\"; descriptions identify items, so they must differ."),
          type = "warning"
        )
        reset_description(handle)
        return()
      }
      item@description = description
    }
    if (!is.null(points)) {
      item@points = as.numeric(points)
    }
    if (identical(item@description, handle$item()@description) && identical(item@points, handle$item()@points)) {
      return()
    }
    handle$update_item(item)
    app_action(
      "edit rubric item",
      call = glue::glue("mark(): edit item \"{item@description}\" of {question_name}"),
      parts = "rubric",
      save_rubric_item(root, question_name, handle$id, item)
    )
  }

  # Confirm before deleting: the item is shared across all repos and its
  # recorded selections stop counting everywhere, so a stray click
  # mid-grading must not delete it
  pending_delete = NULL
  confirm_delete_item = function(question_name, handle) {
    pending_delete <<- list(question = question_name, id = handle$id)
    desc = handle$item()@description
    n_selected = item_selection_count(root, question_name, handle$id)
    undo = if (n_selected > 0) {
      glue::glue(
        "{n_selected} repositor{if (n_selected == 1) 'y has' else 'ies have'} it selected; ",
        "a snapshot is taken first, so the deletion can be undone from the History tab."
      )
    } else {
      "No repository has it selected."
    }
    shiny::showModal(shiny::modalDialog(
      title = "Delete rubric item?",
      paste(
        glue::glue("Delete the rubric item \"{desc}\"?"),
        "This removes it, and any recorded selections of it, for every repository.",
        undo
      ),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("confirm_delete_item"), "Delete", class = "btn-danger")
      )
    ))
  }

  delete_item = function(question_name, handle) {
    rx$remove_item_handle(question_name, handle$id)

    # Soft-delete the item (its grade-selection events stay in the log but
    # stop counting). When the item is selected somewhere, that grading stops
    # counting, so an automatic snapshot is taken in the same transaction;
    # restoring it brings the item and its selections back.
    n_selected = item_selection_count(root, question_name, handle$id)
    snapshot = if (n_selected > 0) {
      list(
        trigger = "mark(): delete item",
        summary = glue::glue(
          "Deleted item \"{handle$item()@description}\" of {question_name}, ",
          "selected by {n_selected} repositor{if (n_selected == 1) 'y' else 'ies'}"
        )
      )
    }
    app_action(
      "delete rubric item",
      call = glue::glue("mark(): delete item \"{handle$item()@description}\" of {question_name}"),
      summary = snapshot$summary, parts = "rubric",
      delete_rubric_item(root, question_name, handle$id, snapshot = snapshot)
    )
    rx$bump_grading_version()

    # Renumber the remaining items' hotkeys (the toggle buttons are patched
    # in place) and keep the database in step with what is shown
    remaining = rx$items[[question_name]]
    if (length(remaining) > 0) {
      persist_item_order(root, question_name, remaining)
    }

    # Remove only the deleted item's rows; the other rows keep their DOM
    # (and any in-progress edit) instead of being rebuilt
    shiny::removeUI(
      selector = paste0("#", rx$all_views_ids(handle$id, "container"), collapse = ", "),
      multiple = TRUE
    )
  }

  shiny::observe({
    event = input$item_event
    question_name = rx$item_questions[[event$item]]
    if (is.null(question_name)) {
      return()
    }
    handle = rx$items[[question_name]][[event$item]]
    if (is.null(handle)) {
      return()
    }
    switch(event$action,
      toggle = toggle_item(question_name, handle),
      move_up = move_block(question_name, handle$id, -1),
      move_down = move_block(question_name, handle$id, 1),
      group = set_item_group(question_name, handle, event$value),
      delete = confirm_delete_item(question_name, handle),
      description = edit_item(question_name, handle, description = as.character(event$value)),
      points = if (is.numeric(event$value)) edit_item(question_name, handle, points = event$value)
    )
  }) |>
    shiny::bindEvent(input$item_event)

  shiny::observe({
    shiny::removeModal()
    pending = pending_delete
    pending_delete <<- NULL
    shiny::req(pending)
    handle = rx$items[[pending$question]][[pending$id]]
    if (!is.null(handle)) {
      delete_item(pending$question, handle)
    }
  }) |>
    shiny::bindEvent(input$confirm_delete_item)

  # --- Groups ----------------------------------------------------------------

  # Patch a group's header, settings controls and the items' menu entries
  # in both views (after a change, or to put back a refused one)
  patch_group = function(handle) {
    group = handle$group()
    session$sendCustomMessage("markermd_update_group", list(
      ids = as.list(rx$all_views_ids(handle$id, "container")),
      group = handle$id,
      label = group@label,
      chips = as.character(rubric_group_chips(group)),
      values = rubric_group_settings_values(group)
    ))
  }

  group_feedback = function(handle, text = "") {
    session$sendCustomMessage("markermd_group_feedback", list(
      ids = as.list(rx$all_views_ids(handle$id, "settings_feedback")),
      text = text
    ))
  }

  commit_group = function(question_name, handle, new_group) {
    handle$update_group(new_group)
    app_action(
      "edit item group",
      call = glue::glue("mark(): edit group \"{new_group@label}\" of {question_name}"),
      parts = "rubric",
      save_item_group(root, question_name, handle$id, new_group)
    )
    group_feedback(handle)
    patch_group(handle)
  }

  # A settings change from a group's popover. A blank label or one another
  # group of the question uses is refused and put back; an out-of-sign bound
  # is left in the field, unsaved, with the reason shown; exclusivity is
  # refused while any repository has more than one of the group's items
  # selected (deselecting across repositories is a grading decision the app
  # must not make silently).
  edit_group = function(question_name, handle, field, value) {
    current = handle$group()
    if (identical(field, "label")) {
      label = if (is.null(value)) "" else trimws(as.character(value))
      if (identical(label, current@label)) {
        return()
      }
      others = rx$groups_of(question_name)
      others = others[setdiff(names(others), handle$id)]
      problem = if (!nzchar(label)) {
        "A group needs a label."
      } else if (label %in% vapply(others, function(g) g@label, character(1))) {
        glue::glue("Another group is already named \"{label}\".")
      }
      if (!is.null(problem)) {
        group_feedback(handle, problem)
        patch_group(handle)
        return()
      }
      current@label = label
      commit_group(question_name, handle, current)
    } else if (field %in% c("floor", "ceiling")) {
      value = if (is.null(value) || !is.numeric(value) || is.na(value)) NA_real_ else as.numeric(value)
      if (identical(value, as.numeric(S7::prop(current, field)))) {
        return()
      }
      valid = if (identical(field, "floor")) is.na(value) || value <= 0 else is.na(value) || value >= 0
      if (!valid) {
        group_feedback(handle, if (identical(field, "floor")) {
          "The floor must be 0 or negative (or blank for none)."
        } else {
          "The ceiling must be 0 or positive (or blank for none)."
        })
        return()
      }
      S7::prop(current, field) = value
      commit_group(question_name, handle, current)
    } else if (identical(field, "exclusive")) {
      value = isTRUE(value)
      if (identical(value, current@exclusive)) {
        return()
      }
      if (value) {
        conflicts = rx$selection_conflicts(question_name, rx$group_members(question_name, handle$id))
        if (nrow(conflicts) > 0) {
          patch_group(handle)
          shiny::showModal(rubric_group_conflict_modal(
            glue::glue("Cannot make \"{current@label}\" exclusive"),
            conflicts
          ))
          return()
        }
      }
      current@exclusive = value
      commit_group(question_name, handle, current)
    }
  }

  # Confirm before ungrouping: the bounds stop applying for every
  # repository, which changes recorded scores across the class
  pending_ungroup = NULL
  confirm_ungroup = function(question_name, handle) {
    pending_ungroup <<- list(question = question_name, id = handle$id)
    for (id in rx$all_views_ids(handle$id, "settings_popover")) {
      session$sendCustomMessage("markermd_show_popover", list(id = id, value = "hide"))
    }
    shiny::showModal(shiny::modalDialog(
      title = "Ungroup these items?",
      glue::glue(
        "The items of \"{handle$group()@label}\" stay in the rubric as ungrouped items, ",
        "but the group's bounds no longer apply to any repository's score."
      ),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("confirm_ungroup"), "Ungroup", class = "btn-danger")
      )
    ))
  }

  # Ungroup: the members become ungrouped items in place, the group row is
  # deleted and its handle dropped
  ungroup = function(question_name, group_id) {
    handles = rx$items[[question_name]]
    label = rx$groups[[question_name]][[group_id]]$group()@label
    for (handle in handles) {
      item = handle$item()
      if (identical(item@group, group_id)) {
        item@group = NA_character_
        handle$update_item(item)
      }
    }
    app_action(
      "delete item group",
      call = glue::glue("mark(): delete group \"{label}\" of {question_name}"),
      parts = "rubric",
      delete_item_group(root, question_name, group_id, snapshot = list(
        trigger = "mark(): delete group",
        summary = glue::glue("Removed group \"{label}\" of {question_name}; its items are now ungrouped")
      ))
    )
    rx$remove_group_handle(question_name, group_id)
    if (length(handles) > 0) {
      persist_item_order(root, question_name, handles)
    }
    rx$redraw()
  }

  shiny::observe({
    event = input$group_event
    question_name = rx$group_questions[[event$group]]
    if (is.null(question_name)) {
      return()
    }
    handle = rx$groups[[question_name]][[event$group]]
    if (is.null(handle)) {
      return()
    }
    switch(event$action,
      move_up = move_block(question_name, handle$id, -1),
      move_down = move_block(question_name, handle$id, 1),
      ungroup = confirm_ungroup(question_name, handle),
      label = ,
      floor = ,
      ceiling = ,
      exclusive = edit_group(question_name, handle, event$action, event$value)
    )
  }) |>
    shiny::bindEvent(input$group_event)

  shiny::observe({
    shiny::removeModal()
    pending = pending_ungroup
    pending_ungroup <<- NULL
    shiny::req(pending)
    if (!is.null(rx$groups[[pending$question]][[pending$id]])) {
      ungroup(pending$question, pending$id)
    }
  }) |>
    shiny::bindEvent(input$confirm_ungroup)

  # --- Adding ----------------------------------------------------------------

  # Add an item with a placeholder description at the end of a question. In
  # the Grade tab the new row is inserted into the list in place, so
  # in-progress edits on the others survive; otherwise the views redraw.
  #
  # question_name: The question to add the item to
  rx$add_item_to = function(question_name) {
    rx$ensure_question_loaded(question_name)
    handles = rx$items[[question_name]]
    item_id = rx$new_ids(1)

    # The new item goes last (ungrouped); past the tenth position it has no
    # hotkey and is toggled by clicking. It starts with a placeholder
    # description unique in the question (descriptions identify items, so a
    # blank one is never stored); the row selects it on focus so typing
    # replaces it.
    descriptions = vapply(handles, function(handle) handle$item()@description, character(1))
    description = "New item"
    k = 2
    while (description %in% descriptions) {
      description = paste("New item", k)
      k = k + 1
    }
    new_item = markermd_rubric_item(position_hotkey(length(handles) + 1), 0, description)
    handle = rx$add_item_handle(question_name, item_id, new_item)

    app_action(
      "add rubric item",
      call = glue::glue("mark(): add item to {question_name}"),
      parts = "rubric",
      save_rubric_item(root, question_name, item_id, new_item)
    )

    if (identical(shiny::isolate(rx$active_tab()), "rubric") ||
          !identical(question_name, shiny::isolate(input$question_select))) {
      rx$redraw()
      return(invisible(handle))
    }

    # Insert only the new row rather than re-rendering the whole list, after
    # the last item block, ahead of any empty groups. The Rubric tab's view
    # redraws whenever that tab is shown.
    shiny::insertUI(
      selector = paste0("#", session$ns("rubric_blocks")),
      where = "beforeEnd",
      ui = mark_rubric_item_ui(rx$view_id("grade", item_id), new_item, fresh = TRUE)
    )
    invisible(handle)
  }

  # Add an empty group with a fresh label to a question, rendered after its
  # last item with its settings popover opened
  #
  # question_name: The question to add the group to
  rx$add_group_to = function(question_name) {
    rx$ensure_question_loaded(question_name)
    labels = vapply(rx$groups_of(question_name), function(g) g@label, character(1))
    label = "New group"
    k = 2
    while (label %in% labels) {
      label = paste("New group", k)
      k = k + 1
    }
    group_id = rx$new_ids(1, prefix = "group_")
    group = markermd_rubric_group(label = label)

    app_action(
      "add item group",
      call = glue::glue("mark(): add group to {question_name}"),
      parts = "rubric",
      save_item_group(root, question_name, group_id, group)
    )
    rx$add_group_handle(question_name, group_id, group)
    rx$redraw()

    view = if (identical(shiny::isolate(rx$active_tab()), "rubric")) "editor" else "grade"
    session$onFlushed(function() {
      session$sendCustomMessage("markermd_show_popover", list(
        id = rx$view_id(view, group_id, "settings_popover"), value = "show"
      ))
    }, once = TRUE)
  }

  # The Grade tab's add buttons act on the current question, the Rubric
  # tab's on their own question
  shiny::observe({
    rx$add_item_to(input$question_select)
  }) |>
    shiny::bindEvent(input$add_item, ignoreInit = TRUE)

  shiny::observe({
    rx$add_group_to(input$question_select)
  }) |>
    shiny::bindEvent(input$add_group, ignoreInit = TRUE)

  lapply(seq_along(question_names), function(k) {
    shiny::observe({
      rx$add_item_to(question_names[k])
    }) |>
      shiny::bindEvent(input[[paste0("editor_add_item_", k)]], ignoreInit = TRUE)
    shiny::observe({
      rx$add_group_to(question_names[k])
    }) |>
      shiny::bindEvent(input[[paste0("editor_add_group_", k)]], ignoreInit = TRUE)
  })

  invisible(rx)
}
