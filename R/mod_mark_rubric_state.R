# Rubric module: the item and group state
#
# The live rubric of mark()'s rubric module (mark_rubric_server()): per
# question, the item handles in display order (rubric_item_handle(), the
# source of truth for the item order and each item's group), the group
# handles keyed by group id (rubric_group_handle()) and the grade server.
# Neither items nor groups have module servers: their events arrive through
# the module's item_event and group_event inputs (mod_mark_rubric_actions.R),
# and both tabs' views render from these handles (mod_mark_rubric_views.R).
# rubric_state_wiring() is a plain wiring function in the style of
# template_persistence(): it fills the module's shared context rx with the
# state and the functions over it.

# Group ids of a question's item handles (NA for ungrouped items), in
# display order, named by item id
#
# handles: named list of item handles (names are item ids)

item_groups_of = function(handles) {
  stats::setNames(
    vapply(handles, function(handle) handle$item()@group, character(1), USE.NAMES = FALSE),
    names(handles)
  )
}

# Persist a question's item handles' current display order: renumber them to
# the hotkeys their positions imply (1-10, then NA; see position_hotkey()),
# push each new hotkey into the item's state, and write the order to the
# database (save_rubric_items() stores positions and group ids) so it
# survives an app restart. Shared by the move, delete, group-menu and
# ungroup handlers.
#
# root: project root holding the grading database
# question_name: the question whose items are being renumbered
# handles: named list of item handles (names are item ids) in display order

persist_item_order = function(root, question_name, handles) {
  ids = names(handles)
  items = list()
  for (i in seq_along(handles)) {
    updated_item = handles[[i]]$item()
    updated_item@hotkey = position_hotkey(i)
    handles[[i]]$update_item(updated_item)
    items[[ids[i]]] = updated_item
  }
  save_rubric_items(root, question_name, items)
}

# Fill the rubric module's context with its state: the handles, the grade
# servers, lazy loading, the selections of the pair on screen, the live
# score and the rebuild after a restore.
#
# rx: The module's shared context (an environment holding session, input,
#   root, template, question_names, database_state, selected_repo,
#   active_tab, on_commit)

rubric_state_wiring = function(rx) {
  session = rx$session
  input = rx$input
  root = rx$root
  question_names = rx$question_names
  database_state = rx$database_state

  # Bumped after grading data (selections, comments, deletions) is
  # persisted, so progress displays re-read the database only once the
  # write has actually happened
  rx$grading_data_version = shiny::reactiveVal(0L)
  rx$bump_grading_version = function() {
    rx$on_commit()
    rx$grading_data_version(shiny::isolate(rx$grading_data_version()) + 1L)
  }

  # The id of an element of one view's copy of an item or group: both tabs
  # draw their own rows ("grade" and "editor"), prefixed so both can be on
  # the page at once
  rx$view_id = function(view, id, suffix = NULL) {
    session$ns(paste0(view, "-", id, if (!is.null(suffix)) paste0("-", suffix)))
  }
  rx$all_views_ids = function(id, suffix = NULL) {
    c(rx$view_id("grade", id, suffix), rx$view_id("editor", id, suffix))
  }

  rx$items = shiny::reactiveValues()
  rx$groups = shiny::reactiveValues()
  rx$grade_servers = shiny::reactiveValues()
  # The question each item and group belongs to, by id
  rx$item_questions = new.env(parent = emptyenv())
  rx$group_questions = new.env(parent = emptyenv())

  for (name in question_names) {
    rx$items[[name]] = list()
    rx$groups[[name]] = list()
    local({
      question_name = name
      initial_grade_state = if (!is.null(database_state) &&
                                  !is.null(database_state$grade_states[[question_name]])) {
        database_state$grade_states[[question_name]]
      } else {
        markermd_grade_state(current_score = 0, total_score = 10)
      }
      rx$grade_servers[[question_name]] = mark_grade_server(
        paste0("grade_", question_dom_id(question_name)),
        initial_grade_state,
        ui_ns = session$ns,
        collection_path = root,
        question_name = question_name
      )
    })
  }

  # A question's groups as markermd_rubric_group objects keyed by group id
  rx$groups_of = function(question_name) {
    lapply(rx$groups[[question_name]], function(handle) handle$group())
  }

  # Ids of the items in one group of a question
  rx$group_members = function(question_name, group_id) {
    group_of = item_groups_of(rx$items[[question_name]])
    names(group_of)[!is.na(group_of) & group_of == group_id]
  }

  # Whether a group of a question is exclusive (an item in it gets a radio
  # toggle, and selecting it deselects its siblings)
  rx$group_is_exclusive = function(question_name, group_id) {
    if (is.na(group_id)) {
      return(FALSE)
    }
    handle = rx$groups[[question_name]][[group_id]]
    !is.null(handle) && isTRUE(handle$group()@exclusive)
  }

  # Repositories with more than one of the given items selected, with the
  # items' descriptions joined for the refusal modals
  rx$selection_conflicts = function(question_name, member_ids) {
    conflicts = load_group_selection_conflicts(root, question_name, member_ids)
    handles = rx$items[[question_name]]
    conflicts$items = vapply(conflicts$item_ids, function(ids) {
      paste(vapply(ids, function(item_id) handles[[item_id]]$item()@description, character(1)), collapse = "; ")
    }, character(1))
    conflicts
  }

  rx$redraw_ui = shiny::reactiveVal(0)
  rx$redraw = function() {
    rx$redraw_ui(shiny::isolate(rx$redraw_ui()) + 1)
  }

  # Bumped when an import or restore changes a question's scoring setup, so
  # the grade widget (whose popover inputs are baked into the rendered HTML)
  # rebuilds with the new values
  rx$grade_redraw = shiny::reactiveVal(0)

  # Put a question's items in a new display order, persist it (positions,
  # hotkeys and group ids) and redraw the lists
  rx$reorder_items = function(question_name, new_ids) {
    handles = rx$items[[question_name]][new_ids]
    rx$items[[question_name]] = handles
    persist_item_order(root, question_name, handles)
    rx$redraw()
  }

  # Create and register the state handle for one rubric item or group under
  # its question. Shared by the loader, the add handlers and the import
  # refresh.
  rx$add_item_handle = function(question_name, item_id, item) {
    handle = rubric_item_handle(item_id, item)
    rx$items[[question_name]][[item_id]] = handle
    assign(item_id, question_name, envir = rx$item_questions)
    handle
  }

  rx$add_group_handle = function(question_name, group_id, group) {
    handle = rubric_group_handle(group_id, group)
    rx$groups[[question_name]][[group_id]] = handle
    assign(group_id, question_name, envir = rx$group_questions)
    handle
  }

  rx$remove_item_handle = function(question_name, item_id) {
    rx$items[[question_name]][[item_id]] = NULL
    if (exists(item_id, envir = rx$item_questions, inherits = FALSE)) {
      rm(list = item_id, envir = rx$item_questions)
    }
  }

  rx$remove_group_handle = function(question_name, group_id) {
    handles = rx$groups[[question_name]]
    handles[[group_id]] = NULL
    rx$groups[[question_name]] = handles
    if (exists(group_id, envir = rx$group_questions, inherits = FALSE)) {
      rm(list = group_id, envir = rx$group_questions)
    }
  }

  # Bring a question's group handles in line with the database: update the
  # groups that survived, add the new ones, drop the rest
  rx$sync_question_groups = function(question_name) {
    stored = load_item_groups(root, question_name)
    handles = rx$groups[[question_name]]
    for (group_id in names(stored)) {
      if (is.null(handles[[group_id]])) {
        rx$add_group_handle(question_name, group_id, stored[[group_id]])
      } else {
        handles[[group_id]]$update_group(stored[[group_id]])
      }
    }
    for (group_id in setdiff(names(handles), names(stored))) {
      rx$remove_group_handle(question_name, group_id)
    }
  }

  # Replace a question's item handles with the given items (in order),
  # after its groups are synced
  rx$rebuild_question_items = function(question_name, items) {
    for (item_id in names(rx$items[[question_name]])) {
      rx$remove_item_handle(question_name, item_id)
    }
    rx$items[[question_name]] = list()
    for (item_id in names(items)) {
      rx$add_item_handle(question_name, item_id, items[[item_id]])
    }
  }

  # Fresh item or group ids: above every id the database ever stored
  # (deleted ones included, see used_item_ids()) and every id in memory
  rx$new_ids = function(n, prefix = "item_") {
    in_memory = unlist(lapply(question_names, function(q) c(names(rx$items[[q]]), names(rx$groups[[q]]))))
    stored = with_database(root, used_item_ids)
    next_item_ids(union(stored, in_memory), n, prefix = prefix)
  }

  # A question's handles are built the first time it is needed (shown in
  # the Grade tab, the Rubric tab opening, an item or group added to it),
  # not at startup, so a rubric of a hundred items costs nothing before the
  # first page shows. Returns TRUE when it built them (the caller redraws).
  rx$loaded_questions = character(0)
  rx$ensure_question_loaded = function(question_name) {
    if (is.null(database_state) || question_name %in% rx$loaded_questions) {
      return(FALSE)
    }
    rx$loaded_questions = c(rx$loaded_questions, question_name)
    rx$sync_question_groups(question_name)
    rx$rebuild_question_items(question_name, load_rubric_items(root, question_name))
    TRUE
  }

  # The question shown in the Grade tab is loaded first (a higher priority
  # than the observers and outputs that read its handles)
  shiny::observe({
    shiny::req(input$question_select)
    if (rx$ensure_question_loaded(input$question_select)) {
      rx$redraw()
      rx$apply_saved_selections()
    }
  }, priority = 10) |>
    shiny::bindEvent(input$question_select)

  # The Rubric tab shows every question
  shiny::observe({
    if (!identical(rx$active_tab(), "rubric")) {
      return()
    }
    loaded = vapply(question_names, rx$ensure_question_loaded, logical(1))
    if (any(loaded)) {
      rx$redraw()
    }
  }, priority = 10) |>
    shiny::bindEvent(rx$active_tab())

  # Put the current question's items in the selection state the database
  # holds for the current repository. Nothing is written: grade rows are
  # written by toggle_item() alone.
  rx$apply_saved_selections = function() {
    current_question = shiny::isolate(input$question_select)
    current_repo = shiny::isolate(rx$selected_repo())
    if (is.null(current_question) || is.null(current_repo) ||
          !current_question %in% names(rx$items)) {
      return(invisible(NULL))
    }
    handles = rx$items[[current_question]]
    if (length(handles) == 0) {
      return(invisible(NULL))
    }
    saved = load_grade_selections(root, current_question, current_repo)
    for (item_id in names(handles)) {
      handle = handles[[item_id]]
      item = handle$item()
      selected = if (item_id %in% names(saved)) saved[[item_id]] else FALSE
      if (item@selected != selected) {
        item@selected = selected
        handle$update_item(item)
      }
    }
    invisible(NULL)
  }

  shiny::observe({
    shiny::req(input$question_select, rx$selected_repo(), cancelOutput = TRUE)
    rx$apply_saved_selections()
  }) |>
    shiny::bindEvent(input$question_select, rx$selected_repo(), ignoreInit = TRUE)

  # Rebuild every question's items, groups and scoring from the database
  # after a snapshot restore, then put the current question back in the
  # state the database holds
  rx$reload_from_db = function() {
    state = initialize_database_state(root, rx$template)
    rx$loaded_questions = question_names
    for (question_name in question_names) {
      rx$sync_question_groups(question_name)
      rx$rebuild_question_items(question_name, state$rubric_items[[question_name]])
      grade_server = rx$grade_servers[[question_name]]
      if (!is.null(grade_server)) {
        grade_server$update_grade(state$grade_states[[question_name]])
      }
    }
    rx$apply_saved_selections()
    rx$redraw()
    rx$grade_redraw(shiny::isolate(rx$grade_redraw()) + 1)
    # A read, not a write: the restore that preceded it noted its own commit
    rx$grading_data_version(shiny::isolate(rx$grading_data_version()) + 1L)
    invisible(NULL)
  }

  # The current question's score from its live selections, groups and
  # scoring setup (see score_question())
  rx$question_score = shiny::reactive({
    shiny::req(input$question_select)
    question_name = input$question_select
    items = lapply(rx$items[[question_name]], function(handle) handle$item())
    selected_ids = names(items)[vapply(items, function(item) item@selected, logical(1))]
    score_question(
      items, selected_ids, rx$groups_of(question_name),
      rx$grade_servers[[question_name]]$grade()
    )
  })

  invisible(rx)
}
