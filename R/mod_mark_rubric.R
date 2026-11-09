# Mark Rubric Interface Module
#
# Shiny module for displaying and navigating rubric questions during marking

# Compute the next selection in a list of choices, wrapping around at the ends.
# Returns NULL when there is nothing to move to (one or zero choices, a NULL
# current selection, or a current value not present in choices).
#
# current: currently selected value
# choices: character vector of available choices
# direction: -1 for previous, 1 for next

navigate_select = function(current, choices, direction) {
  if (length(choices) <= 1 || is.null(current)) {
    return(NULL)
  }

  current_index = match(current, choices)
  if (is.na(current_index)) {
    return(NULL)
  }

  new_index = (current_index - 1 + direction) %% length(choices) + 1
  choices[new_index]
}

# Wire a prev/next button to a select input: each click moves the selection
# one choice in the given direction, wrapping at the ends (see
# navigate_select()). Shared by the rubric (questions) and content (repos)
# module servers.
#
# input, session: the calling module server's input and session objects
# trigger_id: button input id
# select_id: select input id the button advances
# choices: character vector of the select's choices
# direction: -1 for previous, 1 for next

navigate_button = function(input, session, trigger_id, select_id, choices, direction) {
  shiny::observe({
    new_selection = navigate_select(
      shiny::isolate(input[[select_id]]),
      choices,
      direction = direction
    )
    if (!is.null(new_selection)) {
      shiny::updateSelectInput(session, select_id, selected = new_selection)
    }
  }) |>
    shiny::bindEvent(input[[trigger_id]], ignoreInit = TRUE)
}

# Group ids of a question's item servers (NA for ungrouped items), in
# display order, named by item id
#
# servers: named list of item servers (names are item ids)

item_groups_of = function(servers) {
  stats::setNames(
    vapply(servers, function(server) server$item()@group, character(1), USE.NAMES = FALSE),
    names(servers)
  )
}

# Persist a question's item servers' current display order: renumber them to
# the hotkeys their positions imply (1-10, then NA; see position_hotkey()),
# push each new hotkey into the item server's state, and write the order to
# the database (save_rubric_items() stores positions and group ids) so it
# survives an app restart. Shared by the move, delete, group-menu and
# ungroup handlers.
#
# root: project root holding the grading database
# question_name: the question whose items are being renumbered
# servers: named list of item servers (names are item ids) in display order

persist_item_order = function(root, question_name, servers) {
  ids = names(servers)
  items = list()
  for (i in seq_along(servers)) {
    updated_item = servers[[i]]$item()
    updated_item@hotkey = position_hotkey(i)
    servers[[i]]$update_item(updated_item)
    items[[ids[i]]] = updated_item
  }
  save_rubric_items(root, question_name, items)
}

# Mark Rubric UI
#
# id: Character. Module namespace ID

mark_rubric_ui = function(id) {
  ns = shiny::NS(id)

  add_button_style = "width: 30px; height: 30px; display: inline-flex; align-items: center; justify-content: center; padding: 0;"

  bslib::card(
    class = "h-100",
    bslib::card_header(
      class = "bg-light",
      shiny::div(
        class = "d-flex justify-content-between align-items-center w-100",
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::span("Rubric"),
          # Import/Export tucked into a popover off an exchange-arrows icon,
          # shared with the template app's save controls (io_menu_ui)
          io_menu_ui(
            id = ns("rubric_io_menu"),
            title = "Rubric YAML",
            export_buttons = list(
              shiny::downloadButton(
                ns("export_question"),
                "Export question",
                class = "btn-outline-secondary btn-sm"
              ),
              shiny::downloadButton(
                ns("export_all"),
                "Export all",
                class = "btn-outline-secondary btn-sm"
              )
            ),
            import_input_id = ns("rubric_import_file"),
            button_title = "Import / Export rubric"
          ),
          # Take a snapshot of the grading state without leaving the tab; the
          # app opens the History tab's modal (see mark_history_server())
          bslib::tooltip(
            shiny::actionButton(
              ns("take_snapshot"),
              fa_icon("camera"),
              class = "btn-outline-secondary btn-sm"
            ),
            "Take a snapshot: record the current template, rubric and marks with a label and a summary, for comparing or restoring later from the History tab (shortcut: s)"
          )
        ),
        # Same fixed width as the content pane's repo selector so the two
        # headers' prev/select/next controls line up
        shiny::div(
          style = "width: 220px; display: flex; align-items: center; gap: 5px;",
          bslib::tooltip(
            shiny::actionButton(
              ns("question_prev_btn"),
              fa_icon("chevron-left"),
              class = "btn-sm",
              style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;"
            ),
            "Previous question (,)"
          ),
          shiny::div(
            style = "flex: 1;",
            shiny::selectInput(
              ns("question_select"),
              NULL,
              choices = NULL,
              width = "100%",
              selectize = TRUE
            )
          ),
          bslib::tooltip(
            shiny::actionButton(
              ns("question_next_btn"),
              fa_icon("chevron-right"),
              class = "btn-sm",
              style = "padding: 1px 4px; border: none; background: transparent; color: #6c757d; font-size: 12px;"
            ),
            "Next question (.)"
          )
        )
      )
    ),
    bslib::card_body(
      class = "overflow-auto small",
      id = ns("rubric_body"),
      # Grade display at top
      shiny::uiOutput(ns("grade_ui")),
      shiny::div(
        id = ns("rubric_items_container"),
        # Dynamic container for items and groups
        shiny::uiOutput(ns("rubric_items_ui"))
      ),
      shiny::div(
        class = "d-flex justify-content-center align-items-center gap-4",
        shiny::span(
          shiny::actionButton(
            ns("add_item"),
            fa_icon("plus"),
            class = "btn-primary btn-sm rounded-circle",
            style = add_button_style,
            title = "Add Item"
          ),
          shiny::span("Add Item", class = "ms-2 text-dark")
        ),
        # Add Group is the only way to create a group; the items' group menus
        # only move items between existing groups
        shiny::span(
          shiny::actionButton(
            ns("add_group"),
            fa_icon("layer-group"),
            class = "btn-outline-primary btn-sm rounded-circle",
            style = add_button_style,
            title = "Add Group"
          ),
          shiny::span("Add Group", class = "ms-2 text-dark")
        )
      ),
      # Hidden file input the Import menu button triggers directly. Bound
      # once here so the upload binding survives item-list re-renders.
      shiny::div(
        class = "visually-hidden",
        shiny::fileInput(ns("rubric_import_file"), NULL, accept = c(".yaml", ".yml", "text/yaml"))
      ),
      rubric_groups_js()
    ),
    # Per-question comments for the current repo, pinned in their own card
    # body below the scrolling item list. A non-empty public comment counts
    # the question as graded, so full-credit / zero-deduction answers can be
    # marked as reviewed without selecting a rubric item. The private
    # textarea is a separate grader-internal channel that is never shared
    # with students and does not affect grading progress.
    bslib::card_body(
      fill = FALSE,
      class = "small border-top py-2",
      htmltools::tagAppendAttributes(
        shiny::textAreaInput(
          ns("question_comment"),
          NULL,
          value = "",
          rows = 2,
          width = "100%",
          placeholder = "Additional comments (also marks the question as graded)"
        ),
        class = "mb-1"
      ),
      htmltools::tagAppendAttributes(
        shiny::textAreaInput(
          ns("question_private_comment"),
          NULL,
          value = "",
          rows = 2,
          width = "100%",
          placeholder = "Private notes (never shared with students)"
        ),
        class = "mb-1"
      ),
      shiny::p(
        "Grading saves automatically to the project database.",
        class = "text-center text-muted small mb-0"
      )
    )
  )
}

# The Rubric tab: every question's rubric in one scrolling card, edited in
# place (see output$rubric_editor_ui), without the documents. It shares the
# rubric module (and so its state) with the Grade tab.
#
# id: Character. The rubric module's namespace ID

mark_rubric_editor_ui = function(id) {
  ns = shiny::NS(id)
  bslib::card(
    class = "h-100",
    bslib::card_header(
      class = "bg-light d-flex justify-content-between align-items-center",
      shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::span("Rubric"),
        # The same YAML import / export as the Grade tab's rubric card, with
        # this tab's own input ids (both tabs' panes are in the page)
        io_menu_ui(
          id = ns("editor_io_menu"),
          title = "Rubric YAML",
          export_buttons = list(
            shiny::downloadButton(ns("editor_export_all"), "Export all", class = "btn-outline-secondary btn-sm")
          ),
          import_input_id = ns("editor_import_file"),
          button_title = "Import / Export rubric"
        ),
        shiny::div(
          class = "visually-hidden",
          shiny::fileInput(ns("editor_import_file"), NULL, accept = c(".yaml", ".yml", "text/yaml"))
        )
      ),
      shiny::div(
        class = "rubric-editor-jump",
        shiny::selectizeInput(
          ns("editor_jump"), label = NULL, choices = NULL, width = "220px",
          options = list(placeholder = "Jump to question\u2026")
        )
      )
    ),
    bslib::card_body(
      class = "overflow-auto",
      shiny::tags$style(shiny::HTML(".rubric-editor .rubric-group-subtotal { display: none; } .rubric-editor-settings .total-row .shiny-input-container { margin-bottom: 0; } .rubric-editor-settings input[type=number] { height: 28px; padding: 2px 6px; font-size: 12px; } .rubric-editor-jump .shiny-input-container { margin-bottom: 0; } .rubric-editor .rubric-action-btns { opacity: 1; }")),
      shiny::uiOutput(ns("rubric_editor_ui"))
    )
  )
}

# Keyboard hotkey script for the Grade tab: numeric keys toggle rubric
# items, z/x navigate repositories, ,/. navigate questions, h toggles the
# HTML/source view. The ids it drives span both the rubric and content
# modules, so it takes both namespace functions and is included by mark-app's
# Grade tab rather than either module's UI.
#
# rubric_ns: the rubric module's namespace function
# content_ns: the content module's namespace function

rubric_hotkeys_js = function(rubric_ns, content_ns) {
  shiny::tags$script(shiny::HTML(glue::glue("
      $(document).ready(function() {
        // Global keydown listener for hotkeys and navigation when rubric pane is active
        document.addEventListener('keydown', function(e) {
          // Only act when the rubric pane is actually visible: hidden bslib
          // tab panes stay in the DOM, so an existence check alone would keep
          // the hotkeys live (and silently mutating grades) on other tabs.
          var rubricBody = document.getElementById('<<rubric_ns('rubric_body')>>');
          if (!rubricBody || rubricBody.offsetParent === null) return;

          // Suspend hotkeys while a modal dialog is open (e.g. the rubric
          // item delete confirmation): toggling items or moving the question
          // selection behind a modal silently corrupts what gets deleted/saved
          if (document.body.classList.contains('modal-open')) return;

          // Check if any input is currently focused or if user is editing
          var activeElement = document.activeElement;
          var isInputFocused = activeElement && (
            activeElement.matches('input, textarea, select, [contenteditable=\"true\"]') ||
            activeElement.isContentEditable ||
            activeElement.tagName === 'INPUT' ||
            activeElement.tagName === 'TEXTAREA'
          );

          // Also check if there's a text selection in the page
          var hasSelection = window.getSelection && window.getSelection().toString().length > 0;

          // Browser and OS shortcuts (Cmd/Ctrl+1..9 switch tabs, Cmd+Z undoes)
          // are not ours
          if (e.metaKey || e.ctrlKey || e.altKey) return;

          // Only proceed if nothing is being edited and rubric pane is visible
          if (!isInputFocused && !hasSelection) {

            // Handle numeric keys 0-9 for hotkeys
            if (e.key >= '0' && e.key <= '9') {
              // Map key to hotkey number (0 = 10, 1-9 = 1-9)
              var hotkey = e.key === '0' ? 10 : parseInt(e.key);

              // Find button with matching hotkey in rubric pane
              var buttons = document.querySelectorAll('#<<rubric_ns('rubric_items_container')>> button');
              for (var i = 0; i < buttons.length; i++) {
                var btn = buttons[i];
                if (btn.textContent.trim() === hotkey.toString() || 
                    (hotkey === 10 && btn.textContent.trim() === '0')) {
                  e.preventDefault();
                  btn.click();
                  break;
                }
              }
            }

            // Handle navigation keys by triggering button clicks
            else if (e.key === 'x' || e.key === 'z') {
              // Repository navigation (x = next, z = previous)
              e.preventDefault();

              var btnId = e.key === 'x' ? '<<content_ns(\"repo_next_btn\")>>' : '<<content_ns(\"repo_prev_btn\")>>';
              var btn = document.getElementById(btnId);
              if (btn) {
                btn.click();
              }
            }

            else if (e.key === ',' || e.key === '.') {
              // Question navigation (, = previous, . = next)
              e.preventDefault();

              var btnId = e.key === ',' ? '<<rubric_ns(\"question_prev_btn\")>>' : '<<rubric_ns(\"question_next_btn\")>>';
              var btn = document.getElementById(btnId);
              if (btn) {
                btn.click();
              }
            }

            else if (e.key === 'h') {
              // Toggle HTML switch
              e.preventDefault();

              var htmlToggle = document.getElementById('<<content_ns(\"html_toggle\")>>');
              if (htmlToggle) {
                htmlToggle.click();
              }
            }
          }
        });
      });
    ", .open = "<<", .close = ">>")))
}

# Mark Rubric Server
#
# id: Character. Module namespace ID
# template: markermd_template. Static template object containing questions
# repos: Character vector of student repo names, for the graded check-mark
#   labels pushed into the content pane's repo selector
# root: Character string. Project root directory (base for the grading database)
# database_state: List. Database state loaded from SQLite (optional)
# selected_repo: Reactive returning the repo shown in the content pane (the
#   content module's selected_repo), which grading and comments key on
# set_repo_labels: function(choices, selected) rewriting the content pane's
#   repo selector labels (the content module's set_repo_labels)
# external_version: Reactive bumped when the database changed outside the app
# active_tab: Reactive returning the app's selected navbar tab
# app_action: Function(fn, code, call, summary, parts) running a bulk
#   operation as a recorded action of the app (see mark-app.R); the default
#   just runs the code

mark_rubric_server = function(id, template, repos, root, database_state = NULL, selected_repo, set_repo_labels,
                              external_version = shiny::reactive(0L), active_tab = shiny::reactive("grade"),
                              app_action = function(fn, code, ...) code) {
  shiny::moduleServer(id, function(input, output, session) {

    # Which tab draws the rubric's items, groups and scoring inputs: the
    # Grade tab ("grade", the current question) or the Rubric tab ("rubric",
    # every question). Their module ids are shared, so only one may be on
    # the page at a time: on a switch between the two, nothing draws them
    # for one flush (the leaving tab's outputs clear), and the arriving tab
    # draws them after it. Other tabs leave the owner as it was.
    items_owner = shiny::reactiveVal("grade")
    shiny::observe({
      tab = active_tab()
      if (!tab %in% c("grade", "rubric") || identical(tab, shiny::isolate(items_owner()))) {
        return()
      }
      items_owner("none")
      session$onFlushed(function() items_owner(tab), once = TRUE)
    }) |>
      shiny::bindEvent(active_tab())

    # Global id index
    id_idx = 0

    # Every item and group id this session has ever bound a module server
    # to. Module ids share one namespace and dead module instances keep their
    # input observers registered, so an id must never be re-bound within a
    # session even when it is gone from both the database and the server
    # lists (e.g. deleted earlier, or replaced by a rubric import).
    session_item_ids = character(0)
    register_session_id = function(item_id) {
      session_item_ids <<- union(session_item_ids, item_id)
    }

    question_names = template_question_names(template)

    # Bumped after grading data (selections, comments, deletions) is
    # persisted, so progress displays re-read the database only once the
    # write has actually happened
    grading_data_version = shiny::reactiveVal(0L)
    bump_grading_version = function() {
      grading_data_version(shiny::isolate(grading_data_version()) + 1L)
    }

    # Per question: the item handles in display order (rubric_item_handle(),
    # the item's live state; the source of truth for the item order and each
    # item's group), the group servers keyed by group id, and the grade
    # server. "Item servers" below means these handles: items have no module
    # servers, their row events arrive through input$item_event.
    question_item_servers = shiny::reactiveValues()
    question_group_servers = shiny::reactiveValues()
    question_grade_servers = shiny::reactiveValues()
    for(name in question_names) {
      question_item_servers[[name]] = list()
      question_group_servers[[name]] = list()
      # Initialize grade server for each question using database state if available
      local({
        question_name = name
        initial_grade_state = if (!is.null(database_state) && 
                                   !is.null(database_state$grade_states[[question_name]])) {
          database_state$grade_states[[question_name]]
        } else {
          markermd_grade_state(current_score = 0, total_score = 10)
        }

        question_grade_servers[[question_name]] = mark_grade_server(
          paste0("grade_", question_dom_id(question_name)),
          initial_grade_state,
          ui_ns = session$ns,
          collection_path = root,
          question_name = question_name
        )
      })
    }

    # A question's groups as markermd_rubric_group objects keyed by group id
    groups_of = function(question_name) {
      lapply(question_group_servers[[question_name]], function(server) server$group())
    }

    # Ids of the items in one group of a question
    group_members = function(question_name, group_id) {
      group_of = item_groups_of(question_item_servers[[question_name]])
      names(group_of)[!is.na(group_of) & group_of == group_id]
    }

    # Whether a group of a question is exclusive (an item in it gets a radio
    # toggle, and selecting it deselects its siblings)
    group_is_exclusive = function(question_name, group_id) {
      if (is.na(group_id)) {
        return(FALSE)
      }
      server = question_group_servers[[question_name]][[group_id]]
      !is.null(server) && isTRUE(server$group()@exclusive)
    }

    # Repositories with more than one of the given items selected, with the
    # items' descriptions joined for the refusal modals
    selection_conflicts = function(question_name, member_ids) {
      conflicts = load_group_selection_conflicts(root, question_name, member_ids)
      servers = question_item_servers[[question_name]]
      conflicts$items = vapply(conflicts$item_ids, function(ids) {
        paste(vapply(ids, function(item_id) servers[[item_id]]$item()@description, character(1)), collapse = "; ")
      }, character(1))
      conflicts
    }

    redraw_ui = shiny::reactiveVal(0)
    redraw = function() {
      redraw_ui(shiny::isolate(redraw_ui()) + 1)
    }

    # Bumped when a rubric import changes a question's scoring setup, so the
    # grade widget (whose popover inputs -- total points, mode, bounds -- are
    # baked into the rendered HTML) rebuilds with the imported values
    grade_redraw = shiny::reactiveVal(0)

    # Put a question's item servers in a new display order, persist it
    # (positions, hotkeys and group ids) and redraw the list
    reorder_items = function(question_name, new_ids) {
      servers = question_item_servers[[question_name]][new_ids]
      question_item_servers[[question_name]] = servers
      persist_item_order(root, question_name, servers)
      redraw()
    }

    # --- Item events -----------------------------------------------------------
    # Items have no module servers: each is a rubric_item_handle() in
    # question_item_servers, and every interaction with an item row arrives
    # as one item_event ({item, action, value}; see rubric_item_editing_head())
    # handled here. The question an item belongs to, by item id:
    item_questions = new.env(parent = emptyenv())

    # Move an item one position in the given direction (-1 up, 1 down)
    # within its group, or among the top-level blocks when ungrouped,
    # wrapping at the ends (see move_in_blocks()), then renumber
    move_item = function(question_name, server, direction) {
      servers = question_item_servers[[question_name]]
      ids = names(servers)
      new_ids = move_in_blocks(ids, item_groups_of(servers), server$id, direction)
      if (identical(new_ids, ids)) {
        return()
      }
      reorder_items(question_name, new_ids)
    }

    # The group menu: move the item to the end of the chosen group, or out
    # of its group (see move_item_to_group()). Moving into an exclusive
    # group is refused while any repository would end up with two of its
    # items selected, like turning the exclusive switch on.
    #
    # choice: The chosen group id, or "" / NULL for no group
    set_item_group = function(question_name, server, choice) {
      target = if (is.null(choice) || !nzchar(choice)) NA_character_ else choice
      item = server$item()
      if (identical(item@group, target)) {
        return()
      }
      if (!is.na(target)) {
        group_server = question_group_servers[[question_name]][[target]]
        if (is.null(group_server)) {
          return()
        }
        if (isTRUE(group_server$group()@exclusive)) {
          conflicts = selection_conflicts(
            question_name, c(group_members(question_name, target), server$id)
          )
          if (nrow(conflicts) > 0) {
            shiny::showModal(rubric_group_conflict_modal(
              glue::glue("Cannot move this item into \"{group_server$group()@label}\""),
              conflicts
            ))
            return()
          }
        }
      }

      servers = question_item_servers[[question_name]]
      moved = move_item_to_group(names(servers), item_groups_of(servers), server$id, target)
      item@group = target
      server$update_item(item)
      reorder_items(question_name, moved$ids)
    }

    # A user toggle (button click or hotkey) for the repository on screen:
    # the item's new state, and in an exclusive group the deselection of its
    # selected siblings, are written as grade rows at once, here where the
    # action happens. Loading a repository's stored selections goes through
    # update_item() instead (apply_saved_selections()), so a load never
    # writes.
    toggle_item = function(question_name, server) {
      repo = shiny::isolate(selected_repo())
      if (is.null(repo) || !identical(question_name, shiny::isolate(input$question_select))) {
        return()
      }
      item = server$item()
      item@selected = !item@selected
      server$update_item(item)
      writes = stats::setNames(list(item@selected), server$id)
      if (item@selected && group_is_exclusive(question_name, item@group)) {
        for (sibling in question_item_servers[[question_name]]) {
          if (identical(sibling$id, server$id)) {
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
      bump_grading_version()
    }

    # Put a row's description text back to what is stored, after an edit
    # was refused
    reset_description = function(server) {
      session$sendCustomMessage("markermd_item_text", list(
        id = session$ns(paste0(server$id, "-description_text")),
        text = server$item()@description
      ))
    }

    # A points or description edit: persist it when it changes the item,
    # as a recorded action. Descriptions identify items in the exchange
    # formats, so a blank one or one another item of the question already
    # uses is refused and the row put back. The hotkey, group and selection
    # are carried over untouched.
    edit_item = function(question_name, server, description = NULL, points = NULL) {
      item = server$item()
      if (!is.null(description)) {
        description = trimws(description)
        if (!nzchar(description)) {
          shiny::showNotification("A rubric item needs a description.", type = "warning")
          reset_description(server)
          return()
        }
        others = question_item_servers[[question_name]]
        others = others[setdiff(names(others), server$id)]
        if (description %in% vapply(others, function(other) other$item()@description, character(1))) {
          shiny::showNotification(
            glue::glue("Another item of {question_name} is already described \"{description}\"; descriptions identify items, so they must differ."),
            type = "warning"
          )
          reset_description(server)
          return()
        }
        item@description = description
      }
      if (!is.null(points)) {
        item@points = as.numeric(points)
      }
      if (identical(item@description, server$item()@description) && identical(item@points, server$item()@points)) {
        return()
      }
      server$update_item(item)
      app_action(
        "edit rubric item",
        call = glue::glue("mark(): edit item \"{item@description}\" of {question_name}"),
        parts = "rubric",
        save_rubric_item(root, question_name, server$id, item)
      )
    }

    # Confirm before deleting: the item is shared across all repos and its
    # recorded selections are removed everywhere, so a stray click mid-grading
    # must not destroy it (same pattern as question deletion in template())
    pending_delete = NULL
    confirm_delete_item = function(question_name, server) {
      pending_delete <<- list(question = question_name, id = server$id)
      desc = server$item()@description
      label = if (nchar(trimws(desc)) > 0) {
        glue::glue("Delete the rubric item \"{desc}\"?")
      } else {
        "Delete this rubric item?"
      }
      n_selected = item_selection_count(root, question_name, server$id)
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
          label,
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

    delete_item = function(question_name, server) {
      question_item_servers[[question_name]][[server$id]] = NULL
      rm(list = server$id, envir = item_questions)

      # Remove the item's row and its grade-selection events; otherwise it
      # reappears on the next app start via load_rubric_items(). When the
      # item is selected somewhere, that grading is about to be lost, so an
      # automatic snapshot is taken in the same transaction.
      n_selected = item_selection_count(root, question_name, server$id)
      snapshot = if (n_selected > 0) {
        list(
          trigger = "mark(): delete item",
          summary = glue::glue(
            "Deleted item \"{server$item()@description}\" of {question_name}, ",
            "selected by {n_selected} repositor{if (n_selected == 1) 'y' else 'ies'}"
          )
        )
      }
      app_action(
        "delete rubric item",
        call = glue::glue("mark(): delete item \"{server$item()@description}\" of {question_name}"),
        summary = snapshot$summary, parts = "rubric",
        delete_rubric_item(root, question_name, server$id, snapshot = snapshot)
      )
      bump_grading_version()

      # Renumber the remaining items' hotkeys (the toggle buttons are
      # patched in place) and keep the database in step with what is shown
      remaining_servers = question_item_servers[[question_name]]
      if (length(remaining_servers) > 0) {
        persist_item_order(root, question_name, remaining_servers)
      }

      # Remove only the deleted item; existing items keep their DOM (and any
      # in-progress edit) instead of being rebuilt by a full re-render
      shiny::removeUI(selector = paste0("#", session$ns(server$id), "-container"))
    }

    shiny::observe({
      event = input$item_event
      question_name = item_questions[[event$item]]
      if (is.null(question_name)) {
        return()
      }
      server = question_item_servers[[question_name]][[event$item]]
      if (is.null(server)) {
        return()
      }
      switch(event$action,
        toggle = toggle_item(question_name, server),
        move_up = move_item(question_name, server, -1),
        move_down = move_item(question_name, server, 1),
        group = set_item_group(question_name, server, event$value),
        delete = confirm_delete_item(question_name, server),
        description = edit_item(question_name, server, description = as.character(event$value)),
        points = if (is.numeric(event$value)) edit_item(question_name, server, points = event$value)
      )
    }) |>
      shiny::bindEvent(input$item_event)

    shiny::observe({
      shiny::removeModal()
      pending = pending_delete
      pending_delete <<- NULL
      shiny::req(pending)
      server = question_item_servers[[pending$question]][[pending$id]]
      if (!is.null(server)) {
        delete_item(pending$question, server)
      }
    }) |>
      shiny::bindEvent(input$confirm_delete_item)

    # Create and register the state handle for one rubric item, store it
    # under its question and reserve the id for the session. Shared by the
    # startup loader, the add-item handler and the rubric import refresh.
    #
    # question_name: Question the item belongs to
    # item_id: Database item id
    # item: markermd_rubric_item S7 object

    add_question_item_server = function(question_name, item_id, item) {
      server = rubric_item_handle(item_id, item)
      question_item_servers[[question_name]][[item_id]] = server
      assign(item_id, question_name, envir = item_questions)
      register_session_id(item_id)
      server
    }

    # --- Item groups ---------------------------------------------------------

    # The parent-side observer handles of each group server, by group id, so
    # they can be destroyed when the group goes away
    group_handles = list()

    # Remove a group's server and handlers (the members are the caller's job)
    remove_group_server = function(question_name, group_id) {
      servers = question_group_servers[[question_name]]
      servers[[group_id]] = NULL
      question_group_servers[[question_name]] = servers
      for (handle in group_handles[[group_id]]) {
        handle$destroy()
      }
      group_handles[[group_id]] <<- NULL
    }

    # Ungroup: the members become ungrouped items in place, the group row is
    # deleted and its server dropped
    ungroup = function(question_name, group_id) {
      servers = question_item_servers[[question_name]]
      label = question_group_servers[[question_name]][[group_id]]$group()@label
      for (server in servers) {
        item = server$item()
        if (identical(item@group, group_id)) {
          item@group = NA_character_
          server$update_item(item)
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
      remove_group_server(question_name, group_id)
      if (length(servers) > 0) {
        persist_item_order(root, question_name, servers)
      }
      redraw()
    }

    # Wire the parent-side handlers for one group server: moving the whole
    # block, ungrouping, and patching its header (and the items' menus) in
    # place when its label or bounds change
    wire_group_signals = function(question_name, server) {
      handles = list()

      move_group = function(direction) {
        servers = question_item_servers[[question_name]]
        ids = names(servers)
        new_ids = move_in_blocks(ids, item_groups_of(servers), server$id, direction)
        if (identical(new_ids, ids)) {
          return()
        }
        reorder_items(question_name, new_ids)
      }

      handles$move_up = shiny::observe({
        move_group(-1)
      }) |>
        shiny::bindEvent(server$move_up_signal(), ignoreInit = TRUE)

      handles$move_down = shiny::observe({
        move_group(1)
      }) |>
        shiny::bindEvent(server$move_down_signal(), ignoreInit = TRUE)

      handles$ungroup = shiny::observe({
        ungroup(question_name, server$id)
      }) |>
        shiny::bindEvent(server$ungroup_signal(), ignoreInit = TRUE)

      handles$changed = shiny::observe({
        group = server$group()
        session$sendCustomMessage("markermd_update_group", list(
          id = session$ns(paste0(server$id, "-container")),
          container = session$ns("rubric_items_container"),
          group = server$id,
          label = group@label,
          chips = as.character(rubric_group_chips(group))
        ))
      }) |>
        shiny::bindEvent(server$changed(), ignoreInit = TRUE)

      group_handles[[server$id]] <<- handles
    }

    # Create and register the module server for one item group (module id ==
    # group id), wire its signals and reserve the id for the session
    add_question_group_server = function(question_name, group_id, group) {
      server = mark_rubric_group_server(
        group_id,
        group,
        collection_path = root,
        question_name = question_name,
        group_id = group_id,
        app_action = app_action,
        label_available = function(label) {
          others = groups_of(question_name)
          others = others[setdiff(names(others), group_id)]
          !label %in% vapply(others, function(g) g@label, character(1))
        },
        exclusive_conflicts = function() {
          selection_conflicts(question_name, group_members(question_name, group_id))
        }
      )
      question_group_servers[[question_name]][[group_id]] = server
      wire_group_signals(question_name, server)
      register_session_id(group_id)
      server
    }

    # Bring a question's group servers in line with the database after an
    # import: update the groups that survived, add the new ones, drop the rest
    sync_question_groups = function(question_name) {
      stored = load_item_groups(root, question_name)
      servers = question_group_servers[[question_name]]
      for (group_id in names(stored)) {
        if (is.null(servers[[group_id]])) {
          add_question_group_server(question_name, group_id, stored[[group_id]])
        } else {
          servers[[group_id]]$update_group(stored[[group_id]])
        }
      }
      for (group_id in setdiff(names(servers), names(stored))) {
        remove_group_server(question_name, group_id)
      }
    }

    # A question's item and group servers are built the first time it is
    # needed (shown in the Grade tab, the Rubric tab opening, an item or
    # group added to it), not at startup: a rubric of a hundred items would
    # otherwise bind hundreds of observers before the first page shows. The
    # ids stored in the database are reserved at startup all the same, so a
    # new item never takes the id of one in a question not loaded yet.
    loaded_questions = character(0)
    if (!is.null(database_state)) {
      for (question_name in question_names) {
        register_session_id(names(database_state$rubric_items[[question_name]]))
        register_session_id(names(database_state$rubric_groups[[question_name]]))
      }
    }

    # Build a question's servers from the database if they are not yet
    # built. Returns TRUE when it built them (the caller redraws).
    #
    # question_name: The question
    ensure_question_loaded = function(question_name) {
      if (is.null(database_state) || question_name %in% loaded_questions) {
        return(FALSE)
      }
      loaded_questions <<- c(loaded_questions, question_name)
      sync_question_groups(question_name)
      items = load_rubric_items(root, question_name)
      for (item_id in names(items)) {
        add_question_item_server(question_name, item_id, items[[item_id]])
      }
      TRUE
    }

    # The question shown in the Grade tab is loaded first (a higher priority
    # than the observers and outputs that read its servers)
    shiny::observe({
      shiny::req(input$question_select)
      if (ensure_question_loaded(input$question_select)) {
        redraw()
        apply_saved_selections()
      }
    }, priority = 10) |>
      shiny::bindEvent(input$question_select)

    # The Rubric tab shows every question
    shiny::observe({
      if (!identical(items_owner(), "rubric")) {
        return()
      }
      loaded = vapply(question_names, ensure_question_loaded, logical(1))
      if (any(loaded)) {
        redraw()
      }
    }, priority = 10) |>
      shiny::bindEvent(items_owner())

    shiny::observe({
      shiny::updateSelectInput(
        session, "question_select", 
        choices = question_names, selected = question_names[1]
      )
    })

    # The current question's score from its live selections, groups and
    # scoring setup (see score_question())
    question_score = shiny::reactive({
      shiny::req(input$question_select)
      question_name = input$question_select
      items = lapply(question_item_servers[[question_name]], function(server) server$item())
      selected_ids = names(items)[vapply(items, function(item) item@selected, logical(1))]
      score_question(
        items, selected_ids, groups_of(question_name),
        question_grade_servers[[question_name]]$grade()
      )
    })

    # Render grade UI for current question
    output$grade_ui = shiny::renderUI({
      shiny::req(input$question_select)
      if (!identical(items_owner(), "grade")) {
        return(NULL)
      }

      current_grade_server = question_grade_servers[[input$question_select]]
      if (!is.null(current_grade_server)) {
        result = question_score()
        mark_grade_ui(
          session$ns(current_grade_server$id),
          current_grade_server$grade(),
          capped = result$score - result$without_groups,
          breakdown = shiny::uiOutput(session$ns("score_breakdown"))
        )
      }
    }) |>
      # Only re-render when the question changes or an import updates the
      # scoring setup (grade_redraw). Within a question the score is updated
      # imperatively via update_grade() -> update_score_display(), so
      # rubric-item add/move/delete (which bump redraw_ui) must not rebuild
      # this widget.
      shiny::bindEvent(input$question_select, grade_redraw(), items_owner())
    shiny::outputOptions(output, "grade_ui", suspendWhenHidden = FALSE)

    # The cap badge's breakdown; it lives in a popover, so it is only
    # rendered while the popover is open
    output$score_breakdown = shiny::renderUI({
      shiny::req(input$question_select)
      score_breakdown_ui(question_score(), question_grade_servers[[input$question_select]]$grade())
    })

    # The item list as blocks (see rubric_blocks()): ungrouped items and
    # groups wrapping their members, then the groups with no members. Item
    # rows keep their own markup, so the hotkeys (matched by button label)
    # are untouched by grouping. The wrapper routes the rows' events (see
    # rubric_item_container_attrs()).
    output$rubric_items_ui = shiny::renderUI({
      shiny::req(input$question_select)
      if (!identical(items_owner(), "grade")) {
        return(NULL)
      }
      question_name = input$question_select

      servers = question_item_servers[[question_name]]
      groups = groups_of(question_name)
      blocks = rubric_blocks(names(servers), item_groups_of(servers), names(groups))
      result = question_score()

      item_ui = function(item_id) {
        item = servers[[item_id]]$item()
        mark_rubric_item_ui(
          session$ns(item_id), item,
          exclusive = !is.na(item@group) && isTRUE(groups[[item@group]]@exclusive)
        )
      }
      block_ui = function(block) {
        if (block$type == "item") {
          return(item_ui(block$id))
        }
        mark_rubric_group_ui(
          session$ns(block$group), groups[[block$group]], lapply(block$ids, item_ui),
          subtotal = result$groups[result$groups$group_id == block$group, , drop = FALSE]
        )
      }
      is_empty_group = vapply(blocks, function(block) {
        block$type == "group" && length(block$ids) == 0
      }, logical(1))

      do.call(shiny::div, c(
        list(
          id = session$ns("rubric_item_rows"),
          shiny::div(id = session$ns("rubric_blocks"), lapply(blocks[!is_empty_group], block_ui)),
          shiny::div(id = session$ns("rubric_empty_groups"), lapply(blocks[is_empty_group], block_ui))
        ),
        rubric_item_container_attrs(session$ns, groups)
      ))
    }) |>
      shiny::bindEvent(redraw_ui(), input$question_select, items_owner())
    shiny::outputOptions(output, "rubric_items_ui", suspendWhenHidden = FALSE)

    # The Rubric tab's editor: every question with its scoring settings and
    # its items and groups, editable as in the Grade tab (the same item and
    # group modules), each item showing how many repositories selected it in
    # place of its toggle
    output$rubric_editor_ui = shiny::renderUI({
      if (!identical(items_owner(), "rubric")) {
        return(NULL)
      }
      grading_data_version()
      external_version()
      counts = item_selection_counts(root)
      add_button_style = "width: 26px; height: 26px; display: inline-flex; align-items: center; justify-content: center; padding: 0;"

      sections = lapply(seq_along(question_names), function(k) {
        question_name = question_names[k]
        servers = question_item_servers[[question_name]]
        groups = groups_of(question_name)
        blocks = rubric_blocks(names(servers), item_groups_of(servers), names(groups))
        item_ui = function(item_id) {
          n = counts[paste(question_name, item_id, sep = "\x1f")]
          mark_rubric_item_ui(
            session$ns(item_id), servers[[item_id]]$item(),
            selected_count = if (is.na(n)) 0L else unname(n),
            repo_count = length(repos)
          )
        }
        block_ui = function(block) {
          if (block$type == "item") {
            return(item_ui(block$id))
          }
          mark_rubric_group_ui(session$ns(block$group), groups[[block$group]], lapply(block$ids, item_ui))
        }
        grade_server = question_grade_servers[[question_name]]

        bslib::card(
          id = session$ns(paste0("editor_question_", k)),
          class = "mb-3",
          bslib::card_header(
            class = "bg-light d-flex flex-wrap align-items-center gap-3",
            shiny::span(class = "fw-semibold", question_name),
            if (!is.null(grade_server)) {
              shiny::div(class = "ms-auto", mark_grade_settings_ui(session$ns(grade_server$id), grade_server$grade()))
            }
          ),
          bslib::card_body(
            class = "small",
            fillable = FALSE,
            if (length(blocks) == 0) shiny::div(class = "text-muted", "No items yet."),
            do.call(shiny::div, c(list(lapply(blocks, block_ui)), rubric_item_container_attrs(session$ns, groups))),
            shiny::div(
              class = "d-flex align-items-center gap-3 mt-2",
              shiny::span(
                shiny::actionButton(
                  session$ns(paste0("editor_add_item_", k)), fa_icon("plus"),
                  class = "btn-primary btn-sm rounded-circle", style = add_button_style, title = "Add Item"
                ),
                shiny::span("Add Item", class = "ms-1 text-dark")
              ),
              shiny::span(
                shiny::actionButton(
                  session$ns(paste0("editor_add_group_", k)), fa_icon("layer-group"),
                  class = "btn-outline-primary btn-sm rounded-circle", style = add_button_style, title = "Add Group"
                ),
                shiny::span("Add Group", class = "ms-1 text-dark")
              )
            )
          )
        )
      })
      shiny::div(class = "rubric-editor", sections)
    }) |>
      shiny::bindEvent(redraw_ui(), grade_redraw(), items_owner(), grading_data_version(), external_version())
    shiny::outputOptions(output, "rubric_editor_ui", suspendWhenHidden = FALSE)

    # The editor's jump select scrolls to the chosen question's card, then
    # clears, so choosing the same question again scrolls again
    shiny::updateSelectizeInput(session, "editor_jump", choices = question_names, selected = "")
    shiny::observe({
      shiny::req(nzchar(input$editor_jump))
      k = match(input$editor_jump, question_names)
      session$sendCustomMessage("markermd_scroll_to", list(id = session$ns(paste0("editor_question_", k))))
      shiny::updateSelectizeInput(session, "editor_jump", selected = "")
    }) |>
      shiny::bindEvent(input$editor_jump)

    # Add an empty item at the end of a question. In the Grade tab the new
    # item is inserted into the list in place, so in-progress edits on the
    # others survive; in the Rubric tab the editor redraws.
    #
    # question_name: The question to add the item to
    add_item_to = function(question_name) {
      ensure_question_loaded(question_name)
      current_servers = question_item_servers[[question_name]]

      # The unique server_id must not collide with ANY id this session has
      # bound, across questions and including ids since deleted or replaced
      # by an import: module ids share one namespace and dead module
      # instances keep their input observers registered (two servers bound
      # to the same inputs, each saving to its own question)
      server_id = paste0("item_", id_idx)
      while (server_id %in% session_item_ids) {
        id_idx <<- id_idx + 1
        server_id = paste0("item_", id_idx)
      }

      # The new item goes last (ungrouped); past the tenth position it has
      # no hotkey and is toggled by clicking (there is no cap on items). It
      # starts with a placeholder description unique in the question
      # (descriptions identify items, so a blank one is never stored); the
      # row selects it on focus so typing replaces it.
      descriptions = vapply(current_servers, function(server) server$item()@description, character(1))
      description = "New item"
      k = 2
      while (description %in% descriptions) {
        description = paste("New item", k)
        k = k + 1
      }
      new_item = markermd_rubric_item(position_hotkey(length(current_servers) + 1), 0, description)

      server = add_question_item_server(question_name, server_id, new_item)

      # Save new item to database (appended after the question's last item)
      app_action(
        "add rubric item",
        call = glue::glue("mark(): add item to {question_name}"),
        parts = "rubric",
        save_rubric_item(root, question_name, server_id, new_item)
      )

      id_idx <<- id_idx + 1

      if (!identical(shiny::isolate(items_owner()), "grade") || !identical(question_name, shiny::isolate(input$question_select))) {
        redraw()
        return(invisible(server))
      }

      # Insert only the new item rather than re-rendering the whole list, so
      # in-progress edits (e.g. a description being typed) on existing items are
      # preserved. It goes after the last item block, ahead of any empty
      # groups. The list still re-renders from question_item_servers on a
      # move/reorder or question switch, so the source of truth stays consistent.
      shiny::insertUI(
        selector = paste0("#", session$ns("rubric_blocks")),
        where = "beforeEnd",
        ui = mark_rubric_item_ui(session$ns(server$id), new_item, fresh = TRUE)
      )
      invisible(server)
    }

    # Add an empty group with a fresh label to a question, rendered after its
    # last item with its settings popover opened
    #
    # question_name: The question to add the group to
    add_group_to = function(question_name) {
      ensure_question_loaded(question_name)
      labels = vapply(groups_of(question_name), function(g) g@label, character(1))
      label = "New group"
      k = 2
      while (label %in% labels) {
        label = paste("New group", k)
        k = k + 1
      }
      group_id = next_item_ids(session_item_ids, 1, prefix = "group_")
      group = markermd_rubric_group(label = label)

      app_action(
        "add item group",
        call = glue::glue("mark(): add group to {question_name}"),
        parts = "rubric",
        save_item_group(root, question_name, group_id, group)
      )
      add_question_group_server(question_name, group_id, group)
      redraw()

      session$onFlushed(function() {
        session$sendCustomMessage("markermd_show_popover", list(
          id = session$ns(paste0(group_id, "-settings_popover"))
        ))
      }, once = TRUE)
    }

    # The Grade tab's add buttons act on the current question, the editor's
    # on their own question
    shiny::observe({
      add_item_to(input$question_select)
    }) |>
      shiny::bindEvent(input$add_item, ignoreInit = TRUE)

    shiny::observe({
      add_group_to(input$question_select)
    }) |>
      shiny::bindEvent(input$add_group, ignoreInit = TRUE)

    lapply(seq_along(question_names), function(k) {
      shiny::observe({
        add_item_to(question_names[k])
      }) |>
        shiny::bindEvent(input[[paste0("editor_add_item_", k)]], ignoreInit = TRUE)
      shiny::observe({
        add_group_to(question_names[k])
      }) |>
        shiny::bindEvent(input[[paste0("editor_add_group_", k)]], ignoreInit = TRUE)
    })

    # --- Rubric YAML import/export -------------------------------------------
    # Exports read current database state via collect_rubric_data(): every
    # item/scoring edit is persisted immediately by the item and grade
    # servers, so the database is never behind the UI.

    output$export_question = shiny::downloadHandler(
      filename = function() {
        paste0("rubric_", gsub("[^A-Za-z0-9_-]+", "_", input$question_select), ".yaml")
      },
      content = function(file) {
        write_rubric_yaml(collect_rubric_data(root, input$question_select), file)
      },
      contentType = "text/yaml"
    )

    output$export_all = shiny::downloadHandler(
      filename = function() "markermd_rubric.yaml",
      content = function(file) {
        write_rubric_yaml(collect_rubric_data(root, question_names), file)
      },
      contentType = "text/yaml"
    )

    # A parsed rubric import awaiting confirmation while the modal is open
    pending_rubric_import = shiny::reactiveVal(NULL)

    output$editor_export_all = shiny::downloadHandler(
      filename = function() "markermd_rubric.yaml",
      content = function(file) {
        write_rubric_yaml(collect_rubric_data(root, question_names), file)
      },
      contentType = "text/yaml"
    )

    # Import: a menu button (in either tab) triggers its hidden file picker;
    # parse and validate the upload, then confirm mode (append/replace)
    # before applying
    handle_rubric_upload = function(file) {
      parsed = purrr::safely(read_rubric_yaml)(file$datapath)

      if (!is.null(parsed$error)) {
        shiny::showNotification(
          paste0("Could not import rubric: ", conditionMessage(parsed$error)),
          type = "error"
        )
        return()
      }

      rubric = parsed$result
      yaml_names = purrr::map_chr(rubric$questions, "name")

      if (length(yaml_names) == 0) {
        shiny::showNotification("The rubric file contains no questions.", type = "warning")
        return()
      }

      unknown = setdiff(yaml_names, question_names)
      if (length(unknown) > 0) {
        shiny::showNotification(
          glue::glue(
            "Could not import rubric: question(s) {paste0('\"', unknown, '\"', collapse = ', ')} ",
            "are not in this project's template ({paste0('\"', question_names, '\"', collapse = ', ')})."
          ),
          type = "error"
        )
        return()
      }

      summary_items = lapply(rubric$questions, function(q) {
        n_existing = length(load_rubric_items(root, q$name))
        n_groups = length(q$groups)
        groups_note = if (n_groups == 0) "" else glue::glue(", {n_groups} group{if (n_groups == 1) '' else 's'}")
        scoring_note = if (is.null(q$scoring)) "" else "; updates scoring"
        shiny::tags$li(glue::glue(
          "{q$name}: {length(q$items)} item{if (length(q$items) == 1) '' else 's'}{groups_note} ",
          "({n_existing} existing){scoring_note}"
        ))
      })

      pending_rubric_import(list(rubric = rubric, name = file$name))
      shiny::showModal(shiny::modalDialog(
        title = "Import rubric items?",
        shiny::p(glue::glue("Importing \"{file$name}\" affects:")),
        shiny::tags$ul(summary_items),
        shiny::radioButtons(
          session$ns("rubric_import_mode"),
          "Import mode:",
          choices = c(
            "Append to existing items" = "append",
            "Replace existing items" = "replace"
          ),
          selected = "append"
        ),
        shiny::textInput(
          session$ns("rubric_import_summary"),
          "Summary (recorded on the snapshot taken before importing)",
          placeholder = "What this import changes and why",
          width = "100%"
        ),
        shiny::p(
          class = "text-danger small mb-0",
          "Replace deletes the existing rubric items and groups for these questions, and any",
          "recorded selections of the items, for every repository. A snapshot of the current",
          "rubric and marks is taken first, so the import can be undone from the History tab."
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_rubric_import"), "Import", class = "btn-primary")
        )
      ))
    }

    shiny::observe({
      handle_rubric_upload(input$rubric_import_file)
    }) |>
      shiny::bindEvent(input$rubric_import_file)

    shiny::observe({
      handle_rubric_upload(input$editor_import_file)
    }) |>
      shiny::bindEvent(input$editor_import_file)

    # Replace deletes and re-records; the confirm button says so
    shiny::observe({
      shinyjs::toggleClass(
        session$ns("confirm_rubric_import"), "btn-danger", asis = TRUE,
        condition = identical(input$rubric_import_mode, "replace")
      )
      shinyjs::toggleClass(
        session$ns("confirm_rubric_import"), "btn-primary", asis = TRUE,
        condition = !identical(input$rubric_import_mode, "replace")
      )
    }) |>
      shiny::bindEvent(input$rubric_import_mode)

    # Apply a confirmed import, then bring the live module state back in sync
    # with the database
    shiny::observe({
      shiny::removeModal()
      pending = pending_rubric_import()
      pending_rubric_import(NULL)
      shiny::req(pending)
      mode = input$rubric_import_mode

      summary = if (nzchar(trimws(input$rubric_import_summary))) trimws(input$rubric_import_summary)
      applied = purrr::safely(function() {
        app_action(
          "rubric_import",
          call = glue::glue("mark(): rubric import (mode = '{mode}')"),
          summary = summary, parts = "rubric",
          apply_rubric_import(
            root, pending$rubric, mode,
            reserved_ids = session_item_ids,
            snapshot = list(trigger = glue::glue("mark(): Rubric import ({mode})"), summary = summary)
          )
        )
      })()
      if (!is.null(applied$error)) {
        shiny::showNotification(
          paste0("Could not import rubric: ", conditionMessage(applied$error)),
          type = "error", duration = NULL
        )
        return()
      }
      summaries = applied$result

      scoring_updated = FALSE
      for (question_name in names(summaries)) {
        s = summaries[[question_name]]
        # A replace rebuilds every item's server below; an append keeps the
        # existing servers, so a question not loaded yet is loaded first
        # (from the database, which already holds the imported items)
        if (identical(s$mode, "replace")) {
          loaded_questions <<- union(loaded_questions, question_name)
        } else {
          ensure_question_loaded(question_name)
        }

        # Groups first, so the items' group ids resolve to live servers
        if (identical(s$mode, "replace")) {
          # Old servers become inert (their DOM goes away on redraw); rebuild
          # the question's server lists from the database, whose ids are
          # session-fresh by construction
          question_item_servers[[question_name]] = list()
          sync_question_groups(question_name)
          items = load_rubric_items(root, question_name)
          for (item_id in names(items)) {
            add_question_item_server(question_name, item_id, items[[item_id]])
          }
        } else {
          # Keep existing servers (preserving live selection state and
          # in-flight edits); append servers for just the imported items,
          # then take the written order, hotkeys and group ids
          sync_question_groups(question_name)
          for (item_id in names(s$items)) {
            written = s$items[[item_id]]
            server = question_item_servers[[question_name]][[item_id]]
            if (is.null(server)) {
              add_question_item_server(question_name, item_id, written)
            } else {
              item = server$item()
              item@hotkey = written@hotkey
              item@group = written@group
              server$update_item(item)
            }
          }
          question_item_servers[[question_name]] = question_item_servers[[question_name]][names(s$items)]
        }

        if (!is.null(s$scoring)) {
          scoring_updated = TRUE
          grade_server = question_grade_servers[[question_name]]
          if (!is.null(grade_server)) {
            grade_server$update_grade(s$scoring)
          }
        }
      }

      redraw()
      if (scoring_updated) {
        grade_redraw(grade_redraw() + 1)
      }
      # Progress checkmarks re-read the database (replace cascades recorded
      # selections away)
      bump_grading_version()

      n_new = sum(purrr::map_int(summaries, ~ length(.x$new_ids)))
      shiny::showNotification(
        glue::glue(
          "Imported {n_new} item{if (n_new == 1) '' else 's'} into ",
          "{length(summaries)} question{if (length(summaries) == 1) '' else 's'} from {pending$name}."
        ),
        type = "message"
      )
    }) |>
      shiny::bindEvent(input$confirm_rubric_import)

    # Graded (question, repo) pairs re-read from the database after each write
    graded_pairs_r = shiny::reactive({
      grading_data_version()
      graded_question_pairs(root, repos)
    })

    # Which questions count as graded for the current repo
    graded_questions = shiny::reactive({
      shiny::req(selected_repo())
      pairs = graded_pairs_r()
      repo_graded = pairs$question_name[pairs$assignment_repo == selected_repo()]
      stats::setNames(question_names %in% repo_graded, question_names)
    })

    # Prefix graded questions with a check in the selector; only push an
    # update when the labels actually change so open menus are not rebuilt
    last_question_labels = shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::req(input$question_select)
      status = graded_questions()
      labels = ifelse(status[question_names], paste0("\u2713 ", question_names), question_names)
      if (identical(labels, last_question_labels())) return()
      last_question_labels(labels)
      shiny::updateSelectInput(
        session, "question_select",
        choices = stats::setNames(question_names, labels),
        selected = shiny::isolate(input$question_select)
      )
    }) |>
      shiny::bindEvent(graded_questions())

    # Prefix fully graded repos with a check in the content pane's repo
    # selector, through the content module's label setter
    last_repo_labels = shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::req(selected_repo())
      counts = calculate_grading_progress(root, question_names, repos, graded_pairs = graded_pairs_r())
      done = length(question_names) > 0 & counts[repos] >= length(question_names)
      labels = ifelse(done, paste0("\u2713 ", repos), repos)
      if (identical(labels, last_repo_labels())) return()
      last_repo_labels(labels)
      set_repo_labels(
        choices = stats::setNames(repos, labels),
        selected = shiny::isolate(selected_repo())
      )
    }) |>
      shiny::bindEvent(graded_pairs_r())

    # Question prev/next buttons wrap through the question list (see
    # navigate_button())
    navigate_button(input, session, "question_prev_btn", "question_select", question_names, -1)
    navigate_button(input, session, "question_next_btn", "question_select", question_names, 1)


    # --- Per-question comment persistence -----------------------------------
    # Wires a comment textarea to debounced, navigation-flushed persistence
    # for the (question, repo) pair on screen. The textarea reflects one pair
    # at a time: pair records which, and stored the last value persisted (or
    # loaded) for it. Every edit is captured into pending_edit together with
    # the pair it was typed against, so a save (debounced, or flushed on
    # navigation) can never attribute text to a different question/repo even
    # when navigation outruns the textarea's update echo. Programmatic
    # updateTextAreaInput() echoes are consumed via pending_echoes so they are
    # never mistaken for edits.
    #
    # input_id: textarea input id within this module
    # load_fn: function(root, question, repo) returning the stored text or NULL
    # save_fn: function(root, question, repo, text) appending one event row
    # on_saved: optional zero-arg callback run after each persisted save

    bind_comment_autosave = function(input_id, load_fn, save_fn, on_saved = NULL) {
      pair = shiny::reactiveVal(NULL)
      stored = shiny::reactiveVal("")
      pending_edit = shiny::reactiveVal(NULL)
      pending_echoes = shiny::reactiveVal(character(0))

      save_pending = function() {
        edit = pending_edit()
        if (is.null(edit)) {
          return()
        }
        save_fn(root, edit$question, edit$repo, edit$text)
        current = pair()
        if (!is.null(current) && identical(current$question, edit$question) && identical(current$repo, edit$repo)) {
          stored(edit$text)
        }
        pending_edit(NULL)
        if (!is.null(on_saved)) {
          on_saved()
        }
      }

      # Capture typing as a pending edit bound to the pair on screen; consume
      # programmatic echoes, and drop the pending edit when the text is typed
      # back to the stored value
      shiny::observe({
        val = input[[input_id]]
        echoes = pending_echoes()
        hit = match(val, echoes)
        if (!is.na(hit)) {
          pending_echoes(echoes[-hit])
          return()
        }
        current = pair()
        if (is.null(current)) {
          return()
        }
        if (identical(val, stored())) {
          pending_edit(NULL)
          return()
        }
        pending_edit(list(question = current$question, repo = current$repo, text = val))
      }) |>
        shiny::bindEvent(input[[input_id]], ignoreInit = TRUE)

      # On question/repo change: flush the pending edit (to its own pair),
      # then load the new pair's stored comment
      shiny::observe({
        shiny::req(input$question_select, selected_repo())

        save_pending()

        pair(list(question = input$question_select, repo = selected_repo()))
        loaded = load_fn(root, input$question_select, selected_repo())
        if (is.null(loaded)) loaded = ""
        stored(loaded)
        if (!identical(loaded, input[[input_id]])) {
          # The update only echoes back when it changes the client value; cap
          # the outstanding-echo list so an unmatched entry cannot linger forever
          pending_echoes(utils::tail(c(pending_echoes(), loaded), 8))
          shiny::updateTextAreaInput(session, input_id, value = loaded)
        }
      }) |>
        shiny::bindEvent(input$question_select, selected_repo())

      # Debounced autosave while typing; comments are an event log, so one row
      # per pause rather than per keystroke
      text_debounced = shiny::debounce(shiny::reactive(input[[input_id]]), 1000)

      shiny::observe({
        save_pending()
      }) |>
        shiny::bindEvent(text_debounced(), ignoreInit = TRUE)

      # Re-read the pair on screen after a change made outside this session.
      # An unsaved edit is kept (it is saved on the next pause, over the
      # outside change); returns TRUE when it was
      function() {
        current = shiny::isolate(pair())
        if (is.null(current)) {
          return(FALSE)
        }
        if (!is.null(shiny::isolate(pending_edit()))) {
          return(TRUE)
        }
        loaded = load_fn(root, current$question, current$repo)
        if (is.null(loaded)) loaded = ""
        if (!identical(loaded, shiny::isolate(stored()))) {
          stored(loaded)
          pending_echoes(utils::tail(c(shiny::isolate(pending_echoes()), loaded), 8))
          shiny::updateTextAreaInput(session, input_id, value = loaded)
        }
        FALSE
      }
    }

    # Only the public channel feeds grading progress: a non-empty public
    # comment counts the pair as graded, while private notes never do
    refresh_comment = bind_comment_autosave("question_comment", load_comment, save_comment, on_saved = bump_grading_version)
    refresh_private_comment = bind_comment_autosave("question_private_comment", load_private_comment, save_private_comment)

    # Items of the pair on screen that a markermd call selected and nobody
    # has touched here since (see pair_agent_badges()); re-read on every
    # grading write here and on every change noticed outside
    agent_badges = shiny::reactive({
      shiny::req(input$question_select, selected_repo())
      grading_data_version()
      external_version()
      with_database(root, function(conn) pair_agent_badges(conn, input$question_select, selected_repo()))
    })

    # The Grade tab's item rows are drawn once and patched in place after
    # each flush (so a redraw in the same flush cannot clobber a patch): the
    # toggle buttons whenever an item's selection or hotkey, or a group's
    # exclusivity, changes, and the agent badges whenever they change
    shiny::observe({
      shiny::req(input$question_select)
      redraw_ui()
      if (!identical(items_owner(), "grade")) {
        return()
      }
      question_name = input$question_select
      payload = lapply(unname(question_item_servers[[question_name]]), function(server) {
        item = server$item()
        c(
          list(id = session$ns(paste0(server$id, "-hotkey_btn"))),
          rubric_item_button_state(item, group_is_exclusive(question_name, item@group))
        )
      })
      session$onFlushed(function() {
        session$sendCustomMessage("markermd_item_buttons", payload)
      }, once = TRUE)
    })

    shiny::observe({
      redraw_ui()
      if (!identical(items_owner(), "grade")) {
        return()
      }
      badges = agent_badges()
      titles = structure(list(), names = character(0))
      for (item_id in names(badges)) {
        value = badges[[item_id]]
        who = if (is.na(value$fn)) value$actor else paste0(value$actor, " (", value$fn, ")")
        titles[[session$ns(paste0(item_id, "-agent_badge"))]] = paste0(
          "Selected by ", who, " and not yet reviewed; any change to this question clears it"
        )
      }
      session$onFlushed(function() {
        session$sendCustomMessage("markermd_agent_badges", list(
          container = session$ns("rubric_items_container"),
          icon = as.character(fa_icon("robot")),
          badges = titles
        ))
      }, once = TRUE)
    })

    # Put the current question's item servers in the selection state the
    # database holds for the current repository. Nothing is written: grade
    # rows are written by toggle_item() alone.
    apply_saved_selections = function() {
      current_question = shiny::isolate(input$question_select)
      current_repo = shiny::isolate(selected_repo())
      if (is.null(current_question) || is.null(current_repo) ||
            !current_question %in% names(question_item_servers)) {
        return(invisible(NULL))
      }

      current_servers = question_item_servers[[current_question]]

      if (length(current_servers) == 0) {
        return(invisible(NULL))
      }

      # Load saved selections from database
      saved_selections = load_grade_selections(root, current_question, current_repo)

      # Apply saved selections to current servers (or reset to defaults)
      for (item_id in names(current_servers)) {
        server = current_servers[[item_id]]
        current_item = server$item()

        # Get saved selection state, default to FALSE if not found
        saved_selected = if (length(saved_selections) > 0 && item_id %in% names(saved_selections)) {
          saved_selections[[item_id]]
        } else {
          FALSE  # Default state when no saved selection exists
        }

        # Update if different from current state
        if (current_item@selected != saved_selected) {
          current_item@selected = saved_selected
          server$update_item(current_item)
        }
      }
      invisible(NULL)
    }

    # Observer to load saved grade selections when switching repos or questions
    shiny::observe({
      shiny::req(input$question_select,
                 selected_repo(),
                 cancelOutput = TRUE)
      apply_saved_selections()
    }) |>
      shiny::bindEvent(input$question_select, selected_repo(), ignoreInit = TRUE)

    # Rebuild every question's items, groups and scoring from the database
    # after a snapshot restore (which replaces items with fresh ids, so the
    # old servers become inert and their DOM goes away on redraw), then put
    # the current question back in the state the database holds
    reload_from_db = function() {
      state = initialize_database_state(root, template)
      loaded_questions <<- question_names
      for (question_name in question_names) {
        question_item_servers[[question_name]] = list()
        sync_question_groups(question_name)
        items = state$rubric_items[[question_name]]
        for (item_id in names(items)) {
          add_question_item_server(question_name, item_id, items[[item_id]])
        }
        grade_server = question_grade_servers[[question_name]]
        if (!is.null(grade_server)) {
          grade_server$update_grade(state$grade_states[[question_name]])
        }
      }
      apply_saved_selections()
      redraw()
      grade_redraw(shiny::isolate(grade_redraw()) + 1)
      bump_grading_version()
      invisible(NULL)
    }

    # Patch the group headers' subtotals (and capped styling) in place after
    # the flush, the same way the score is patched, so no item row is
    # re-rendered and a question switch's stale render cannot clobber it
    send_group_subtotals = function(result) {
      rows = result$groups
      payload = lapply(seq_len(nrow(rows)), function(i) {
        list(
          id = session$ns(paste0(rows$group_id[i], "-container")),
          capped = !isTRUE(all.equal(rows$sum[i], rows$applied[i])),
          html = as.character(rubric_group_subtotal(rows$sum[i], rows$applied[i]))
        )
      })
      session$onFlushed(function() {
        session$sendCustomMessage("markermd_update_group_subtotals", payload)
      }, once = TRUE)
    }

    # Observer to update the grade, the cap badge and the group subtotals
    # when rubric selections (or the groups, or the scoring setup) change
    shiny::observe({
      shiny::req(input$question_select)

      current_grade_server = question_grade_servers[[input$question_select]]
      if (!is.null(current_grade_server)) {
        current_grade_state = current_grade_server$grade()
        result = question_score()
        capped = result$score - result$without_groups

        if (result$score != current_grade_state@current_score) {
          # The live score is per repository; it is shown, never persisted
          current_grade_server$set_score(result$score, capped = capped)
        } else {
          current_grade_server$refresh_display(capped)
        }
        send_group_subtotals(result)
      }
    })

    # Current question's item ids, descriptions and grouping, for shinytest2
    # assertions (namespaced under this module's id in get_values())
    shiny::exportTestValues(
      rubric_item_ids = if (is.null(input$question_select)) character(0) else {
        names(question_item_servers[[input$question_select]])
      },
      rubric_item_descriptions = if (is.null(input$question_select)) character(0) else {
        unname(purrr::map_chr(question_item_servers[[input$question_select]], ~ .x$item()@description))
      },
      rubric_item_groups = if (is.null(input$question_select)) character(0) else {
        item_groups_of(question_item_servers[[input$question_select]])
      },
      rubric_group_labels = if (is.null(input$question_select)) character(0) else {
        vapply(groups_of(input$question_select), function(g) g@label, character(1))
      }
    )

    # Return reactive values for external use
    return(list(
      selected_question = shiny::reactive(input$question_select),
      # Programmatic question selection (the Assignments table's score
      # click-through)
      select_question = function(question_name) {
        shiny::updateSelectInput(session, "question_select", selected = question_name)
      },
      selected_content_repo = selected_repo,
      grading_data_version = shiny::reactive(grading_data_version()),
      question_grade_servers = question_grade_servers,
      # For the History tab: rebuild from the database after a restore
      reload_from_db = reload_from_db,
      # After marks changed outside this session: put the pair on screen
      # back in the state the database holds. Returns TRUE when an unsaved
      # comment edit was kept over the outside change.
      refresh_pair = function() {
        apply_saved_selections()
        kept = c(refresh_comment(), refresh_private_comment())
        any(kept)
      },
      current_grade = shiny::reactive({
        shiny::req(input$question_select)
        current_server = question_grade_servers[[input$question_select]]
        if (!is.null(current_server)) {
          current_server$grade()
        } else {
          NULL
        }
      })
    ))
  })
}
