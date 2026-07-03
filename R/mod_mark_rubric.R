# Mark Rubric Interface Module
#
# Shiny module for displaying and navigating rubric questions during marking

# Renumber a question's item servers to sequential hotkeys (1-10, then NA) by
# their current display order, pushing each new hotkey into the item server's
# state and persisting it so load_rubric_items()'s hotkey ordering survives an
# app restart. Shared by the move-up, move-down, and delete handlers.
#
# root: project root holding the grading database
# question_name: the question whose items are being renumbered
# servers: named list of item servers (names are item ids) in display order

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

# Next free hotkey for a new rubric item: one past the largest existing hotkey,
# or NA once all ten slots are taken. existing_hotkeys may contain NA (items
# past the first ten carry NA), so NAs are dropped before taking the max.
#
# existing_hotkeys: integer vector of the question's current item hotkeys

next_item_hotkey = function(existing_hotkeys) {
  hotkey = max(0L, existing_hotkeys, na.rm = TRUE) + 1L
  if (hotkey > 10) NA_integer_ else as.integer(hotkey)
}

renumber_and_persist_hotkeys = function(root, question_name, servers) {
  ids = names(servers)
  items = list()
  for (i in seq_along(servers)) {
    current_item = servers[[i]]$item()
    updated_item = markermd_rubric_item(
      hotkey = if (i <= 10) as.integer(i) else NA_integer_,
      points = current_item@points,
      description = current_item@description,
      selected = current_item@selected
    )
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
          )
        ),
        shiny::div(
          style = "min-width: 150px; display: flex; align-items: center; gap: 5px;",
          bslib::tooltip(
            shiny::actionButton(
              ns("question_prev_btn"),
              shiny::icon("chevron-left"),
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
              shiny::icon("chevron-right"),
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
        # Dynamic container for items
        shiny::uiOutput(ns("rubric_items_ui"))
      ),
      shiny::div(
        class = "text-center",
        shiny::actionButton(
          ns("add_item"),
          shiny::icon("plus"),
          class = "btn-primary btn-sm rounded-circle",
          style = "width: 30px; height: 30px; display: inline-flex; align-items: center; justify-content: center; padding: 0;",
          title = "Add Item"
        ),
        shiny::span("Add Item", class = "ms-2 text-dark")
      ),
      # Hidden file input the Import menu button triggers directly. Bound
      # once here so the upload binding survives item-list re-renders.
      shiny::div(
        class = "visually-hidden",
        shiny::fileInput(ns("rubric_import_file"), NULL, accept = c(".yaml", ".yml", "text/yaml"))
      )
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

# Keyboard hotkey script for the Rubric tab: numeric keys toggle rubric
# items, z/x navigate repositories, ,/. navigate questions, h toggles the
# HTML/source view. The ids it drives span both the rubric and content
# modules, so it takes both namespace functions and is included by mark-app's
# Rubric tab rather than either module's UI.
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

mark_rubric_server = function(id, template, repos, root, database_state = NULL, selected_repo, set_repo_labels) {
  shiny::moduleServer(id, function(input, output, session) {

    # Global id index
    id_idx = 0

    # Every item id this session has ever bound a module server to. Module ids
    # share one namespace and dead module instances keep their input observers
    # registered, so an id must never be re-bound within a session even when it
    # is gone from both the database and question_item_servers (e.g. deleted
    # earlier, or replaced by a rubric import).
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

    question_item_servers = shiny::reactiveValues()
    question_grade_servers = shiny::reactiveValues()
    for(name in question_names) {
      question_item_servers[[name]] = list()
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
          paste0("grade_", question_name),
          initial_grade_state,
          ui_ns = session$ns,
          collection_path = root,
          question_name = question_name
        )
      })
    }


    redraw_ui = shiny::reactiveVal(0)

    # Bumped when a rubric import changes a question's scoring setup, so the
    # grade widget (whose popover inputs -- total points, mode, bounds -- are
    # baked into the rendered HTML) rebuilds with the imported values
    grade_redraw = shiny::reactiveVal(0)

    # Wire the parent-side handlers for one rubric item server: move up/down
    # reordering and deletion. Called for items added in this session and for
    # items recreated from the database, so loaded items respond to their
    # buttons too.
    wire_item_signals = function(server) {
      handles = list()

      # Move this item one position in the given direction (-1 up, 1 down),
      # wrapping at the ends, then renumber hotkeys to the new display order
      move_item = function(direction) {
        server_list = question_item_servers[[input$question_select]]
        server_names = names(server_list)
        current_index = which(server_names == server$id)

        if (length(current_index) == 0 || length(server_names) <= 1) {
          return()
        }

        n = length(server_list)
        if (direction == -1 && current_index == 1) {
          # Moving up from top - move item to end of list
          new_order = c(2:n, 1)
        } else if (direction == 1 && current_index == n) {
          # Moving down from bottom - move item to beginning of list
          new_order = c(n, 1:(n - 1))
        } else {
          new_index = current_index + direction
          new_order = seq_len(n)
          new_order[c(current_index, new_index)] = new_order[c(new_index, current_index)]
        }

        # Reorder the server list
        new_server_list = server_list[new_order]
        names(new_server_list) = server_names[new_order]
        question_item_servers[[input$question_select]] = new_server_list

        # Update all hotkeys to maintain sequence
        renumber_and_persist_hotkeys(root, input$question_select, new_server_list)

        redraw_ui(redraw_ui() + 1)
      }

      handles$move_up = shiny::observe({
        move_item(-1)
      }) |>
        shiny::bindEvent(server$move_up_signal(), ignoreInit = TRUE)

      handles$move_down = shiny::observe({
        move_item(1)
      }) |>
        shiny::bindEvent(server$move_down_signal(), ignoreInit = TRUE)

      # Handle delete signal
      handles$delete = shiny::observe({
        question_item_servers[[input$question_select]][[server$id]] = NULL

        # Remove the item's row and its grade-selection events; otherwise it
        # reappears on the next app start via load_rubric_items()
        delete_rubric_item(root, input$question_select, server$id)
        bump_grading_version()

        # Reorder hotkeys for remaining items to ensure continuity. Each item's
        # hotkey button self-renders from its server state, so remaining labels
        # renumber without a parent re-render, and persistence keeps the database
        # in sync with what is shown.
        remaining_servers = question_item_servers[[input$question_select]]
        if (length(remaining_servers) > 0) {
          renumber_and_persist_hotkeys(root, input$question_select, remaining_servers)
        }

        # Remove only the deleted item; existing items keep their DOM (and any
        # in-progress edit) instead of being rebuilt by a full re-render.
        shiny::removeUI(selector = paste0("#", session$ns(server$id), "-container"))

        # The item is gone, so destroy its handlers; repeated add/delete cycles
        # would otherwise accumulate dead observers for the session's lifetime.
        handles$move_up$destroy()
        handles$move_down$destroy()
        handles$delete$destroy()
      }) |>
        shiny::bindEvent(server$delete_signal(), ignoreInit = TRUE)
    }

    # Create and register the module server for one rubric item: bind the
    # module (module id == item id), store it under its question, wire the
    # move/delete signals and reserve the id for the session. Shared by the
    # startup loader, the add-item handler and the rubric import refresh.
    #
    # question_name: Question the item belongs to
    # item_id: Database item id, used as the module server id
    # item: markermd_rubric_item S7 object

    add_question_item_server = function(question_name, item_id, item) {
      server = mark_rubric_item_server(
        item_id,
        item,
        collection_path = root,
        question_name = question_name,
        item_id = item_id
      )
      question_item_servers[[question_name]][[item_id]] = server
      wire_item_signals(server)
      register_session_id(item_id)
      server
    }

    # Initialize rubric items from database state inside an observer
    shiny::observe({
      if (!is.null(database_state) && !is.null(database_state$rubric_items)) {
        for (question_name in question_names) {
          saved_items = database_state$rubric_items[[question_name]]
          for (item_id in names(saved_items)) {
            add_question_item_server(question_name, item_id, saved_items[[item_id]])
          }
        }
        # Trigger UI redraw to show loaded items
        redraw_ui(redraw_ui() + 1)
      }
    }) |>
      shiny::bindEvent(TRUE, once = TRUE)  # Run once when module starts

    shiny::observe({
      shiny::updateSelectInput(
        session, "question_select", 
        choices = question_names, selected = question_names[1]
      )
    })

    # Render grade UI for current question
    output$grade_ui = shiny::renderUI({
      shiny::req(input$question_select)

      current_grade_server = question_grade_servers[[input$question_select]]
      if (!is.null(current_grade_server)) {
        current_grade_state = current_grade_server$grade()
        mark_grade_ui(session$ns(current_grade_server$id), current_grade_state)
      }
    }) |>
      # Only re-render when the question changes or an import updates the
      # scoring setup (grade_redraw). Within a question the score is updated
      # imperatively via update_grade() -> update_score_display(), so
      # rubric-item add/move/delete (which bump redraw_ui) must not rebuild
      # this widget.
      shiny::bindEvent(input$question_select, grade_redraw())

    output$rubric_items_ui = shiny::renderUI({
      shiny::req(input$question_select)

      uis = lapply(question_item_servers[[input$question_select]], function(server) {
        mark_rubric_item_ui(session$ns(server$id), server$item())
      })

      return(shiny::tagList(uis))
    }) |>
      shiny::bindEvent(redraw_ui(), input$question_select)

    # Handle add item button
    shiny::observe({
      current_servers = question_item_servers[[input$question_select]]

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

      hotkey = next_item_hotkey(purrr::map_int(current_servers, ~ .x$item()@hotkey))

      new_item = markermd_rubric_item(hotkey, 0, "")

      server = add_question_item_server(input$question_select, server_id, new_item)

      # Save new item to database
      save_rubric_item(root, input$question_select, server_id, new_item)

      id_idx <<- id_idx + 1

      # Insert only the new item rather than re-rendering the whole list, so
      # in-progress edits (e.g. a description being typed) on existing items are
      # preserved. The list still re-renders from question_item_servers on a
      # move/reorder or question switch, so the source of truth stays consistent.
      shiny::insertUI(
        selector = paste0("#", session$ns("rubric_items_ui")),
        where = "beforeEnd",
        ui = mark_rubric_item_ui(session$ns(server$id), new_item)
      )
    }) |>
      shiny::bindEvent(input$add_item, ignoreInit = TRUE)

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

    # Import: the menu button triggers the hidden file picker; parse and
    # validate the upload, then confirm mode (append/replace) before applying
    shiny::observe({
      file = input$rubric_import_file
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
        n_existing = length(question_item_servers[[q$name]])
        scoring_note = if (is.null(q$scoring)) "" else "; updates scoring"
        shiny::tags$li(glue::glue(
          "{q$name}: {length(q$items)} item{if (length(q$items) == 1) '' else 's'} ",
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
        shiny::p(
          class = "text-danger small mb-0",
          "Replace deletes the existing rubric items for these questions, and any",
          "recorded selections of them, for every repository. This cannot be undone."
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_rubric_import"), "Import", class = "btn-danger")
        )
      ))
    }) |>
      shiny::bindEvent(input$rubric_import_file)

    # Apply a confirmed import, then bring the live module state back in sync
    # with the database
    shiny::observe({
      shiny::removeModal()
      pending = pending_rubric_import()
      pending_rubric_import(NULL)
      shiny::req(pending)
      mode = input$rubric_import_mode

      summaries = apply_rubric_import(
        root, pending$rubric, mode,
        reserved_ids = session_item_ids
      )

      scoring_updated = FALSE
      for (question_name in names(summaries)) {
        s = summaries[[question_name]]

        if (identical(s$mode, "replace")) {
          # Old servers become inert (their DOM goes away on redraw); rebuild
          # the question's server list from the database, whose ids are
          # session-fresh by construction
          question_item_servers[[question_name]] = list()
          items = load_rubric_items(root, question_name)
          for (item_id in names(items)) {
            add_question_item_server(question_name, item_id, items[[item_id]])
          }
        } else {
          # Keep existing servers (preserving live selection state and
          # in-flight edits); append servers for just the imported items
          for (j in seq_along(s$new_ids)) {
            add_question_item_server(question_name, s$new_ids[j], s$new_items[[j]])
          }
          for (item_id in names(s$hotkey_changes)) {
            server = question_item_servers[[question_name]][[item_id]]
            if (!is.null(server)) {
              item = server$item()
              item@hotkey = s$hotkey_changes[[item_id]]
              server$update_item(item)
            }
          }
        }

        if (!is.null(s$scoring)) {
          scoring_updated = TRUE
          grade_server = question_grade_servers[[question_name]]
          if (!is.null(grade_server)) {
            grade_server$update_grade(s$scoring)
          }
        }
      }

      # Re-baseline selection tracking so the save observer records the fresh
      # item set without writing spurious grade rows
      previous_selections(list())
      redraw_ui(redraw_ui() + 1)
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
      graded_question_pairs(root)
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
    }

    # Only the public channel feeds grading progress: a non-empty public
    # comment counts the pair as graded, while private notes never do
    bind_comment_autosave("question_comment", load_comment, save_comment, on_saved = bump_grading_version)
    bind_comment_autosave("question_private_comment", load_private_comment, save_private_comment)

    # Reactive calculation for selected rubric items' points for current question
    selected_rubric_points = shiny::reactive({
      shiny::req(input$question_select)

      current_servers = question_item_servers[[input$question_select]]
      if (length(current_servers) == 0) {
        return(0)
      }

      # Sum points from all selected items
      selected_points = purrr::map_dbl(current_servers, function(server) {
        item = server$item()
        if (item@selected) {
          item@points
        } else {
          0
        }
      })

      sum(selected_points)
    })

    # Store previous selection states to detect changes
    previous_selections = shiny::reactiveVal(list())

    # Observer to log grade selection changes to database
    shiny::observe({
      shiny::req(input$question_select,
                 selected_repo(),
                 cancelOutput = TRUE)

      current_question = input$question_select
      current_repo = selected_repo()

      # Ensure question_item_servers is available and has the current question
      shiny::req(length(question_item_servers) > 0, 
                 current_question %in% names(question_item_servers),
                 cancelOutput = TRUE)

      current_servers = question_item_servers[[current_question]]

      if (length(current_servers) == 0) {
        return()
      }

      # Get current selection states
      current_selections = purrr::map_lgl(current_servers, function(server) {
        server$item()@selected
      })
      names(current_selections) = names(current_servers)

      # Get previous selections for this question/repo combination
      prev_key = paste(current_question, current_repo, sep = ":")
      all_prev = previous_selections()
      prev_selections = if (!is.null(all_prev) && prev_key %in% names(all_prev)) {
        all_prev[[prev_key]]
      } else {
        NULL
      }

      if (!is.null(prev_selections) && length(prev_selections) > 0) {
        # Compare with previous state and log changes
        any_saved = FALSE
        for (item_id in names(current_selections)) {
          current_selected = current_selections[[item_id]]
          prev_selected = if (item_id %in% names(prev_selections)) prev_selections[[item_id]] else NULL

          # Log to database if selection state changed
          if (is.null(prev_selected) || current_selected != prev_selected) {
            save_grade_selection(root, current_question, current_repo, item_id, current_selected)
            any_saved = TRUE
          }
        }
        if (any_saved) {
          bump_grading_version()
        }
      }

      # Update stored selections
      if (is.null(all_prev)) all_prev = list()
      all_prev[[prev_key]] = current_selections
      previous_selections(all_prev)
    })

    # Observer to load saved grade selections when switching repos or questions
    shiny::observe({
      shiny::req(input$question_select,
                 selected_repo(),
                 cancelOutput = TRUE)

      current_question = input$question_select
      current_repo = selected_repo()

      # Ensure question_item_servers is available and has the current question
      shiny::req(length(question_item_servers) > 0, 
                 current_question %in% names(question_item_servers),
                 cancelOutput = TRUE)

      current_servers = question_item_servers[[current_question]]

      if (length(current_servers) == 0) {
        return()
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
          updated_item = markermd_rubric_item(
            hotkey = current_item@hotkey,
            points = current_item@points,
            description = current_item@description,
            selected = saved_selected
          )
          server$update_item(updated_item)
        }
      }

      # Initialize current selections in tracking
      current_selections = purrr::map_lgl(current_servers, function(server) {
        server$item()@selected
      })
      names(current_selections) = names(current_servers)

      prev_key = paste(current_question, current_repo, sep = ":")
      all_prev = previous_selections()
      if (is.null(all_prev)) all_prev = list()
      all_prev[[prev_key]] = current_selections
      previous_selections(all_prev)

    }) |>
      shiny::bindEvent(input$question_select, selected_repo(), ignoreInit = TRUE)

    # Observer to update grade when rubric selections change
    shiny::observe({
      shiny::req(input$question_select)

      current_grade_server = question_grade_servers[[input$question_select]]
      if (!is.null(current_grade_server)) {
        current_grade_state = current_grade_server$grade()
        selected_points_sum = selected_rubric_points()

        # Calculate new current score based on grading mode
        new_current_score = if (current_grade_state@grading_mode == "positive") {
          0 + selected_points_sum
        } else {
          current_grade_state@total_score + selected_points_sum
        }

        # Apply bounds if enabled
        if (current_grade_state@bound_above_zero && new_current_score < 0) {
          new_current_score = 0
        }
        if (current_grade_state@bound_below_max && new_current_score > current_grade_state@total_score) {
          new_current_score = current_grade_state@total_score
        }

        # Only update if the score actually changed
        if (new_current_score != current_grade_state@current_score) {
          updated_grade_state = markermd_grade_state(
            current_score = new_current_score,
            total_score = current_grade_state@total_score,
            grading_mode = current_grade_state@grading_mode,
            bound_above_zero = current_grade_state@bound_above_zero,
            bound_below_max = current_grade_state@bound_below_max
          )

          current_grade_server$update_grade(updated_grade_state)
        }
      }
    })

    # Current question's item ids and descriptions, for shinytest2 assertions
    # (namespaced under this module's id in get_values())
    shiny::exportTestValues(
      rubric_item_ids = if (is.null(input$question_select)) character(0) else {
        names(question_item_servers[[input$question_select]])
      },
      rubric_item_descriptions = if (is.null(input$question_select)) character(0) else {
        unname(purrr::map_chr(question_item_servers[[input$question_select]], ~ .x$item()@description))
      }
    )

    # Return reactive values for external use
    return(list(
      selected_question = shiny::reactive(input$question_select),
      selected_content_repo = selected_repo,
      grading_data_version = shiny::reactive(grading_data_version()),
      question_grade_servers = question_grade_servers,
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
