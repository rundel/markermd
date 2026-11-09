# Mark Rubric Interface Module
#
# mark()'s rubric module: the Grade tab's rubric card (the current
# question's score, items, groups and comments) and the Rubric tab (every
# question's rubric, edited in place). This file holds the UI, the keyboard
# hotkeys and mark_rubric_server(), which assembles the module from its
# parts: the item and group state (mod_mark_rubric_state.R), the item and
# group actions (mod_mark_rubric_actions.R), comments, imports, exports and
# the graded labels (mod_mark_rubric_persist.R), and the two tabs' views
# (mod_mark_rubric_views.R). The item rows' and groups' markup and client
# scripts live in mod_mark_rubric_item.R and mod_mark_rubric_group.R.

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

# Mark Rubric Server. Builds the module's shared context rx (an environment
# the wiring functions fill and read) and wires its parts onto the session.
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
# on_commit: Zero-argument function called right after the module writes
#   selections or comments (the app's watcher absorb(), so its own writes do
#   not look like changes made outside the app)

mark_rubric_server = function(id, template, repos, root, database_state = NULL, selected_repo, set_repo_labels,
                              external_version = shiny::reactive(0L), active_tab = shiny::reactive("grade"),
                              app_action = function(fn, code, ...) code, on_commit = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    question_names = template_question_names(template)

    rx = new.env(parent = emptyenv())
    rx$session = session
    rx$input = input
    rx$output = output
    rx$template = template
    rx$repos = repos
    rx$root = root
    rx$database_state = database_state
    rx$question_names = question_names
    rx$selected_repo = selected_repo
    rx$set_repo_labels = set_repo_labels
    rx$external_version = external_version
    rx$active_tab = active_tab
    rx$app_action = app_action
    rx$on_commit = on_commit

    rubric_state_wiring(rx)
    rubric_actions_wiring(rx)
    rubric_persistence_wiring(rx)
    rubric_views_wiring(rx)

    shiny::observe({
      shiny::updateSelectInput(session, "question_select", choices = question_names, selected = question_names[1])
    })

    # Question prev/next buttons wrap through the question list (see
    # navigate_button())
    navigate_button(input, session, "question_prev_btn", "question_select", question_names, -1)
    navigate_button(input, session, "question_next_btn", "question_select", question_names, 1)

    # Current question's item ids, descriptions and grouping, for shinytest2
    # assertions (namespaced under this module's id in get_values())
    shiny::exportTestValues(
      rubric_item_ids = if (is.null(input$question_select)) character(0) else {
        names(rx$items[[input$question_select]])
      },
      rubric_item_descriptions = if (is.null(input$question_select)) character(0) else {
        unname(purrr::map_chr(rx$items[[input$question_select]], ~ .x$item()@description))
      },
      rubric_item_groups = if (is.null(input$question_select)) character(0) else {
        item_groups_of(rx$items[[input$question_select]])
      },
      rubric_group_labels = if (is.null(input$question_select)) character(0) else {
        vapply(rx$groups_of(input$question_select), function(g) g@label, character(1))
      }
    )

    list(
      selected_question = shiny::reactive(input$question_select),
      # Programmatic question selection (the Assignments table's score
      # click-through)
      select_question = function(question_name) {
        shiny::updateSelectInput(session, "question_select", selected = question_name)
      },
      selected_content_repo = selected_repo,
      grading_data_version = shiny::reactive(rx$grading_data_version()),
      question_grade_servers = rx$grade_servers,
      # For the History tab: rebuild from the database after a restore
      reload_from_db = rx$reload_from_db,
      # After marks changed outside this session: put the pair on screen back
      # in the state the database holds. Returns TRUE when an unsaved comment
      # edit was kept over the outside change.
      refresh_pair = function() {
        rx$apply_saved_selections()
        rx$refresh_comments()
      },
      current_grade = shiny::reactive({
        shiny::req(input$question_select)
        grade_server = rx$grade_servers[[input$question_select]]
        if (!is.null(grade_server)) grade_server$grade() else NULL
      })
    )
  })
}

