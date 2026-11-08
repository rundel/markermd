# Mark Rubric Item Module
#
# Shiny module for individual rubric items with hotkey selection and point values

# Colors used to indicate positive vs negative point values
POINTS_POSITIVE_COLOR = "#28a745"
POINTS_NEGATIVE_COLOR = "#dc3545"

# Builds the toggle button for a rubric item, applying the selected-state
# styling (green when selected, outline otherwise). The first ten items show
# their hotkey (1-9, then 0); items past the tenth have none, so their button
# is blank with a dashed outline and a tooltip, and is toggled by clicking.
# The number-key handler (rubric_hotkeys_js) matches buttons by this label,
# so a blank button is never reached from the keyboard. In an exclusive
# group the button is round and announced as a radio button, since selecting
# it deselects the group's other items.
#
# id: Character. Module namespace ID
# rubric_item: markermd_rubric_item S7 object containing item properties
# exclusive: Logical. Whether the item's group allows one selected item

mark_rubric_item_hotkey_btn = function(id, rubric_item, exclusive = FALSE) {
  ns = shiny::NS(id)
  has_hotkey = !is.na(rubric_item@hotkey)

  # Sized explicitly in every dimension, min and max included. The uiOutput
  # wrapper this renders into is display: contents, so the button itself is
  # the flex item in the item row and a long description used to squeeze it
  # (flex items shrink below a plain width, down to the label's width);
  # min-width pins it. Centred with line-height rather than flex, since
  # browsers disagree about flex layout inside a <button>.
  shiny::actionButton(
    ns("hotkey_btn"),
    label = if (has_hotkey) as.character(rubric_item@hotkey %% 10) else "",
    class = paste(
      if (rubric_item@selected) "btn-success" else "btn-outline-secondary",
      "btn-sm",
      if (exclusive) "rubric-hk-radio"
    ),
    style = paste0(
      "display: inline-block; box-sizing: border-box; ",
      "width: 28px; min-width: 28px; max-width: 28px; ",
      "height: 28px; min-height: 28px; max-height: 28px; ",
      "padding: 0; margin: 0; line-height: 26px; text-align: center; ",
      "font-size: 10px; vertical-align: top;",
      if (!has_hotkey) " border-style: dashed;" else ""
    ),
    title = if (has_hotkey) NULL else "No hotkey: click to toggle",
    role = if (exclusive) "radio" else NULL,
    `aria-checked` = if (exclusive) tolower(rubric_item@selected) else NULL
  )
}

# One entry of an item's group menu: choosing it reports the group id (or ""
# for no group) to the item module's group_choice input as an event. Entries
# are links rather than buttons so the number-key handler, which scans the
# pane's buttons by label, never matches a group label.
#
# ns: The item module's namespace function
# group_id: Group id, or NA for the "No group" entry
# label: Text shown for the entry
# active: Whether the item is currently in this group

rubric_group_menu_item = function(ns, group_id, label, active) {
  value = if (is.na(group_id)) "" else group_id
  shiny::tags$li(
    shiny::tags$a(
      class = "dropdown-item rubric-group-menu-item d-flex align-items-center gap-2",
      href = "#",
      `data-group` = value,
      onclick = glue::glue(
        "Shiny.setInputValue('<<ns('group_choice')>>', '<<value>>', {priority: 'event'}); return false;",
        .open = "<<", .close = ">>"
      ),
      shiny::icon("check", class = paste("fa-fw", if (!active) "invisible")),
      shiny::span(class = "rubric-group-menu-label", label)
    )
  )
}

# Creates UI for a single rubric item with hotkey button and description
#
# id: Character. Module namespace ID
# rubric_item: markermd_rubric_item S7 object containing item properties
# groups: Named list of the question's markermd_rubric_group objects keyed by
#   group id, in display order, for the item's group menu

mark_rubric_item_ui = function(id, rubric_item, groups = list()) {
  ns = shiny::NS(id)

  action_style = "border: none; background: transparent; color: #6c757d; font-size: 12px; line-height: 1;"

  shiny::div(
    id = ns("container"),
    class = "rubric-item-row px-2 py-1 m-0 rounded position-relative",
    # Action buttons row: always present at reduced emphasis, full opacity on
    # hover or keyboard focus (styled via .rubric-action-btns in mark-app.R).
    # Every item has the full set, grouped or not: move within its run,
    # move to another group (or out of its group), delete.
    shiny::div(
      class = "rubric-action-btns position-absolute",
      style = "top: 0; right: 8px;",
      shiny::div(
        style = "display: flex; gap: 2px;",
        shiny::actionButton(
          ns("move_up_btn"),
          label = NULL,
          icon = shiny::icon("chevron-up"),
          class = "btn btn-sm p-1",
          style = action_style,
          title = "Move item up"
        ),
        shiny::actionButton(
          ns("move_down_btn"),
          label = NULL,
          icon = shiny::icon("chevron-down"),
          class = "btn btn-sm p-1",
          style = action_style,
          title = "Move item down"
        ),
        # Group menu: a Bootstrap dropdown listing "No group" and the
        # question's groups. Groups are created with Add Group; this menu
        # only moves the item between existing ones.
        shiny::div(
          class = "dropdown d-inline-block",
          shiny::tags$button(
            type = "button",
            class = "btn btn-sm p-1 rubric-group-menu-btn",
            style = action_style,
            `data-bs-toggle` = "dropdown",
            `aria-expanded` = "false",
            title = "Move item to a group",
            shiny::icon("layer-group")
          ),
          shiny::tags$ul(
            class = "dropdown-menu dropdown-menu-end shadow-sm rubric-group-menu",
            shiny::tags$li(shiny::tags$h6(class = "dropdown-header", "Move item to group")),
            rubric_group_menu_item(ns, NA_character_, "No group", is.na(rubric_item@group)),
            lapply(names(groups), function(group_id) {
              rubric_group_menu_item(
                ns, group_id, groups[[group_id]]@label,
                active = identical(rubric_item@group, group_id)
              )
            })
          )
        ),
        shiny::actionButton(
          ns("delete_btn"),
          label = NULL,
          icon = shiny::icon("times"),
          class = "btn btn-sm p-1",
          style = "border: none; background: transparent; color: #6c757d; font-size: 16px;",
          title = "Delete rubric item"
        )
      )
    ),
    # Rubric item content: hotkey button beside a tight points/description stack
    shiny::div(
      class = "d-flex align-items-start gap-2",
      # Hotkey button (re-rendered reactively to reflect selection state).
      # The output wrapper is display: contents, so flex-shrink-0 here does
      # nothing; the button carries its own fixed size (see
      # mark_rubric_item_hotkey_btn())
      shiny::uiOutput(ns("hotkey_btn_ui"), class = "flex-shrink-0"),
      shiny::div(
        class = "flex-grow-1",
        style = "min-width: 0;",
        # Points
        shiny::div(
          id = ns("points_text"),
          contenteditable = "true",
          `data-points` = rubric_item@points,
          style = glue::glue(
            "border: 1px solid transparent; ",
            "background: transparent; ",
            "padding: 2px 4px; ",
            "border-radius: 4px; ",
            "outline: none; ",
            "transition: all 0.2s ease; ",
            "font-size: 14px; ",
            "font-weight: bold; ",
            "display: inline-block; ",
            "width: fit-content; ",
            "color: <<if (rubric_item@points >= 0) POINTS_POSITIVE_COLOR else POINTS_NEGATIVE_COLOR>>;",
            .open = "<<", .close = ">>"
          ),
          cli::pluralize(paste0(
            if (rubric_item@points >= 0) "+" else "",
            "{rubric_item@points} pt{?s}"
          ))
        ),
        # Marks an item a markermd call (an agent's import) selected and
        # nobody has reviewed in the app since
        shiny::uiOutput(ns("agent_badge"), inline = TRUE),
        # Description editable div
        shiny::div(
          id = ns("description_text"),
          contenteditable = "true",
          class = "form-control-like",
          style = paste0(
            "border: 1px solid transparent; ",
            "background: transparent; ",
            "padding: 2px 4px; ",
            "margin: 0; ",
            "font-size: 12px; ",
            "line-height: 1.3; ",
            "min-height: 20px; ",
            "border-radius: 4px; ",
            "white-space: pre-wrap; ",
            "word-wrap: break-word; ",
            "overflow-wrap: break-word; ",
            "outline: none; ",
            "transition: all 0.2s ease;"
          ),
          `data-placeholder` = "Enter description...",
          rubric_item@description
        )
      )
    ),
    shiny::tags$style(shiny::HTML(glue::glue("
        #<<ns('description_text')>>:hover {
          border: 1px solid #80bdff !important;
          background: #f8f9fa !important;
        }
        #<<ns('description_text')>>:focus {
          border: 1px solid #80bdff !important;
          background: white !important;
        }
        #<<ns('description_text')>>:empty:before {
          content: attr(data-placeholder);
          color: #adb5bd;
        }
        #<<ns('description_text')>>:focus:before {
          display: none;
        }
        #<<ns('points_text')>>:hover {
          border: 1px solid #80bdff !important;
          background: #f8f9fa !important;
        }
        #<<ns('points_text')>>:focus {
          border: 1px solid #80bdff !important;
          background: white !important;
          color: #495057 !important;
          width: fit-content !important;
        }
      ", .open = "<<", .close = ">>"))),
      shiny::tags$script(shiny::HTML(glue::glue("
        $(document).ready(function() {
          var editableDiv = document.getElementById('<<ns('description_text')>>');
          var pointsDiv = document.getElementById('<<ns('points_text')>>');
          
          if (editableDiv) {
            // Handle input changes
            editableDiv.addEventListener('input', function() {
              var newValue = this.innerText || this.textContent || '';
              Shiny.setInputValue('<<ns('description_text')>>', newValue);
            });
            
            // Handle paste to clean up formatting
            editableDiv.addEventListener('paste', function(e) {
              e.preventDefault();
              var text = e.clipboardData.getData('text/plain');
              document.execCommand('insertText', false, text);
            });
          }
          
          if (pointsDiv) {
            // Store original formatted text
            var originalText = pointsDiv.innerHTML;
            var originalPoints = parseFloat(pointsDiv.getAttribute('data-points'));
            
            // Handle focus - switch to plain number and select all
            pointsDiv.addEventListener('focus', function() {
              this.innerHTML = originalPoints.toString();
              
              // Select all text after a brief delay to ensure content is set
              setTimeout(() => {
                var range = document.createRange();
                range.selectNodeContents(this);
                var selection = window.getSelection();
                selection.removeAllRanges();
                selection.addRange(range);
              }, 10);
            });
            
            // Handle keydown to prevent multiline and exit on Enter
            pointsDiv.addEventListener('keydown', function(e) {
              if (e.key === 'Enter') {
                e.preventDefault();
                this.blur();
              }
            });
            
            // Handle blur - format and update
            pointsDiv.addEventListener('blur', function() {
              var newValue = parseFloat(this.innerText || this.textContent || '0');
              if (isNaN(newValue)) newValue = originalPoints;
              
              // Update data attribute
              this.setAttribute('data-points', newValue);
              originalPoints = newValue;
              
              // Format display text. Match the initial cli::pluralize() render,
              // which is singular only when the value is exactly 1 (so -1 and
              // 1.5 are both plural).
              var sign = newValue >= 0 ? '+' : '';
              var suffix = newValue === 1 ? 'pt' : 'pts';
              this.innerHTML = sign + newValue + ' ' + suffix;
              
              // Update color
              this.style.color = newValue >= 0 ? '<<POINTS_POSITIVE_COLOR>>' : '<<POINTS_NEGATIVE_COLOR>>';
              
              Shiny.setInputValue('<<ns('points_text')>>', newValue);
            });
            
            // Handle paste to clean up formatting
            pointsDiv.addEventListener('paste', function(e) {
              e.preventDefault();
              var text = e.clipboardData.getData('text/plain');
              document.execCommand('insertText', false, text);
            });
          }
        });
      ", .open = "<<", .close = ">>")))
  )
}

# Server logic for rubric item with state management using S7 class
#
# id: Character. Module namespace ID
# initial_item: markermd_rubric_item S7 object with initial state
# collection_path: Character string. Path to collection directory (optional)
# question_name: Character string. Name of the question (optional)
# item_id: Character string. Unique identifier for this item (optional)
# group_exclusive: function(group_id) returning whether that group of the
#   item's question is exclusive (read reactively by the toggle button
#   render, so a change in the group's setting restyles the button)
# badge: function() returning NULL, or a list with actor and fn when a
#   markermd call selected this item for the pair on screen and nobody has
#   touched the pair in the app since (read reactively)

mark_rubric_item_server = function(id, initial_item, collection_path = NULL, question_name = NULL, item_id = NULL,
                                   group_exclusive = function(group_id) FALSE, badge = function() NULL) {
  ns = shiny::NS(id)

  shiny::moduleServer(id, function(input, output, session) {
  
    # Internal state using S7 class
    rubric_item_state = shiny::reactiveVal(initial_item)

    # Persist the item when its editable fields change; the parent owns the
    # hotkey, group and selection, so those are carried over untouched
    save_edit = function(new_item) {
      rubric_item_state(new_item)
      if (!is.null(collection_path) && !is.null(question_name) && !is.null(item_id)) {
        save_rubric_item(collection_path, question_name, item_id, new_item)
      }
    }

    # Handle description text changes
    shiny::observe({
      current_item = rubric_item_state()

      # Only update if the value actually changed
      if (input$description_text != current_item@description) {
        current_item@description = input$description_text
        save_edit(current_item)
      }
    }) |>
      shiny::bindEvent(input$description_text, ignoreInit = TRUE)
    
    # Handle points text changes
    shiny::observe({
      current_item = rubric_item_state()

      # Only update if the value actually changed
      if (input$points_text != current_item@points) {
        current_item@points = as.numeric(input$points_text)
        save_edit(current_item)
      }
    }) |>
      shiny::bindEvent(input$points_text, ignoreInit = TRUE)
  
    # The agent badge, re-rendered only when it actually changes (the badge
    # source is re-read on every grading write)
    badge_state = shiny::reactiveVal(NULL)
    shiny::observe({
      value = badge()
      if (!identical(value, shiny::isolate(badge_state()))) {
        badge_state(value)
      }
    })

    output$agent_badge = shiny::renderUI({
      value = badge_state()
      if (is.null(value)) {
        return(NULL)
      }
      who = if (is.na(value$fn)) value$actor else paste0(value$actor, " (", value$fn, ")")
      shiny::span(
        class = "agent-badge text-info ms-1",
        title = paste0("Selected by ", who, " and not yet reviewed; any change to this question clears it"),
        shiny::icon("robot")
      )
    })

    # Render the hotkey button so its styling tracks the selection state and
    # the exclusivity of the item's group
    output$hotkey_btn_ui = shiny::renderUI({
      item = rubric_item_state()
      mark_rubric_item_hotkey_btn(
        session$ns(NULL), item,
        exclusive = isTRUE(group_exclusive(item@group))
      )
    })

    # A user toggle (button click or hotkey), as opposed to a programmatic
    # update_item(): bumped after the state change so the parent's exclusive
    # group handling reads the new selection and never fires on loads
    toggle_signal = shiny::reactiveVal(0)

    shiny::observe({
      current_item = rubric_item_state()
      current_item@selected = !current_item@selected
      rubric_item_state(current_item)
      toggle_signal(toggle_signal() + 1)
    }) |>
      shiny::bindEvent(input$hotkey_btn, ignoreInit = TRUE)
    
    # Return reactive rubric item and move/delete signals for external use
    delete_signal = shiny::reactiveVal(0)
    move_up_signal = shiny::reactiveVal(0)
    move_down_signal = shiny::reactiveVal(0)

    # Confirm before deleting: the item is shared across all repos and its
    # recorded selections are removed everywhere, so a stray click mid-grading
    # must not destroy it (same pattern as question deletion in template())
    shiny::observe({
      desc = rubric_item_state()@description
      label = if (nchar(trimws(desc)) > 0) {
        glue::glue("Delete the rubric item \"{desc}\"?")
      } else {
        "Delete this rubric item?"
      }
      shiny::showModal(shiny::modalDialog(
        title = "Delete rubric item?",
        paste(
          label,
          "This removes it, and any recorded selections of it, for every repository.",
          "This cannot be undone."
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_delete"), "Delete", class = "btn-danger")
        )
      ))
    }) |>
      shiny::bindEvent(input$delete_btn)

    shiny::observe({
      shiny::removeModal()
      delete_signal(delete_signal() + 1)
    }) |>
      shiny::bindEvent(input$confirm_delete)

    # Handle move button clicks
    shiny::observe({
      move_up_signal(move_up_signal() + 1)
    }) |>
      shiny::bindEvent(input$move_up_btn)

    shiny::observe({
      move_down_signal(move_down_signal() + 1)
    }) |>
      shiny::bindEvent(input$move_down_btn)
    
    # Return reactive rubric item, move/delete/group signals, and update method for external use
    return(list(
      id = id,
      item = shiny::reactive(rubric_item_state()),
      delete_signal = delete_signal,
      move_up_signal = move_up_signal,
      move_down_signal = move_down_signal,
      toggle_signal = toggle_signal,
      # The group chosen in the item's group menu: a group id, or "" for
      # "No group"; an event input, so re-choosing the same entry re-fires
      group_signal = shiny::reactive(input$group_choice),
      update_item = function(new_item) {
        rubric_item_state(new_item)
      }
    ))
  })
}
