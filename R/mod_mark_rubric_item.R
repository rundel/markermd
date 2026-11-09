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
# for no group) to the item module's group_choice input as an event (through
# the delegated handler in rubric_item_editing_head()). Entries
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
      `data-input` = ns("group_choice"),
      cached_icon("check", class = if (active) "fa-fw" else "fa-fw invisible"),
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
# selected_count: NULL in the Grade tab (the item shows its toggle button);
#   in the Rubric tab's editor, how many repositories have the item
#   selected, shown in place of the toggle

mark_rubric_item_ui = function(id, rubric_item, groups = list(), selected_count = NULL) {
  ns = shiny::NS(id)

  action_style = "border: none; background: transparent; color: #6c757d; font-size: 12px; line-height: 1;"

  shiny::div(
    id = ns("container"),
    class = "rubric-item-row px-2 py-1 m-0 rounded position-relative",
    # Action buttons row: always present at reduced emphasis, full opacity on
    # hover or keyboard focus (styled via .rubric-action-btns in mark-app.R).
    # Every item has the full set, grouped or not: move within its run,
    # move to another group (or out of its group), delete. In the Rubric
    # tab's editor the item's selection count sits just left of them.
    shiny::div(
      class = "position-absolute d-flex align-items-center gap-2",
      style = "top: 0; right: 8px;",
      if (!is.null(selected_count)) {
        shiny::span(
          class = "badge rounded-pill text-bg-secondary rubric-selected-count",
          title = paste0("Selected for ", selected_count, " repositor", if (selected_count == 1) "y" else "ies"),
          paste0(selected_count, "\u00d7")
        )
      },
      shiny::div(
        class = "rubric-action-btns d-flex",
        style = "gap: 2px;",
        shiny::actionButton(
          ns("move_up_btn"),
          label = NULL,
          icon = cached_icon("chevron-up"),
          class = "btn btn-sm p-1",
          style = action_style,
          title = "Move item up"
        ),
        shiny::actionButton(
          ns("move_down_btn"),
          label = NULL,
          icon = cached_icon("chevron-down"),
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
            cached_icon("layer-group")
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
          icon = cached_icon("times"),
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
      if (is.null(selected_count)) {
        shiny::uiOutput(ns("hotkey_btn_ui"), class = "flex-shrink-0")
      },
      shiny::div(
        class = "flex-grow-1",
        style = "min-width: 0;",
        # Points
        shiny::div(
          id = ns("points_text"),
          class = "rubric-points-edit",
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
          class = "form-control-like rubric-desc-edit",
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
    )
  )
}

# The editing behavior of every rubric item's points and description
# (contenteditable divs), delegated from the document once in the page head
# rather than a style and script per item, which made a long rubric slow to
# draw. Points show the bare number while focused, commit on blur or Enter
# and report to the item's points_text input; descriptions report each edit
# to description_text. Pastes are reduced to plain text in both. A group
# menu entry reports its group to the item's group_choice input.

rubric_item_editing_head = function() {
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .rubric-points-edit:hover, .rubric-desc-edit:hover {
        border: 1px solid #80bdff !important; background: #f8f9fa !important;
      }
      .rubric-points-edit:focus, .rubric-desc-edit:focus {
        border: 1px solid #80bdff !important; background: white !important;
      }
      .rubric-points-edit:focus { color: #495057 !important; }
      .rubric-desc-edit:empty:before { content: attr(data-placeholder); color: #adb5bd; }
      .rubric-desc-edit:focus:before { display: none; }
    ")),
    shiny::tags$script(shiny::HTML(glue::glue("
      (function() {
        function points(el) {
          var v = parseFloat(el.getAttribute('data-points'));
          return isNaN(v) ? 0 : v;
        }
        document.addEventListener('click', function(e) {
          var el = e.target.closest && e.target.closest('a.rubric-group-menu-item[data-input]');
          if (!el) return;
          e.preventDefault();
          Shiny.setInputValue(el.dataset.input, el.dataset.group, {priority: 'event'});
        });
        document.addEventListener('input', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-desc-edit');
          if (el) Shiny.setInputValue(el.id, el.innerText || el.textContent || '');
        });
        document.addEventListener('paste', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-desc-edit, .rubric-points-edit');
          if (!el) return;
          e.preventDefault();
          document.execCommand('insertText', false, e.clipboardData.getData('text/plain'));
        });
        document.addEventListener('focusin', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-points-edit');
          if (!el) return;
          el.innerHTML = points(el).toString();
          setTimeout(function() {
            var range = document.createRange();
            range.selectNodeContents(el);
            var selection = window.getSelection();
            selection.removeAllRanges();
            selection.addRange(range);
          }, 10);
        });
        document.addEventListener('keydown', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-points-edit');
          if (el && e.key === 'Enter') {
            e.preventDefault();
            el.blur();
          }
        });
        document.addEventListener('focusout', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-points-edit');
          if (!el) return;
          var value = parseFloat(el.innerText || el.textContent || '0');
          if (isNaN(value)) value = points(el);
          el.setAttribute('data-points', value);
          // Singular only for exactly 1, matching the cli::pluralize() render
          el.innerHTML = (value >= 0 ? '+' : '') + value + ' ' + (value === 1 ? 'pt' : 'pts');
          el.style.color = value >= 0 ? '<<POINTS_POSITIVE_COLOR>>' : '<<POINTS_NEGATIVE_COLOR>>';
          Shiny.setInputValue(el.id, value);
        });
      })();
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
