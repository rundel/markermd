# Mark Rubric Item Module
#
# Shiny module for individual rubric items with hotkey selection and point values

# Colors used to indicate positive vs negative point values
POINTS_POSITIVE_COLOR = "#28a745"
POINTS_NEGATIVE_COLOR = "#dc3545"

# Builds the hotkey action button for a rubric item, applying the
# selected-state styling (green when selected, outline otherwise)
#
# id: Character. Module namespace ID
# rubric_item: markermd_rubric_item S7 object containing item properties

mark_rubric_item_hotkey_btn = function(id, rubric_item) {
  ns = shiny::NS(id)

  shiny::actionButton(
    ns("hotkey_btn"),
    label = if (is.na(rubric_item@hotkey)) "" else as.character(rubric_item@hotkey %% 10),
    class = paste(
      if (rubric_item@selected) "btn-success" else "btn-outline-secondary",
      "btn-sm d-flex align-items-center justify-content-center"
    ),
    style = "font-size: 10px; padding: 0; width: 28px; height: 28px;"
  )
}

# Creates UI for a single rubric item with hotkey button and description
#
# id: Character. Module namespace ID
# rubric_item: markermd_rubric_item S7 object containing item properties

mark_rubric_item_ui = function(id, rubric_item) {
  ns = shiny::NS(id)
  
  shiny::div(
    id = ns("container"),
    class = "rubric-item-row px-2 py-1 m-0 rounded position-relative",
    # Action buttons row: always present at reduced emphasis, full opacity on
    # hover or keyboard focus (styled via .rubric-action-btns in mark-app.R)
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
          style = "border: none; background: transparent; color: #6c757d; font-size: 12px; line-height: 1;",
          title = "Move item up"
        ),
        shiny::actionButton(
          ns("move_down_btn"),
          label = NULL,
          icon = shiny::icon("chevron-down"),
          class = "btn btn-sm p-1",
          style = "border: none; background: transparent; color: #6c757d; font-size: 12px; line-height: 1;",
          title = "Move item down"
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
      # Hotkey button (re-rendered reactively to reflect selection state)
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

mark_rubric_item_server = function(id, initial_item, collection_path = NULL, question_name = NULL, item_id = NULL) {
  ns = shiny::NS(id)

  shiny::moduleServer(id, function(input, output, session) {
  
    # Internal state using S7 class
    rubric_item_state = shiny::reactiveVal(initial_item)
    

    
    # Handle description text changes
    shiny::observe({
      current_item = rubric_item_state()

      # Only update if the value actually changed
      if (input$description_text != current_item@description) {
        # Create new item with updated description
        new_item = markermd_rubric_item(
          hotkey = current_item@hotkey,
          points = current_item@points,
          description = input$description_text,
          selected = current_item@selected
        )

        rubric_item_state(new_item)

        # Save to database if parameters are provided
        if (!is.null(collection_path) && !is.null(question_name) && !is.null(item_id)) {
          save_rubric_item(collection_path, question_name, item_id, new_item)
        }
      }
    }) |>
      shiny::bindEvent(input$description_text, ignoreInit = TRUE)
    
    # Handle points text changes
    shiny::observe({
      current_item = rubric_item_state()

      # Only update if the value actually changed
      if (input$points_text != current_item@points) {
        # Create new item with updated points
        new_item = markermd_rubric_item(
          hotkey = current_item@hotkey,
          points = input$points_text,
          description = current_item@description,
          selected = current_item@selected
        )

        rubric_item_state(new_item)

        # Save to database if parameters are provided
        if (!is.null(collection_path) && !is.null(question_name) && !is.null(item_id)) {
          save_rubric_item(collection_path, question_name, item_id, new_item)
        }
      }
    }) |>
      shiny::bindEvent(input$points_text, ignoreInit = TRUE)
  
    # Render the hotkey button so its styling tracks the selection state
    output$hotkey_btn_ui = shiny::renderUI({
      mark_rubric_item_hotkey_btn(session$ns(NULL), rubric_item_state())
    })

    shiny::observe({
      current_item = rubric_item_state()
      
      new_item = markermd_rubric_item(
        hotkey = current_item@hotkey,
        points = current_item@points,
        description = current_item@description,
        selected = !current_item@selected
      )
      
      rubric_item_state(new_item)
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
    
    # Return reactive rubric item, move/delete signals, and update method for external use
    return(list(
      id = id,
      item = shiny::reactive(rubric_item_state()),
      delete_signal = delete_signal,
      move_up_signal = move_up_signal,
      move_down_signal = move_down_signal,
      update_item = function(new_item) {
        rubric_item_state(new_item)
      }
    ))
  })
}