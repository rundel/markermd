#' Mark Rubric Item Module
#'
#' Shiny module for individual rubric items with hotkey selection and point values

#' Mark Rubric Item UI
#'
#' Creates UI for a single rubric item with hotkey button and description
#'
#' @param id Character. Module namespace ID
#' @param rubric_item markermd_rubric_item S7 object containing item properties
#'
mark_rubric_item_ui = function(id, rubric_item) {
  ns = shiny::NS(id)
  
  shiny::div(
    class = "p-2 m-0 rounded position-relative",
    style = "transition: background-color 0.2s ease;",
    onmouseover = "this.style.backgroundColor='#e9ecef'; this.querySelector('.rubric-delete-btn').style.opacity='1';",
    onmouseout = "this.style.backgroundColor='transparent'; this.querySelector('.rubric-delete-btn').style.opacity='0';",
    # Delete button (hidden by default, shown on hover)
    shiny::div(
      class = "rubric-delete-btn position-absolute",
      style = "top: 0; right: 8px; opacity: 0; transition: opacity 0.2s ease; cursor: pointer;",
      shiny::actionButton(
        ns("delete_btn"),
        label = NULL,
        icon = shiny::icon("times"),
        class = "btn btn-sm p-1",
        style = "border: none; background: transparent; color: #6c757d; font-size: 16px;",
        title = "Delete rubric item"
      )
    ),
    # Rubric item content in single layout
    bslib::layout_columns(
      col_widths = c(1, 11, -1, 11),
      class = "mb-0",
      row_heights = c("auto", "auto"),
      # Hotkey button
      shiny::actionButton(
        ns("hotkey_btn"),
        label = if (is.na(rubric_item@hotkey)) "" else as.character(rubric_item@hotkey %% 10),
        class = paste(
          if (rubric_item@selected) "btn-success" else "btn-outline-secondary",
          "btn-sm d-flex align-items-center justify-content-center"
        ),
        style = "font-size: 10px; padding: 0; width: 28px; height: 28px;"
      ),
      # Points
      shiny::div(
        id = ns("points_text"),
        contenteditable = "true",
        `data-points` = rubric_item@points,
        style = glue::glue(
          "border: 1px solid transparent; ",
          "background: transparent; ",
          "padding: 4px; ",
          "border-radius: 4px; ",
          "outline: none; ",
          "transition: all 0.2s ease; ",
          "font-size: 14px; ",
          "font-weight: bold; ",
          "display: inline-block; ",
          "width: fit-content; ",
          "color: <<if (rubric_item@points >= 0) '#28a745' else '#dc3545'>>;",
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
        style = glue::glue(
          "border: 1px solid transparent; ",
          "background: transparent; ",
          "padding: 4px; ",
          "margin: 0; margin-top: 2px; ",
          "font-size: 12px; ",
          "line-height: 1.3; ",
          "color: #6c757d; ",
          "min-height: 20px; ",
          "border-radius: 4px; ",
          "white-space: pre-wrap; ",
          "word-wrap: break-word; ",
          "overflow-wrap: break-word; ",
          "outline: none; ",
          "transition: all 0.2s ease; ",
          "<<if (nchar(rubric_item@description) == 0) 'opacity: 0.6;' else ''>>",
          .open = "<<", .close = ">>"
        ),
        `data-placeholder` = "Enter description...",
        rubric_item@description
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
          opacity: 1;
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
              
              // Format display text
              var sign = newValue >= 0 ? '+' : '';
              var suffix = Math.abs(newValue) === 1 ? 'pt' : 'pts';
              this.innerHTML = sign + newValue + ' ' + suffix;
              
              // Update color
              this.style.color = newValue >= 0 ? '#28a745' : '#dc3545';
              
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
  )
}

#' Mark Rubric Item Server
#'
#' Server logic for rubric item with state management using S7 class
#'
#' @param id Character. Module namespace ID
#' @param initial_item markermd_rubric_item S7 object with initial state
#'
mark_rubric_item_server = function(id, initial_item) {
  ns = shiny::NS(id)

  shiny::moduleServer(id, function(input, output, session) {
  
    # Internal state using S7 class
    rubric_item_state = shiny::reactiveVal(initial_item)
    

    
    # Handle description text changes
    shiny::observeEvent(input$description_text, {
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
      }
    }, ignoreInit = TRUE)
    
    # Handle points text changes
    shiny::observeEvent(input$points_text, {
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
      }
    }, ignoreInit = TRUE)
  
    # Update button appearance when state changes
    shiny::observe({
      current_item = rubric_item_state()
      
      # Update button class based on selection state
      if (current_item@selected) {
        shinyjs::removeClass("hotkey_btn", "btn-outline-secondary")
        shinyjs::addClass("hotkey_btn", "btn-success")
      } else {
        shinyjs::removeClass("hotkey_btn", "btn-success")
        shinyjs::addClass("hotkey_btn", "btn-outline-secondary")
      }
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
    
    # Return reactive rubric item and delete signal for external use
    delete_signal = shiny::reactiveVal(0)
    
    # Handle delete button clicks
    shiny::observeEvent(input$delete_btn, {
      delete_signal(delete_signal() + 1)
    })
    
    # Return reactive rubric item, delete signal, and update method for external use
    return(list(
      id = id,
      item = shiny::reactive(rubric_item_state()),
      delete_signal = delete_signal,
      update_item = function(new_item) {
        rubric_item_state(new_item)
      }
    ))
  })
}