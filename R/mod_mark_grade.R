#' Mark Grade Module - Clean Version
#'
#' Shiny module for displaying and managing question grades with scoring options

#' Mark Grade UI
#'
#' Creates UI for displaying current score and grading configuration options
#'
#' @param id Character. Module namespace ID
#' @param grade_state markermd_grade_state S7 object containing grade properties
#' @export
mark_grade_ui = function(id, grade_state) {
  # Sanitize ID to avoid spaces and special characters that could break JavaScript
  safe_id = gsub("[^A-Za-z0-9_-]", "_", id)
  ns = shiny::NS(safe_id)
  
  shiny::div(
    class = "border-bottom",
    #style = "background-color: #f8f9fa; padding: 12px; border-radius: 6px;",
    bslib::layout_columns(
      col_widths = c(8, 4),
      # Score display (left side)
      shiny::div(
        shiny::div(
          style = "font-size: 14px;",
          "Points:"
        ),
        shiny::div(
          id = ns("score_display"),
          `data-current` = grade_state@current_score,
          `data-total` = grade_state@total_score,
          style = "font-size: 20px; font-weight: bold; border: 1px solid transparent; background: transparent; padding: 8px 12px; border-radius: 4px; outline: none; transition: all 0.2s ease; cursor: pointer; display: inline-block;",
          onmouseover = "this.style.backgroundColor='#e9ecef'; this.style.border='1px solid #80bdff';",
          onmouseout = "this.style.backgroundColor='transparent'; this.style.border='1px solid transparent';",
          title = "Click to edit total score",
          paste0(
            grade_state@current_score, " / ", grade_state@total_score, " ",
            if (grade_state@total_score == 1) "pt" else "pts"
          )
        )
      ),
      # Configuration popover (right side) 
      shiny::div(
        class = "d-flex justify-content-end align-items-center",
        bslib::popover(
          shiny::icon("cog"),
          title = "Settings",
          placement = "bottom",
          shiny::div(
            style = "min-width: 200px;",
            # Fix popover styling
            shiny::tags$style(shiny::HTML("
              .popover .form-check-input[type='radio'] {
                opacity: 1 !important;
                position: static !important;
              }
              .popover-header {
                margin-top: 0 !important;
              }
            ")),
            # Grading mode radio buttons
            shiny::radioButtons(
              ns("grading_mode"),
              label = shiny::strong("Grading method:"),
              choices = list(
                "Positive (start from 0)" = "positive",
                "Negative (start from max)" = "negative"
              ),
              selected = grade_state@grading_mode,
              inline = FALSE
            ),
            # Bounds checkboxes

            shiny::checkboxGroupInput(
              ns("grade_bounds"),
              shiny::strong("Grade bounds:"),
              choices = list(
                "Enforce score ≥ 0" = "above_zero",
                "Enforce score ≤ max" = "below_max"
              ),
              selected = c(
                if (grade_state@bound_above_zero) "above_zero",
                if (grade_state@bound_below_max) "below_max"
              )
            )
        
          )
        )
      )
    ),
    # Simple, clean JavaScript for score editing
    shiny::tags$script(shiny::HTML(paste0("
      $(document).ready(function() {
        // Click handler for score editing
        $(document).on('click', '#", ns("score_display"), "', function() {
          var $el = $(this);
          var current = parseFloat($el.data('current'));
          var total = parseFloat($el.data('total'));
          
          // Create editable version
          $el.html(current + ' / <input type=\"number\" min=\"0\" step=\"0.5\" value=\"' + total + '\" style=\"width: 60px; border: 1px solid #80bdff; padding: 2px; border-radius: 2px;\"> pts');
          $el.css('background-color', '#f8f9fa');
          
          // Focus input and select text
          var input = $el.find('input');
          input.focus().select();
          
          // Handle Enter key and blur
          input.on('keydown', function(e) {
            if (e.key === 'Enter') {
              $(this).blur();
            }
          });
          
          input.on('blur', function() {
            var newTotal = parseFloat($(this).val());
            if (isNaN(newTotal) || newTotal < 0) newTotal = total;
            
            // Update display
            var suffix = newTotal === 1 ? 'pt' : 'pts';
            $el.html(current + ' / ' + newTotal + ' ' + suffix);
            $el.css('background-color', 'transparent');
            $el.data('total', newTotal);
            
            // Send to Shiny if changed
            if (newTotal !== total) {
              Shiny.setInputValue('", ns("total_score_input"), "', newTotal);
            }
          });
        });
      });
    "))),
    shiny::hr(class="my-0")
  )
}

#' Mark Grade Server
#'
#' Server logic for grade management with S7 state management
#'
#' @param id Character. Module namespace ID
#' @param initial_grade markermd_grade_state S7 object with initial state
#' @param ui_ns Function. Optional UI namespace function for JavaScript element targeting
#' @export
mark_grade_server = function(id, initial_grade, ui_ns = NULL) {
  # Use same sanitized ID as the UI
  safe_id = gsub("[^A-Za-z0-9_-]", "_", id)
  ns = shiny::NS(safe_id)
  
  # If ui_ns is provided, construct the correct target namespace for JavaScript
  target_ns = if (!is.null(ui_ns)) {
    function(id) ui_ns(paste0(safe_id, "-", id))
  } else {
    ns
  }

  shiny::moduleServer(safe_id, function(input, output, session) {
  
    # Internal state using S7 class
    grade_state = shiny::reactiveVal(initial_grade)
    
    
    # Handle total score changes
    shiny::observeEvent(input$total_score_input, {
      current_grade = grade_state()
      
      new_total = input$total_score_input
      if (new_total < 0) new_total = 0  # Total can't be negative
      
      # Only update if the value actually changed
      if (new_total != current_grade@total_score) {
        # Adjust current score if needed
        new_current = current_grade@current_score
        
        # In negative grading mode, if current score equals the old total, 
        # update it to the new total (maintain "full points" status)
        if (current_grade@grading_mode == "negative" && 
            current_grade@current_score == current_grade@total_score) {
          new_current = new_total
        }
        
        # Apply bounds if enabled
        if (current_grade@bound_below_max && new_current > new_total) {
          new_current = new_total
        }
        
        # Create new grade state with updated total
        new_grade = markermd_grade_state(
          current_score = new_current,
          total_score = new_total,
          grading_mode = current_grade@grading_mode,
          bound_above_zero = current_grade@bound_above_zero,
          bound_below_max = current_grade@bound_below_max
        )
        
        grade_state(new_grade)
        
        # Update display with new total and potentially adjusted current score
        shinyjs::runjs(glue::glue("
          var scoreDiv = document.getElementById('{target_ns('score_display')}');
          if (scoreDiv) {{
            scoreDiv.setAttribute('data-current', {new_current});
            scoreDiv.setAttribute('data-total', {new_total});
            var suffix = {new_total} === 1 ? 'pt' : 'pts';
            scoreDiv.innerHTML = {new_current} + ' / ' + {new_total} + ' ' + suffix;
          }}
        ", .open = "{", .close = "}"))
      }
    }, ignoreInit = TRUE)
    
    # Handle grading mode changes
    shiny::observeEvent(input$grading_mode, {
      current_grade = grade_state()
      
      if (input$grading_mode != current_grade@grading_mode) {
        # Set appropriate starting score based on grading mode
        new_current_score = if (input$grading_mode == "positive") {
          0
        } else {
          current_grade@total_score
        }
        
        new_grade = markermd_grade_state(
          current_score = new_current_score,
          total_score = current_grade@total_score,
          grading_mode = input$grading_mode,
          bound_above_zero = current_grade@bound_above_zero,
          bound_below_max = current_grade@bound_below_max
        )
        
        grade_state(new_grade)
        
        # Update the display
        shinyjs::runjs(glue::glue("
          var scoreDiv = document.getElementById('{target_ns('score_display')}');
          if (scoreDiv) {{
            scoreDiv.setAttribute('data-current', {new_current_score});
            var total = parseFloat(scoreDiv.getAttribute('data-total'));
            var suffix = total === 1 ? 'pt' : 'pts';
            scoreDiv.innerHTML = {new_current_score} + ' / ' + total + ' ' + suffix;
          }}
        ", .open = "{", .close = "}"))
        
      }
    }, ignoreInit = TRUE)
    
    # Handle bounds changes
    shiny::observeEvent(input$grade_bounds, {
      current_grade = grade_state()
      
      new_bound_above_zero = "above_zero" %in% input$grade_bounds
      new_bound_below_max = "below_max" %in% input$grade_bounds
      
      if (new_bound_above_zero != current_grade@bound_above_zero || 
          new_bound_below_max != current_grade@bound_below_max) {
        new_grade = markermd_grade_state(
          current_score = current_grade@current_score,
          total_score = current_grade@total_score,
          grading_mode = current_grade@grading_mode,
          bound_above_zero = new_bound_above_zero,
          bound_below_max = new_bound_below_max
        )
        
        grade_state(new_grade)
      }
    }, ignoreInit = TRUE)
    
    
    # Return reactive grade state and update method for external use
    return(list(
      id = safe_id,
      grade = shiny::reactive(grade_state()),
      update_grade = function(new_grade) {
        grade_state(new_grade)
        
        # Update the display
        shinyjs::runjs(glue::glue("
          var scoreDiv = document.getElementById('{target_ns('score_display')}');
          if (scoreDiv) {{
            scoreDiv.setAttribute('data-current', {new_grade@current_score});
            scoreDiv.setAttribute('data-total', {new_grade@total_score});
            var suffix = {new_grade@total_score} === 1 ? 'pt' : 'pts';
            scoreDiv.innerHTML = {new_grade@current_score} + ' / ' + {new_grade@total_score} + ' ' + suffix;
          }}
        ", .open = "{", .close = "}"))
      }
    ))
  })
}