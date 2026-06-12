# Shiny module for displaying and managing question grades with scoring options

# Creates UI for displaying current score and grading configuration options
#
# id: Character. Module namespace ID
# grade_state: markermd_grade_state S7 object containing grade properties

mark_grade_ui = function(id, grade_state) {
  # Sanitize ID to avoid spaces and special characters that could break JavaScript
  safe_id = gsub("[^A-Za-z0-9_-]", "_", id)
  ns = shiny::NS(safe_id)
  
  shiny::div(
    class = "border-bottom",
    bslib::layout_columns(
      col_widths = c(8, 4),
      # Score display (left side, read-only; the total is edited in the
      # settings popover so one student's adjustment cannot silently change
      # the question-wide denominator)
      shiny::div(
        shiny::div(
          style = "font-size: 14px;",
          "Points:"
        ),
        shiny::div(
          id = ns("score_display"),
          `data-current` = grade_state@current_score,
          `data-total` = grade_state@total_score,
          class = "fw-bold d-inline-block py-2",
          style = "font-size: 20px;",
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
          shiny::actionButton(
            ns("grade_settings"),
            shiny::icon("gear"),
            class = "btn-outline-secondary btn-sm",
            title = "Grading settings"
          ),
          title = "Grading settings",
          placement = "bottom",
          shiny::div(
            style = "min-width: 220px;",
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
            shiny::numericInput(
              ns("total_score_input"),
              shiny::strong("Total points:"),
              value = grade_state@total_score,
              min = 0,
              step = 0.5,
              width = "100%"
            ),
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
                "Enforce score \u2265 0" = "above_zero",
                "Enforce score \u2264 max" = "below_max"
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
    shiny::tags$script(shiny::HTML("
      $(document).ready(function() {
        // Patch the score display in place. The grade widget is deliberately
        // not re-rendered on score changes (see output$grade_ui in
        // mod_mark_rubric.R), so the server updates it via this message.
        // jQuery .data() caches after first read, so update both the cache and
        // the attributes.
        Shiny.addCustomMessageHandler('markermd_update_score', function(msg) {
          var el = document.getElementById(msg.id);
          if (!el) return;
          var $el = $(el);
          $el.data('current', msg.current).data('total', msg.total);
          $el.attr('data-current', msg.current).attr('data-total', msg.total);
          var suffix = msg.total === 1 ? 'pt' : 'pts';
          $el.html(msg.current + ' / ' + msg.total + ' ' + suffix);
        });
      });
    ")),
    shiny::hr(class="my-0")
  )
}

# Server logic for grade management with S7 state management
#
# id: Character. Module namespace ID
# initial_grade: markermd_grade_state S7 object with initial state
# ui_ns: Function. Optional UI namespace function for JavaScript element targeting
# collection_path: Character string. Path to collection directory (optional)
# question_name: Character string. Name of the question (optional)

mark_grade_server = function(id, initial_grade, ui_ns = NULL, collection_path = NULL, question_name = NULL) {
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

    # Patch the score display's text and data attributes in place (the widget
    # is not re-rendered on score changes; see output$grade_ui in
    # mod_mark_rubric.R). The patch is deferred to onFlushed because a
    # question switch re-renders the widget in the same flush from a grade
    # snapshot taken before the loaded selections recompute the score; a
    # message sent mid-flush would land first and be clobbered by that
    # stale re-render.
    update_score_display = function(current, total) {
      session$onFlushed(function() {
        session$sendCustomMessage("markermd_update_score", list(
          id = target_ns("score_display"),
          current = current,
          total = total
        ))
      }, once = TRUE)
    }


    # Handle total score changes. numericInput reports NA while cleared, so
    # ignore that rather than crash.
    shiny::observe({
      current_grade = grade_state()

      new_total = input$total_score_input
      if (is.null(new_total) || is.na(new_total)) {
        return()
      }
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
        
        # Save to database if parameters are provided
        if (!is.null(collection_path) && !is.null(question_name)) {
          save_grade_state(collection_path, question_name, new_grade)
        }
        
        # Update display with new total and potentially adjusted current score
        update_score_display(new_current, new_total)
      }
    }) |> shiny::bindEvent(input$total_score_input, ignoreInit = TRUE)

    # Handle grading mode changes
    shiny::observe({
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
        
        # Save to database if parameters are provided
        if (!is.null(collection_path) && !is.null(question_name)) {
          save_grade_state(collection_path, question_name, new_grade)
        }
        
        # Update the display
        update_score_display(new_current_score, current_grade@total_score)

      }
    }) |> shiny::bindEvent(input$grading_mode, ignoreInit = TRUE)

    # Handle bounds changes
    shiny::observe({
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
        
        # Save to database if parameters are provided
        if (!is.null(collection_path) && !is.null(question_name)) {
          save_grade_state(collection_path, question_name, new_grade)
        }
      }
    }) |> shiny::bindEvent(input$grade_bounds, ignoreInit = TRUE)


    # Return reactive grade state and update method for external use
    return(list(
      id = safe_id,
      grade = shiny::reactive(grade_state()),
      update_grade = function(new_grade) {
        grade_state(new_grade)
        
        # Save to database if parameters are provided
        if (!is.null(collection_path) && !is.null(question_name)) {
          save_grade_state(collection_path, question_name, new_grade)
        }
        
        # Update the display
        update_score_display(new_grade@current_score, new_grade@total_score)
      }
    ))
  })
}