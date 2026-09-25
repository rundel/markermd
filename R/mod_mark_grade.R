# Shiny module for displaying and managing question grades with scoring options

# Text of the cap badge beside the score: how much the item group bounds
# changed the score ("1 pt capped")
#
# capped: Numeric. score - score without group bounds

cap_badge_text = function(capped) {
  paste(format_points(abs(capped)), "capped")
}

# A score as signed text: "+3", "-1.5", "0"
#
# x: Numeric

format_score_signed = function(x) {
  paste0(if (x > 0) "+" else "", format_score(x))
}

# The score breakdown shown in the cap badge's popover: the start value, the
# ungrouped items, each group's applied value (with its raw sum and the
# bound that changed it), the bounded score and the score without the group
# bounds
#
# result: List from score_question()
# state: markermd_grade_state with the question's scoring setup

score_breakdown_ui = function(result, state) {
  row = function(label, value, note = NULL, total = FALSE) {
    shiny::tags$tr(
      class = if (total) "fw-bold",
      shiny::tags$td(
        class = if (total) "border-top",
        label,
        if (!is.null(note)) shiny::span(class = "text-muted fw-normal ms-1", note)
      ),
      shiny::tags$td(
        class = paste("text-end", if (total) "border-top"),
        style = "font-variant-numeric: tabular-nums;",
        value
      )
    )
  }

  group_rows = lapply(seq_len(nrow(result$groups)), function(i) {
    g = result$groups[i, ]
    note = if (!isTRUE(all.equal(g$sum, g$applied))) {
      bound = if (!is.na(g$floor) && g$applied == g$floor) {
        paste("floor", format_score(g$floor))
      } else {
        paste("ceiling", format_score(g$ceiling))
      }
      paste0(format_score_signed(g$sum), ", ", bound)
    }
    row(g$label, format_score_signed(g$applied), note = note)
  })

  bounds = if (state@bound_above_zero && state@bound_below_max) {
    paste0("(bounded to 0 to ", format_score(state@total_score), ")")
  } else if (state@bound_above_zero) {
    "(bounded to \u2265 0)"
  } else if (state@bound_below_max) {
    paste0("(bounded to \u2264 ", format_score(state@total_score), ")")
  } else {
    "(unbounded)"
  }

  shiny::tagList(
    shiny::tags$table(
      class = "w-100 rubric-score-breakdown",
      shiny::tags$tbody(
        row(paste0("Start (", state@grading_mode, " mode)"), format_score(result$base)),
        row("Ungrouped items", format_score_signed(result$ungrouped)),
        group_rows,
        row("Score", format_score(result$score), note = bounds, total = TRUE)
      )
    ),
    shiny::div(
      class = "text-muted mt-1",
      style = "font-size: 11px;",
      paste0("Without group bounds: ", format_score(result$without_groups))
    )
  )
}

# Creates UI for displaying current score and grading configuration options
#
# id: Character. Module namespace ID
# grade_state: markermd_grade_state S7 object containing grade properties
# capped: Numeric. How much the item group bounds changed the current score
#   (score minus the score without them); the cap badge is hidden at 0
# breakdown: UI shown in the cap badge's popover (the rubric module's score
#   breakdown output), or NULL for a badge without a popover

mark_grade_ui = function(id, grade_state, capped = 0, breakdown = NULL) {
  # Sanitize ID to avoid spaces and special characters that could break JavaScript
  safe_id = gsub("[^A-Za-z0-9_-]", "_", id)
  ns = shiny::NS(safe_id)

  # The cap badge: shown only while the group bounds make a difference, and
  # patched along with the score (markermd_update_score below)
  cap_badge = shiny::span(
    id = ns("cap_badge"),
    class = paste("rubric-cap-badge", if (capped == 0) "d-none"),
    tabindex = "0",
    role = "button",
    title = "Item group bounds changed this score",
    shiny::icon("compress"), " ",
    shiny::span(id = ns("cap_badge_text"), cap_badge_text(capped))
  )
  if (!is.null(breakdown)) {
    # Wider than Bootstrap's default popover so the value column has room
    # (the rubric-wide-popover class is styled in mark-app.R)
    cap_badge = bslib::popover(
      cap_badge,
      title = "Score breakdown",
      placement = "bottom",
      id = ns("cap_badge_popover"),
      options = list(customClass = "rubric-wide-popover"),
      shiny::div(style = "min-width: 320px;", breakdown)
    )
  }
  
  shiny::div(
    class = "border-bottom",
    bslib::layout_columns(
      col_widths = c(8, 4),
      # Score display (left side). Clicking it opens a popover holding the
      # Total points input; the total is per-question (one denominator shared
      # by every repo), so it is edited through this deliberate popover rather
      # than inline. The trigger wraps score_display so the in-place score
      # patches (markermd_update_score below) never touch the popover wiring
      # or the pen affordance. The cap badge sits beside it, outside the
      # trigger, so clicking the badge opens the breakdown rather than the
      # total.
      shiny::div(
        shiny::div(
          style = "font-size: 14px;",
          "Points:"
        ),
        shiny::div(
          class = "d-flex align-items-center gap-2 flex-wrap",
          bslib::popover(
            shiny::div(
              id = ns("score_popover_trigger"),
              class = "d-inline-flex align-items-center gap-1",
              style = "cursor: pointer;",
              tabindex = "0",
              role = "button",
              `aria-label` = "Set total points",
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
              ),
              shiny::icon("pen", class = "text-muted small")
            ),
            title = "Total points",
            placement = "bottom",
            shiny::div(
              style = "min-width: 180px;",
              shiny::numericInput(
                ns("total_score_input"),
                NULL,
                value = grade_state@total_score,
                min = 0,
                step = 0.5,
                width = "100%"
              )
            )
          ),
          cap_badge
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
            ),
            # Optional: an ungraded submission scores as if no item were
            # selected, for a document-level question most submissions skip
            shiny::checkboxInput(
              ns("grade_optional"),
              "Optional question (ungraded counts as no items selected)",
              value = grade_state@optional
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
          if (el) {
            var $el = $(el);
            $el.data('current', msg.current).data('total', msg.total);
            $el.attr('data-current', msg.current).attr('data-total', msg.total);
            var suffix = msg.total === 1 ? 'pt' : 'pts';
            $el.html(msg.current + ' / ' + msg.total + ' ' + suffix);
          }
          // The cap badge is only touched when the message carries it (a
          // total or mode edit leaves it to the next score recompute)
          if (msg.badge_id && msg.capped_text !== null && msg.capped_text !== undefined) {
            var badge = document.getElementById(msg.badge_id);
            if (badge) {
              badge.classList.toggle('d-none', !msg.capped);
              var text = document.getElementById(msg.badge_id + '_text');
              if (text) text.textContent = msg.capped_text;
            }
          }
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
    # mod_mark_rubric.R), and the cap badge when capped is given. The patch
    # is deferred to onFlushed because a question switch re-renders the
    # widget in the same flush from a grade snapshot taken before the loaded
    # selections recompute the score; a message sent mid-flush would land
    # first and be clobbered by that stale re-render.
    update_score_display = function(current, total, capped = NULL) {
      session$onFlushed(function() {
        session$sendCustomMessage("markermd_update_score", list(
          id = target_ns("score_display"),
          current = current,
          total = total,
          badge_id = target_ns("cap_badge"),
          capped = if (is.null(capped)) NULL else capped != 0,
          capped_text = if (is.null(capped)) NULL else cap_badge_text(capped)
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
          bound_below_max = current_grade@bound_below_max,
          optional = current_grade@optional
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
          bound_below_max = current_grade@bound_below_max,
          optional = current_grade@optional
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
          bound_below_max = new_bound_below_max,
          optional = current_grade@optional
        )
        
        grade_state(new_grade)
        
        # Save to database if parameters are provided
        if (!is.null(collection_path) && !is.null(question_name)) {
          save_grade_state(collection_path, question_name, new_grade)
        }
      }
    }) |> shiny::bindEvent(input$grade_bounds, ignoreInit = TRUE)

    # Handle the optional toggle
    shiny::observe({
      current_grade = grade_state()
      new_optional = isTRUE(input$grade_optional)

      if (new_optional != current_grade@optional) {
        new_grade = current_grade
        new_grade@optional = new_optional

        grade_state(new_grade)

        if (!is.null(collection_path) && !is.null(question_name)) {
          save_grade_state(collection_path, question_name, new_grade)
        }
      }
    }) |> shiny::bindEvent(input$grade_optional, ignoreInit = TRUE)


    # Return reactive grade state and update methods for external use
    return(list(
      id = safe_id,
      grade = shiny::reactive(grade_state()),
      # Replace the grade state (persisting it) and patch the display; capped
      # is how much the item group bounds changed the score, for the badge
      update_grade = function(new_grade, capped = NULL) {
        grade_state(new_grade)
        
        # Save to database if parameters are provided
        if (!is.null(collection_path) && !is.null(question_name)) {
          save_grade_state(collection_path, question_name, new_grade)
        }
        
        # Update the display
        update_score_display(new_grade@current_score, new_grade@total_score, capped)
      },
      # Patch the display (score and cap badge) from the current state
      # without changing it: the score is unchanged but the cap badge may be
      refresh_display = function(capped) {
        current = grade_state()
        update_score_display(current@current_score, current@total_score, capped)
      }
    ))
  })
}