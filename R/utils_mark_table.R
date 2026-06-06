# Cell renderers for the repository overview table in the marking app
#
# Each returns the HTML string for one repository's cell in a gt column.

# Validation summary cell: pass/fail count with a tooltip listing failures
#
# repo: Character. Repository name
# validation_results: List of per-repo validation results
# template_obj: markermd_template object, or NULL

validation_status_cell = function(repo, validation_results, template_obj) {
  if (is.null(template_obj)) {
    return("")  # No validation without template
  }

  repo_validation = validation_results[[repo]]
  if (is.null(repo_validation) || length(repo_validation) == 0) {
    return("")  # No validation data
  }

  # Count validation results (treat errors as failures)
  pass_count = sum(sapply(repo_validation, function(v) v$status == "pass"))
  fail_count = sum(sapply(repo_validation, function(v) v$status %in% c("fail", "error")))
  total_count = length(repo_validation)

  # Collect failed questions (including errors) in template order
  template_question_names = sapply(template_obj@questions, function(q) q@name)
  validation_names = names(repo_validation)
  failed_questions = character(0)
  for (question_name in template_question_names) {
    if (question_name %in% validation_names) {
      validation = repo_validation[[question_name]]
      if (validation$status %in% c("fail", "error")) {
        failed_questions = c(failed_questions, question_name)
      }
    }
  }

  # Build tooltip text
  tooltip_parts = c()
  if (fail_count > 0) {
    if (length(failed_questions) > 0) {
      tooltip_parts = c(tooltip_parts, "Failed validation:")
      tooltip_parts = c(tooltip_parts, paste("•", failed_questions))
    }
  } else {
    tooltip_parts = paste("All", total_count, "validation rules passed")
  }

  tooltip_text = paste(tooltip_parts, collapse = "&#10;")

  if (fail_count == 0) {
    # All passed
    paste0('<span class="text-success" title="', tooltip_text, '"><i class="fas fa-check-circle"></i> ', pass_count, '/', total_count, '</span>')
  } else {
    # Has failures (including errors)
    paste0('<span class="text-danger" title="', tooltip_text, '"><i class="fas fa-times-circle"></i> ', pass_count, '/', total_count, '</span>')
  }
}

# Grading progress cell: a sparkline bar showing graded/total questions
#
# repo: Character. Repository name
# template_obj: markermd_template object, or NULL
# all_progress: Named list of graded-question counts per repo
# collection_path: Path to the collection directory (for the database)
# question_names: Character vector of question names from the template

grading_progress_cell = function(repo, template_obj, all_progress, collection_path, question_names) {
  if (is.null(template_obj)) {
    # No template - show empty or placeholder
    return("")
  }

  # Get number of questions from template
  total_questions = length(template_obj@questions)
  if (total_questions == 0) {
    return("")
  }

  # Get graded questions from pre-calculated progress
  graded_questions = all_progress[[repo]]
  percentage = round((graded_questions / total_questions) * 100)

  # Determine ungraded questions for tooltip (only if needed)
  if (graded_questions < total_questions) {
    ungraded_questions = character(0)

    # Get ungraded questions efficiently
    graded_status = with_database(collection_path, function(conn) {
      question_status = rep(FALSE, length(question_names))
      names(question_status) = question_names

      for (i in seq_along(question_names)) {
        question_name = question_names[i]

        # Check for selected rubric items
        grade_query = DBI::dbGetQuery(conn, "
          SELECT COUNT(*) as selected_count
          FROM grades g1
          INNER JOIN (
            SELECT item_id, MAX(timestamp) as max_timestamp
            FROM grades
            WHERE question_name = ? AND assignment_repo = ?
            GROUP BY item_id
          ) g2 ON g1.item_id = g2.item_id AND g1.timestamp = g2.max_timestamp
          WHERE g1.question_name = ? AND g1.assignment_repo = ? AND g1.selected = 1
        ", params = list(question_name, repo, question_name, repo))

        if (grade_query$selected_count > 0) {
          question_status[question_name] = TRUE
        } else {
          # Check for non-empty comment
          comment_query = DBI::dbGetQuery(conn, "
            SELECT COUNT(*) as comment_count
            FROM comments
            WHERE question_name = ? AND assignment_repo = ? AND TRIM(comment_text) != ''
            ORDER BY timestamp DESC
            LIMIT 1
          ", params = list(question_name, repo))

          if (comment_query$comment_count > 0) {
            question_status[question_name] = TRUE
          }
        }
      }

      return(question_status)
    })

    ungraded_questions = names(graded_status)[!graded_status]
  } else {
    ungraded_questions = character(0)
  }

  # Create tooltip text
  tooltip_parts = c()
  if (graded_questions == total_questions) {
    tooltip_parts = paste("All", total_questions, "questions graded")
  } else {
    if (length(ungraded_questions) > 0) {
      tooltip_parts = c(tooltip_parts, "Ungraded questions:")
      tooltip_parts = c(tooltip_parts, paste("•", ungraded_questions))
    }
  }

  tooltip_text = paste(tooltip_parts, collapse = "&#10;")

  # Create sparkline bar with percentage
  bar_width = percentage  # Width as percentage
  bar_color = if (percentage == 100) {
    "#28a745"  # Green for complete
  } else if (percentage >= 50) {
    "#ffc107"  # Yellow for partial
  } else {
    "#dc3545"  # Red for minimal progress
  }

  # HTML for sparkline bar with text inside and tooltip
  paste0(
    '<div title="', tooltip_text, '" style="width: 100%; height: 16px; background-color: #e9ecef; border-radius: 8px; position: relative; overflow: hidden;">',
    '<div style="height: 100%; background-color: ', bar_color, '; width: ', bar_width, '%; border-radius: 8px; transition: width 0.3s ease;"></div>',
    '<span style="position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); font-size: 10px; font-weight: 600; color: #333; white-space: nowrap; pointer-events: none;">', graded_questions, '/', total_questions, '</span>',
    '</div>'
  )
}
