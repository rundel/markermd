# Cell renderers for the repository overview table in the marking app
#
# Each returns the HTML string for one repository's cell in a gt column.

# Validation summary cell: pass/fail count with a tooltip listing failures,
# or a warning marker when the repo's document is missing or failed to parse
#
# repo: Character. Repository name
# validation_results: List of per-repo validation results
# template_obj: markermd_template object, or NULL
# repo_errors: Named list of per-repo missing-document/parse error messages

validation_status_cell = function(repo, validation_results, template_obj, repo_errors = list()) {
  err = repo_errors[[repo]]
  if (!is.null(err)) {
    first_line = strsplit(err, "\n")[[1]][1]
    return(as.character(htmltools::span(
      class = "text-danger",
      title = first_line,
      shiny::icon("triangle-exclamation")
    )))
  }

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
  ordered_names = template_question_names(template_obj)
  validation_names = names(repo_validation)
  failed_questions = character(0)
  for (question_name in ordered_names) {
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
      tooltip_parts = c(tooltip_parts, paste("\u2022", failed_questions))
    }
  } else {
    tooltip_parts = paste("All", total_count, "validation rules passed")
  }

  # Parts joined with literal newlines: htmltools escapes the title attribute
  # structurally (quotes as entities, newlines as &#10;), so instructor-authored
  # question names cannot inject markup.
  tooltip_text = paste(tooltip_parts, collapse = "\n")

  if (fail_count == 0) {
    # All passed
    as.character(htmltools::span(
      class = "text-success",
      title = tooltip_text,
      shiny::icon("circle-check"),
      paste0(" ", pass_count, "/", total_count)
    ))
  } else {
    # Has failures (including errors)
    as.character(htmltools::span(
      class = "text-danger",
      title = tooltip_text,
      shiny::icon("circle-xmark"),
      paste0(" ", pass_count, "/", total_count)
    ))
  }
}

# Grading progress cell: a Bootstrap progress bar showing graded/total
# questions. Completion is always drawn in success-green at partial width so
# red stays reserved for validation failures in the adjacent column.
#
# repo: Character. Repository name
# template_obj: markermd_template object, or NULL
# all_progress: Named list of graded-question counts per repo
# graded_pairs: Precomputed graded_question_pairs() result used to list the
#   ungraded questions in the tooltip without further queries
# question_names: Character vector of question names from the template

grading_progress_cell = function(repo, template_obj, all_progress, graded_pairs, question_names) {
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

  # Determine ungraded questions for the tooltip from the precomputed pairs
  if (graded_questions < total_questions) {
    repo_graded = graded_pairs$question_name[graded_pairs$assignment_repo == repo]
    ungraded_questions = setdiff(question_names, repo_graded)
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
      tooltip_parts = c(tooltip_parts, paste("\u2022", ungraded_questions))
    }
  }

  tooltip_text = paste(tooltip_parts, collapse = "\n")

  # Bootstrap progress component with the count overlaid
  as.character(htmltools::div(
    class = "progress position-relative",
    title = tooltip_text,
    style = "height: 16px;",
    htmltools::div(
      class = "progress-bar bg-success",
      role = "progressbar",
      style = paste0("width: ", percentage, "%;"),
      `aria-valuenow` = graded_questions,
      `aria-valuemin` = "0",
      `aria-valuemax` = total_questions
    ),
    htmltools::span(
      class = "position-absolute top-50 start-50 translate-middle fw-semibold text-dark",
      style = "font-size: 10px; pointer-events: none;",
      paste0(graded_questions, "/", total_questions)
    )
  ))
}
