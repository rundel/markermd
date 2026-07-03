# Builders and cell renderers for the repository overview table in the
# marking app. The pure pieces live here (filtering, table-data assembly, gt
# construction, and the per-cell renderers) so the renderUI in mark-app.R is
# a thin reactive shell over them.

# Icon tag for the gt cells; the Font Awesome dependency is already on the
# page via the static shiny::icon() uses in the rubric tab
cell_icon = function(name, ...) {
  shiny::icon(name, class = "fa-fw fs-6", ...)
}

# Apply the repo-table name and status filters, returning the visible repo
# names in repo_list order. "failed" counts a repo with a parse/missing-doc
# error as failed regardless of validation; "ungraded"/"graded" compare
# grading progress against the question count and no-op without a template
# (all_progress NULL). Any other status value applies only the name filter.
#
# repo_list: Character vector of all repository names (the full index space)
# name_filter: Character or NULL. Case-insensitive fixed substring match
# status_filter: Character or NULL. "all", "failed", "ungraded", or "graded"
# validation_results: List of per-repo validation results
# repo_errors: Named list of per-repo missing-document/parse error messages
# all_progress: Named vector of graded-question counts per repo, or NULL
# question_names: Character vector of question names from the template

filter_repo_table_rows = function(repo_list, name_filter, status_filter,
                                  validation_results, repo_errors,
                                  all_progress, question_names) {
  visible = repo_list
  if (!is.null(name_filter) && nzchar(trimws(name_filter))) {
    visible = visible[grepl(tolower(trimws(name_filter)), tolower(visible), fixed = TRUE)]
  }
  if (identical(status_filter, "failed")) {
    failed = vapply(repo_list, function(repo) {
      if (!is.null(repo_errors[[repo]])) return(TRUE)
      res = validation_results[[repo]]
      !is.null(res) && any(vapply(res, function(q) q$status %in% c("fail", "error"), logical(1)))
    }, logical(1))
    visible = intersect(visible, repo_list[failed])
  }
  if (identical(status_filter, "ungraded") && !is.null(all_progress)) {
    visible = intersect(visible, repo_list[all_progress[repo_list] < length(question_names)])
  }
  if (identical(status_filter, "graded") && !is.null(all_progress)) {
    visible = intersect(visible, repo_list[all_progress[repo_list] >= length(question_names)])
  }
  visible
}

# Assemble the repo table's display data, one row per visible repo, with the
# seven display columns as HTML strings. Every action button reports its row
# index through one shared input per column (priority: 'event' so re-clicking
# the same row re-fires), keeping the server at one observer per column
# instead of one per repo. Button ids stay keyed to each repo's position in
# the FULL repo_list (visible_idx, not seq_along(visible)) so the observers
# registered over seq_along(repo_list) and the shinyjs active-row mover keep
# working. Cells are built as htmltools tags (escaping is structural) and
# rendered to strings at the gt boundary.
#
# visible: Character vector of visible repo names (filter_repo_table_rows())
# visible_idx: Integer vector. match(visible, repo_list)
# repo_list: Character vector of all repository names
# collection: The repo collection; collection$repo lists repos with a document
# artifact_paths: Named list of resolved local report paths (NA when none)
# repo_to_github: Named list mapping repo names to GitHub owner/repo slugs
# validation_results: List of per-repo validation results
# template_obj: markermd_template object, or NULL
# repo_errors: Named list of per-repo missing-document/parse error messages
# all_progress: Named vector of graded-question counts per repo, or NULL
# graded_pairs: Precomputed graded_question_pairs() result
# question_names: Character vector of question names from the template
# active_row: Integer. The selected repo's position in repo_list, baked in as
#   the initial .active class (later selections toggle it via shinyjs)

build_repo_table_data = function(visible, visible_idx, repo_list, collection,
                                 artifact_paths, repo_to_github,
                                 validation_results, template_obj, repo_errors,
                                 all_progress, graded_pairs, question_names,
                                 active_row) {
  repo_df = data.frame(
    Repository = visible,
    OriginalName = visible,  # Store original names for button creation
    stringsAsFactors = FALSE
  )

  repo_df$Folder = sapply(visible_idx, function(i) {
    as.character(htmltools::tags$button(
      onclick = paste0("Shiny.setInputValue('folder_clicked', ", i, ", {priority: 'event'})"),
      class = "btn btn-link p-0 border-0 text-reset",
      title = "Open folder",
      cell_icon("folder-open")
    ))
  })

  repo_df$GitHub = sapply(visible, function(repo) {
    if (repo %in% names(repo_to_github)) {
      github_url = paste0("https://github.com/", repo_to_github[[repo]])
      as.character(htmltools::a(
        href = github_url,
        target = "_blank",
        class = "text-reset text-decoration-none",
        title = "Open on GitHub",
        cell_icon("github")
      ))
    } else {
      as.character(htmltools::span(
        class = "opacity-25",
        title = "No GitHub repository",
        cell_icon("github")
      ))
    }
  })

  # Artifact column: clickable icons for repos with a local report
  repo_df$Artifacts = sapply(visible_idx, function(i) {
    repo = repo_list[i]
    if (!is.na(artifact_paths[[repo]])) {
      # Has a resolved local report - clickable file icon
      as.character(htmltools::tags$button(
        onclick = paste0("Shiny.setInputValue('artifact_clicked', ", i, ", {priority: 'event'})"),
        class = "btn btn-link p-0 border-0 text-reset",
        title = "View artifact",
        cell_icon("file")
      ))
    } else {
      # No report found - greyed out unclickable icon
      as.character(htmltools::span(
        class = "opacity-25",
        title = "No artifact available",
        cell_icon("file")
      ))
    }
  })

  # Source code column: clickable file-code icons; repos without a matching
  # document get a greyed-out marker instead of a dead button
  repos_with_doc = collection$repo
  repo_df$Source = sapply(visible_idx, function(i) {
    repo = repo_list[i]
    if (!repo %in% repos_with_doc) {
      return(as.character(htmltools::span(
        class = "opacity-25",
        title = "No document found",
        cell_icon("file-code")
      )))
    }
    as.character(htmltools::tags$button(
      onclick = paste0("Shiny.setInputValue('source_clicked', ", i, ", {priority: 'event'})"),
      class = "btn btn-link p-0 border-0 text-reset",
      title = "View source code",
      cell_icon("file-code")
    ))
  })

  repo_df$Validation = sapply(
    visible, validation_status_cell,
    validation_results = validation_results, template_obj = template_obj,
    repo_errors = repo_errors
  )

  repo_df$Grading = sapply(
    visible, grading_progress_cell,
    template_obj = template_obj, all_progress = all_progress,
    graded_pairs = graded_pairs, question_names = question_names
  )

  # Clickable repository names; the active-selection highlight is baked into
  # the class and subsequently moved via shinyjs (see mark-app.R)
  repo_df$Repository = purrr::map_chr(seq_along(visible), function(k) {
    i = visible_idx[k]
    active_class = if (i == active_row) " active" else ""

    # Just show the repo name - no GitHub icon here; htmltools escapes it,
    # so a directory name cannot inject markup into the table
    as.character(htmltools::tags$button(
      onclick = paste0("Shiny.setInputValue('repo_select_clicked', ", i, ", {priority: 'event'})"),
      class = paste0("repo-select-btn", active_class),
      `data-row` = i,
      visible[k]
    ))
  })

  repo_df[, c("Repository", "Folder", "GitHub", "Artifacts", "Source", "Validation", "Grading"), drop = FALSE]
}

# Build the styled gt table from the repo table data
#
# table_data: Data frame from build_repo_table_data()

repo_table_gt = function(table_data) {
  gt_table = gt::gt(table_data) |>
    gt::fmt_markdown(columns = .data$Repository) |>
    gt::fmt_markdown(columns = .data$Folder) |>
    gt::fmt_markdown(columns = .data$GitHub) |>
    gt::fmt_markdown(columns = .data$Artifacts) |>
    gt::fmt_markdown(columns = .data$Source) |>
    gt::fmt_markdown(columns = .data$Grading) |>
    gt::fmt_markdown(columns = .data$Validation) |>
    gt::cols_label(
      Repository = "Repository",
      Folder = "", GitHub = "", Artifacts = "", Source = "",
      Validation = "Validation", Grading = "Progress"
    ) |>
    # Icon and validation columns hold fixed-size content, so they get
    # fixed widths; Repository is left unspecified so it absorbs all
    # remaining width (table-layout is fixed), keeping the icon cluster
    # tight at any viewport size.
    gt::cols_width(
      Folder ~ gt::px(32),
      GitHub ~ gt::px(32),
      Artifacts ~ gt::px(32),
      Source ~ gt::px(32),
      Validation ~ gt::px(80),
      Grading ~ gt::pct(24)
    ) |>
    gt::cols_align(align = "center", columns = .data$Validation)

  gt_table |>
    gt::tab_options(
      table.width = gt::pct(100),
      table.font.size = "12px",  # Smaller font size
      data_row.padding = "2px",
      data_row.padding.horizontal = "6px",
      column_labels.hidden = FALSE,  # Show column headers
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table.border.left.style = "none",
      table.border.right.style = "none"
    ) |>
    gt::opt_css(
      css = "
      .gt_table {
        border: none !important;
      }
      .gt_col_heading {
        font-size: 11px !important;
        font-weight: bold !important;
        padding: 4px 6px !important;
      }
      /* Gutter between the repo name and the icon cluster: scales with
         the table width but is capped so wide windows stay together */
      .gt_table td:first-child {
        padding-right: clamp(8px, 3%, 24px) !important;
      }
      "
    )
}

# Cell renderers
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
