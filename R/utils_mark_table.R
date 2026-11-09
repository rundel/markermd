# Builders and cell renderers for the repository overview table in the
# marking app's Assignments tab: one row per repository with its links, its
# validation status and grading progress, and its per-question scores and
# total, with the class summary statistics pinned beneath as column footers.
# The pure pieces live here (filtering, table-data assembly, the reactable
# construction, and the per-cell renderers) so the render in mark-app.R is a
# thin reactive shell over them. The score columns and the summary come from
# the builders in utils_mark_scores.R, which reuse collect_score_data() so
# the table and export_scores() always agree. The table is a reactable
# rather than gt because gt's interactive mode cannot pin summary rows under
# a sortable table.

# Icon tags built once per name and class, for renders that need many (the
# table needs a few per row, the rubric a few per item); see fa_icon()
.icon_cache = new.env(parent = emptyenv())

cached_icon = function(name, class = NULL) {
  key = paste(name, if (is.null(class)) "" else class, sep = "|")
  icon = .icon_cache[[key]]
  if (is.null(icon)) {
    icon = fa_icon(name, class = class)
    .icon_cache[[key]] = icon
  }
  icon
}

# Icon tag for the table cells; the Font Awesome dependency is on the page
# via fa_dependency()
cell_icon = function(name) {
  cached_icon(name, class = "fa-fw fs-6")
}

# Classes shared by every icon cell element (button, link or greyed span) so
# they all occupy the same compact box and sit at the same height: a
# Bootstrap button carries its own 24px line box and vertical-align: middle,
# while a link or span sits on the text baseline, so the box is flattened to
# the icon and everything is aligned middle
icon_cell_class = "d-inline-flex align-items-center align-middle lh-1"
icon_button_class = paste("btn btn-link p-0 border-0 text-reset", icon_cell_class)
icon_muted_class = paste("opacity-25", icon_cell_class)

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

# Sort key for the Validation column: the fraction of the repo's questions
# that passed, -1 for a repo whose document is missing or failed to parse
# (below every real result, as the "failed" filter also treats it), and NA
# when there is nothing to validate
#
# repo: Character. Repository name
# validation_results: List of per-repo validation results
# repo_errors: Named list of per-repo missing-document/parse error messages

validation_sort_key = function(repo, validation_results, repo_errors) {
  if (!is.null(repo_errors[[repo]])) {
    return(-1)
  }
  res = validation_results[[repo]]
  if (is.null(res) || length(res) == 0) {
    return(NA_real_)
  }
  mean(vapply(res, function(q) identical(q$status, "pass"), logical(1)))
}

# Assemble the repo table's data, one row per visible repo. The link columns
# (Folder, GitHub, Artifacts, Source) hold their cell HTML directly, since
# they never sort. Validation and Progress do sort, so they hold a numeric
# sort key (validation_sort_key(); the graded-question count) and their
# rendered HTML sits in the hidden Validation_html / Progress_html columns
# that the table's cell renderers read. The per-question score columns
# (named score_column_id() of each question), Total and Total_partial are
# copied from scores_table when the template has questions.
# Every action button reports its row index through one shared input per
# column (priority: 'event' so re-clicking the same row re-fires), keeping
# the server at one observer per column instead of one per repo. Button ids
# stay keyed to each repo's position in the FULL repo_list (visible_idx, not
# seq_along(visible)) so the observers registered over repo_list keep
# working under any filter. Cells are built as htmltools tags (escaping is
# structural) and rendered to strings here.
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
# scores_table: Data frame from build_scores_table_data() covering every
#   repo (Repository, one column per question, Total, Graded), or NULL when
#   the template has no questions

build_repo_table_data = function(visible, visible_idx, repo_list, collection,
                                 artifact_paths, repo_to_github,
                                 validation_results, template_obj, repo_errors,
                                 all_progress, graded_pairs, question_names,
                                 scores_table = NULL) {
  # Repository names stay plain text (reactable escapes them, so a directory
  # name cannot inject markup); the row's links live in the icon cells and
  # the validation marker
  repo_df = data.frame(
    Repository = visible,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  repo_df$Folder = vapply(visible_idx, function(i) {
    as.character(htmltools::tags$button(
      onclick = paste0("Shiny.setInputValue('folder_clicked', ", i, ", {priority: 'event'})"),
      class = icon_button_class,
      title = "Open folder",
      cell_icon("folder-open")
    ))
  }, character(1))

  repo_df$GitHub = vapply(visible, function(repo) {
    if (repo %in% names(repo_to_github)) {
      github_url = paste0("https://github.com/", repo_to_github[[repo]])
      as.character(htmltools::a(
        href = github_url,
        target = "_blank",
        class = paste("text-reset text-decoration-none", icon_cell_class),
        title = "Open on GitHub",
        cell_icon("github")
      ))
    } else {
      as.character(htmltools::span(
        class = icon_muted_class,
        title = "No GitHub repository",
        cell_icon("github")
      ))
    }
  }, character(1), USE.NAMES = FALSE)

  # Artifact column: clickable icons for repos with a local report
  repo_df$Artifacts = vapply(visible_idx, function(i) {
    repo = repo_list[i]
    if (!is.na(artifact_paths[[repo]])) {
      as.character(htmltools::tags$button(
        onclick = paste0("Shiny.setInputValue('artifact_clicked', ", i, ", {priority: 'event'})"),
        class = icon_button_class,
        title = "View artifact",
        cell_icon("file")
      ))
    } else {
      as.character(htmltools::span(
        class = icon_muted_class,
        title = "No artifact available",
        cell_icon("file")
      ))
    }
  }, character(1))

  # Source code column: clickable file-code icons; repos without a matching
  # document get a greyed-out marker instead of a dead button
  repos_with_doc = collection$repo
  repo_df$Source = vapply(visible_idx, function(i) {
    repo = repo_list[i]
    if (!repo %in% repos_with_doc) {
      return(as.character(htmltools::span(
        class = icon_muted_class,
        title = "No document found",
        cell_icon("file-code")
      )))
    }
    as.character(htmltools::tags$button(
      onclick = paste0("Shiny.setInputValue('source_clicked', ", i, ", {priority: 'event'})"),
      class = icon_button_class,
      title = "View source code",
      cell_icon("file-code")
    ))
  }, character(1))

  repo_df$Validation = vapply(
    visible, validation_sort_key,
    numeric(1), USE.NAMES = FALSE,
    validation_results = validation_results, repo_errors = repo_errors
  )
  repo_df$Validation_html = purrr::map_chr(seq_along(visible), function(k) {
    validation_status_cell(
      visible[k], validation_results, template_obj, repo_errors,
      row_index = visible_idx[k],
      n_warnings = length(document_parse_warnings(collection_ast_for(collection, visible[k])))
    )
  })

  repo_df$Progress = if (is.null(all_progress)) {
    rep(NA_integer_, length(visible))
  } else {
    as.integer(unname(all_progress[visible]))
  }
  repo_df$Progress_html = vapply(
    visible, grading_progress_cell,
    character(1), USE.NAMES = FALSE,
    template_obj = template_obj, all_progress = all_progress,
    graded_pairs = graded_pairs, question_names = question_names
  )

  if (!is.null(scores_table) && length(question_names) > 0) {
    rows = match(visible, scores_table$Repository)
    # The score columns carry ids derived from the question names, so a
    # question named like a fixed column ("Total", "Source") cannot collide
    # with it; the headers show the names
    for (k in seq_along(question_names)) {
      repo_df[[score_column_id(question_names[k])]] = scores_table[[question_names[k]]][rows]
    }
    repo_df$Total = scores_table$Total[rows]
    repo_df$Total_partial = scores_table$Total_partial[rows]
  }

  rownames(repo_df) = NULL
  repo_df
}

# The table column id of a question's score column
#
# question_name: Character vector of question names

score_column_id = function(question_name) {
  paste0("score_", question_dom_id(question_name))
}

# JavaScript click handler for the table: a click on a question's score cell
# reports the repo and question to the given Shiny input, as an event so
# repeated clicks on the same cell fire again. Every other column (the link
# buttons handle their own clicks) and the summary footer are ignored.
#
# input_id: Shiny input id to set
# question_names: Character vector of the question names (their columns are
#   score_column_id() of each)

repo_table_click_js = function(input_id, question_names) {
  columns = stats::setNames(as.list(question_names), score_column_id(question_names))
  reactable::JS(glue::glue("
    function(rowInfo, column) {
      var questions = <<jsonlite::toJSON(columns, auto_unbox = TRUE)>>;
      if (Object.prototype.hasOwnProperty.call(questions, column.id)) {
        Shiny.setInputValue('<<input_id>>', {repo: rowInfo.values.Repository, question: questions[column.id]}, {priority: 'event'});
      }
    }
  ", .open = "<<", .close = ">>"))
}

# The interactive Assignments table: the repository with its link buttons,
# validation marker and progress bar, then one score column per question and
# the total, sortable (ungraded last) with the summary rows from
# build_scores_summary_rows() pinned as column footers so they stay put
# under any sort order. The table fills its container and scrolls inside it
# with the header and footer sticky; every cell is vertically centered so the
# icon buttons, progress bars and text line up; the Repository column absorbs
# the width the fixed-size columns leave, up to a cap that keeps the link
# icons near the name on a wide window. Score headers carry the points on a
# muted second line (an optional question's name carries a small red dot),
# score cells are tinted by their shortfall (score_tint()) and are clickable
# and keyboard-reachable (class "score-link", styled by the app) when
# click_input is given; totals are bold, and an incomplete repository's
# running total shows in gray. Without questions there are no score columns,
# footers or click handler.
#
# table_data: Data frame from build_repo_table_data()
# summary_rows: Data frame from build_scores_summary_rows(), or NULL
# totals: Named numeric vector of per-question maximum points (empty when
#   the template has no questions)
# click_input: Shiny input id that score clicks report to, or NULL for a
#   table with no click-through
# optional: Names of the optional questions, whose names carry a small red
#   dot marker with an explanatory tooltip

repo_table_reactable = function(table_data, summary_rows, totals, click_input = NULL,
                                optional = character(0)) {
  question_names = names(totals)
  has_scores = length(question_names) > 0 && !is.null(summary_rows)

  # Column name on one line, cut with an ellipsis when it does not fit (the
  # full name shows on hover), an optional question's name followed by a
  # small red dot, and the points muted beneath
  column_header = function(name, sub, is_optional = FALSE) {
    htmltools::tagList(
      htmltools::div(
        class = "d-flex justify-content-end",
        style = "white-space: nowrap;",
        title = name,
        htmltools::span(class = "text-truncate", name),
        if (is_optional) {
          htmltools::span(
            class = "markermd-optional-tag text-danger",
            style = "font-size: 6px; vertical-align: super; margin-left: 2px;",
            title = "Optional: nothing selected still counts as graded",
            `aria-label` = "optional",
            "\u25cf"
          )
        }
      ),
      htmltools::div(class = "fw-normal text-muted", style = "font-size: 11px;", sub)
    )
  }

  # One line per summary row; a blank keeps its line (a no-break space
  # character, since reactable escapes footer text so an HTML entity would
  # show literally) so the rows stay aligned across columns
  footer_lines = function(values, class = NULL) {
    htmltools::div(class = class, lapply(values, function(value) {
      htmltools::div(
        style = "white-space: nowrap;",
        if (nzchar(value)) value else "\u00a0"
      )
    }))
  }
  blank_footer = if (has_scores) footer_lines(rep("", nrow(summary_rows))) else NULL

  link_class = if (is.null(click_input)) NULL else "score-link"

  icon_column = function() {
    reactable::colDef(
      name = "", html = TRUE, sortable = FALSE, align = "center", width = 36,
      footer = blank_footer
    )
  }

  # A sortable column whose cell is pre-rendered HTML in a hidden column
  html_column = function(html_column, name, width) {
    reactable::colDef(
      name = name,
      cell = function(value, index) table_data[[html_column]][index],
      html = TRUE, align = "center", width = width, sortNALast = TRUE,
      footer = blank_footer
    )
  }

  # A score cell; with click_input it is a focusable button-like span so the
  # click-through is reachable from the keyboard as well as the mouse (the
  # app's page script turns Enter or Space on it into a click). Cells return
  # tags, never HTML strings: an html = TRUE column would make reactable
  # inject the header as a string too, breaking its two-line layout.
  score_cell = function(value) {
    text = format_score(value)
    if (is.null(click_input) || !nzchar(text)) {
      return(text)
    }
    htmltools::span(
      tabindex = "0", role = "button", class = "score-link",
      title = "Open this question for this repository in the Grade tab",
      text
    )
  }

  score_column = function(column, name, sub, max_points = NA_real_, bold = FALSE, class = NULL,
                          summary_column = column, cell = function(value, index) score_cell(value)) {
    reactable::colDef(
      header = function(value) column_header(name, sub, name %in% optional),
      cell = cell,
      style = function(value) {
        c(if (bold) list(fontWeight = "bold"), score_tint(value, max_points))
      },
      class = class,
      footer = footer_lines(format_score(summary_rows[[summary_column]])),
      sortNALast = TRUE,
      minWidth = 84,
      maxWidth = 120
    )
  }

  # The Total column: the complete total, or the running sum so far in gray
  # with how many questions it covers, so an incomplete repository is never
  # mistaken for a low one
  total_cell = function(value, index) {
    if (!is.na(value)) {
      return(format_score(value))
    }
    partial = table_data$Total_partial[index]
    if (is.na(partial)) {
      return("")
    }
    n = table_data$Progress[index]
    htmltools::span(
      class = "text-muted fw-normal",
      title = paste0("Total so far: ", n, " of ", length(question_names), " questions graded"),
      format_score(partial)
    )
  }

  columns = c(
    list(
      Repository = reactable::colDef(
        name = "Repository",
        align = "left",
        minWidth = 140,
        maxWidth = 400,
        sticky = "left",
        footer = if (has_scores) footer_lines(summary_rows$Repository, class = "text-muted")
      ),
      Folder = icon_column(),
      GitHub = icon_column(),
      Artifacts = icon_column(),
      Source = icon_column(),
      Validation = html_column("Validation_html", "Validation", 80),
      Progress = html_column("Progress_html", "Progress", 88),
      Validation_html = reactable::colDef(show = FALSE),
      Progress_html = reactable::colDef(show = FALSE)
    ),
    if (has_scores) {
      c(
        stats::setNames(
          lapply(question_names, function(question_name) {
            score_column(
              score_column_id(question_name), question_name, format_points(totals[[question_name]]),
              max_points = totals[[question_name]], class = link_class, summary_column = question_name
            )
          }),
          score_column_id(question_names)
        ),
        list(
          Total = score_column("Total", "Total", format_points(sum(totals)), bold = TRUE, cell = total_cell),
          Total_partial = reactable::colDef(show = FALSE)
        )
      )
    }
  )

  reactable::reactable(
    table_data,
    columns = columns,
    defaultColDef = reactable::colDef(align = "right", na = "", vAlign = "center"),
    onClick = if (has_scores && !is.null(click_input)) repo_table_click_js(click_input, question_names),
    fullWidth = TRUE,
    height = "100%",
    sortable = TRUE,
    highlight = TRUE,
    compact = TRUE,
    pagination = FALSE,
    language = reactable::reactableLang(noData = "No repositories match the current filters."),
    theme = reactable::reactableTheme(
      backgroundColor = "#fff",
      style = list(fontSize = "12px"),
      cellPadding = "3px 8px",
      headerStyle = list(fontWeight = "bold"),
      footerStyle = list(borderTop = "2px solid #dee2e6")
    )
  )
}

# Cell renderers
#
# Each returns the HTML string for one repository's cell in a table column.

# Validation summary cell: pass/fail count with a tooltip listing failures,
# or a warning marker when the repo's document is missing or failed to parse
#
# repo: Character. Repository name
# validation_results: List of per-repo validation results
# template_obj: markermd_template object, or NULL
# repo_errors: Named list of per-repo missing-document/parse error messages
# row_index: Integer. The repo's position in repo_list; when given, the
#   marker becomes a button that opens the repo's validation modal (the
#   validation_clicked input in mark-app.R). NULL renders the bare marker.
# n_warnings: Number of parser warnings for the repo's document, shown as an
#   amber mark after the count (the modal lists them)

validation_status_cell = function(repo, validation_results, template_obj, repo_errors = list(), row_index = NULL,
                                  n_warnings = 0) {
  as_cell = function(tag) {
    if (!is.null(row_index)) {
      tag = htmltools::tags$button(
        onclick = paste0("Shiny.setInputValue('validation_clicked', ", row_index, ", {priority: 'event'})"),
        class = icon_button_class,
        tag
      )
    }
    as.character(tag)
  }

  err = repo_errors[[repo]]
  if (!is.null(err)) {
    first_line = strsplit(err, "\n")[[1]][1]
    return(as_cell(htmltools::span(
      class = "text-danger",
      title = first_line,
      cached_icon("triangle-exclamation")
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

  warnings_mark = if (n_warnings > 0) {
    htmltools::span(
      class = "text-warning-emphasis ms-1",
      title = paste0(n_warnings, " parser warning", if (n_warnings == 1) "" else "s", " (open to see)"),
      cached_icon("triangle-exclamation")
    )
  }

  if (fail_count == 0) {
    # All passed
    as_cell(htmltools::span(
      htmltools::span(
        class = "text-success",
        title = tooltip_text,
        cached_icon("circle-check"),
        paste0(" ", pass_count, "/", total_count)
      ),
      warnings_mark
    ))
  } else {
    # Has failures (including errors)
    as_cell(htmltools::span(
      htmltools::span(
        class = "text-danger",
        title = tooltip_text,
        cached_icon("circle-xmark"),
        paste0(" ", pass_count, "/", total_count)
      ),
      warnings_mark
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
