# Score summaries for the marking app's Scores tab
#
# The data builders take the per-pair scores from collect_score_data() plus
# each question's maximum points and return plain data frames; the table
# builder turns those into one interactive (sortable, paged) reactable with
# the summary statistics pinned beneath the repository rows as
# column footers, so sorting never moves them. All are pure, so they are
# unit-tested without a browser; the Shiny wiring lives in mod_mark_scores.R.

# Maximum points per question from the settings table, defaulting like
# collect_score_data() to 10 when a question has no settings row
#
# collection_path: Project root holding the grading database
# question_names: Character vector of question names
# Returns: Named numeric vector keyed by question name

question_total_scores = function(collection_path, question_names) {
  settings = with_database(collection_path, load_all_settings)
  totals = vapply(question_names, function(question_name) {
    state = db_row_to_grade_state(settings[settings$question_name == question_name, , drop = FALSE])
    if (is.null(state)) 10 else state@total_score
  }, numeric(1))
  stats::setNames(totals, question_names)
}

# Wide per-repository score table: one row per repository, one column per
# question (NA for a pair that does not count as graded), the total over the
# graded questions (NA when none are) and how many questions are graded
#
# scores: Data frame from collect_score_data() (question_name,
#   assignment_repo, score)
# question_names: Character vector of question names, in template order
# repo_names: Character vector of repository names, in display order
# Returns: Data frame with columns Repository, one per question, Total, Graded

build_scores_table_data = function(scores, question_names, repo_names) {
  out = data.frame(Repository = repo_names, stringsAsFactors = FALSE, check.names = FALSE)
  for (question_name in question_names) {
    rows = scores[scores$question_name == question_name, , drop = FALSE]
    out[[question_name]] = rows$score[match(repo_names, rows$assignment_repo)]
  }
  by_question = as.matrix(out[, question_names, drop = FALSE])
  n_graded = rowSums(!is.na(by_question))
  out$Total = ifelse(n_graded > 0, rowSums(by_question, na.rm = TRUE), NA_real_)
  out$Graded = as.integer(n_graded)
  out
}

# Summary statistics of one vector of scores
#
# x: Numeric scores (NA = not graded)
# Returns: List with n (graded count), mean, median, sd (NA when there are
#   too few graded scores)

score_stats = function(x) {
  x = x[!is.na(x)]
  n = length(x)
  list(
    n = n,
    mean = if (n > 0) mean(x) else NA_real_,
    median = if (n > 0) stats::median(x) else NA_real_,
    sd = if (n > 1) stats::sd(x) else NA_real_
  )
}

# Summary rows for the score table, in the table's own column layout: how
# many repositories are graded, then the mean, median and standard deviation
# of their scores, per question. The Total column summarises only the
# repositories with every question graded (a partial total would drag the
# statistics down); the Graded column has no summary.
#
# table_data: Data frame from build_scores_table_data()
# question_names: Character vector of question names
# Returns: Data frame with the same columns as table_data and one row per
#   statistic, labelled in Repository ("Graded", "Mean", "Median", "SD")

build_scores_summary_rows = function(table_data, question_names) {
  stat_names = c("Graded", "Mean", "Median", "SD")
  column_stats = function(x) {
    s = score_stats(x)
    c(s$n, s$mean, s$median, s$sd)
  }
  complete = length(question_names) > 0 & table_data$Graded == length(question_names)

  rows = data.frame(Repository = stat_names, stringsAsFactors = FALSE, check.names = FALSE)
  for (question_name in question_names) {
    rows[[question_name]] = column_stats(table_data[[question_name]])
  }
  rows$Total = column_stats(table_data$Total[complete])
  rows$Graded = rep(NA_real_, length(stat_names))
  rows
}

# A score as text: at most two decimals, no trailing zeros ("10", "7.5"),
# and blank for NA
#
# x: Numeric

format_score = function(x) {
  ifelse(is.na(x), "", format(round(x, 2), trim = TRUE, drop0trailing = TRUE))
}

# Points as text with the unit: "1 pt", "10 pts", "7.5 pts"
#
# x: Numeric points

format_points = function(x) {
  # qty() goes last: cli pluralises on the most recent substitution, and the
  # formatted text before it would otherwise count as a length-one quantity
  as.character(cli::pluralize("{format_score(x)}{cli::qty(x)} pt{?s}"))
}

# Caption under the score table explaining what the summary rows cover and
# that score cells open the Rubric tab
#
# summary_rows: Data frame from build_scores_summary_rows()
# n_repos: Number of repositories in the project

scores_caption = function(summary_rows, n_repos) {
  n_complete = summary_rows$Total[summary_rows$Repository == "Graded"]
  cli::pluralize(
    "Summary rows use graded repositories only; the Total column uses the ",
    "{n_complete} of {n_repos} repositor{?y/ies} with every question graded. ",
    "Click a score to open that repository and question in the Rubric tab."
  )
}

# JavaScript click handler for the score table: a click on a question cell
# (or the repository name) reports the repo and question to the given Shiny
# input, as an event so repeated clicks on the same cell fire again. Other
# columns and the summary footer are ignored.
#
# input_id: Namespaced Shiny input id to set
# question_names: Character vector of the question column ids

scores_click_js = function(input_id, question_names) {
  reactable::JS(glue::glue("
    function(rowInfo, column) {
      var questions = <<jsonlite::toJSON(question_names)>>;
      var repo = rowInfo.values.Repository;
      if (column.id === 'Repository') {
        Shiny.setInputValue('<<input_id>>', {repo: repo, question: null}, {priority: 'event'});
      } else if (questions.indexOf(column.id) !== -1) {
        Shiny.setInputValue('<<input_id>>', {repo: repo, question: column.id}, {priority: 'event'});
      }
    }
  ", .open = "<<", .close = ">>"))
}

# Background tint for a score cell: none at full marks, deepening to a light
# red at zero, so only shortfalls draw the eye and a fully correct class reads
# as a plain table
#
# value: Numeric score (NA gives no tint)
# max_points: Maximum points for the column (NA or non-positive gives none)
# Returns: List of CSS properties for a reactable cell, or NULL

score_tint = function(value, max_points) {
  if (is.na(value) || is.na(max_points) || max_points <= 0) {
    return(NULL)
  }
  shortfall = 1 - min(max(value / max_points, 0), 1)
  if (shortfall == 0) {
    return(NULL)
  }
  list(background = paste0("rgba(220, 53, 69, ", round(0.22 * shortfall, 3), ")"))
}

# Grading progress cell: the count over a thin version of the Validation
# tab's green progress bar, stacked so the pair fits the column (the count
# overlaid on the bar, as that tab does, is illegible at this table's size,
# and side by side it clips)
#
# n_graded: Number of graded questions
# n_questions: Number of questions in the template

graded_progress_cell = function(n_graded, n_questions) {
  percentage = if (n_questions > 0) round(100 * n_graded / n_questions) else 0
  htmltools::div(
    class = "d-inline-flex flex-column align-items-center",
    style = "line-height: 1.2;",
    title = glue::glue("{n_graded} of {n_questions} questions graded"),
    htmltools::span(
      class = if (n_graded >= n_questions && n_questions > 0) "fw-semibold" else "text-muted",
      paste0(n_graded, "/", n_questions)
    ),
    htmltools::div(
      class = "progress mt-1",
      style = "height: 4px; width: 40px;",
      htmltools::div(
        class = "progress-bar bg-success",
        role = "progressbar",
        style = paste0("width: ", percentage, "%;"),
        `aria-valuenow` = n_graded,
        `aria-valuemin` = "0",
        `aria-valuemax` = n_questions
      )
    )
  )
}

# The interactive score table: one column per question plus the total and
# graded count, sortable (ungraded last) with paging for large classes, and
# the summary rows from build_scores_summary_rows() pinned as
# column footers so they stay put under any sort order. Columns are sized to
# their content rather than stretched across the card, headers carry the
# points on a muted second line, score cells are tinted by their shortfall
# (score_tint()), totals are bold and the graded count is a progress bar.
# Question cells and repository names are clickable (class "score-link",
# styled by the module UI) and report to click_input via scores_click_js().
#
# table_data: Data frame from build_scores_table_data()
# summary_rows: Data frame from build_scores_summary_rows()
# totals: Named numeric vector of per-question maximum points
# click_input: Namespaced Shiny input id that clicks report to, or NULL for
#   a table with no click-through

scores_reactable = function(table_data, summary_rows, totals, click_input = NULL) {
  question_names = names(totals)

  # Column name on one line, the points (or count) muted beneath
  column_header = function(name, sub) {
    htmltools::tagList(
      htmltools::div(name),
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
        if (nzchar(value)) value else " "
      )
    }))
  }

  link_class = if (is.null(click_input)) NULL else "score-link"

  score_column = function(column, name, sub, max_points = NA_real_, bold = FALSE, class = NULL) {
    reactable::colDef(
      header = function(value) column_header(name, sub),
      cell = function(value) format_score(value),
      style = function(value) {
        c(if (bold) list(fontWeight = "bold"), score_tint(value, max_points))
      },
      class = class,
      footer = footer_lines(format_score(summary_rows[[column]])),
      sortNALast = TRUE,
      minWidth = 84
    )
  }

  columns = c(
    list(
      Repository = reactable::colDef(
        name = "Repository",
        align = "left",
        minWidth = 140,
        sticky = "left",
        class = link_class,
        footer = footer_lines(summary_rows$Repository, class = "text-muted")
      )
    ),
    stats::setNames(
      lapply(question_names, function(question_name) {
        score_column(
          question_name, question_name, format_points(totals[[question_name]]),
          max_points = totals[[question_name]], class = link_class
        )
      }),
      question_names
    ),
    list(
      Total = score_column("Total", "Total", format_points(sum(totals)), bold = TRUE),
      Graded = reactable::colDef(
        header = function(value) column_header("Graded", paste("of", length(question_names))),
        cell = function(value) graded_progress_cell(value, length(question_names)),
        align = "center",
        footer = footer_lines(rep("", nrow(summary_rows))),
        minWidth = 84
      )
    )
  )

  reactable::reactable(
    table_data,
    columns = columns,
    defaultColDef = reactable::colDef(align = "right", na = ""),
    onClick = if (is.null(click_input)) NULL else scores_click_js(click_input, question_names),
    fullWidth = FALSE,
    sortable = TRUE,
    highlight = TRUE,
    compact = TRUE,
    pagination = TRUE,
    defaultPageSize = 25,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100),
    theme = reactable::reactableTheme(
      style = list(fontSize = "12px"),
      cellPadding = "3px 10px",
      headerStyle = list(fontWeight = "bold"),
      footerStyle = list(borderTop = "2px solid #dee2e6")
    )
  )
}
