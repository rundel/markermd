# Scoring and score data for the marking app
#
# score_question() is the one implementation of a question's score, shared by
# the live score in the Grade tab (mark_rubric_server()) and the database
# recomputation behind the Assignments table and export_scores()
# (collect_score_data()). The builders below it take the per-pair scores from
# collect_score_data() plus each question's maximum points and return plain
# data frames: the wide per-repository score table and the class summary
# rows, which utils_mark_table.R folds into the Assignments table and pins
# beneath it as column footers. All are pure, so they are unit-tested without
# a browser.

# Score one question from its rubric items, the selected item ids, its item
# groups and its scoring setup:
#
#   group_sum(g)     = sum of the points of the selected items in g
#   group_applied(g) = group_sum(g) clamped to [floor(g), ceiling(g)]
#   raw              = base + selected ungrouped points + sum(group_applied)
#   score            = raw clamped by bound_above_zero / bound_below_max
#   base             = 0 (positive mode) or total_score (negative mode)
#
# The group bounds apply first, then the question bounds. An item whose group
# id is not in groups counts as ungrouped. without_groups is the score the
# same selections would give with the group bounds ignored (question bounds
# still applied), so score - without_groups is what the bounds changed.
#
# items: Named list of markermd_rubric_item keyed by item id
# selected_ids: Character vector of the selected item ids
# groups: Named list of markermd_rubric_group keyed by group id
# state: markermd_grade_state with the question's scoring setup
# Returns: List with score, raw, without_groups, base, ungrouped, and groups
#   (a data frame with one row per group: group_id, label, sum, applied,
#   floor, ceiling)

score_question = function(items, selected_ids, groups, state) {
  points = vapply(items, function(item) item@points, numeric(1))
  group_of = vapply(items, function(item) item@group, character(1))
  selected = names(items) %in% selected_ids
  grouped = !is.na(group_of) & group_of %in% names(groups)

  base = if (state@grading_mode == "positive") 0 else state@total_score
  ungrouped = sum(points[selected & !grouped])

  group_ids = as.character(names(groups))
  group_rows = data.frame(
    group_id = group_ids,
    label = vapply(groups, function(g) g@label, character(1), USE.NAMES = FALSE),
    sum = vapply(group_ids, function(gid) {
      sum(points[selected & grouped & group_of == gid])
    }, numeric(1), USE.NAMES = FALSE),
    floor = vapply(groups, function(g) g@floor, numeric(1), USE.NAMES = FALSE),
    ceiling = vapply(groups, function(g) g@ceiling, numeric(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  group_rows$applied = pmin(
    pmax(group_rows$sum, ifelse(is.na(group_rows$floor), -Inf, group_rows$floor)),
    ifelse(is.na(group_rows$ceiling), Inf, group_rows$ceiling)
  )
  group_rows = group_rows[, c("group_id", "label", "sum", "applied", "floor", "ceiling")]

  clamp = function(x) {
    if (state@bound_above_zero && x < 0) x = 0
    if (state@bound_below_max && x > state@total_score) x = state@total_score
    x
  }

  raw = base + ungrouped + sum(group_rows$applied)
  list(
    score = clamp(raw),
    raw = raw,
    without_groups = clamp(base + ungrouped + sum(group_rows$sum)),
    base = base,
    ungrouped = ungrouped,
    groups = group_rows
  )
}

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

# Names of the optional questions (see markermd_grade_state) among
# question_names, in their order, from the settings table
#
# collection_path: Project root holding the grading database
# question_names: Character vector of question names

optional_question_names = function(collection_path, question_names) {
  settings = with_database(collection_path, load_all_settings)
  flagged = settings$question_name[is_true_flag(settings$optional)]
  question_names[question_names %in% flagged]
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

# Points as signed text: "+3 pts", "-2 pts", "0 pts" (a group subtotal)
#
# x: Numeric points

format_points_signed = function(x) {
  paste0(if (x > 0) "+" else "", format_points(x))
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
