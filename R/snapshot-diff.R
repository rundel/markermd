# Comparing snapshots: what changed in the rubric, the marks and the scores
# between two snapshots, or between a snapshot and the live state. This is
# the before/after table that was built by hand at every step of a long
# grading process. The diff works on the plain exchange documents a snapshot
# stores (see snapshots.R), so it needs no S7 round trip and tolerates
# anything the database allows.
#
# Rubric items and groups are matched across the two sides by their database
# id first (a snapshot carries them), so a reworded item or a relabeled group
# is a rename rather than a delete plus an add. Items that lost their id (a
# replace import mints new ones) are matched as a likely rename when they
# carry the same points and were selected by the same repositories on both
# sides. Selections are compared by item id as well when both sides' marks
# carry them, with descriptions only for display, so a deleted item whose
# description is reused is still reported; the rename map is the fallback
# for marks without ids (snapshots written before the ids were recorded).

# --- Rubric ------------------------------------------------------------------

# One plain rubric question's items as a data frame: id (NA when the entry
# has none), description, points, group (label or NA) and group_id (NA when
# ungrouped or the group entry has no id)
#
# question: One entry of a plain rubric list's questions

rubric_item_table = function(question) {
  rows = list()
  add = function(entry, group, group_id) {
    rows[[length(rows) + 1]] <<- data.frame(
      id = if (is.null(entry$id)) NA_character_ else as.character(entry$id),
      description = as.character(entry$description),
      points = as.numeric(entry$points),
      group = group,
      group_id = group_id,
      stringsAsFactors = FALSE
    )
  }
  for (entry in question$items) {
    if (is_rubric_group_entry(entry)) {
      group_id = if (is.null(entry$id)) NA_character_ else as.character(entry$id)
      for (member in entry$items) {
        add(member, as.character(entry$group), group_id)
      }
    } else {
      add(entry, NA_character_, NA_character_)
    }
  }
  if (length(rows) == 0) {
    return(data.frame(
      id = character(0), description = character(0), points = numeric(0), group = character(0),
      group_id = character(0), stringsAsFactors = FALSE
    ))
  }
  out = do.call(rbind, rows)
  rownames(out) = NULL
  out
}

# One plain rubric question's groups as a data frame: id (NA when the entry
# has none), label, floor, ceiling, exclusive
#
# question: One entry of a plain rubric list's questions

rubric_group_table = function(question) {
  groups = Filter(is_rubric_group_entry, question$items)
  number_or_na = function(value) if (is.null(value)) NA_real_ else as.numeric(value)
  data.frame(
    id = vapply(groups, function(g) if (is.null(g$id)) NA_character_ else as.character(g$id), character(1)),
    label = vapply(groups, function(g) as.character(g$group), character(1)),
    floor = vapply(groups, function(g) number_or_na(g$floor), numeric(1)),
    ceiling = vapply(groups, function(g) number_or_na(g$ceiling), numeric(1)),
    exclusive = vapply(groups, function(g) isTRUE(g$exclusive), logical(1)),
    stringsAsFactors = FALSE
  )
}

# Match the rows of two tables by a key column where both sides have one,
# else by a fallback column (a label or description); each row is matched at
# most once. Returns an integer vector parallel to a's rows holding the
# matched row of b, NA when unmatched.
#
# a, b: Data frames
# key: Name of the id column (NA for rows without one)
# fallback: Name of the column to match rows without ids on

match_rows = function(a, b, key, fallback) {
  matched = rep(NA_integer_, nrow(a))
  for (i in seq_len(nrow(a))) {
    if (!is.na(a[[key]][i])) {
      j = which(!is.na(b[[key]]) & b[[key]] == a[[key]][i])
      if (length(j) == 1) matched[i] = j
    }
  }
  for (i in which(is.na(matched))) {
    free = setdiff(seq_len(nrow(b)), matched)
    j = free[b[[fallback]][free] == a[[fallback]][i]]
    if (length(j) >= 1) matched[i] = j[1]
  }
  matched
}

# A plain scoring list as a named character vector of its settings, with
# the defaults filled in, for field-by-field comparison
#
# scoring: A question's scoring list, or NULL

scoring_fields = function(scoring) {
  if (is.null(scoring)) {
    return(c(scoring = "none"))
  }
  value = function(field, default) {
    v = scoring[[field]]
    as.character(if (is.null(v)) default else v)
  }
  c(
    total_score = value("total_score", NA),
    grading_mode = value("grading_mode", "positive"),
    bound_above_zero = value("bound_above_zero", TRUE),
    bound_below_max = value("bound_below_max", TRUE),
    optional = value("optional", FALSE)
  )
}

# Which repositories select each item in a plain marks list, per question:
# list(by_id, by_description), each a named list keyed by
# "question\x1f<id or description>" holding the sorted repository names.
# by_id is filled only from marks that carry item_ids.
#
# marks: Plain marks list

selectors_by_item = function(marks) {
  by_id = list()
  by_description = list()
  for (repo in marks$repos) {
    for (q in repo$questions) {
      for (item in as.character(unlist(q$items))) {
        key = paste(q$name, item, sep = "\x1f")
        by_description[[key]] = c(by_description[[key]], repo$name)
      }
      for (id in as.character(unlist(q$item_ids))) {
        key = paste(q$name, id, sep = "\x1f")
        by_id[[key]] = c(by_id[[key]], repo$name)
      }
    }
  }
  list(by_id = lapply(by_id, sort), by_description = lapply(by_description, sort))
}

# The repositories selecting one item: by its id when the marks carry ids,
# else by its description
#
# selectors: A selectors_by_item() result
# question: Question name
# id: The item's id, or NA
# description: The item's description

item_selectors = function(selectors, question, id, description) {
  if (!is.na(id) && length(selectors$by_id) > 0) {
    return(selectors$by_id[[paste(question, id, sep = "\x1f")]])
  }
  selectors$by_description[[paste(question, description, sep = "\x1f")]]
}

# Item descriptions by id, per question, from two plain rubric lists: a
# named list keyed by question holding a named character vector keyed by
# item id. The first rubric's descriptions win, so pass the "to" side first
# and the "from" side supplies the deleted items.
#
# ...: Plain rubric lists (NULL entries are skipped)

rubric_item_labels = function(...) {
  labels = list()
  for (rubric in list(...)) {
    for (question in rubric$questions) {
      name = as.character(question$name)
      table = rubric_item_table(question)
      table = table[!is.na(table$id), , drop = FALSE]
      known = labels[[name]]
      new = table[!table$id %in% names(known), , drop = FALSE]
      labels[[name]] = c(known, stats::setNames(new$description, new$id))
    }
  }
  labels
}

# A one-row change record for the rubric diff
#
# question, change, item, detail: The record's fields (item is the item's
#   description on the "to" side, or the group label)

rubric_change = function(question, change, item = NA_character_, detail = "") {
  data.frame(question = question, change = change, item = item, detail = detail, stringsAsFactors = FALSE)
}

# Diff two plain rubric lists. Returns list(changes, renames): changes is a
# data frame with question, change, item and detail, and renames maps each
# question's old descriptions to the new ones for items matched across the
# two sides (a named list keyed by question holding a named character vector).
#
# from, to: Plain rubric lists
# from_marks, to_marks: The corresponding plain marks lists, for matching
#   re-minted items by their selecting repositories

diff_rubric = function(from, to, from_marks = NULL, to_marks = NULL) {
  from_q = stats::setNames(from$questions, vapply(from$questions, function(q) as.character(q$name), character(1)))
  to_q = stats::setNames(to$questions, vapply(to$questions, function(q) as.character(q$name), character(1)))
  from_sel = if (is.null(from_marks)) list() else selectors_by_item(from_marks)
  to_sel = if (is.null(to_marks)) list() else selectors_by_item(to_marks)

  changes = list()
  renames = list()
  id_renames = list()
  push = function(...) changes[[length(changes) + 1]] <<- rubric_change(...)

  for (name in setdiff(names(from_q), names(to_q))) {
    push(name, "question removed", detail = paste0(nrow(rubric_item_table(from_q[[name]])), " items"))
  }
  for (name in setdiff(names(to_q), names(from_q))) {
    push(name, "question added", detail = paste0(nrow(rubric_item_table(to_q[[name]])), " items"))
  }

  for (name in intersect(names(from_q), names(to_q))) {
    a = rubric_item_table(from_q[[name]])
    b = rubric_item_table(to_q[[name]])

    # Match by id, then by points plus identical selecting repositories
    matched_a = rep(NA_integer_, nrow(a))
    for (i in seq_len(nrow(a))) {
      if (!is.na(a$id[i])) {
        j = which(!is.na(b$id) & b$id == a$id[i])
        if (length(j) == 1) matched_a[i] = j
      }
    }
    for (i in which(is.na(matched_a))) {
      free = setdiff(seq_len(nrow(b)), matched_a)
      sel_a = item_selectors(from_sel, name, a$id[i], a$description[i])
      candidates = free[b$points[free] == a$points[i]]
      same_desc = candidates[b$description[candidates] == a$description[i]]
      if (length(same_desc) >= 1) {
        matched_a[i] = same_desc[1]
        next
      }
      if (length(sel_a) == 0) next
      likely = candidates[vapply(candidates, function(j) {
        identical(item_selectors(to_sel, name, b$id[j], b$description[j]), sel_a)
      }, logical(1))]
      if (length(likely) == 1) matched_a[i] = likely
    }

    # Groups are matched by id, then by label, so a relabeled group is one
    # rename rather than a removal, an addition and a move per member
    ga = rubric_group_table(from_q[[name]])
    gb = rubric_group_table(to_q[[name]])
    matched_g = match_rows(ga, gb, "id", "label")
    group_label_map = stats::setNames(gb$label[matched_g[!is.na(matched_g)]], ga$label[!is.na(matched_g)])
    mapped_group = function(label) {
      if (!is.na(label) && label %in% names(group_label_map)) group_label_map[[label]] else label
    }

    map = character(0)
    id_map = character(0)
    for (i in seq_len(nrow(a))) {
      j = matched_a[i]
      if (is.na(j)) {
        push(name, "item removed", a$description[i], format_points(a$points[i]))
        next
      }
      if (!is.na(a$id[i]) && !is.na(b$id[j]) && a$id[i] != b$id[j]) {
        id_map[a$id[i]] = b$id[j]
      }
      if (a$description[i] != b$description[j]) {
        map[a$description[i]] = b$description[j]
        how = if (!is.na(a$id[i]) && identical(a$id[i], b$id[j])) "item reworded" else "item likely reworded"
        push(name, how, b$description[j], paste0("was: ", a$description[i]))
      }
      if (a$points[i] != b$points[j]) {
        push(name, "item points", b$description[j], paste0(format_points(a$points[i]), " to ", format_points(b$points[j])))
      }
      if (!identical(mapped_group(a$group[i]), b$group[j])) {
        push(name, "item group", b$description[j], paste0(
          if (is.na(a$group[i])) "ungrouped" else mapped_group(a$group[i]), " to ",
          if (is.na(b$group[j])) "ungrouped" else b$group[j]
        ))
      }
    }
    for (j in setdiff(seq_len(nrow(b)), matched_a)) {
      push(name, "item added", b$description[j], format_points(b$points[j]))
    }
    if (length(map) > 0) {
      renames[[name]] = map
    }
    if (length(id_map) > 0) {
      id_renames[[name]] = id_map
    }

    for (g in which(is.na(matched_g))) push(name, "group removed", ga$label[g])
    for (g in setdiff(seq_len(nrow(gb)), matched_g)) push(name, "group added", gb$label[g])
    for (g in which(!is.na(matched_g))) {
      ra = ga[g, ]
      rb = gb[matched_g[g], ]
      if (ra$label != rb$label) {
        push(name, "group renamed", rb$label, paste0("was: ", ra$label))
      }
      bound = function(x) if (is.na(x)) "none" else format_score(x)
      parts = c(
        if (!identical(ra$floor, rb$floor)) paste0("floor ", bound(ra$floor), " to ", bound(rb$floor)),
        if (!identical(ra$ceiling, rb$ceiling)) paste0("ceiling ", bound(ra$ceiling), " to ", bound(rb$ceiling)),
        if (ra$exclusive != rb$exclusive) paste0("exclusive ", ra$exclusive, " to ", rb$exclusive)
      )
      if (length(parts) > 0) push(name, "group bounds", rb$label, paste(parts, collapse = "; "))
    }

    sa = scoring_fields(from_q[[name]]$scoring)
    sb = scoring_fields(to_q[[name]]$scoring)
    fields = union(names(sa), names(sb))
    diffs = fields[is.na(sa[fields]) != is.na(sb[fields]) | (!is.na(sa[fields]) & !is.na(sb[fields]) & sa[fields] != sb[fields])]
    if (length(diffs) > 0) {
      detail = paste(vapply(diffs, function(f) {
        paste0(f, " ", if (is.na(sa[f])) "unset" else sa[[f]], " to ", if (is.na(sb[f])) "unset" else sb[[f]])
      }, character(1)), collapse = "; ")
      push(name, "scoring", detail = detail)
    }
  }

  changes = if (length(changes) == 0) rubric_change(character(0), character(0), character(0), character(0))[0, ] else do.call(rbind, changes)
  rownames(changes) = NULL
  list(changes = changes, renames = renames, id_renames = id_renames)
}

# --- Marks -------------------------------------------------------------------

# Plain marks list as a lookup keyed by "repo\x1fquestion". Each entry
# carries the selected item descriptions (items) and, when the marks record
# them, the selected item ids (ids; NULL otherwise).
#
# marks: Plain marks list

marks_pairs = function(marks) {
  out = list()
  for (repo in marks$repos) {
    for (q in repo$questions) {
      out[[paste(repo$name, q$name, sep = "\x1f")]] = list(
        repo = as.character(repo$name), question = as.character(q$name),
        items = as.character(unlist(q$items)),
        ids = if (is.null(q$item_ids)) NULL else as.character(unlist(q$item_ids)),
        comment = if (is.null(q$comment)) "" else as.character(q$comment),
        private_comment = if (is.null(q$private_comment)) "" else as.character(q$private_comment)
      )
    }
  }
  out
}

# Whether a marks list records item ids on every pair it holds (a snapshot
# written since the ids were recorded); an empty marks list does, since it
# has nothing to match by description
#
# marks: Plain marks list

marks_have_ids = function(marks) {
  all(vapply(marks$repos, function(repo) {
    all(vapply(repo$questions, function(q) !is.null(q$item_ids), logical(1)))
  }, logical(1)))
}

# Diff two plain marks lists. When both sides record item ids, selections
# are compared by id and descriptions serve only for display (looked up in
# labels, else taken from the marks themselves), so a deleted item whose
# description another item took over is still a removed selection.
# Otherwise old item descriptions are mapped through the rubric diff's
# renames first, so a reworded item is not a selection change.
#
# from, to: Plain marks lists
# renames: The renames element of diff_rubric()'s result
# labels: rubric_item_labels() of the two sides' rubrics, for display
# id_renames: The id_renames element of diff_rubric()'s result: old item id
#   to new for items the rubric diff matched across a re-mint (a replace
#   import), so their selections are compared under the new id
# Returns: Data frame with repo, question, change ("selection", "comment",
#   "private_comment"), before and after (for a selection, the removed and
#   added item descriptions joined with "; ")

diff_marks = function(from, to, renames = list(), labels = list(), id_renames = list()) {
  a = marks_pairs(from)
  b = marks_pairs(to)
  by_id = marks_have_ids(from) && marks_have_ids(to)
  rows = list()
  push = function(repo, question, change, before, after) {
    rows[[length(rows) + 1]] <<- data.frame(
      repo = repo, question = question, change = change, before = before, after = after,
      stringsAsFactors = FALSE
    )
  }
  empty = list(items = character(0), ids = character(0), comment = "", private_comment = "")
  describe = function(ids, pair, question) {
    known = labels[[question]]
    vapply(ids, function(id) {
      if (!is.null(known) && id %in% names(known)) return(known[[id]])
      at = match(id, pair$ids)
      if (!is.na(at) && at <= length(pair$items)) pair$items[at] else id
    }, character(1), USE.NAMES = FALSE)
  }

  for (key in union(names(a), names(b))) {
    pa = a[[key]]
    pb = b[[key]]
    parts = strsplit(key, "\x1f", fixed = TRUE)[[1]]
    repo = parts[1]
    question = parts[2]
    if (is.null(pa)) pa = empty
    if (is.null(pb)) pb = empty

    if (by_id) {
      ids_a = pa$ids
      id_map = id_renames[[question]]
      if (!is.null(id_map)) {
        hit = ids_a %in% names(id_map)
        ids_a[hit] = id_map[ids_a[hit]]
      }
      removed = describe(pa$ids[!ids_a %in% pb$ids], pa, question)
      added = describe(setdiff(pb$ids, ids_a), pb, question)
    } else {
      items_a = pa$items
      map = renames[[question]]
      if (!is.null(map)) {
        hit = items_a %in% names(map)
        items_a[hit] = map[items_a[hit]]
      }
      removed = setdiff(items_a, pb$items)
      added = setdiff(pb$items, items_a)
    }
    if (length(removed) > 0 || length(added) > 0) {
      push(repo, question, "selection", paste(removed, collapse = "; "), paste(added, collapse = "; "))
    }
    if (!identical(pa$comment, pb$comment)) {
      push(repo, question, "comment", pa$comment, pb$comment)
    }
    if (!identical(pa$private_comment, pb$private_comment)) {
      push(repo, question, "private_comment", pa$private_comment, pb$private_comment)
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      repo = character(0), question = character(0), change = character(0),
      before = character(0), after = character(0), stringsAsFactors = FALSE
    ))
  }
  out = do.call(rbind, rows)
  out[order(out$repo, out$question, out$change), , drop = FALSE]
}

# --- Scores ------------------------------------------------------------------

# Diff two snapshot_stats data frames (assignment_repo, question_name,
# score, with "(total)" rows). Returns list(scores, totals): the pairs whose
# score changed and every repository's total before and after.
#
# from, to: Data frames in the snapshot_stats layout

diff_scores = function(from, to) {
  key = function(df) paste(df$assignment_repo, df$question_name, sep = "\x1f")
  keys = union(key(from), key(to))
  parts = strsplit(keys, "\x1f", fixed = TRUE)
  out = data.frame(
    repo = vapply(parts, `[`, character(1), 1),
    question = vapply(parts, `[`, character(1), 2),
    before = from$score[match(keys, key(from))],
    after = to$score[match(keys, key(to))],
    stringsAsFactors = FALSE
  )
  out$change = out$after - out$before
  changed = (is.na(out$before) != is.na(out$after)) |
    (!is.na(out$before) & !is.na(out$after) & abs(out$before - out$after) > 1e-8)

  totals = out[out$question == "(total)", c("repo", "before", "after", "change"), drop = FALSE]
  totals = totals[order(totals$repo), , drop = FALSE]
  rownames(totals) = NULL

  scores = out[changed & out$question != "(total)", , drop = FALSE]
  scores = scores[order(scores$repo, scores$question), , drop = FALSE]
  rownames(scores) = NULL
  list(scores = scores, totals = totals)
}

# --- Template ---------------------------------------------------------------

# Diff two plain template lists at the question level: questions added,
# removed and changed (renamed by id, nodes, rules, filters or scope)
#
# from, to: Plain template lists (NULL for no template)
# Returns: Data frame with question, change and detail

diff_template = function(from, to) {
  rows = list()
  push = function(question, change, detail = "") {
    rows[[length(rows) + 1]] <<- data.frame(question = question, change = change, detail = detail, stringsAsFactors = FALSE)
  }
  if (is.null(from) && !is.null(to)) push("(template)", "template added")
  if (!is.null(from) && is.null(to)) push("(template)", "template removed")
  if (!is.null(from) && !is.null(to)) {
    files_a = as.character(unlist(from$files))
    files_b = as.character(unlist(to$files))
    for (path in setdiff(files_b, files_a)) push("(template)", "file added", path)
    for (path in setdiff(files_a, files_b)) push("(template)", "file removed", path)
    by_id = function(x) stats::setNames(x$questions, vapply(x$questions, function(q) as.character(q$id), character(1)))
    a = by_id(from)
    b = by_id(to)
    for (id in setdiff(names(a), names(b))) push(a[[id]]$name, "question removed")
    for (id in setdiff(names(b), names(a))) push(b[[id]]$name, "question added")
    for (id in intersect(names(a), names(b))) {
      qa = a[[id]]
      qb = b[[id]]
      if (!identical(qa$name, qb$name)) push(qb$name, "question renamed", paste0("was: ", qa$name))
      same = function(field) identical(canonical_json(qa[[field]]), canonical_json(qb[[field]]))
      changed = c(
        if (!same("node_ids")) "nodes",
        if (!same("rules")) "rules",
        if (!same("filters")) "filters",
        if (!same("scope")) "scope",
        if (!same("file")) "file"
      )
      if (length(changed) > 0) push(qb$name, "question changed", paste(changed, collapse = ", "))
    }
  }
  if (length(rows) == 0) {
    return(data.frame(question = character(0), change = character(0), detail = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

# --- The diff object ----------------------------------------------------------

#' @title Differences between two snapshots
#' @description S7 class returned by [snapshot_diff()]: the rubric, marks,
#'   score and template changes between two snapshots, or between a snapshot
#'   and the live state. Its print method groups the changes by question.
#' @param from Character. The "from" side's snapshot id.
#' @param to Character. The "to" side's snapshot id, or `"current"`.
#' @param from_label Character. A short description of the "from" side.
#' @param to_label Character. A short description of the "to" side.
#' @param rubric Data frame of rubric changes (`question`, `change`, `item`,
#'   `detail`): items added, removed, reworded, re-pointed or regrouped, group
#'   and scoring changes, questions added or removed.
#' @param marks Data frame of marks changes (`repo`, `question`, `change`,
#'   `before`, `after`): pairs whose selections, public comment or private
#'   note changed.
#' @param scores Data frame of score changes per pair (`repo`, `question`,
#'   `before`, `after`, `change`).
#' @param totals Data frame of every repository's total (`repo`, `before`,
#'   `after`, `change`).
#' @param template Data frame of template changes (`question`, `change`, `detail`).
#' @return A `markermd_snapshot_diff` S7 object.
#' @seealso [snapshot_diff()]
#' @export
markermd_snapshot_diff = S7::new_class(
  "markermd_snapshot_diff",
  properties = list(
    from = S7::class_character,
    to = S7::class_character,
    from_label = S7::class_character,
    to_label = S7::class_character,
    rubric = S7::class_data.frame,
    marks = S7::class_data.frame,
    scores = S7::class_data.frame,
    totals = S7::class_data.frame,
    template = S7::class_data.frame
  ),
  package = "markermd"
)

S7::method(print, markermd_snapshot_diff) = function(x, ...) {
  cli::cli_h1("Changes from {x@from_label} to {x@to_label}")

  if (nrow(x@template) > 0) {
    cli::cli_h2("Template")
    cli::cli_ul()
    for (i in seq_len(nrow(x@template))) {
      detail = if (nzchar(x@template$detail[i])) paste0(" (", x@template$detail[i], ")") else ""
      cli::cli_li("{x@template$question[i]}: {x@template$change[i]}{detail}")
    }
    cli::cli_end()
  }

  cli::cli_h2("Rubric")
  if (nrow(x@rubric) == 0) {
    cli::cli_text("No rubric changes.")
  } else {
    for (question in unique(x@rubric$question)) {
      rows = x@rubric[x@rubric$question == question, , drop = FALSE]
      cli::cli_text("{.strong {question}}")
      cli::cli_ul()
      # Descriptions and details are user text, so they are passed to cli as
      # values and never pasted into the template string
      for (i in seq_len(nrow(rows))) {
        change = rows$change[i]
        item = rows$item[i]
        detail = rows$detail[i]
        if (is.na(item) && !nzchar(detail)) {
          cli::cli_li("{change}")
        } else if (is.na(item)) {
          cli::cli_li("{change}: {detail}")
        } else if (!nzchar(detail)) {
          cli::cli_li("{change} {.val {item}}")
        } else {
          cli::cli_li("{change} {.val {item}}: {detail}")
        }
      }
      cli::cli_end()
    }
  }

  cli::cli_h2("Marks")
  if (nrow(x@marks) == 0) {
    cli::cli_text("No marks changes.")
  } else {
    selection = x@marks[x@marks$change == "selection", , drop = FALSE]
    comments = x@marks[x@marks$change == "comment", , drop = FALSE]
    private = x@marks[x@marks$change == "private_comment", , drop = FALSE]
    if (nrow(selection) > 0) {
      cli::cli_text("{nrow(selection)} pair{?s} with changed selections")
      cli::cli_ul()
      for (i in seq_len(nrow(selection))) {
        removed = if (nzchar(selection$before[i])) paste0("removed: ", selection$before[i]) else NULL
        added = if (nzchar(selection$after[i])) paste0("added: ", selection$after[i]) else NULL
        cli::cli_li("{selection$repo[i]} / {selection$question[i]}: {paste(c(removed, added), collapse = '; ')}")
      }
      cli::cli_end()
    }
    if (nrow(comments) > 0) {
      cli::cli_text("{nrow(comments)} public comment{?s} changed")
      cli::cli_ul()
      for (i in seq_len(nrow(comments))) {
        cli::cli_li("{comments$repo[i]} / {comments$question[i]}: {.val {comments$before[i]}} to {.val {comments$after[i]}}")
      }
      cli::cli_end()
    }
    if (nrow(private) > 0) {
      cli::cli_text("{nrow(private)} private note{?s} changed (see {.code $marks})")
    }
  }

  cli::cli_h2("Scores")
  if (nrow(x@scores) == 0) {
    cli::cli_text("No score changes.")
  } else {
    cli::cli_text("{nrow(x@scores)} pair{?s} changed score")
    changed_totals = x@totals[
      (is.na(x@totals$before) != is.na(x@totals$after)) |
        (!is.na(x@totals$change) & abs(x@totals$change) > 1e-8), , drop = FALSE
    ]
    if (nrow(changed_totals) > 0) {
      df = data.frame(
        repo = changed_totals$repo,
        before = format_score(changed_totals$before),
        after = format_score(changed_totals$after),
        change = ifelse(is.na(changed_totals$change), "", ifelse(changed_totals$change > 0, paste0("+", format_score(changed_totals$change)), format_score(changed_totals$change))),
        stringsAsFactors = FALSE
      )
      print.data.frame(df, row.names = FALSE, right = FALSE)
    }
  }
  invisible(x)
}

#' Compare two snapshots, or a snapshot with the live state
#'
#' Reports what changed between two snapshots (or between a snapshot and the
#' project's current grading state): rubric items added, removed, reworded,
#' re-pointed or regrouped, groups added, removed, renamed or re-bounded,
#' scoring changes; per-repository selection, public comment and private
#' note changes; per-pair score
#' deltas and every repository's total before and after; and template
#' questions added, removed, renamed or changed.
#'
#' @details
#' Rubric items and groups are matched across the two sides by the database
#' ids a snapshot records, so an item reworded or a group relabeled in
#' [mark()] is reported as a rename. A replace import mints new ids; items it
#' re-created are matched as a likely rename when they keep their points and
#' are selected by the same repositories on both sides. Selections are
#' compared by item id as well, so a reworded item is not a selection change
#' while a deleted item whose description another item took over still is.
#'
#' The comparison is only meaningful when the snapshot's part is present on
#' both sides; a `part` left out is reported as unchanged.
#'
#' @param from A snapshot reference (see [snapshot_info()]).
#' @param to A snapshot reference, or `"current"` (the default) for the live state.
#' @param project Path to the project directory. Defaults to the working directory.
#' @param part Which parts to compare: any of `"rubric"`, `"marks"`,
#'   `"template"`. Scores are always compared.
#'
#' @return A [markermd_snapshot_diff] object. Its `rubric`, `marks`,
#'   `scores`, `totals` and `template` properties are data frames.
#' @seealso [snapshot_scores()], [snapshot_restore()]
#' @export
#'
#' @examples
#' \dontrun{
#' # What did everything since the latest snapshot change?
#' snapshot_diff("latest", project = "hw01")
#'
#' # What did the last import change?
#' snapshot_diff(-1, "latest", project = "hw01")
#' }
snapshot_diff = function(from, to = "current", project = ".", part = c("rubric", "marks", "template")) {
  proj = project_config(project)
  part = match.arg(part, several.ok = TRUE)

  with_database(proj@root, function(conn) {
    side = function(ref) {
      if (identical(ref, "current")) {
        return(list(
          id = "current", label = "current state",
          parts = snapshot_payload(conn),
          stats = current_snapshot_stats(conn, proj@root)
        ))
      }
      snapshot_id = resolve_snapshot_id(conn, ref)
      row = snapshot_rows(conn, "s.snapshot_id = ?", list(snapshot_id))
      label = if (!is.na(row$label)) paste0(snapshot_id, " (", row$label, ")") else snapshot_id
      list(
        id = snapshot_id, label = label,
        parts = read_snapshot_parts(conn, snapshot_id),
        stats = read_snapshot_stats(conn, snapshot_id)
      )
    }
    a = side(from)
    b = side(to)

    rubric = if ("rubric" %in% part) {
      diff_rubric(a$parts$rubric, b$parts$rubric, a$parts$marks, b$parts$marks)
    } else {
      list(changes = diff_rubric(list(questions = list()), list(questions = list()))$changes, renames = list())
    }
    marks = if ("marks" %in% part) {
      diff_marks(
        a$parts$marks, b$parts$marks, rubric$renames,
        rubric_item_labels(b$parts$rubric, a$parts$rubric), rubric$id_renames
      )
    } else {
      diff_marks(list(), list())
    }
    template = if ("template" %in% part) diff_template(a$parts$template, b$parts$template) else diff_template(NULL, NULL)
    scores = diff_scores(a$stats, b$stats)

    markermd_snapshot_diff(
      from = a$id, to = b$id, from_label = a$label, to_label = b$label,
      rubric = rubric$changes, marks = marks, scores = scores$scores, totals = scores$totals,
      template = template
    )
  })
}
