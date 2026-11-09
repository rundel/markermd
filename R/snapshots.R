# Snapshots of the grading state, kept inside the project database
#
# A snapshot is the resolved grading state at one instant: the template, the
# rubric and the marks, stored as the three exchange documents markermd
# already knows how to validate and import (template_to_list(),
# rubric_to_list() with item and group ids, marks_to_list()). Snapshots are
# taken by hand (snapshot_create()) or automatically before every bulk change
# to the database (imports, restores, the apps' destructive actions, schema
# migrations), inside the same transaction as the change, so a change cannot
# succeed without the snapshot of the state it replaced. They are listed,
# annotated, compared (snapshot-diff.R), scored, exported and restored from R
# and from the History tab of mark().
#
# Storage (schema version 5, see create_snapshot_tables()): each part is
# canonical JSON (sorted keys) in a content-addressed blob shared by every
# snapshot whose part is identical, so a snapshot that changed only the marks
# costs only the marks. An automatic snapshot whose content matches the latest
# snapshot's is not created; the operation is recorded as a "trigger" note on
# that snapshot instead. The template part drops the template's metadata
# (its save timestamp), so re-saving an unchanged template is not a change.
#
# Everything that touches the database here is conn-based: the writer runs
# inside callers' transactions (and inside the schema migration, where
# with_database() must not be re-entered), and the readers are wrapped by the
# exported functions with with_database().

snapshot_part_names = function() c("template", "rubric", "marks")

# A fresh snapshot id: the UTC creation time (readable in file names and
# messages) plus a short random suffix so two snapshots in one second stay
# distinct
#
# Returns: Character string such as "snap-20260929T235540Z-3f9a"

new_snapshot_id = function() {
  stamp = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  suffix = paste(sample(c(0:9, letters[1:6]), 4, replace = TRUE), collapse = "")
  paste0("snap-", stamp, "-", suffix)
}

# The session's time zone name, for display of a snapshot's local time

snapshot_timezone = function() {
  tz = Sys.timezone()
  if (is.null(tz) || is.na(tz) || !nzchar(tz)) format(Sys.time(), "%Z") else tz
}

# --- Canonical JSON ----------------------------------------------------------

# Sort every named list by name, recursively, so two payloads with the same
# content hash the same regardless of construction order
#
# x: A plain list (the exchange documents are plain lists)

canonical_list = function(x) {
  if (is.list(x)) {
    if (length(x) > 0 && !is.null(names(x))) {
      x = x[order(names(x))]
    }
    x = lapply(x, canonical_list)
  }
  x
}

# A plain exchange document as canonical JSON text. auto_unbox matches how
# the template is already stored in the metadata table and what the *_from_list()
# readers accept (single values and one-element sequences both read back)
#
# x: A plain list

canonical_json = function(x) {
  enc2utf8(as.character(jsonlite::toJSON(
    canonical_list(x), auto_unbox = TRUE, null = "null", na = "null", digits = NA
  )))
}

# Parse JSON text back into the plain list shape the *_from_list() readers take
#
# json: Character string

parse_json = function(json) {
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

# Content hash of a text (128-bit, hex), used for both part blobs and the
# snapshot's content_hash
#
# text: Character string

snapshot_hash = function(text) {
  rlang::hash(enc2utf8(text))
}

# --- The live payload ----------------------------------------------------------

# The stored template as a plain list, or NULL when the database has none
#
# conn: DBI connection object

db_template_list = function(conn) {
  json = get_metadata(conn, "template")
  if (is.null(json)) NULL else parse_json(json)
}

# Question names of a plain template list, in template order
#
# x: Plain template list (template_to_list() shape)

template_list_question_names = function(x) {
  vapply(x$questions, function(q) as.character(q$name), character(1))
}

# Question names stored in the database: in template order when a template
# is stored, otherwise the distinct names in the items table (sorted).
# Returns list(names, had_template, document) like project_question_names(),
# which wraps this; reading the names off the stored JSON avoids re-parsing
# the assignment document, which loading the template object would do.
#
# conn: DBI connection object

db_question_names = function(conn) {
  x = db_template_list(conn)
  if (!is.null(x)) {
    names = template_list_question_names(x)
    is_document = vapply(x$questions, function(q) identical(q$scope, "document"), logical(1))
    return(list(names = names, had_template = TRUE, document = names[is_document]))
  }
  names = sort(unique(load_all_items(conn)$question_name))
  list(names = names, had_template = FALSE, document = character(0))
}

# The current grading state as the three plain exchange documents. The
# template part is the stored template minus its metadata (created_at is
# stamped on every save, so it would make every re-save a change); the rubric
# part carries the database's item and group ids so a diff can tell a
# reworded item from a delete plus an add, and the marks part carries each
# pair's selected item ids beside the descriptions so a restore and the
# cached scores work by id, whatever the descriptions. NULL for a part with
# nothing to store (no template).
#
# conn: DBI connection object
# Returns: List with template, rubric and marks

snapshot_payload = function(conn) {
  template = db_template_list(conn)
  if (!is.null(template)) {
    template$metadata = NULL
  }
  question_names = db_question_names(conn)$names
  list(
    template = template,
    rubric = rubric_to_list(rubric_data_from_conn(conn, question_names, ids = TRUE), ids = TRUE),
    marks = marks_to_list(marks_data_from_conn(conn, ids = TRUE))
  )
}

# A plain rubric list without the item and group ids a snapshot carries, for
# the content hash: a replace import mints new ids for identical items, and
# that churn must not count as a change of state
#
# rubric: Plain rubric list (rubric_to_list(ids = TRUE) shape)

rubric_without_ids = function(rubric) {
  rubric$questions = lapply(rubric$questions, function(question) {
    question$items = lapply(question$items, function(entry) {
      entry$id = NULL
      if (is_rubric_group_entry(entry)) {
        entry$items = lapply(entry$items, function(member) {
          member$id = NULL
          member
        })
      }
      entry
    })
    question
  })
  rubric
}

# Encode a payload's parts: canonical JSON, its hash and the part's format
# version, plus the snapshot-level content hash. The part hashes address the
# stored blobs (with the rubric's and marks' ids, so a diff and a restore can
# match items); the content hash, which decides whether the state changed,
# is taken over the rubric and marks without their ids.
#
# payload: List from snapshot_payload() (or read back from a snapshot)
# Returns: List with parts (named list of json / hash / format_version) and
#   content_hash

encode_snapshot_parts = function(payload) {
  parts = list()
  content = character(0)
  for (part in snapshot_part_names()) {
    x = payload[[part]]
    if (is.null(x)) {
      next
    }
    json = canonical_json(x)
    parts[[part]] = list(
      json = json,
      hash = snapshot_hash(json),
      format_version = as.character(x$format_version)
    )
    content[[part]] = switch(
      part,
      rubric = snapshot_hash(canonical_json(rubric_without_ids(x))),
      marks = snapshot_hash(canonical_json(marks_without_ids(x))),
      parts[[part]]$hash
    )
  }
  list(
    parts = parts,
    content_hash = snapshot_hash(paste(names(content), content, sep = "=", collapse = "\n"))
  )
}

# Whether the database holds any grading state worth a snapshot: a template,
# rubric items, or any recorded selection or comment
#
# conn: DBI connection object

snapshot_has_state = function(conn) {
  if (!is.null(get_metadata(conn, "template"))) {
    return(TRUE)
  }
  for (table in c("items", "grades", "comments", "private_comments")) {
    n = DBI::dbGetQuery(conn, glue::glue("SELECT COUNT(*) AS n FROM {table}"))$n
    if (n > 0) {
      return(TRUE)
    }
  }
  FALSE
}

# Student repository names of the project the database belongs to, read from
# the project config, for the cached scores; empty (never an error) when the
# directory is not a project or has no repos directory, since snapshots are
# also taken from bare databases in tests and during migration
#
# root: Project root

snapshot_repo_universe = function(root) {
  if (is.null(root) || !is_markermd_project(root)) {
    return(character(0))
  }
  proj = project_config(root)
  if (is.na(proj@repos) || !fs::dir_exists(fs::path(proj@root, proj@repos))) {
    return(character(0))
  }
  project_repo_names(proj)
}

# --- Scoring a payload --------------------------------------------------------

# One plain rubric question's items and groups as the objects score_question()
# takes. Tolerant of what the database allows (duplicate descriptions): no
# S7 exchange round trip, the entries are read directly. Items are keyed by
# their stored id when the snapshot carries ids (they always do), otherwise
# by a "missing_<k>" placeholder that restore_rubric_conn() replaces with a
# fresh id.
#
# question: One entry of a plain rubric list's questions
# Returns: List with items (named list of markermd_rubric_item keyed by id),
#   descriptions (parallel character vector), groups (named list of
#   markermd_rubric_group keyed by group id) and state (markermd_grade_state)

rubric_question_parts = function(question) {
  items = list()
  groups = list()
  k = 0
  add_item = function(entry, group_id) {
    k <<- k + 1
    id = if (is.null(entry$id)) paste0("missing_", k) else as.character(entry$id)
    items[[id]] <<- markermd_rubric_item(
      hotkey = NA_integer_,
      points = as.numeric(entry$points),
      description = as.character(entry$description),
      group = group_id
    )
  }
  for (entry in question$items) {
    if (is_rubric_group_entry(entry)) {
      group_id = if (is.null(entry$id)) paste0("missing_group_", length(groups) + 1) else as.character(entry$id)
      groups[[group_id]] = rubric_group_from_list(entry)
      for (member in entry$items) {
        add_item(member, group_id)
      }
    } else {
      add_item(entry, NA_character_)
    }
  }
  state = if (is.null(question$scoring)) {
    markermd_grade_state(current_score = 0, total_score = 10)
  } else {
    scoring_from_list(question$scoring)
  }
  list(
    items = items,
    descriptions = vapply(items, function(item) item@description, character(1), USE.NAMES = FALSE),
    groups = groups,
    state = state
  )
}

# A snapshot's rubric and marks documents as a grading frame (see
# utils_mark_scores.R), the same shape grading_frames_from_conn() builds from
# the live database, so both are scored by score_pairs_from_frames().
# Selections are matched by the item ids a snapshot's marks carry
# (item_ids); a marks document without them (an older snapshot) is matched by
# description, where a duplicated description can only pick the first item.
# A selection of an item the rubric no longer has is dropped, as the database
# path drops a grade row naming a deleted item.
#
# rubric: Plain rubric list (rubric_to_list() shape)
# marks: Plain marks list (marks_to_list() shape)

grading_frames_from_parts = function(rubric, marks) {
  parts = lapply(rubric$questions, rubric_question_parts)
  names(parts) = vapply(rubric$questions, function(q) as.character(q$name), character(1))

  selections = list(question_name = character(0), assignment_repo = character(0), item_id = character(0))
  comment_pairs = list(question_name = character(0), assignment_repo = character(0))
  for (repo in marks$repos) {
    for (entry in repo$questions) {
      question_name = as.character(entry$name)
      question = parts[[question_name]]
      selected_ids = if (is.null(question)) {
        character(0)
      } else if (!is.null(entry$item_ids)) {
        intersect(as.character(unlist(entry$item_ids)), names(question$items))
      } else {
        wanted = as.character(unlist(entry$items))
        names(question$items)[match(wanted, question$descriptions, nomatch = 0)]
      }
      if (length(selected_ids) > 0) {
        selections$question_name = c(selections$question_name, rep(question_name, length(selected_ids)))
        selections$assignment_repo = c(selections$assignment_repo, rep(as.character(repo$name), length(selected_ids)))
        selections$item_id = c(selections$item_id, selected_ids)
      }
      if (!is.null(entry$comment) && nzchar(trimws(entry$comment))) {
        comment_pairs$question_name = c(comment_pairs$question_name, question_name)
        comment_pairs$assignment_repo = c(comment_pairs$assignment_repo, as.character(repo$name))
      }
    }
  }

  list(
    questions = lapply(parts, function(p) p[c("items", "groups", "state")]),
    selections = as.data.frame(selections, stringsAsFactors = FALSE),
    comment_pairs = as.data.frame(comment_pairs, stringsAsFactors = FALSE)
  )
}

# Score every (repository, question) pair of a rubric and marks document
# with the scorer behind collect_score_data(), score_pairs_from_frames(), so
# a snapshot's cached scores and the live scores are the same computation
# over the same frame shape (grading_frames_from_parts()). Ungraded pairs
# score NA.
#
# rubric: Plain rubric list (rubric_to_list() shape)
# marks: Plain marks list (marks_to_list() shape)
# repo_names: Repositories to score, in output order; repositories in the
#   marks but not listed are appended (sorted)
# Returns: Data frame with question_name, assignment_repo and score

score_snapshot_parts = function(rubric, marks, repo_names = character(0)) {
  mark_repos = vapply(marks$repos, function(repo) as.character(repo$name), character(1))
  repos = c(repo_names, sort(setdiff(mark_repos, repo_names)))
  question_names = vapply(rubric$questions, function(q) as.character(q$name), character(1))
  score_pairs_from_frames(grading_frames_from_parts(rubric, marks), question_names, repos)
}

# Per-repository totals of a scores data frame, NA until every question is
# graded (the export_scores() convention: the total a student would receive)
#
# scores: Data frame with question_name, assignment_repo, score
# question_names: The questions a complete total needs
# Returns: Data frame with assignment_repo and total

snapshot_totals = function(scores, question_names) {
  repos = unique(scores$assignment_repo)
  totals = vapply(repos, function(repo) {
    rows = scores[scores$assignment_repo == repo, , drop = FALSE]
    values = rows$score[match(question_names, rows$question_name)]
    if (length(question_names) > 0 && !anyNA(values)) sum(values) else NA_real_
  }, numeric(1), USE.NAMES = FALSE)
  data.frame(assignment_repo = repos, total = totals, stringsAsFactors = FALSE)
}

# The snapshot_stats rows for a payload: every pair's score plus one
# "(total)" row per repository
#
# payload: List with rubric and marks (plain lists)
# repo_names: Repositories to score (see score_snapshot_parts())

snapshot_stats_rows = function(payload, repo_names) {
  scores = score_snapshot_parts(payload$rubric, payload$marks, repo_names)
  question_names = vapply(payload$rubric$questions, function(q) as.character(q$name), character(1))
  totals = snapshot_totals(scores, question_names)
  rbind(
    scores[, c("assignment_repo", "question_name", "score")],
    data.frame(
      assignment_repo = totals$assignment_repo, question_name = rep("(total)", nrow(totals)),
      score = totals$total, stringsAsFactors = FALSE
    )
  )
}

# --- Writing -------------------------------------------------------------------

# A part's JSON as the blob row to store: gzip when that saves space
#
# json: Character string
# Returns: List with encoding, content (raw) and size (uncompressed bytes)

encode_snapshot_blob = function(json) {
  raw = charToRaw(json)
  gz = memCompress(raw, "gzip")
  if (length(gz) < length(raw)) {
    list(encoding = "json+gzip", content = gz, size = length(raw))
  } else {
    list(encoding = "json", content = raw, size = length(raw))
  }
}

# A stored blob row back to its JSON text
#
# row: One-row data frame from snapshot_blobs

decode_snapshot_blob = function(row) {
  content = row$content[[1]]
  if (identical(row$encoding[1], "json+gzip")) {
    content = memDecompress(content, "gzip")
  }
  out = rawToChar(content)
  Encoding(out) = "UTF-8"
  out
}

# The newest snapshot row, or NULL when there is none. Ordering is by
# created_at (UTC) with the autoincrement id breaking ties.
#
# conn: DBI connection object

latest_snapshot_row = function(conn) {
  row = DBI::dbGetQuery(conn, "
    SELECT * FROM snapshots ORDER BY created_at DESC, id DESC LIMIT 1
  ")
  if (nrow(row) == 0) NULL else row
}

# Append a note (or keyed metadata) to a snapshot's note log
#
# conn: DBI connection object
# snapshot_id: Snapshot id
# value: Note text
# key: Metadata key, or NULL for a plain note

insert_snapshot_note = function(conn, snapshot_id, value, key = NULL) {
  DBI::dbExecute(conn, "
    INSERT INTO snapshot_notes (snapshot_id, timestamp, username, key, value)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(
    snapshot_id, utc_timestamp(), get_current_username(),
    if (is.null(key)) NA_character_ else as.character(key),
    as.character(value)
  ))
}

# Text of the "trigger" note recorded on an existing snapshot when an
# automatic snapshot found the state unchanged
#
# trigger: What ran
# summary: Its summary, or NULL

trigger_note_text = function(trigger, summary = NULL) {
  if (is.null(summary) || !nzchar(trimws(summary))) trigger else paste0(trigger, ": ", summary)
}

# Write a snapshot of the current state. Runs inside the caller's transaction
# (it begins none itself) so a destructive operation and the snapshot of what
# it replaces commit or roll back together. An automatic snapshot (kind
# "auto") is skipped when the database holds no grading state, and when the
# state equals the latest snapshot's it is not created either: the trigger is
# recorded as a note on that snapshot instead. A manual snapshot is always
# created. Identical parts are stored once (content-addressed blobs), and the
# per-pair scores are cached in snapshot_stats.
#
# conn: DBI connection object
# root: Project root (for the repository list the cached scores cover)
# kind: "manual" or "auto"
# trigger: What took the snapshot, e.g. "rubric_import(mode = 'replace')"
# label: Short name (optional)
# summary: Free text: what is about to change and why (optional)
# metadata: Named list written as keyed notes
# Returns: List with snapshot_id and created (FALSE when an unchanged state
#   was recorded as a note on the existing snapshot), or NULL when an
#   automatic snapshot found nothing to snapshot

snapshot_write = function(conn, root, kind = c("manual", "auto"), trigger = NULL,
                          label = NULL, summary = NULL, metadata = list()) {
  kind = match.arg(kind)
  blank = function(x) is.null(x) || !nzchar(trimws(x))
  if (blank(label)) label = NA_character_
  if (blank(summary)) summary = NA_character_
  if (blank(trigger)) trigger = NA_character_

  latest = latest_snapshot_row(conn)
  if (identical(kind, "auto")) {
    if (!snapshot_has_state(conn)) {
      return(NULL)
    }
  }

  payload = snapshot_payload(conn)
  encoded = encode_snapshot_parts(payload)

  if (identical(kind, "auto") && !is.null(latest) && identical(latest$content_hash, encoded$content_hash)) {
    insert_snapshot_note(conn, latest$snapshot_id, trigger_note_text(trigger, summary), key = "trigger")
    return(list(snapshot_id = latest$snapshot_id, created = FALSE))
  }

  snapshot_id = new_snapshot_id()
  while (nrow(DBI::dbGetQuery(conn, "SELECT 1 FROM snapshots WHERE snapshot_id = ?", params = list(snapshot_id))) > 0) {
    snapshot_id = new_snapshot_id()
  }

  for (part in names(encoded$parts)) {
    blob = encode_snapshot_blob(encoded$parts[[part]]$json)
    DBI::dbExecute(conn, "
      INSERT OR IGNORE INTO snapshot_blobs (hash, encoding, content, size) VALUES (?, ?, ?, ?)
    ", params = list(encoded$parts[[part]]$hash, blob$encoding, list(blob$content), blob$size))
  }

  event_id = DBI::dbGetQuery(conn, "SELECT MAX(id) AS id FROM grades")$id
  DBI::dbExecute(conn, "
    INSERT INTO snapshots (snapshot_id, created_at, created_tz, username, kind, trigger, label,
                           summary, parent_snapshot_id, schema_version, markermd_version,
                           content_hash, event_id, pinned)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0)
  ", params = list(
    snapshot_id, utc_timestamp(), snapshot_timezone(), get_current_username(), kind, trigger, label,
    summary, if (is.null(latest)) NA_character_ else latest$snapshot_id,
    markermd_db_version(), as.character(utils::packageVersion("markermd")),
    encoded$content_hash, if (is.na(event_id)) NA_integer_ else as.integer(event_id)
  ))

  for (part in names(encoded$parts)) {
    DBI::dbExecute(conn, "
      INSERT INTO snapshot_parts (snapshot_id, part, format_version, hash) VALUES (?, ?, ?, ?)
    ", params = list(snapshot_id, part, encoded$parts[[part]]$format_version, encoded$parts[[part]]$hash))
  }

  stats = snapshot_stats_rows(payload, snapshot_repo_universe(root))
  if (nrow(stats) > 0) {
    DBI::dbExecute(conn, "
      INSERT INTO snapshot_stats (snapshot_id, assignment_repo, question_name, score) VALUES (?, ?, ?, ?)
    ", params = list(rep(snapshot_id, nrow(stats)), stats$assignment_repo, stats$question_name, stats$score))
  }

  for (key in names(metadata)) {
    insert_snapshot_note(conn, snapshot_id, paste(as.character(metadata[[key]]), collapse = ", "), key = key)
  }

  list(snapshot_id = snapshot_id, created = TRUE)
}

# Take an automatic snapshot before a change, inside the caller's
# transaction: snapshot_write() with kind "auto", tolerant of a NULL
# snapshot spec (no snapshot wanted)
#
# conn: DBI connection object
# root: Project root
# spec: NULL, or a list with trigger and optionally summary

snapshot_before_change = function(conn, root, spec) {
  if (is.null(spec)) {
    return(NULL)
  }
  result = snapshot_write(conn, root, kind = "auto", trigger = spec$trigger, summary = spec$summary)
  note_action_snapshot(result$snapshot_id)
  result
}

# Delete blobs no snapshot part or action part references any more
#
# conn: DBI connection object

gc_snapshot_blobs = function(conn) {
  DBI::dbExecute(conn, "
    DELETE FROM snapshot_blobs
    WHERE hash NOT IN (SELECT hash FROM snapshot_parts)
      AND hash NOT IN (SELECT hash FROM action_parts)
  ")
}

# --- Reading -------------------------------------------------------------------

# Snapshot rows newest first, with the note count and the cached totals'
# summary (how many repositories have a complete total, and its mean, min
# and max) joined on
#
# conn: DBI connection object
# where: Optional SQL condition on the snapshots table (aliased s)
# params: Its parameters

snapshot_rows = function(conn, where = NULL, params = NULL) {
  where_clause = if (is.null(where)) "" else paste0("WHERE ", where)
  DBI::dbGetQuery(conn, glue::glue("
    SELECT s.*,
      (SELECT COUNT(*) FROM snapshot_notes n WHERE n.snapshot_id = s.snapshot_id) AS n_notes,
      (SELECT COUNT(*) FROM snapshot_stats t
         WHERE t.snapshot_id = s.snapshot_id AND t.question_name = '(total)' AND t.score IS NOT NULL) AS n_complete,
      (SELECT AVG(score) FROM snapshot_stats t
         WHERE t.snapshot_id = s.snapshot_id AND t.question_name = '(total)') AS mean_total,
      (SELECT MIN(score) FROM snapshot_stats t
         WHERE t.snapshot_id = s.snapshot_id AND t.question_name = '(total)') AS min_total,
      (SELECT MAX(score) FROM snapshot_stats t
         WHERE t.snapshot_id = s.snapshot_id AND t.question_name = '(total)') AS max_total
    FROM snapshots s
    <<where_clause>>
    ORDER BY s.created_at DESC, s.id DESC
  ", .open = "<<", .close = ">>"), params = params)
}

# Resolve a user-supplied snapshot reference to a snapshot_id: "latest", a
# non-positive integer counting back from the latest (0 is the latest, -1
# the one before it), an exact snapshot_id, a unique prefix of one, or a
# unique label. Anything ambiguous or unknown aborts listing the candidates.
#
# conn: DBI connection object
# id: The reference

resolve_snapshot_id = function(conn, id) {
  rows = snapshot_rows(conn)
  if (nrow(rows) == 0) {
    cli::cli_abort(c(
      "This project has no snapshots.",
      "i" = "Take one with {.code snapshot_create()}."
    ))
  }

  if (is.numeric(id) && length(id) == 1 && !is.na(id)) {
    k = as.integer(id)
    if (k > 0) {
      cli::cli_abort("A numeric snapshot reference counts back from the latest: {.code 0} is the latest, {.code -1} the one before it.")
    }
    idx = 1L - k
    if (idx > nrow(rows)) {
      cli::cli_abort("Snapshot {.val {id}} does not exist: this project has {nrow(rows)} snapshot{?s}.")
    }
    return(rows$snapshot_id[idx])
  }

  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) {
    cli::cli_abort("{.arg id} must be a snapshot id, a unique prefix or label, {.val latest}, or a non-positive integer.")
  }
  if (identical(id, "latest")) {
    return(rows$snapshot_id[1])
  }
  if (id %in% rows$snapshot_id) {
    return(id)
  }

  hits = rows$snapshot_id[startsWith(rows$snapshot_id, id)]
  if (length(hits) == 1) {
    return(hits)
  }
  if (length(hits) > 1) {
    cli::cli_abort(c(
      "Snapshot prefix {.val {id}} is ambiguous.",
      "i" = "It matches: {.val {hits}}."
    ))
  }

  hits = rows$snapshot_id[!is.na(rows$label) & rows$label == id]
  if (length(hits) == 1) {
    return(hits)
  }
  if (length(hits) > 1) {
    cli::cli_abort(c(
      "Several snapshots carry the label {.val {id}}.",
      "i" = "Use their ids instead: {.val {hits}}."
    ))
  }

  cli::cli_abort(c(
    "No snapshot matches {.val {id}}.",
    "i" = "Snapshots (newest first): {.val {utils::head(rows$snapshot_id, 5)}}{if (nrow(rows) > 5) ', ...' else ''}.",
    "i" = "See {.code snapshot_list()}."
  ))
}

# One stored part of a snapshot as a plain list, or NULL when the snapshot
# has no such part
#
# conn: DBI connection object
# snapshot_id: Snapshot id
# part: "template", "rubric" or "marks"

read_snapshot_part = function(conn, snapshot_id, part) {
  row = DBI::dbGetQuery(conn, "
    SELECT b.encoding, b.content FROM snapshot_parts p
    INNER JOIN snapshot_blobs b ON b.hash = p.hash
    WHERE p.snapshot_id = ? AND p.part = ?
  ", params = list(snapshot_id, part))
  if (nrow(row) == 0) NULL else parse_json(decode_snapshot_blob(row))
}

# All three parts of a snapshot (NULL for an absent part)
#
# conn: DBI connection object
# snapshot_id: Snapshot id

read_snapshot_parts = function(conn, snapshot_id) {
  stats::setNames(
    lapply(snapshot_part_names(), function(part) read_snapshot_part(conn, snapshot_id, part)),
    snapshot_part_names()
  )
}

# A snapshot's cached scores: one row per (repository, question) plus the
# "(total)" rows
#
# conn: DBI connection object
# snapshot_id: Snapshot id

read_snapshot_stats = function(conn, snapshot_id) {
  DBI::dbGetQuery(conn, "
    SELECT assignment_repo, question_name, score FROM snapshot_stats
    WHERE snapshot_id = ? ORDER BY assignment_repo, question_name
  ", params = list(snapshot_id))
}

# A snapshot's notes, oldest first
#
# conn: DBI connection object
# snapshot_id: Snapshot id

read_snapshot_notes = function(conn, snapshot_id) {
  notes = DBI::dbGetQuery(conn, "
    SELECT timestamp, username, key, value FROM snapshot_notes
    WHERE snapshot_id = ? ORDER BY id
  ", params = list(snapshot_id))
  notes$timestamp = parse_utc_timestamp(notes$timestamp)
  notes
}

# Whether the live state differs from the latest snapshot, with the number
# of grade events recorded since it. No snapshot counts as changed when the
# database holds any state. Deciding `changed` means building and hashing
# the whole payload, which grows with the class; the marking app's navbar
# indicator, refreshed on every grading write, asks for the cheap part only
# (content = FALSE gives changed = NA when a snapshot exists).
#
# conn: DBI connection object
# content: Whether to compare the live content with the latest snapshot's
# Returns: List with changed, latest (the latest snapshot row or NULL) and
#   events_since

snapshot_state_status = function(conn, content = TRUE) {
  latest = latest_snapshot_row(conn)
  if (is.null(latest)) {
    return(list(changed = snapshot_has_state(conn), latest = NULL, events_since = 0L))
  }
  changed = NA
  if (content) {
    current = encode_snapshot_parts(snapshot_payload(conn))$content_hash
    changed = !identical(current, latest$content_hash)
  }
  events = if (is.na(latest$event_id)) {
    DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM grades")$n
  } else {
    DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM grades WHERE id > ?", params = list(latest$event_id))$n
  }
  list(changed = changed, latest = latest, events_since = as.integer(events))
}

# The number of recorded selection events since the latest snapshot beyond
# which the marking app's status indicator nudges the grader to take one
# (option markermd.snapshot_warn_events, default 200)

snapshot_warn_events = function() {
  as.integer(getOption("markermd.snapshot_warn_events", 200L))
}

# Human-readable age of a UTC timestamp ("just now", "5 min ago", "2 h ago",
# "3 d ago")
#
# created_at: A UTC ISO 8601 timestamp string or POSIXct
# now: The reference time

format_age = function(created_at, now = Sys.time()) {
  if (is.character(created_at)) {
    created_at = parse_utc_timestamp(created_at)
  }
  secs = as.numeric(difftime(now, created_at, units = "secs"))
  if (is.na(secs)) {
    return("unknown")
  }
  if (secs < 60) "just now"
  else if (secs < 3600) paste0(floor(secs / 60), " min ago")
  else if (secs < 86400) paste0(floor(secs / 3600), " h ago")
  else paste0(floor(secs / 86400), " d ago")
}

# --- Classes -------------------------------------------------------------------

#' @title Snapshot of a project's grading state
#' @description S7 class returned by [snapshot_info()]: every stored field of
#'   one snapshot, its notes in order, its parts' format versions and sizes,
#'   and its cached per-repository scores.
#' @param snapshot_id Character. The snapshot's id (`snap-<UTC time>-<suffix>`).
#' @param created_at POSIXct. Creation time, in the session's time zone.
#' @param created_tz Character. The creator's time zone name.
#' @param username Character. Who took it.
#' @param kind Character. `"manual"` or `"auto"`.
#' @param trigger Character. What took an automatic snapshot (`NA` for a manual one).
#' @param label Character. Short name, or `NA`.
#' @param summary Character. What changed or why, or `NA`.
#' @param parent_snapshot_id Character. The previous snapshot's id, or `NA` for the first.
#' @param schema_version Character. Database schema version at creation.
#' @param markermd_version Character. Package version at creation.
#' @param content_hash Character. Hash of the stored parts.
#' @param pinned Logical. Whether the snapshot is protected from pruning.
#' @param notes Data frame of notes and keyed metadata (`timestamp`,
#'   `username`, `key`, `value`), oldest first.
#' @param parts Data frame with one row per stored part (`part`,
#'   `format_version`, `size` in bytes, `hash`).
#' @param stats Data frame of cached scores (`assignment_repo`,
#'   `question_name`, `score`), with `"(total)"` rows for each repository's total.
#' @return A `markermd_snapshot` S7 object.
#' @seealso [snapshot_info()], [snapshot_list()]
#' @export
markermd_snapshot = S7::new_class(
  "markermd_snapshot",
  properties = list(
    snapshot_id = S7::class_character,
    created_at = S7::class_POSIXct,
    created_tz = S7::class_character,
    username = S7::class_character,
    kind = S7::class_character,
    trigger = S7::class_character,
    label = S7::class_character,
    summary = S7::class_character,
    parent_snapshot_id = S7::class_character,
    schema_version = S7::class_character,
    markermd_version = S7::class_character,
    content_hash = S7::class_character,
    pinned = S7::class_logical,
    notes = S7::class_data.frame,
    parts = S7::class_data.frame,
    stats = S7::class_data.frame
  ),
  package = "markermd"
)

S7::method(print, markermd_snapshot) = function(x, ...) {
  cli::cli_h1("Snapshot {x@snapshot_id}")
  fields = c(
    "created" = paste0(format(x@created_at, "%Y-%m-%d %H:%M:%S"), " (", x@created_tz, ")"),
    "by" = x@username,
    "kind" = x@kind
  )
  if (!is.na(x@trigger)) fields["trigger"] = x@trigger
  if (!is.na(x@label)) fields["label"] = x@label
  if (!is.na(x@summary)) fields["summary"] = x@summary
  if (!is.na(x@parent_snapshot_id)) fields["parent"] = x@parent_snapshot_id
  fields["pinned"] = if (x@pinned) "yes" else "no"
  fields["versions"] = paste0("schema ", x@schema_version, ", markermd ", x@markermd_version)
  cli::cli_dl(fields)

  cli::cli_h2("Parts")
  cli::cli_ul()
  for (i in seq_len(nrow(x@parts))) {
    cli::cli_li("{x@parts$part[i]}: format {x@parts$format_version[i]}, {x@parts$size[i]} bytes")
  }
  cli::cli_end()

  totals = x@stats[x@stats$question_name == "(total)", , drop = FALSE]
  cli::cli_h2("Scores")
  n_complete = sum(!is.na(totals$score))
  if (nrow(totals) == 0) {
    cli::cli_text("No repositories scored.")
  } else {
    cli::cli_text("{n_complete} of {nrow(totals)} repositor{?y/ies} with a complete total{if (n_complete > 0) paste0('; mean ', format_score(mean(totals$score, na.rm = TRUE)), ', range ', format_score(min(totals$score, na.rm = TRUE)), ' to ', format_score(max(totals$score, na.rm = TRUE))) else ''}.")
  }

  if (nrow(x@notes) > 0) {
    cli::cli_h2("Notes")
    cli::cli_ul()
    for (i in seq_len(nrow(x@notes))) {
      prefix = if (is.na(x@notes$key[i])) "" else paste0(x@notes$key[i], " = ")
      cli::cli_li("{format(x@notes$timestamp[i], '%Y-%m-%d %H:%M')} {x@notes$username[i]}: {prefix}{x@notes$value[i]}")
    }
    cli::cli_end()
  }
  invisible(x)
}

# --- Exported API ---------------------------------------------------------------

#' Take a snapshot of a project's grading state
#'
#' Records the project's current grading state (its template, rubric and
#' marks) as a named, timestamped snapshot inside the project database. The
#' snapshot can be listed ([snapshot_list()]), inspected ([snapshot_info()]),
#' compared with another or with the live state ([snapshot_diff()]), exported
#' to YAML ([snapshot_export()]) and restored ([snapshot_restore()]).
#'
#' @details
#' Snapshots are also taken automatically before every bulk change to the
#' database: [rubric_import()], [marks_import()], [template_import()],
#' [snapshot_restore()], the imports and deletions in [mark()] and saves in
#' [template()], and schema upgrades. Those functions take a `summary`
#' argument so the reason for a change is recorded with it. An automatic
#' snapshot of a state identical to the latest snapshot's is not created; the
#' operation is recorded as a note on that snapshot instead. A manual
#' snapshot is always created, since its label and summary carry information,
#' and identical parts are stored once.
#'
#' @param project Path to the project directory. Defaults to the working directory.
#' @param label Optional short name, e.g. `"Before feedback review"`. A
#'   unique label can be used in place of the snapshot id in the other
#'   `snapshot_*()` functions.
#' @param summary Optional free text: what is about to change, or why the
#'   snapshot was taken.
#' @param metadata Optional named list of extra key/value metadata, recorded
#'   as keyed notes (e.g. `list(review_doc = "notes/review.md")`).
#'
#' @return The new snapshot's id, invisibly.
#' @seealso [snapshot_list()], [snapshot_info()], [snapshot_diff()],
#'   [snapshot_restore()], [snapshot_annotate()]
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_create("hw01", label = "Before review",
#'   summary = "First-pass machine grading reviewed item by item next.")
#' }
snapshot_create = function(project = ".", label = NULL, summary = NULL, metadata = list()) {
  proj = project_config(project)
  for (field in c("label", "summary")) {
    value = get(field)
    if (!is.null(value) && (!is.character(value) || length(value) != 1)) {
      cli::cli_abort("{.arg {field}} must be a single string.")
    }
  }
  if (!is.list(metadata) || (length(metadata) > 0 && is.null(names(metadata)))) {
    cli::cli_abort("{.arg metadata} must be a named list.")
  }

  result = with_database(proj@root, function(conn) {
    DBI::dbBegin(conn)
    out = tryCatch({
      res = snapshot_write(conn, proj@root, kind = "manual", label = label, summary = summary, metadata = metadata)
      info = list(
        questions = length(db_question_names(conn)$names),
        items = nrow(load_all_items(conn)),
        stats = read_snapshot_stats(conn, res$snapshot_id)
      )
      DBI::dbCommit(conn)
      c(res, info)
    }, error = function(e) {
      DBI::dbRollback(conn)
      cli::cli_abort("Failed to take the snapshot: {conditionMessage(e)}")
    })
    out
  })

  pairs = result$stats[result$stats$question_name != "(total)", , drop = FALSE]
  n_graded = sum(!is.na(pairs$score))
  cli::cli_alert_success(
    "Snapshot {.val {result$snapshot_id}}: {result$questions} question{?s}, {result$items} rubric item{?s}, {n_graded} graded pair{?s}{if (!is.null(label)) paste0(' (', label, ')') else ''}."
  )
  invisible(result$snapshot_id)
}

#' List a project's snapshots
#'
#' Returns the project's snapshots, newest first, with their metadata and a
#' summary of the cached totals (how many repositories had a complete total,
#' and its mean and range).
#'
#' @param project Path to the project directory. Defaults to the working directory.
#' @param kind Optional filter: `"manual"` or `"auto"`.
#' @param since Optional lower bound on creation time (a `POSIXct`, `Date`,
#'   or a string `as.POSIXct()` understands).
#' @param n Optional maximum number of snapshots to return.
#'
#' @return A data frame (class `markermd_snapshot_list`) with one row per
#'   snapshot: `snapshot_id`, `created_at` (in the session's time zone),
#'   `created_tz`, `username`, `kind`, `trigger`, `label`, `summary`,
#'   `n_notes`, `pinned`, `n_complete`, `mean_total`, `min_total` and
#'   `max_total`. Its print method shows one line per snapshot.
#' @seealso [snapshot_create()], [snapshot_info()]
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_list("hw01")
#' snapshot_list("hw01", kind = "manual")
#' }
snapshot_list = function(project = ".", kind = NULL, since = NULL, n = NULL) {
  proj = project_config(project)
  if (!is.null(kind)) {
    kind = match.arg(kind, c("manual", "auto"))
  }
  rows = with_database(proj@root, snapshot_rows)
  as_snapshot_list(rows, kind = kind, since = since, n = n)
}

# Shape the snapshot_rows() result as the exported listing
#
# rows: Data frame from snapshot_rows()
# kind, since, n: The filters of snapshot_list()

as_snapshot_list = function(rows, kind = NULL, since = NULL, n = NULL) {
  out = data.frame(
    snapshot_id = as.character(rows$snapshot_id),
    created_at = parse_utc_timestamp(rows$created_at),
    created_tz = as.character(rows$created_tz),
    username = as.character(rows$username),
    kind = as.character(rows$kind),
    trigger = as.character(rows$trigger),
    label = as.character(rows$label),
    summary = as.character(rows$summary),
    n_notes = as.integer(rows$n_notes),
    pinned = as.logical(rows$pinned),
    n_complete = as.integer(rows$n_complete),
    mean_total = as.numeric(rows$mean_total),
    min_total = as.numeric(rows$min_total),
    max_total = as.numeric(rows$max_total),
    stringsAsFactors = FALSE
  )
  if (!is.null(kind)) {
    out = out[out$kind == kind, , drop = FALSE]
  }
  if (!is.null(since)) {
    out = out[out$created_at >= as.POSIXct(since), , drop = FALSE]
  }
  if (!is.null(n)) {
    out = utils::head(out, n)
  }
  rownames(out) = NULL
  class(out) = c("markermd_snapshot_list", "data.frame")
  out
}

#' @export
print.markermd_snapshot_list = function(x, ...) {
  if (nrow(x) == 0) {
    cli::cli_text("No snapshots.")
    return(invisible(x))
  }
  totals = ifelse(
    x$n_complete > 0,
    paste0(format_score(x$mean_total), " (", format_score(x$min_total), "-", format_score(x$max_total), ", n=", x$n_complete, ")"),
    ""
  )
  df = data.frame(
    snapshot_id = x$snapshot_id,
    created = format(x$created_at, "%Y-%m-%d %H:%M"),
    kind = ifelse(x$pinned, paste0(x$kind, "*"), x$kind),
    label = ifelse(is.na(x$label), "", x$label),
    summary = ifelse(is.na(x$summary), ifelse(is.na(x$trigger), "", one_line_text(x$trigger, 60)), one_line_text(x$summary, 60)),
    total = totals,
    stringsAsFactors = FALSE
  )
  print.data.frame(df, row.names = FALSE, right = FALSE)
  if (any(x$pinned)) {
    cli::cli_text("{.emph * pinned}")
  }
  invisible(x)
}

#' Inspect one snapshot
#'
#' Returns every stored field of a snapshot, its notes, its parts' format
#' versions and sizes, and its cached per-repository scores.
#'
#' @param id The snapshot: its id, a unique prefix of it, a unique label,
#'   `"latest"`, or a non-positive integer counting back from the latest
#'   (`0` is the latest, `-1` the one before it).
#' @param project Path to the project directory. Defaults to the working directory.
#'
#' @return A [markermd_snapshot] object.
#' @seealso [snapshot_list()], [snapshot_scores()]
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_info("latest", project = "hw01")
#' snapshot_info("Before review", project = "hw01")
#' }
snapshot_info = function(id, project = ".") {
  proj = project_config(project)
  with_database(proj@root, function(conn) {
    snapshot_info_from_conn(conn, resolve_snapshot_id(conn, id))
  })
}

# snapshot_info() for a resolved snapshot id on an open connection
#
# conn: DBI connection object
# snapshot_id: Snapshot id

snapshot_info_from_conn = function(conn, snapshot_id) {
  row = snapshot_rows(conn, "s.snapshot_id = ?", list(snapshot_id))
  parts = DBI::dbGetQuery(conn, "
    SELECT p.part, p.format_version, b.size, p.hash FROM snapshot_parts p
    INNER JOIN snapshot_blobs b ON b.hash = p.hash
    WHERE p.snapshot_id = ?
  ", params = list(snapshot_id))
  parts = parts[order(match(parts$part, snapshot_part_names())), , drop = FALSE]
  rownames(parts) = NULL
  markermd_snapshot(
    snapshot_id = row$snapshot_id,
    created_at = parse_utc_timestamp(row$created_at),
    created_tz = row$created_tz,
    username = row$username,
    kind = row$kind,
    trigger = as.character(row$trigger),
    label = as.character(row$label),
    summary = as.character(row$summary),
    parent_snapshot_id = as.character(row$parent_snapshot_id),
    schema_version = row$schema_version,
    markermd_version = row$markermd_version,
    content_hash = row$content_hash,
    pinned = as.logical(row$pinned),
    notes = read_snapshot_notes(conn, snapshot_id),
    parts = parts,
    stats = read_snapshot_stats(conn, snapshot_id)
  )
}

# A snapshot's cached scores as the wide per-repository table the
# Assignments tab uses (Repository, one column per question, Total, Graded)
#
# stats: Data frame in the snapshot_stats layout
# question_names: Question columns, in order

snapshot_scores_wide = function(stats, question_names) {
  scores = stats[stats$question_name != "(total)", , drop = FALSE]
  repos = sort(unique(stats$assignment_repo))
  build_scores_table_data(scores, question_names, repos)
}

# Choices for a select of the snapshots that hold a template, newest first,
# keeping only the snapshots whose template differs from the one before
# (consecutive identical templates collapse to the oldest). Labeled with
# the local time and the label or trigger.
#
# root: Project root
# Returns: Named character vector (labels as names) of snapshot ids

template_snapshot_choices = function(root) {
  rows = with_database(root, function(conn) {
    DBI::dbGetQuery(conn, "
      SELECT s.snapshot_id, s.created_at, s.kind, s.label, s.trigger, p.hash
      FROM snapshots s
      INNER JOIN snapshot_parts p ON p.snapshot_id = s.snapshot_id AND p.part = 'template'
      ORDER BY s.created_at ASC, s.id ASC
    ")
  })
  if (nrow(rows) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  keep = c(TRUE, rows$hash[-1] != rows$hash[-nrow(rows)])
  rows = rows[rev(which(keep)), , drop = FALSE]
  what = ifelse(!is.na(rows$label), rows$label, ifelse(!is.na(rows$trigger), rows$trigger, rows$kind))
  labels = paste0(format(parse_utc_timestamp(rows$created_at), "%Y-%m-%d %H:%M"), " \u00b7 ", what)
  stats::setNames(rows$snapshot_id, labels)
}

#' Annotate a snapshot
#'
#' Adds a note or keyed metadata to a snapshot's append-only note log, or
#' changes its label or summary. Nothing is edited in place: a new label or
#' summary is written to the snapshot and also recorded as a note holding the
#' new value, so the record of who changed what, and when, is kept.
#'
#' @inheritParams snapshot_info
#' @param note Optional free-text note to append.
#' @param metadata Optional named list of key/value metadata to append as
#'   keyed notes.
#' @param label Optional new label.
#' @param summary Optional new summary.
#'
#' @return The snapshot's id, invisibly.
#' @seealso [snapshot_info()]
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_annotate("latest", note = "Reviewed with the TA on Friday.", project = "hw01")
#' snapshot_annotate("latest", metadata = list(review_doc = "notes/review.md"), project = "hw01")
#' }
snapshot_annotate = function(id, note = NULL, metadata = list(), label = NULL, summary = NULL, project = ".") {
  proj = project_config(project)
  if (is.null(note) && length(metadata) == 0 && is.null(label) && is.null(summary)) {
    cli::cli_abort("Supply at least one of {.arg note}, {.arg metadata}, {.arg label}, or {.arg summary}.")
  }
  for (field in c("note", "label", "summary")) {
    value = get(field)
    if (!is.null(value) && (!is.character(value) || length(value) != 1)) {
      cli::cli_abort("{.arg {field}} must be a single string.")
    }
  }
  if (!is.list(metadata) || (length(metadata) > 0 && is.null(names(metadata)))) {
    cli::cli_abort("{.arg metadata} must be a named list.")
  }

  snapshot_id = with_database(proj@root, function(conn) {
    snapshot_id = resolve_snapshot_id(conn, id)
    if (!is.null(note)) {
      insert_snapshot_note(conn, snapshot_id, note)
    }
    for (key in names(metadata)) {
      insert_snapshot_note(conn, snapshot_id, paste(as.character(metadata[[key]]), collapse = ", "), key = key)
    }
    if (!is.null(label)) {
      DBI::dbExecute(conn, "UPDATE snapshots SET label = ? WHERE snapshot_id = ?", params = list(label, snapshot_id))
      insert_snapshot_note(conn, snapshot_id, label, key = "label")
    }
    if (!is.null(summary)) {
      DBI::dbExecute(conn, "UPDATE snapshots SET summary = ? WHERE snapshot_id = ?", params = list(summary, snapshot_id))
      insert_snapshot_note(conn, snapshot_id, summary, key = "summary")
    }
    snapshot_id
  })
  cli::cli_alert_success("Annotated snapshot {.val {snapshot_id}}.")
  invisible(snapshot_id)
}

#' Pin or unpin a snapshot
#'
#' A pinned snapshot is never removed by [snapshot_prune()] and needs
#' `force = TRUE` in [snapshot_delete()].
#'
#' @inheritParams snapshot_info
#' @param pinned `TRUE` to pin, `FALSE` to unpin.
#'
#' @return The snapshot's id, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_pin("Scores sent to students", project = "hw01")
#' }
snapshot_pin = function(id, pinned = TRUE, project = ".") {
  proj = project_config(project)
  if (!rlang::is_bool(pinned)) {
    cli::cli_abort("{.arg pinned} must be {.code TRUE} or {.code FALSE}.")
  }
  snapshot_id = with_database(proj@root, function(conn) {
    snapshot_id = resolve_snapshot_id(conn, id)
    DBI::dbExecute(conn, "UPDATE snapshots SET pinned = ? WHERE snapshot_id = ?", params = list(as.integer(pinned), snapshot_id))
    snapshot_id
  })
  cli::cli_alert_success("{if (pinned) 'Pinned' else 'Unpinned'} snapshot {.val {snapshot_id}}.")
  invisible(snapshot_id)
}

# Delete snapshot rows (their parts, notes and stats cascade) and collect
# the blobs nothing references any more
#
# conn: DBI connection object
# snapshot_ids: Character vector of snapshot ids

delete_snapshot_rows = function(conn, snapshot_ids) {
  if (length(snapshot_ids) == 0) {
    return(invisible(0L))
  }
  DBI::dbExecute(conn, "DELETE FROM snapshots WHERE snapshot_id = ?", params = list(snapshot_ids))
  gc_snapshot_blobs(conn)
  invisible(length(snapshot_ids))
}

#' Delete a snapshot
#'
#' Permanently removes a snapshot, its notes and its cached scores. A manual
#' or pinned snapshot is only deleted with `force = TRUE`. Stored content no
#' other snapshot shares is removed with it.
#'
#' @inheritParams snapshot_info
#' @param force Delete a manual or pinned snapshot.
#'
#' @return The deleted snapshot's id, invisibly.
#' @seealso [snapshot_prune()]
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_delete("snap-20260929T235540Z-3f9a", project = "hw01")
#' }
snapshot_delete = function(id, project = ".", force = FALSE) {
  proj = project_config(project)
  if (!rlang::is_bool(force)) {
    cli::cli_abort("{.arg force} must be {.code TRUE} or {.code FALSE}.")
  }
  snapshot_id = with_database(proj@root, function(conn) {
    snapshot_id = resolve_snapshot_id(conn, id)
    row = snapshot_rows(conn, "s.snapshot_id = ?", list(snapshot_id))
    if (!force && (identical(row$kind, "manual") || isTRUE(as.logical(row$pinned)))) {
      what = if (isTRUE(as.logical(row$pinned))) "pinned" else "manual"
      cli::cli_abort(c(
        "Snapshot {.val {snapshot_id}} is {what}.",
        "i" = "Pass {.code force = TRUE} to delete it anyway."
      ))
    }
    delete_snapshot_rows(conn, snapshot_id)
    snapshot_id
  })
  cli::cli_alert_success("Deleted snapshot {.val {snapshot_id}}.")
  invisible(snapshot_id)
}

#' Prune old automatic snapshots
#'
#' Deletes unpinned automatic snapshots that are both older than `keep_days`
#' and outside the `keep_last` most recent ones. Manual snapshots, pinned
#' snapshots, the latest snapshot and the oldest (baseline) snapshot are
#' never pruned. Stored content no remaining snapshot shares is removed.
#'
#' @param project Path to the project directory. Defaults to the working directory.
#' @param keep_last Keep at least this many of the most recent automatic snapshots.
#' @param keep_days Keep automatic snapshots younger than this many days.
#' @param kind Which snapshots are candidates; only `"auto"` is supported.
#'
#' @return The deleted snapshot ids, invisibly.
#' @seealso [snapshot_delete()], [snapshot_pin()]
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_prune("hw01", keep_last = 10, keep_days = 14)
#' }
snapshot_prune = function(project = ".", keep_last = 20, keep_days = 30, kind = "auto") {
  proj = project_config(project)
  kind = match.arg(kind, "auto")
  if (!is.numeric(keep_last) || length(keep_last) != 1 || keep_last < 0) {
    cli::cli_abort("{.arg keep_last} must be a non-negative number.")
  }
  if (!is.numeric(keep_days) || length(keep_days) != 1 || keep_days < 0) {
    cli::cli_abort("{.arg keep_days} must be a non-negative number.")
  }

  deleted = with_database(proj@root, function(conn) {
    rows = snapshot_rows(conn)
    if (nrow(rows) == 0) {
      return(character(0))
    }
    protected = c(rows$snapshot_id[1], rows$snapshot_id[nrow(rows)])
    auto = rows[rows$kind == "auto" & rows$pinned == 0 & !rows$snapshot_id %in% protected, , drop = FALSE]
    beyond_last = auto[seq_len(nrow(auto)) > keep_last, , drop = FALSE]
    cutoff = Sys.time() - keep_days * 86400
    old = beyond_last[parse_utc_timestamp(beyond_last$created_at) < cutoff, , drop = FALSE]
    delete_snapshot_rows(conn, old$snapshot_id)
    old$snapshot_id
  })

  cli::cli_alert_success("Pruned {length(deleted)} automatic snapshot{?s}.")
  invisible(deleted)
}

#' Scores recorded in a snapshot, or the live scores
#'
#' Returns per-repository, per-question scores as [snapshot_diff()] and the
#' History tab compare them: for a snapshot, the scores cached when it was
#' taken; for `"current"`, the live database. Both are computed by the one
#' scorer behind [export_scores()] and the Assignments table, over the same
#' in-memory frame of items, selections and comments, so which pairs count as
#' graded and what they score cannot differ between a snapshot and the live
#' state.
#'
#' @param id A snapshot reference (see [snapshot_info()]), or `"current"` for
#'   the live state.
#' @param project Path to the project directory. Defaults to the working directory.
#'
#' @return A data frame with `assignment_repo`, `question_name` and `score`,
#'   including a `"(total)"` row per repository (`NA` until every question is
#'   graded).
#' @seealso [snapshot_diff()], [export_scores()]
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_scores("latest", project = "hw01")
#' snapshot_scores(project = "hw01")
#' }
snapshot_scores = function(id = "current", project = ".") {
  proj = project_config(project)
  with_database(proj@root, function(conn) {
    if (identical(id, "current")) {
      return(current_snapshot_stats(conn, proj@root))
    }
    read_snapshot_stats(conn, resolve_snapshot_id(conn, id))
  })
}

# The live state's scores in the snapshot_stats layout
#
# conn: DBI connection object
# root: Project root

current_snapshot_stats = function(conn, root) {
  stats = snapshot_stats_rows(snapshot_payload(conn), snapshot_repo_universe(root))
  stats = stats[order(stats$assignment_repo, stats$question_name), , drop = FALSE]
  rownames(stats) = NULL
  stats
}

#' Export a snapshot to YAML files
#'
#' Writes a snapshot's parts as the project's exchange files, validated by the
#' same readers the imports use: `markermd-template.yaml` (when the snapshot
#' holds a template), `markermd-rubric.yaml` and `markermd-marks.yaml`, plus a
#' `snapshot.yaml` sidecar with the snapshot's fields and notes, so the
#' directory stands alone as an archive. The rubric and marks files drop the
#' item and group ids a snapshot carries internally, so they validate against
#' the published schemas.
#'
#' @inheritParams snapshot_info
#' @param dir Output directory, created when missing.
#'
#' @return The written file paths, invisibly.
#' @seealso [template_export()], [rubric_export()], [marks_export()]
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_export("Before review", "backups/before-review", project = "hw01")
#' }
snapshot_export = function(id, dir, project = ".") {
  proj = project_config(project)
  if (!is.character(dir) || length(dir) != 1 || !nzchar(dir)) {
    cli::cli_abort("{.arg dir} must be a directory path.")
  }

  data = with_database(proj@root, function(conn) {
    snapshot_id = resolve_snapshot_id(conn, id)
    list(
      id = snapshot_id,
      row = snapshot_rows(conn, "s.snapshot_id = ?", list(snapshot_id)),
      notes = read_snapshot_notes(conn, snapshot_id),
      parts = read_snapshot_parts(conn, snapshot_id)
    )
  })

  fs::dir_create(dir)
  written = character(0)

  if (!is.null(data$parts$template)) {
    path = fs::path(dir, "markermd-template.yaml")
    yaml::write_yaml(data$parts$template, path)
    written = c(written, path)
  }

  path = fs::path(dir, "markermd-rubric.yaml")
  write_rubric_yaml(rubric_from_list(data$parts$rubric), path)
  written = c(written, path)

  path = fs::path(dir, "markermd-marks.yaml")
  write_marks_yaml(marks_from_list(marks_without_ids(data$parts$marks)), path)
  written = c(written, path)

  row = data$row
  sidecar = list(
    snapshot_id = row$snapshot_id,
    created_at = row$created_at,
    created_tz = row$created_tz,
    username = row$username,
    kind = row$kind,
    trigger = na_to_null(row$trigger),
    label = na_to_null(row$label),
    summary = na_to_null(row$summary),
    parent_snapshot_id = na_to_null(row$parent_snapshot_id),
    schema_version = row$schema_version,
    markermd_version = row$markermd_version,
    content_hash = row$content_hash,
    pinned = as.logical(row$pinned),
    files = as.list(fs::path_file(written)),
    notes = lapply(seq_len(nrow(data$notes)), function(i) {
      note = list(
        timestamp = format(data$notes$timestamp[i], "%Y-%m-%dT%H:%M:%S%z"),
        username = data$notes$username[i]
      )
      if (!is.na(data$notes$key[i])) note$key = data$notes$key[i]
      note$value = data$notes$value[i]
      note
    })
  )
  path = fs::path(dir, "snapshot.yaml")
  yaml::write_yaml(sidecar, path, handlers = list(logical = yaml::verbatim_logical))
  written = c(written, path)

  cli::cli_alert_success("Exported snapshot {.val {data$id}} to {.path {dir}} ({length(written)} file{?s}).")
  invisible(as.character(written))
}

#' Back up the project database file
#'
#' Writes a consistent copy of the grading database (with SQLite's
#' `VACUUM INTO`, safe while [mark()] is open) and of the project config to a
#' backup directory, and records the backup's path as a note on the latest
#' snapshot. Snapshots live inside the database, so this is the protection
#' against losing or corrupting the file itself.
#'
#' @param project Path to the project directory. Defaults to the working directory.
#' @param dir Backup directory, relative to the project root unless absolute.
#'   Created when missing.
#'
#' @return The path of the database copy, invisibly.
#' @seealso [snapshot_create()], [snapshot_export()]
#' @export
#'
#' @examples
#' \dontrun{
#' project_backup("hw01")
#' }
project_backup = function(project = ".", dir = "backups") {
  proj = project_config(project)
  if (!is.character(dir) || length(dir) != 1 || !nzchar(dir)) {
    cli::cli_abort("{.arg dir} must be a directory path.")
  }
  backup_dir = if (fs::is_absolute_path(dir)) dir else fs::path(proj@root, dir)
  fs::dir_create(backup_dir)

  stamp = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  db_copy = fs::path(backup_dir, paste0("markermd-", stamp, ".sqlite"))
  cfg_copy = fs::path(backup_dir, paste0("config-", stamp, ".yml"))

  with_database(proj@root, function(conn) {
    DBI::dbExecute(conn, paste("VACUUM INTO", DBI::dbQuoteString(conn, as.character(db_copy))))
    latest = latest_snapshot_row(conn)
    if (!is.null(latest)) {
      insert_snapshot_note(conn, latest$snapshot_id, as.character(fs::path_rel(db_copy, proj@root)), key = "backup")
    }
  })
  fs::file_copy(project_config_path(proj@root), cfg_copy, overwrite = TRUE)

  cli::cli_alert_success("Backed up the grading database to {.path {db_copy}}.")
  invisible(as.character(db_copy))
}

# Record an export (scores.csv, the comments directory) as a note on the
# snapshot of the state it came from: an automatic snapshot is taken first
# when the state changed since the latest one, so "which grading did the
# students receive" has an answer for every export
#
# root: Project root
# what: "scores" or "comments"
# path: The export's location, recorded root-relative

record_export_note = function(root, what, path) {
  with_database(root, function(conn) {
    DBI::dbBegin(conn)
    tryCatch({
      res = snapshot_write(conn, root, kind = "auto", trigger = paste0("export_", what, "()"))
      if (!is.null(res)) {
        insert_snapshot_note(
          conn, res$snapshot_id,
          paste0(what, ": ", as.character(fs::path_rel(path, root))), key = "export"
        )
      }
      DBI::dbCommit(conn)
    }, error = function(e) {
      DBI::dbRollback(conn)
      cli::cli_abort("Failed to record the export: {conditionMessage(e)}")
    })
  })
  invisible(TRUE)
}

#' Restore a snapshot's grading state
#'
#' Writes a snapshot's rubric and marks (and optionally its template) back
#' into the project database, undoing every change made since. An automatic
#' snapshot of the current state is taken first, so a restore can itself be
#' undone. Everything is written in one transaction: the template is stored,
#' each chosen question's rubric items, groups and scoring are put back in
#' place by id (an item the snapshot knows keeps its id, and with it every
#' repository's recorded selections of it; items and groups the snapshot does
#' not have are removed), and each chosen repository's selections and
#' comments are set to the snapshot's, matched by the item ids the snapshot
#' carries. Pairs with grading activity the snapshot did not record are
#' cleared. The restored scores are then recomputed and compared with the
#' scores the snapshot recorded; a mismatch aborts and rolls everything back.
#'
#' @details
#' Restoring the rubric alone keeps every repository's selections (of the
#' items the snapshot still has), and restoring the marks alone aborts when
#' the snapshot's selections name items the current rubric lacks, so the
#' default restores both and a partial restore warns. `question` and `repo`
#' narrow the restore ("undo the review for Q1c only", "for one student
#' only"); repositories outside `repo` are left exactly as they are, and only
#' the restored repositories are checked against the recorded scores.
#' Without `repo`, every repository under the project's repos directory is
#' restored, including repositories graded only after the snapshot, whose
#' grading is cleared. Clearing appends deselection and empty-comment events
#' (the grading tables are append-only logs), so a cleared pair still counts
#' as having grading activity for [marks_import()]'s default skip; pass
#' `overwrite = TRUE` to re-mark it. Repositories in the snapshot that no
#' longer exist under the repos directory are skipped with a warning. A question the
#' snapshot has that is not in the current template aborts unless
#' `"template"` is among the parts.
#'
#' @inheritParams snapshot_info
#' @param part Which parts to restore: any of `"rubric"`, `"marks"`,
#'   `"template"`. Defaults to the rubric and the marks.
#' @param question Optional character vector of question names to restore
#'   (rubric and marks). Defaults to every question in the snapshot.
#' @param repo Optional character vector of repository names whose marks to
#'   restore. Defaults to every repository of the project.
#' @param summary Optional summary recorded on the automatic pre-restore
#'   snapshot: why the restore was needed.
#'
#' @return Invisibly, a list with `snapshot_id` (the restored snapshot) and
#'   `pre_restore` (the id of the snapshot taken before restoring, or `NULL`
#'   when the state was already recorded).
#' @seealso [snapshot_create()], [snapshot_diff()]
#' @export
#'
#' @examples
#' \dontrun{
#' snapshot_restore("Before review", project = "hw01",
#'   summary = "The review's Q1c severities were wrong; going back to re-do them.")
#'
#' # Only one question
#' snapshot_restore("Before review", project = "hw01", question = "Q1c")
#' }
snapshot_restore = function(id, project = ".", part = c("rubric", "marks"), question = NULL, repo = NULL,
                            summary = NULL) {
  proj = project_config(project)
  part = match.arg(part, c("rubric", "marks", "template"), several.ok = TRUE)
  if (!is.null(summary) && (!is.character(summary) || length(summary) != 1)) {
    cli::cli_abort("{.arg summary} must be a single string.")
  }

  result = snapshot_restore_action(proj, id, part = part, question = question, repo = repo, summary = summary)

  restored = c(
    if ("template" %in% part) "the template",
    if ("rubric" %in% part) "{result$n_questions} question{?s}' rubric",
    if ("marks" %in% part) "the marks of {result$n_repos} repositor{?y/ies}"
  )
  cli::cli_alert_success(paste0("Restored snapshot {.val {result$snapshot_id}}: ", paste(restored, collapse = ", "), "."))
  if (!is.null(result$pre_restore)) {
    cli::cli_bullets(c("i" = "The state before the restore is snapshot {.val {result$pre_restore}}."))
  }
  if (!is.null(result$name_changes)) {
    report_question_name_changes(result$name_changes)
  }
  invisible(result[c("snapshot_id", "pre_restore")])
}

# A restore recorded as an action (see with_action()): the work of
# snapshot_restore(), shared with the marking app's History tab so a restore
# made there is in the activity log like one made from R
#
# proj: markermd_project object
# id, part, question, repo, summary: See snapshot_restore() (part already validated)
# actor: The actor recorded on the action (NULL for markermd_actor())
# Returns: See restore_snapshot()

snapshot_restore_action = function(proj, id, part, question = NULL, repo = NULL, summary = NULL, actor = NULL) {
  call = paste0("snapshot_restore('", id, "', part = c(", paste0("'", part, "'", collapse = ", "), "))")
  with_action(
    proj@root, "snapshot_restore", call = call, summary = summary,
    parts = intersect(part, c("template", "rubric")), actor = actor,
    restore_snapshot(proj, id, part = part, question = question, repo = repo, summary = summary)
  )
}

# The work of snapshot_restore()
#
# proj: markermd_project object
# id, part, question, repo, summary: See snapshot_restore() (part already validated)
# Returns: List with snapshot_id, pre_restore, n_questions, n_repos, skipped

restore_snapshot = function(proj, id, part, question = NULL, repo = NULL, summary = NULL) {
  grading_parts = intersect(part, c("rubric", "marks"))
  if (length(grading_parts) == 1) {
    other = setdiff(c("rubric", "marks"), grading_parts)
    cli::cli_warn(
      "Restoring the {grading_parts} without the {other} can leave selections and rubric items out of step; the default restores both."
    )
  }

  with_database(proj@root, function(conn) {
    snapshot_id = resolve_snapshot_id(conn, id)
    parts = read_snapshot_parts(conn, snapshot_id)
    stats = read_snapshot_stats(conn, snapshot_id)

    if ("template" %in% part && is.null(parts$template)) {
      cli::cli_abort("Snapshot {.val {snapshot_id}} holds no template to restore.")
    }

    snap_questions = vapply(parts$rubric$questions, function(q) as.character(q$name), character(1))
    questions = restrict_names(
      question, snap_questions,
      "Question{?s} {.val {unknown}} not in this snapshot.",
      "The snapshot's questions: {.val {available}}."
    )
    if (!"template" %in% part && length(grading_parts) > 0) {
      current = db_question_names(conn)
      unknown = setdiff(questions, current$names)
      if (length(unknown) > 0) {
        cli::cli_abort(c(
          "Question{?s} {.val {unknown}} of the snapshot {?is/are} not in the current template.",
          "i" = "Include {.val template} in {.arg part} to restore the snapshot's template as well."
        ))
      }
    }

    # The repositories whose marks are restored: the project's (every pair
    # the snapshot did not record is cleared), or the chosen subset
    repos = character(0)
    skipped_repos = character(0)
    narrowed = !is.null(repo)
    if ("marks" %in% part) {
      snap_repos = vapply(parts$marks$repos, function(r) as.character(r$name), character(1))
      universe = project_repo_names(proj)
      if (narrowed) {
        repos = restrict_names(
          repo, union(snap_repos, universe),
          "Repositor{?y/ies} {.val {unknown}} not in this snapshot or under the project's repos directory.",
          "The snapshot's repositories: {.val {available}}."
        )
      } else {
        repos = universe
      }
      skipped_repos = setdiff(if (narrowed) repos else snap_repos, universe)
      repos = intersect(repos, universe)
      if (length(skipped_repos) > 0) {
        cli::cli_warn("Skipping {length(skipped_repos)} repositor{?y/ies} no longer under {.path {proj@repos}}: {.val {skipped_repos}}.")
      }
    }

    DBI::dbBegin(conn)
    tryCatch({
      pre = snapshot_write(
        conn, proj@root, kind = "auto",
        trigger = glue::glue("snapshot_restore('{snapshot_id}')"), summary = summary
      )

      # The grading follows the question ids back to the restored names, so
      # undoing a template change also undoes the renames it carried out
      name_changes = NULL
      if ("template" %in% part) {
        name_changes = reconcile_question_names_conn(conn, db_template_list(conn), parts$template)
        set_metadata(conn, "template", as.character(jsonlite::toJSON(parts$template, auto_unbox = TRUE, null = "null")))
      }

      if ("rubric" %in% part) {
        restore_rubric_conn(conn, parts$rubric, questions)
      }

      if ("marks" %in% part) {
        restore_marks_conn(conn, parts$marks, repos, questions)
      }

      if (all(c("rubric", "marks") %in% part) && length(repos) > 0) {
        expected = stats[stats$question_name %in% questions & stats$assignment_repo %in% repos, , drop = FALSE]
        actual = collect_score_data_conn(conn, questions, repos)
        actual$expected = expected$score[match(
          paste(actual$assignment_repo, actual$question_name, sep = "\x1f"),
          paste(expected$assignment_repo, expected$question_name, sep = "\x1f")
        )]
        # A repository the snapshot recorded no score for (it did not exist
        # then) is restored to nothing, which no cached score describes
        known = actual$assignment_repo %in% expected$assignment_repo
        same = !known |
          (is.na(actual$score) & is.na(actual$expected)) |
          (!is.na(actual$score) & !is.na(actual$expected) & abs(actual$score - actual$expected) < 1e-8)
        if (!all(same)) {
          bad = actual[!same, , drop = FALSE]
          detail = glue::glue("{bad$assignment_repo} / {bad$question_name}: {format_score(bad$expected)} recorded, {format_score(bad$score)} after restore")
          cli::cli_abort(c(
            "The restored scores differ from the scores the snapshot recorded; nothing was changed.",
            stats::setNames(as.character(utils::head(detail, 10)), rep("x", min(10, length(detail))))
          ))
        }
      }

      DBI::dbCommit(conn)
      list(snapshot_id = snapshot_id, pre_restore = if (is.null(pre) || !pre$created) NULL else pre$snapshot_id,
           n_questions = length(questions), n_repos = length(repos), skipped = skipped_repos,
           name_changes = name_changes)
    }, error = function(e) {
      DBI::dbRollback(conn)
      cli::cli_abort("Failed to restore snapshot {snapshot_id}: {conditionMessage(e)}", parent = e, call = NULL)
    })
  })
}

# Write a snapshot's rubric for the chosen questions back into the database
# in place, inside the caller's transaction: each item and group the
# snapshot carries is upserted under its own id with the snapshot's
# position, hotkey, group, points and description, so the recorded
# selections of a surviving item are kept for every repository; items and
# groups absent from the snapshot are deleted (an item's grade rows with it),
# and the question's scoring is set to the snapshot's or removed when it had
# none. An item without an id (not written by this package's snapshots) gets
# a fresh one.
#
# conn: DBI connection object
# rubric: Plain rubric list from the snapshot (rubric_to_list(ids = TRUE) shape)
# questions: Questions to restore

restore_rubric_conn = function(conn, rubric, questions) {
  all_items = load_all_items(conn)
  all_groups = load_all_item_groups(conn)
  taken = all_items$item_id
  taken_groups = all_groups$group_id

  for (question in rubric$questions) {
    question_name = as.character(question$name)
    if (!question_name %in% questions) {
      next
    }
    parts = rubric_question_parts(question)

    # Ids this package did not mint (none in practice) are replaced by fresh ones
    minted = !grepl("^item_[0-9]+$", names(parts$items)) | duplicated(names(parts$items))
    if (any(minted)) {
      fresh = next_item_ids(taken, sum(minted))
      taken = union(taken, fresh)
      names(parts$items)[minted] = fresh
    }
    minted_groups = !grepl("^group_[0-9]+$", names(parts$groups))
    if (any(minted_groups)) {
      fresh = next_item_ids(taken_groups, sum(minted_groups), prefix = "group_")
      taken_groups = union(taken_groups, fresh)
      old = names(parts$groups)[minted_groups]
      names(parts$groups)[minted_groups] = fresh
      parts$items = lapply(parts$items, function(item) {
        if (!is.na(item@group) && item@group %in% old) {
          item@group = fresh[match(item@group, old)]
        }
        item
      })
    }

    existing = all_items$item_id[all_items$question_name == question_name]
    existing_groups = all_groups$group_id[all_groups$question_name == question_name]
    for (item_id in setdiff(existing, names(parts$items))) {
      delete_item_records(conn, question_name, item_id)
    }
    for (group_id in setdiff(existing_groups, names(parts$groups))) {
      delete_item_group_records(conn, question_name, group_id)
    }
    for (group_id in names(parts$groups)) {
      upsert_item_group(conn, question_name, group_id, parts$groups[[group_id]])
    }
    for (i in seq_along(parts$items)) {
      item = parts$items[[i]]
      item@hotkey = position_hotkey(i)
      upsert_items(conn, question_name, names(parts$items)[i], item, position = i)
    }

    if (is.null(question$scoring)) {
      DBI::dbExecute(conn, "DELETE FROM settings WHERE question_name = ?", params = list(question_name))
    } else {
      upsert_settings(conn, question_name, parts$state)
    }
  }
  invisible(NULL)
}

# Write a snapshot's marks for the chosen repositories and questions back
# into the database, inside the caller's transaction: every chosen pair the
# snapshot recorded gets its selections and both comments (a comment the
# snapshot lacks is cleared), and every chosen pair with grading activity the
# snapshot did not record is cleared. Selections are matched by the item ids
# the snapshot carries (by description for a snapshot without them); an id
# the current rubric lacks aborts, since a marks-only restore cannot select
# an item that is not there. Pairs already in the target state are left
# alone, so a restore adds no needless event rows.
#
# conn: DBI connection object
# marks: Plain marks list from the snapshot
# repos: Repositories to restore
# questions: Questions to restore

restore_marks_conn = function(conn, marks, repos, questions) {
  all_items = load_all_items(conn)
  items_of = function(question_name) {
    rows = all_items[all_items$question_name == question_name, , drop = FALSE]
    rows = rows[order(rows$position, rows$hotkey, rows$id), , drop = FALSE]
    rows[c("item_id", "description")]
  }
  item_cache = list()
  question_items = function(question_name) {
    if (is.null(item_cache[[question_name]])) {
      item_cache[[question_name]] <<- items_of(question_name)
    }
    item_cache[[question_name]]
  }

  # The item ids a snapshot entry selects, against the current rubric
  entry_ids = function(repo_name, q) {
    items = question_items(q$name)
    if (!is.null(q$item_ids)) {
      ids = as.character(unlist(q$item_ids))
      unknown = setdiff(ids, items$item_id)
      if (length(unknown) > 0) {
        cli::cli_abort(c(
          "The snapshot's marks for {.val {repo_name}} / {.val {q$name}} select items the current rubric does not have ({.val {unknown}}).",
          "i" = "Restore the rubric as well (the default), or restore a snapshot taken with this rubric."
        ))
      }
      return(ids)
    }
    wanted = as.character(unlist(q$items))
    dupes = unique(items$description[duplicated(items$description)])
    if (length(dupes) > 0 && any(wanted %in% dupes)) {
      cli::cli_abort(
        "Question {.val {q$name}} has rubric items with duplicate descriptions ({.val {dupes}}), so this snapshot's selections cannot be matched by description."
      )
    }
    unknown = setdiff(wanted, items$description)
    if (length(unknown) > 0) {
      cli::cli_abort(c(
        "The snapshot's marks for {.val {repo_name}} / {.val {q$name}} list items not in the current rubric: {.val {unknown}}.",
        "i" = "Restore the rubric as well (the default), or restore a snapshot taken with this rubric."
      ))
    }
    items$item_id[items$description %in% wanted]
  }

  target = list()
  for (repo in marks$repos) {
    if (!repo$name %in% repos) {
      next
    }
    for (q in repo$questions) {
      if (!q$name %in% questions) {
        next
      }
      target[[paste(repo$name, q$name, sep = "\x1f")]] = list(
        repo = repo$name, question = q$name,
        items = entry_ids(repo$name, q),
        comment = if (is.null(q$comment)) "" else as.character(q$comment),
        private_comment = if (is.null(q$private_comment)) "" else as.character(q$private_comment)
      )
    }
  }

  marked = marked_question_pairs_conn(conn)
  marked = marked[marked$assignment_repo %in% repos & marked$question_name %in% questions, , drop = FALSE]
  for (i in seq_len(nrow(marked))) {
    key = paste(marked$assignment_repo[i], marked$question_name[i], sep = "\x1f")
    if (is.null(target[[key]])) {
      target[[key]] = list(
        repo = marked$assignment_repo[i], question = marked$question_name[i],
        items = character(0), comment = "", private_comment = ""
      )
    }
  }
  if (length(target) == 0) {
    return(invisible(NULL))
  }

  # The current state of every target pair, to skip no-op writes
  current = marks_data_from_conn(conn, repo_names = repos, question_names = questions, ids = TRUE)
  current_by = list()
  for (repo in current$repos) {
    for (q in repo$questions) {
      current_by[[paste(repo$name, q$name, sep = "\x1f")]] = q
    }
  }

  plan = list()
  for (key in names(target)) {
    t = target[[key]]
    cur = current_by[[key]]
    cur_items = if (is.null(cur)) character(0) else as.character(cur$item_ids)
    cur_comment = if (is.null(cur) || is.null(cur$comment)) "" else cur$comment
    cur_private = if (is.null(cur) || is.null(cur$private_comment)) "" else cur$private_comment
    same_items = !is.null(cur) && setequal(cur_items, t$items)
    items = question_items(t$question)$item_id
    entry = list(
      repo = t$repo,
      question = t$question,
      selections = if (same_items) NULL else stats::setNames(items %in% t$items, items),
      comment = if (identical(cur_comment, t$comment)) NULL else t$comment,
      private_comment = if (identical(cur_private, t$private_comment)) NULL else t$private_comment
    )
    if (!is.null(entry$selections) || !is.null(entry$comment) || !is.null(entry$private_comment)) {
      plan[[length(plan) + 1]] = entry
    }
  }
  if (length(plan) == 0) {
    return(invisible(NULL))
  }
  apply_marks_import_conn(conn, plan)
  invisible(length(plan))
}
