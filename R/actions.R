# The action log (schema version 6): one row per call of an exported
# markermd function that changed grading state (marks_import(), marks_set(),
# rubric_import(), rubric_item_set(), template_import(),
# project_set(template = ...), snapshot_restore()), recording who made the
# call, what it was and what it touched, so the marking app can show what an
# agent or an outside process did. The grading event rows a call writes carry
# its action_id; the template and rubric it replaced are kept as before /
# after blobs (shared with the snapshot blob store) for a diff. The app's own
# grading (toggles and comments) is not an action: those rows keep a NULL
# action_id and are grouped by idle gaps when listed. The app's bulk
# operations (rubric imports and edits, item and group deletion, restores)
# are recorded as actions with the actor "app".

# Table definitions --------------------------------------------------------

# Create the actions and action_parts tables when missing
#
#   actions       one row per recorded call; created_at is UTC ISO 8601 with
#                 milliseconds, scope a JSON summary of what it touched
#                 (counts, repositories, questions, changed parts)
#   action_parts  the template / rubric a call replaced ("before") and wrote
#                 ("after"), as hashes into snapshot_blobs
#
# conn: DBI connection object

create_action_tables = function(conn) {
  if (!DBI::dbExistsTable(conn, "actions")) {
    DBI::dbExecute(conn, "
      CREATE TABLE actions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        action_id TEXT NOT NULL UNIQUE,
        created_at TEXT NOT NULL,
        created_tz TEXT NOT NULL,
        username TEXT NOT NULL,
        actor TEXT NOT NULL,
        fn TEXT NOT NULL,
        call TEXT,
        summary TEXT,
        scope TEXT NOT NULL,
        snapshot_id TEXT,
        markermd_version TEXT NOT NULL
      )
    ")
  }
  if (!DBI::dbExistsTable(conn, "action_parts")) {
    DBI::dbExecute(conn, "
      CREATE TABLE action_parts (
        action_id TEXT NOT NULL REFERENCES actions(action_id) ON DELETE CASCADE,
        part TEXT NOT NULL CHECK (part IN ('template', 'rubric')),
        phase TEXT NOT NULL CHECK (phase IN ('before', 'after')),
        hash TEXT NOT NULL REFERENCES snapshot_blobs(hash),
        PRIMARY KEY (action_id, part, phase)
      )
    ")
  }
}

# The grading event tables whose rows carry an action_id

action_event_tables = function() c("grades", "comments", "private_comments")

# Schema version 5 -> 6: add the action_id column (and an index on it) to the
# grading event tables, then stamp the new schema version. Existing rows keep
# a NULL action_id, so they list as app grading. Same shape as the earlier
# migrations: a no-op once every column exists, otherwise an immediate
# transaction across the check and the ALTERs.
#
# conn: DBI connection object

migrate_actions = function(conn) {
  has_column = function(table) "action_id" %in% DBI::dbListFields(conn, table)
  stamp = function() {
    stored = get_metadata(conn, "schema_version")
    if (!is.null(stored) && utils::compareVersion(stored, "6") < 0) {
      set_metadata(conn, "schema_version", "6")
    }
  }
  if (all(vapply(action_event_tables(), has_column, logical(1)))) {
    stamp()
    return(invisible(FALSE))
  }

  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  for (table in action_event_tables()) {
    if (!has_column(table)) {
      DBI::dbExecute(conn, glue::glue("ALTER TABLE {table} ADD COLUMN action_id TEXT"))
      DBI::dbExecute(conn, glue::glue("CREATE INDEX IF NOT EXISTS idx_{table}_action ON {table} (action_id)"))
    }
  }
  stamp()
  DBI::dbExecute(conn, "COMMIT")

  invisible(TRUE)
}

# Recording ---------------------------------------------------------------

# Who is making the current call: an explicit MARKERMD_ACTOR wins, then the
# markers the agent CLIs set in the environment of the commands they run
# (Claude Code's CLAUDECODE, Codex's sandbox and thread variables), and
# otherwise "R" (a person at an R prompt or a script)
#
# env: Named character vector of environment variables (for tests)

markermd_actor = function(env = Sys.getenv()) {
  get = function(name) {
    val = env[name]
    if (is.na(val)) "" else as.character(val)
  }
  explicit = trimws(get("MARKERMD_ACTOR"))
  if (nzchar(explicit)) {
    return(explicit)
  }
  if (identical(get("CLAUDECODE"), "1")) {
    return("claude")
  }
  if (any(nzchar(vapply(c("CODEX_SANDBOX", "CODEX_THREAD_ID", "CODEX_SANDBOX_NETWORK_DISABLED"), get, character(1))))) {
    return("codex")
  }
  "R"
}

# The action being recorded in this R process, if any: its id (stamped on
# every grading event row written meanwhile) and the pre-change snapshot it
# took. One at a time; a recorded call made inside another belongs to the
# outer one.
.action_state = new.env(parent = emptyenv())

current_action_id = function() {
  current = .action_state$current
  if (is.null(current)) NA_character_ else current$id
}

# Called by snapshot_before_change() so the action can link the snapshot of
# the state it replaced
#
# snapshot_id: The snapshot id, or NULL

note_action_snapshot = function(snapshot_id) {
  if (!is.null(.action_state$current) && !is.null(snapshot_id)) {
    .action_state$current$snapshot_id = snapshot_id
  }
}

# A fresh action id, like new_snapshot_id(): UTC time plus a random suffix

new_action_id = function() {
  sub("^snap-", "act-", new_snapshot_id())
}

# The template and rubric parts an action keeps, read the way a snapshot
# reads them (the template without its metadata, the rubric with ids)
#
# conn: DBI connection object
# parts: Character vector, a subset of c("template", "rubric")

action_part_lists = function(conn, parts) {
  out = list()
  if ("template" %in% parts) {
    template = db_template_list(conn)
    if (!is.null(template)) {
      template$metadata = NULL
    }
    out["template"] = list(template)
  }
  if ("rubric" %in% parts) {
    question_names = db_question_names(conn)$names
    out$rubric = rubric_to_list(rubric_data_from_conn(conn, question_names, ids = TRUE), ids = TRUE)
  }
  out
}

# A part's comparison key: canonical JSON (the rubric without its ids, so a
# replace that re-mints identical items is no change), NA for an absent part
#
# part: "template" or "rubric"
# x: The part's plain list, or NULL

action_part_key = function(part, x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  canonical_json(if (identical(part, "rubric")) rubric_without_ids(x) else x)
}

# Run `code` as a recorded action. The grading events it writes are stamped
# with the action's id and, when it changed anything, one actions row is
# written once it returns (a call that changed nothing, e.g. an import whose
# pairs were all skipped, is not recorded). A call that errors records
# nothing; its own transaction has rolled its writes back. The code's value
# is returned with its visibility kept, so an invisible result stays
# invisible.
#
# root: Project root
# fn: The exported function's name
# code: The work (evaluated lazily, inside the action)
# call: A short rendering of the call, e.g. "rubric_import(mode = 'append')"
# summary: The caller's summary text, or NULL
# parts: Which of "template" and "rubric" the call may replace
# actor: The actor to record, or NULL for markermd_actor() (the marking app
#   records its own bulk operations as "app")

with_action = function(root, fn, code, call = fn, summary = NULL, parts = character(0), actor = NULL) {
  if (!is.null(.action_state$current)) {
    return(code)
  }
  before = if (length(parts) > 0) with_database(root, function(conn) action_part_lists(conn, parts))
  id = new_action_id()
  .action_state$current = list(id = id, snapshot_id = NULL)
  on.exit({
    .action_state$current = NULL
  }, add = TRUE)

  result = withVisible(code)

  snapshot_id = .action_state$current$snapshot_id
  record_action(root, id, fn, call, summary, before, parts, snapshot_id, actor = actor)
  if (result$visible) result$value else invisible(result$value)
}

# The id of the action most recently recorded in this R process, or NULL
# when the last with_action() changed nothing (the marking app uses it to
# tell its own actions from those noticed in the database)

last_recorded_action_id = function() {
  .action_state$last_recorded
}

# Write the actions row (and its before / after parts) for a finished call,
# in one transaction; nothing when it changed nothing
#
# root: Project root
# id: The action id its events carry
# fn, call, summary: See with_action()
# before: Named list of the parts as they were before the call
# parts: The parts it may have replaced
# snapshot_id: The pre-change snapshot it took, or NULL
# actor: The actor to record, or NULL for markermd_actor()

record_action = function(root, id, fn, call, summary, before, parts, snapshot_id = NULL, actor = NULL) {
  .action_state$last_recorded = NULL
  with_database(root, function(conn) {
    DBI::dbBegin(conn)
    after = if (length(parts) > 0) action_part_lists(conn, parts)
    changed = parts[vapply(parts, function(part) {
      !identical(action_part_key(part, before[[part]]), action_part_key(part, after[[part]]))
    }, logical(1))]
    scope = action_event_scope(conn, id)
    if (scope$n_events == 0 && length(changed) == 0) {
      DBI::dbCommit(conn)
      return(invisible(NULL))
    }
    scope$parts = changed

    blank = function(x) is.null(x) || !nzchar(trimws(x))
    DBI::dbExecute(conn, "
      INSERT INTO actions (action_id, created_at, created_tz, username, actor, fn, call, summary,
                           scope, snapshot_id, markermd_version)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      id, utc_timestamp(), snapshot_timezone(), get_current_username(),
      if (is.null(actor)) markermd_actor() else actor, fn,
      if (blank(call)) NA_character_ else call,
      if (blank(summary)) NA_character_ else summary,
      as.character(jsonlite::toJSON(scope, auto_unbox = TRUE)),
      if (is.null(snapshot_id)) NA_character_ else snapshot_id,
      as.character(utils::packageVersion("markermd"))
    ))

    for (part in changed) {
      sides = list(before = before[[part]], after = after[[part]])
      for (phase in names(sides)) {
        if (is.null(sides[[phase]])) {
          next
        }
        json = canonical_json(sides[[phase]])
        hash = snapshot_hash(json)
        blob = encode_snapshot_blob(json)
        DBI::dbExecute(conn, "
          INSERT OR IGNORE INTO snapshot_blobs (hash, encoding, content, size) VALUES (?, ?, ?, ?)
        ", params = list(hash, blob$encoding, list(blob$content), blob$size))
        DBI::dbExecute(conn, "
          INSERT INTO action_parts (action_id, part, phase, hash) VALUES (?, ?, ?, ?)
        ", params = list(id, part, phase, hash))
      }
    }
    DBI::dbCommit(conn)
    .action_state$last_recorded = id
    invisible(id)
  })
}

# What an action's event rows touched: counts per table, the distinct
# (repository, question) pairs, and the repositories and questions
#
# conn: DBI connection object
# id: Action id

action_event_scope = function(conn, id) {
  counts = list()
  pairs = list()
  for (table in action_event_tables()) {
    rows = DBI::dbGetQuery(
      conn, glue::glue("SELECT question_name, assignment_repo FROM {table} WHERE action_id = ?"),
      params = list(id)
    )
    counts[[table]] = nrow(rows)
    pairs[[table]] = rows
  }
  pairs = unique(do.call(rbind, pairs))
  list(
    n_events = sum(unlist(counts)),
    n_grades = counts$grades,
    n_comments = counts$comments,
    n_private_comments = counts$private_comments,
    n_pairs = nrow(pairs),
    repos = I(sort(unique(pairs$assignment_repo))),
    questions = I(sort(unique(pairs$question_name)))
  )
}

# Reading -------------------------------------------------------------------

# The recorded actions, newest first, with their scope unpacked
#
# conn: DBI connection object
# where: Optional SQL condition on the actions table
# params: Its parameters
# Returns: Data frame with the actions columns plus created_at as POSIXct,
#   n_pairs, n_events and list columns repos, questions and parts

action_rows = function(conn, where = NULL, params = NULL) {
  sql = paste(
    "SELECT * FROM actions",
    if (!is.null(where)) paste("WHERE", where),
    "ORDER BY created_at DESC, id DESC"
  )
  rows = DBI::dbGetQuery(conn, sql, params = params)
  scopes = lapply(rows$scope, jsonlite::fromJSON, simplifyVector = TRUE)
  field = function(name, f) lapply(scopes, function(s) f(s[[name]]))
  rows$created_at = parse_utc_timestamp(rows$created_at)
  count = function(x) if (is.null(x)) 0L else as.integer(x)
  rows$n_pairs = vapply(scopes, function(s) count(s$n_pairs), integer(1))
  rows$n_events = vapply(scopes, function(s) count(s$n_events), integer(1))
  rows$repos = field("repos", function(x) as.character(unlist(x)))
  rows$questions = field("questions", function(x) as.character(unlist(x)))
  rows$parts = field("parts", function(x) as.character(unlist(x)))
  rows$scope = NULL
  rows
}

# The app's own grading events (no action_id), grouped into sessions of
# activity: a new group starts when the user changes or after `gap` seconds
# without an event
#
# conn: DBI connection object
# gap: Idle gap in seconds
# Returns: List with groups (a data frame: group_id, started_at, created_at
#   (the last event), username, n_events, n_pairs and list columns repos and
#   questions) and events (every app event with its table, id and group_id)

app_event_groups = function(conn, gap = 15 * 60) {
  events = do.call(rbind, lapply(action_event_tables(), function(table) {
    rows = DBI::dbGetQuery(conn, glue::glue("
      SELECT id, question_name, assignment_repo, timestamp, username
      FROM {table} WHERE action_id IS NULL
    "))
    rows$table = rep(table, nrow(rows))
    rows
  }))
  empty_groups = data.frame(
    group_id = character(0), started_at = as.POSIXct(character(0)), created_at = as.POSIXct(character(0)),
    username = character(0), n_events = integer(0), n_pairs = integer(0), stringsAsFactors = FALSE
  )
  if (nrow(events) == 0) {
    empty_groups$repos = list()
    empty_groups$questions = list()
    events$group_id = character(0)
    return(list(groups = empty_groups, events = events))
  }

  events$time = parse_event_timestamp(events$timestamp)
  events = events[order(events$username, events$time, events$table, events$id), , drop = FALSE]
  step = c(Inf, as.numeric(diff(events$time), units = "secs"))
  new_user = c(TRUE, events$username[-1] != events$username[-nrow(events)])
  starts = new_user | is.na(step) | step > gap
  group_index = cumsum(starts)
  first = events[starts, , drop = FALSE]
  ids = paste0("app-", format(first$time, "%Y%m%dT%H%M%S"), "-", first$username)
  events$group_id = ids[group_index]

  groups = do.call(rbind, lapply(split(events, factor(events$group_id, levels = ids)), function(g) {
    pairs = unique(g[c("assignment_repo", "question_name")])
    data.frame(
      group_id = g$group_id[1], started_at = min(g$time), created_at = max(g$time),
      username = g$username[1], n_events = nrow(g), n_pairs = nrow(pairs),
      repos = I(list(sort(unique(pairs$assignment_repo)))),
      questions = I(list(sort(unique(pairs$question_name)))),
      stringsAsFactors = FALSE
    )
  }))
  rownames(groups) = NULL
  list(groups = groups, events = events)
}

# The activity log: recorded actions and the app's grouped grading, newest
# first, in one data frame
#
# conn: DBI connection object
# gap: Idle gap in seconds for grouping app events
# app: Whether to include the app's grouped grading

activity_rows = function(conn, gap = 15 * 60, app = TRUE) {
  actions = action_rows(conn)
  out = data.frame(
    id = actions$action_id,
    kind = rep("action", nrow(actions)),
    created_at = actions$created_at,
    started_at = actions$created_at,
    actor = actions$actor,
    username = actions$username,
    fn = actions$fn,
    call = actions$call,
    summary = actions$summary,
    n_pairs = actions$n_pairs,
    n_events = actions$n_events,
    snapshot_id = actions$snapshot_id,
    stringsAsFactors = FALSE
  )
  out$repos = actions$repos
  out$questions = actions$questions
  out$parts = actions$parts

  if (app) {
    groups = app_event_groups(conn, gap)$groups
    if (nrow(groups) > 0) {
      app_rows = data.frame(
        id = groups$group_id,
        kind = "app",
        created_at = groups$created_at,
        started_at = groups$started_at,
        actor = "app",
        username = groups$username,
        fn = NA_character_,
        call = NA_character_,
        summary = NA_character_,
        n_pairs = groups$n_pairs,
        n_events = groups$n_events,
        snapshot_id = NA_character_,
        stringsAsFactors = FALSE
      )
      app_rows$repos = groups$repos
      app_rows$questions = groups$questions
      app_rows$parts = rep(list(character(0)), nrow(groups))
      out = rbind(out, app_rows)
    }
  }
  out = out[order(out$created_at, decreasing = TRUE), , drop = FALSE]
  rownames(out) = NULL
  out
}

# Marks changes from a set of grading event rows, in diff_marks() shape: per
# (repository, question) pair, the items deselected and selected (each item's
# state before the first of the rows against its state after the last), and
# the public and private comments before and after
#
# conn: DBI connection object
# ids: Named list of event row ids per table (grades, comments,
#   private_comments)
# Returns: Data frame with repo, question, change, before and after

event_marks_diff = function(conn, ids) {
  out = diff_marks(list(), list())
  key = function(df, cols) do.call(paste, c(unname(as.list(df[cols])), sep = "\x1f"))
  pair_rows = function(table, set) {
    if (length(set) == 0) {
      return(NULL)
    }
    touched = DBI::dbGetQuery(
      conn, glue::glue("SELECT DISTINCT question_id, assignment_repo FROM {table} WHERE id IN ({paste(set, collapse = ',')})")
    )
    rows = DBI::dbGetQuery(
      conn, glue::glue("SELECT * FROM {table} WHERE question_id IN ({paste(as.integer(unique(touched$question_id)), collapse = ',')}) AND assignment_repo IN ({paste(DBI::dbQuoteString(conn, unique(touched$assignment_repo)), collapse = ',')}) ORDER BY id")
    )
    rows[key(rows, c("assignment_repo", "question_id")) %in% key(touched, c("assignment_repo", "question_id")), , drop = FALSE]
  }
  push = function(repo, question, change, before, after) {
    out[nrow(out) + 1, ] <<- list(repo, question, change, before, after)
  }

  grades = pair_rows("grades", ids$grades)
  if (!is.null(grades)) {
    items = DBI::dbGetQuery(conn, "SELECT question_id, item_id, description FROM items")
    describe = function(question, item_ids) {
      hit = items$description[match(paste(question, item_ids), paste(items$question_id, items$item_id))]
      ifelse(is.na(hit), item_ids, hit)
    }
    grades$k = key(grades, c("assignment_repo", "question_id", "item_id"))
    in_set = grades$id %in% ids$grades
    for (pk in unique(key(grades[in_set, , drop = FALSE], c("assignment_repo", "question_id")))) {
      rows = grades[key(grades, c("assignment_repo", "question_id")) == pk, , drop = FALSE]
      set_rows = rows[rows$id %in% ids$grades, , drop = FALSE]
      removed = character(0)
      added = character(0)
      for (k in unique(set_rows$k)) {
        mine = set_rows[set_rows$k == k, , drop = FALSE]
        prior = rows[rows$k == k & rows$id < min(mine$id), , drop = FALSE]
        was = nrow(prior) > 0 && isTRUE(as.logical(prior$selected[nrow(prior)]))
        now = isTRUE(as.logical(mine$selected[nrow(mine)]))
        item = describe(mine$question_id[1], mine$item_id[1])
        if (was && !now) removed = c(removed, item)
        if (!was && now) added = c(added, item)
      }
      if (length(removed) > 0 || length(added) > 0) {
        push(set_rows$assignment_repo[1], set_rows$question_name[1], "selection",
             paste(removed, collapse = "; "), paste(added, collapse = "; "))
      }
    }
  }

  for (table in c("comments", "private_comments")) {
    rows = pair_rows(table, ids[[table]])
    if (is.null(rows)) {
      next
    }
    change = if (identical(table, "comments")) "comment" else "private_comment"
    rows$k = key(rows, c("assignment_repo", "question_id"))
    for (pk in unique(rows$k[rows$id %in% ids[[table]]])) {
      pr = rows[rows$k == pk, , drop = FALSE]
      mine = pr[pr$id %in% ids[[table]], , drop = FALSE]
      prior = pr[pr$id < min(mine$id), , drop = FALSE]
      was = if (nrow(prior) > 0) prior$comment_text[nrow(prior)] else ""
      now = mine$comment_text[nrow(mine)]
      if (!identical(was, now)) {
        push(mine$assignment_repo[1], mine$question_name[1], change, was, now)
      }
    }
  }

  out = out[order(out$repo, out$question, out$change), , drop = FALSE]
  rownames(out) = NULL
  out
}

# An activity entry's changes as a markermd_snapshot_diff (with no score
# columns): the marks from its event rows and, for an action that replaced
# the template or rubric, the diff of the parts it kept
#
# conn: DBI connection object
# id: An action id or an app group id (from activity_rows())
# gap: Idle gap used to group app events

activity_diff_conn = function(conn, id, gap = 15 * 60) {
  empty_rubric = diff_rubric(list(questions = list()), list(questions = list()))$changes
  rubric = empty_rubric
  template = diff_template(NULL, NULL)

  if (startsWith(id, "app-")) {
    events = app_event_groups(conn, gap)$events
    events = events[events$group_id == id, , drop = FALSE]
    if (nrow(events) == 0) {
      cli::cli_abort("No activity entry {.val {id}}.")
    }
    ids = lapply(stats::setNames(action_event_tables(), action_event_tables()), function(t) events$id[events$table == t])
    label = paste0("grading in the app (", events$username[1], ")")
  } else {
    row = action_rows(conn, "action_id = ?", list(id))
    if (nrow(row) == 0) {
      cli::cli_abort("No activity entry {.val {id}}.")
    }
    ids = lapply(stats::setNames(action_event_tables(), action_event_tables()), function(t) {
      DBI::dbGetQuery(conn, glue::glue("SELECT id FROM {t} WHERE action_id = ?"), params = list(id))$id
    })
    parts = DBI::dbGetQuery(conn, "
      SELECT p.part, p.phase, b.encoding, b.content
      FROM action_parts p JOIN snapshot_blobs b ON b.hash = p.hash
      WHERE p.action_id = ?
    ", params = list(id))
    side = function(part, phase, absent = NULL) {
      r = parts[parts$part == part & parts$phase == phase, , drop = FALSE]
      if (nrow(r) == 0) absent else parse_json(decode_snapshot_blob(r))
    }
    if ("rubric" %in% parts$part) {
      empty = list(questions = list())
      rubric = diff_rubric(side("rubric", "before", empty), side("rubric", "after", empty))$changes
    }
    if ("template" %in% parts$part) {
      template = diff_template(side("template", "before"), side("template", "after"))
    }
    label = paste0(row$fn, " by ", row$actor)
  }

  markermd_snapshot_diff(
    from = id, to = id, from_label = label, to_label = label,
    rubric = rubric,
    marks = event_marks_diff(conn, ids),
    scores = data.frame(
      repo = character(0), question = character(0), before = numeric(0),
      after = numeric(0), change = numeric(0), stringsAsFactors = FALSE
    ),
    totals = data.frame(
      repo = character(0), before = numeric(0), after = numeric(0), change = numeric(0),
      stringsAsFactors = FALSE
    ),
    template = template
  )
}

# Exported readers -----------------------------------------------------------

#' The activity log of a project
#'
#' Lists what has changed the project's grading state: every call of a
#' markermd function that changed it ([marks_import()], [marks_set()],
#' [rubric_import()], [rubric_item_set()], [template_import()],
#' `project_set(template = ...)`, [snapshot_restore()]), the bulk operations
#' made in [mark()] (rubric imports and edits, item and group deletions,
#' restores, recorded with the actor `"app"`), and the grading done in
#' [mark()], grouped into sessions of activity. Each recorded call notes who
#' made it: the `MARKERMD_ACTOR` environment variable when set, `"claude"`
#' or `"codex"` when the call ran under one of those agent CLIs, and `"R"`
#' otherwise. Grading recorded before the log existed lists as app grading.
#'
#' Use [activity_detail()] for what one entry changed.
#'
#' @param project Path to the project directory. Defaults to the working
#'   directory.
#' @param since Optional date-time (or something [as.POSIXct()] accepts):
#'   only entries whose latest change is at or after it.
#' @param actor Optional character vector: only entries by these actors
#'   (`"app"` for grading in [mark()]).
#' @param app Whether to include the grading done in [mark()].
#' @param gap Seconds without activity that end one app grading session.
#'
#' @return A data frame, newest first, with columns `id`, `kind` (`"action"`
#'   or `"app"`), `created_at` (the call, or an app session's last change),
#'   `started_at`, `actor`, `username`, `fn`, `call`, `summary`, `n_pairs`,
#'   `n_events`, `snapshot_id` (the snapshot taken before the change, if
#'   any) and the list columns `repos`, `questions` and `parts` (the
#'   template / rubric parts an action changed).
#' @seealso [activity_detail()], [snapshot_list()]
#' @export
#'
#' @examples
#' \dontrun{
#' project_activity("hw01", actor = "claude")
#' }
project_activity = function(project = ".", since = NULL, actor = NULL, app = TRUE, gap = 15 * 60) {
  proj = project_config(project)
  rows = with_database(proj@root, function(conn) activity_rows(conn, gap = gap, app = app))
  if (!is.null(since)) {
    rows = rows[rows$created_at >= as.POSIXct(since), , drop = FALSE]
  }
  if (!is.null(actor)) {
    rows = rows[rows$actor %in% actor, , drop = FALSE]
  }
  rownames(rows) = NULL
  rows
}

#' What one activity entry changed
#'
#' The changes of one entry of [project_activity()]: for a recorded call or
#' an app grading session, the item selections and the public and private
#' comments it changed, per repository and question (each against the state
#' just before it); for a call that replaced the rubric or the template, the
#' rubric and template changes too.
#'
#' @param id An entry id from [project_activity()].
#' @inheritParams project_activity
#'
#' @return A [markermd_snapshot_diff] object whose `rubric`, `marks` and
#'   `template` data frames hold the changes (its score data frames are
#'   empty).
#' @seealso [project_activity()]
#' @export
#'
#' @examples
#' \dontrun{
#' latest = project_activity("hw01", app = FALSE)$id[1]
#' activity_detail(latest, "hw01")
#' }
activity_detail = function(id, project = ".", gap = 15 * 60) {
  if (!is.character(id) || length(id) != 1 || !nzchar(id)) {
    cli::cli_abort("{.arg id} must be a single activity entry id.")
  }
  proj = project_config(project)
  with_database(proj@root, function(conn) activity_diff_conn(conn, id, gap = gap))
}

# Agent badges --------------------------------------------------------------

# The items of one (repository, question) pair that a recorded call selected
# and nobody has touched in the app since: the latest grade row of each such
# item is selected and carries an action_id, and the pair has no app grade
# row after it (grade ids share one sequence) and no app comment or private
# comment saved later (compared by timestamp, the comment tables having their
# own id sequences)
#
# conn: DBI connection object
# question_name: Question name
# assignment_repo: Repository name
# Returns: Named list keyed by item id of lists with actor, fn and timestamp

pair_agent_badges = function(conn, question_name, assignment_repo) {
  latest = latest_rows(
    conn, "grades", "item_id",
    where = "question_id = ? AND assignment_repo = ?",
    params = list(question_id_conn(conn, question_name, create = FALSE), assignment_repo)
  )
  agent = latest[latest$selected == 1 & !is.na(latest$action_id), , drop = FALSE]
  if (nrow(agent) == 0) {
    return(list())
  }
  pair = list(question_id_conn(conn, question_name, create = FALSE), assignment_repo)
  last_app_grade = DBI::dbGetQuery(conn, "
    SELECT MAX(id) AS id FROM grades
    WHERE question_id = ? AND assignment_repo = ? AND action_id IS NULL
  ", params = pair)$id
  last_app_comment = DBI::dbGetQuery(conn, "
    SELECT MAX(timestamp) AS timestamp FROM (
      SELECT timestamp FROM comments WHERE question_id = ? AND assignment_repo = ? AND action_id IS NULL
      UNION ALL
      SELECT timestamp FROM private_comments WHERE question_id = ? AND assignment_repo = ? AND action_id IS NULL
    )
  ", params = c(pair, pair))$timestamp

  touched = (!is.na(last_app_grade) & last_app_grade > agent$id) |
    (!is.na(last_app_comment) & parse_event_timestamp(last_app_comment) > parse_event_timestamp(agent$timestamp))
  agent = agent[!touched, , drop = FALSE]
  if (nrow(agent) == 0) {
    return(list())
  }

  who = DBI::dbGetQuery(conn, glue::glue(
    "SELECT action_id, actor, fn FROM actions WHERE action_id IN ({paste(DBI::dbQuoteString(conn, unique(agent$action_id)), collapse = ',')})"
  ))
  hit = match(agent$action_id, who$action_id)
  # A restore made in the app is the grader's own doing, not an agent's
  agent = agent[is.na(hit) | who$actor[hit] != "app", , drop = FALSE]
  hit = match(agent$action_id, who$action_id)
  if (nrow(agent) == 0) {
    return(list())
  }
  stats::setNames(lapply(seq_len(nrow(agent)), function(i) {
    list(
      actor = if (is.na(hit[i])) "a markermd call" else who$actor[hit[i]],
      fn = if (is.na(hit[i])) NA_character_ else who$fn[hit[i]],
      timestamp = agent$timestamp[i]
    )
  }), agent$item_id)
}

# The app side --------------------------------------------------------------

# Poll the project database for commits made by any other connection (an
# agent's import, another app session, a script) and call `on_change` when
# one lands. PRAGMA data_version on a long-lived read-only connection changes
# exactly then and reads no table, so a check costs next to nothing. The loop
# runs on later timers, outside the reactive graph, so an idle app never
# looks busy; it stops and closes its connection when the session ends.
# Keep on_change cheap (bump a reactive value): errors in a timer callback
# are not caught by Shiny.
#
# The session's own writes use other connections, so they change the version
# too. The returned absorb() is called right after such a write: it takes the
# new version as seen, so the next check does not call on_change (and the
# app does not query the action log) for a change it made itself. A commit by
# another connection landing in the instant between the write and absorb()
# would be absorbed with it, so whenever something was absorbed on_change
# also runs once `sweep` seconds later, which catches it.
#
# root: Project root
# session: The Shiny session
# on_change: Zero-argument function
# interval: Seconds between checks
# sweep: Seconds after an absorbed change before on_change runs anyway
# Returns: Invisibly, list(absorb), absorb a zero-argument function

watch_project_db = function(root, session, on_change, interval = 1, sweep = 30) {
  conn = initialize_database(root)
  version = function() DBI::dbGetQuery(conn, "PRAGMA data_version")[[1]]
  last = version()
  active = TRUE
  absorbed_at = NULL

  absorb = function() {
    if (!active) {
      return(invisible(NULL))
    }
    current = version()
    if (!identical(current, last)) {
      last <<- current
      if (is.null(absorbed_at)) absorbed_at <<- Sys.time()
    }
    invisible(NULL)
  }

  tick = function() {
    if (!active) {
      return(invisible(NULL))
    }
    current = version()
    if (!identical(current, last)) {
      last <<- current
      absorbed_at <<- NULL
      on_change()
    } else if (!is.null(absorbed_at) && difftime(Sys.time(), absorbed_at, units = "secs") >= sweep) {
      absorbed_at <<- NULL
      on_change()
    }
    later::later(tick, interval)
  }
  later::later(tick, interval)

  session$onSessionEnded(function() {
    active <<- FALSE
    close_database(conn)
  })
  invisible(list(absorb = absorb))
}

# The newest action row id, 0 when there is none
#
# conn: DBI connection object

latest_action_row_id = function(conn) {
  id = DBI::dbGetQuery(conn, "SELECT MAX(id) AS id FROM actions")$id
  if (is.na(id)) 0L else as.integer(id)
}

# The notification text for an action noticed by the app
#
# row: One row of action_rows()

action_notice_text = function(row) {
  scope = activity_scope_text(row)
  paste0(
    row$actor, ": ", row$fn,
    if (nzchar(scope)) paste0(" (", scope, ")"),
    if (!is.na(row$summary)) paste0(" \u2013 ", one_line_text(row$summary, 80))
  )
}
