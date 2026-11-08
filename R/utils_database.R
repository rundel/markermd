# Database utility functions for markermd SQLite storage
#
# collection_path: Path to collection directory

# Current grading-database schema version. Bumped whenever the SQLite schema
# changes incompatibly. assert_db_compatible() rejects a database written by a
# newer markermd than this.
#
# History:
#   1: initial schema
#   2: items.position (explicit display order; see migrate_items_position())
#   3: items.group_id and the item_groups table (rubric item groups; see
#      migrate_items_group())
#   4: settings.optional (optional questions; see migrate_settings_optional())
#   5: the snapshot tables (snapshots, snapshot_blobs, snapshot_parts,
#      snapshot_notes, snapshot_stats; see migrate_snapshots() and
#      snapshots.R)
#   6: the action log (actions, action_parts) and the action_id column on
#      grades, comments and private_comments (see migrate_actions() and
#      actions.R)

markermd_db_version = function() "6"

# ORDER BY clause giving a question's rubric items in display order. position
# is authoritative; a row without one (written before schema version 2 and
# not yet migrated, or inserted directly) falls back to the legacy rule those
# versions used, hotkey slots first then insertion order, so it still lands
# where it used to be displayed rather than sorting first as a bare NULL.

items_display_order_sql = "(position IS NULL), position, (hotkey IS NULL), hotkey, id"

# Reject a database written by a newer markermd than this one.
#
# conn: DBI connection object

assert_db_compatible = function(conn) {
  stored = get_metadata(conn, "schema_version")
  if (!is.null(stored) && utils::compareVersion(stored, markermd_db_version()) > 0) {
    cli::cli_abort(c(
      "This grading database requires a newer version of markermd.",
      "x" = "Database schema_version is {stored}; this markermd supports up to {markermd_db_version()}.",
      "i" = "Please upgrade the markermd package."
    ))
  }
}

get_database_path = function(collection_path) {
  cache_dir = file.path(path.expand(collection_path), ".markermd")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  # Normalize the directory (which exists) rather than the sqlite file (which
  # may not yet): on macOS normalizePath(mustWork = FALSE) leaves a missing
  # file's path unresolved but resolves it once the file exists, which would
  # give the same database two different .initialized_dbs cache keys
  file.path(normalizePath(cache_dir), "markermd.sqlite")
}

# Initialize database connection and create tables if needed
#
# collection_path: Path to collection directory

# Database files whose tables have already been verified this session, keyed
# by database path. Every keystroke-level save opens a fresh connection, so
# re-probing all six tables each time is measurable overhead; a database file
# that was deleted and re-created at the same path is detected via the
# pre-connect existence check and re-initialized.
.initialized_dbs = new.env(parent = emptyenv())

initialize_database = function(collection_path) {
  db_path = get_database_path(collection_path)
  fresh = !file.exists(db_path)

  # Create connection
  conn = DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Enable foreign key constraints
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")

  # Wait up to 5s for a competing writer (e.g. a grading skill running while
  # mark() is open) instead of failing immediately with "database is locked".
  DBI::dbExecute(conn, "PRAGMA busy_timeout = 5000")

  # Write-ahead logging, so the app's reads and a concurrent import by an
  # agent do not block each other. Persistent once set; backups use VACUUM
  # INTO, which reads through the log.
  DBI::dbGetQuery(conn, "PRAGMA journal_mode = WAL")

  if (fresh || !isTRUE(.initialized_dbs[[db_path]])) {
    create_tables_if_needed(conn, collection_path)
    .initialized_dbs[[db_path]] = TRUE
  }

  # Always re-checked (one metadata read): a concurrent skill run may have
  # upgraded the schema while this session held the table cache
  assert_db_compatible(conn)

  return(conn)
}

# Create database tables if they don't exist
#
# conn: DBI connection object
# collection_path: Project root the database belongs to, for the baseline
#   snapshot the schema 5 migration takes (NULL skips it)

create_tables_if_needed = function(conn, collection_path = NULL) {
  # Settings table - stores markermd_grade_state properties
  if (!DBI::dbExistsTable(conn, "settings")) {
    DBI::dbExecute(conn, "
      CREATE TABLE settings (
        question_name TEXT PRIMARY KEY,
        current_score REAL NOT NULL,
        total_score REAL NOT NULL,
        grading_mode TEXT NOT NULL CHECK (grading_mode IN ('positive', 'negative')),
        bound_above_zero INTEGER NOT NULL CHECK (bound_above_zero IN (0, 1)),
        bound_below_max INTEGER NOT NULL CHECK (bound_below_max IN (0, 1)),
        optional INTEGER NOT NULL DEFAULT 0 CHECK (optional IN (0, 1)),
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )
    ")
  }
  
  # Items table - stores markermd_rubric_item properties (without selected
  # state). position is the 1-based display order within the question and the
  # only ordering key; hotkey is derived from it (position <= 10) and kept in
  # the row so the table reads sensibly by hand. group_id names the item's
  # group in item_groups (NULL = ungrouped); a group's members are kept
  # contiguous in position order (normalize_item_order()), and the group's
  # display position is its first member's, so no separate group position
  # is stored.
  if (!DBI::dbExistsTable(conn, "items")) {
    DBI::dbExecute(conn, "
      CREATE TABLE items (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_name TEXT NOT NULL,
        item_id TEXT NOT NULL,
        hotkey INTEGER,
        position INTEGER,
        group_id TEXT,
        points REAL NOT NULL,
        description TEXT NOT NULL,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        UNIQUE(question_name, item_id)
      )
    ")
  }

  # Item groups table - stores markermd_rubric_group properties: a named run
  # of a question's items whose combined points are bounded by floor (<= 0)
  # and ceiling (>= 0), optionally exclusive (at most one member selected).
  # Labels are unique within a question because the rubric import matches
  # groups by label.
  if (!DBI::dbExistsTable(conn, "item_groups")) {
    DBI::dbExecute(conn, "
      CREATE TABLE item_groups (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_name TEXT NOT NULL,
        group_id TEXT NOT NULL,
        label TEXT NOT NULL,
        floor REAL CHECK (floor IS NULL OR floor <= 0),
        ceiling REAL CHECK (ceiling IS NULL OR ceiling >= 0),
        exclusive INTEGER NOT NULL DEFAULT 0 CHECK (exclusive IN (0, 1)),
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        UNIQUE (question_name, group_id),
        UNIQUE (question_name, label)
      )
    ")
  }
  
  # Grades table - stores grading events (selection/deselection of rubric items)
  if (!DBI::dbExistsTable(conn, "grades")) {
    DBI::dbExecute(conn, "
      CREATE TABLE grades (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_name TEXT NOT NULL,
        assignment_repo TEXT NOT NULL,
        item_id TEXT NOT NULL,
        selected INTEGER NOT NULL CHECK (selected IN (0, 1)),
        timestamp TEXT NOT NULL,
        username TEXT NOT NULL
      )
    ")
    
    # Create index for efficient querying of most recent grades
    DBI::dbExecute(conn, "
      CREATE INDEX idx_grades_lookup ON grades (question_name, assignment_repo, item_id, timestamp)
    ")
  }
  
  # Comments table - stores per-question/repo comments (event log; the most
  # recent row per pair is the current comment)
  if (!DBI::dbExistsTable(conn, "comments")) {
    DBI::dbExecute(conn, "
      CREATE TABLE comments (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_name TEXT NOT NULL,
        assignment_repo TEXT NOT NULL,
        comment_text TEXT NOT NULL,
        timestamp TEXT NOT NULL,
        username TEXT NOT NULL
      )
    ")
    
    # Create index for efficient querying of most recent comments
    DBI::dbExecute(conn, "
      CREATE INDEX idx_comments_lookup ON comments (question_name, assignment_repo, timestamp)
    ")
  }

  # Private comments table - grader/skill-internal notes per question/repo
  # pair, never shown to students (the comments table is the student-facing
  # channel). Same event-log semantics as comments. Created here on first
  # touch, so existing databases gain it without migration.
  if (!DBI::dbExistsTable(conn, "private_comments")) {
    DBI::dbExecute(conn, "
      CREATE TABLE private_comments (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_name TEXT NOT NULL,
        assignment_repo TEXT NOT NULL,
        comment_text TEXT NOT NULL,
        timestamp TEXT NOT NULL,
        username TEXT NOT NULL
      )
    ")

    DBI::dbExecute(conn, "
      CREATE INDEX idx_private_comments_lookup ON private_comments (question_name, assignment_repo, timestamp)
    ")
  }

  # Metadata table - key/value store for project-level singletons (the grading
  # template, the schema version, ...). The database is the canonical store for
  # these; YAML is an optional import/export format.
  if (!DBI::dbExistsTable(conn, "metadata")) {
    DBI::dbExecute(conn, "
      CREATE TABLE metadata (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )
    ")

    set_metadata(conn, "schema_version", markermd_db_version())
  }

  snapshots_are_new = create_snapshot_tables(conn)

  migrate_items_position(conn)
  migrate_items_group(conn)
  migrate_settings_optional(conn)
  migrate_snapshots(conn, collection_path, baseline = snapshots_are_new)
  create_action_tables(conn)
  migrate_actions(conn)
}

# Snapshot tables (schema version 5): named, timestamped, annotated copies of
# the grading state (the template, rubric and marks exchange documents) kept
# inside the project database. See snapshots.R for the writer and readers.
#
#   snapshots       one row per snapshot; created_at is UTC ISO 8601 with
#                   milliseconds and created_tz the creator's zone (for
#                   display); event_id is the grades log's max id at creation,
#                   so the events recorded since are countable
#   snapshot_blobs  content-addressed payloads (canonical JSON, gzip when
#                   smaller), shared by every snapshot whose part is identical
#                   (most snapshots change only the marks); a blob is deleted
#                   once no part references it (gc_snapshot_blobs())
#   snapshot_parts  which blob holds each of a snapshot's parts
#   snapshot_notes  append-only log of notes and keyed metadata per snapshot
#   snapshot_stats  the per-repository, per-question scores at creation,
#                   with "(total)" rows, so listing and comparing snapshots
#                   need no re-scoring
#
# conn: DBI connection object
# Returns: TRUE when the snapshots table was created by this call (the
#   database had no snapshot tables before), FALSE otherwise

create_snapshot_tables = function(conn) {
  created = !DBI::dbExistsTable(conn, "snapshots")
  if (created) {
    DBI::dbExecute(conn, "
      CREATE TABLE snapshots (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        snapshot_id TEXT NOT NULL UNIQUE,
        created_at TEXT NOT NULL,
        created_tz TEXT NOT NULL,
        username TEXT NOT NULL,
        kind TEXT NOT NULL CHECK (kind IN ('manual', 'auto')),
        trigger TEXT,
        label TEXT,
        summary TEXT,
        parent_snapshot_id TEXT,
        schema_version TEXT NOT NULL,
        markermd_version TEXT NOT NULL,
        content_hash TEXT NOT NULL,
        event_id INTEGER,
        pinned INTEGER NOT NULL DEFAULT 0 CHECK (pinned IN (0, 1))
      )
    ")
  }

  if (!DBI::dbExistsTable(conn, "snapshot_blobs")) {
    DBI::dbExecute(conn, "
      CREATE TABLE snapshot_blobs (
        hash TEXT PRIMARY KEY,
        encoding TEXT NOT NULL CHECK (encoding IN ('json', 'json+gzip')),
        content BLOB NOT NULL,
        size INTEGER NOT NULL
      )
    ")
  }

  if (!DBI::dbExistsTable(conn, "snapshot_parts")) {
    DBI::dbExecute(conn, "
      CREATE TABLE snapshot_parts (
        snapshot_id TEXT NOT NULL REFERENCES snapshots(snapshot_id) ON DELETE CASCADE,
        part TEXT NOT NULL CHECK (part IN ('template', 'rubric', 'marks')),
        format_version TEXT NOT NULL,
        hash TEXT NOT NULL REFERENCES snapshot_blobs(hash),
        PRIMARY KEY (snapshot_id, part)
      )
    ")
  }

  if (!DBI::dbExistsTable(conn, "snapshot_notes")) {
    DBI::dbExecute(conn, "
      CREATE TABLE snapshot_notes (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        snapshot_id TEXT NOT NULL REFERENCES snapshots(snapshot_id) ON DELETE CASCADE,
        timestamp TEXT NOT NULL,
        username TEXT NOT NULL,
        key TEXT,
        value TEXT NOT NULL
      )
    ")
    DBI::dbExecute(conn, "
      CREATE INDEX idx_snapshot_notes_lookup ON snapshot_notes (snapshot_id, id)
    ")
  }

  if (!DBI::dbExistsTable(conn, "snapshot_stats")) {
    DBI::dbExecute(conn, "
      CREATE TABLE snapshot_stats (
        snapshot_id TEXT NOT NULL REFERENCES snapshots(snapshot_id) ON DELETE CASCADE,
        assignment_repo TEXT NOT NULL,
        question_name TEXT NOT NULL,
        score REAL,
        PRIMARY KEY (snapshot_id, assignment_repo, question_name)
      )
    ")
  }

  created
}

# Schema version 4 -> 5: stamp the new schema version (the snapshot tables
# themselves are created on first touch by create_snapshot_tables()) and,
# when the snapshot tables have just been added to a database that already
# holds grading state, take a baseline automatic snapshot, so every existing
# project's history starts from the state it had when snapshots arrived. A
# fresh database is stamped 5 by create_tables_if_needed() and has nothing
# to snapshot; a legacy database with no schema version gets its baseline
# all the same. Same shape as the earlier migrations: runs on first touch
# per session, is a no-op once stamped and baselined, and holds an
# immediate transaction across the check and the writes.
#
# conn: DBI connection object
# collection_path: Project root, for the baseline snapshot's cached scores
#   (NULL takes no baseline)
# baseline: Whether the snapshot tables were created just now

migrate_snapshots = function(conn, collection_path = NULL, baseline = FALSE) {
  stored = get_metadata(conn, "schema_version")
  needs_stamp = !is.null(stored) && utils::compareVersion(stored, "5") < 0
  needs_baseline = baseline && !is.null(collection_path)
  if (!needs_stamp && !needs_baseline) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  if (needs_baseline && snapshot_has_state(conn) && is.null(latest_snapshot_row(conn))) {
    snapshot_write(
      conn, collection_path, kind = "auto",
      trigger = "migrate to schema 5",
      summary = "Baseline taken when the grading database gained snapshots (schema version 5)."
    )
  }
  stored = get_metadata(conn, "schema_version")
  if (!is.null(stored) && utils::compareVersion(stored, "5") < 0) {
    set_metadata(conn, "schema_version", "5")
  }
  DBI::dbExecute(conn, "COMMIT")

  invisible(TRUE)
}

# Schema version 1 -> 2: add items.position to a database created by an
# earlier markermd and backfill it from the order those versions displayed
# (hotkey slots first, un-hotkeyed items by insertion id), then stamp the new
# schema version. Before this column existed, items past the tenth all shared
# a NULL hotkey, so any reordering among them was lost on reload; the backfill
# freezes whatever order the database currently shows. Runs on first touch of
# each database per session (via create_tables_if_needed) and is a no-op once
# the column exists. The check and the ALTER run under an immediate
# transaction so two processes opening the same legacy database cannot both
# try to add the column.
#
# conn: DBI connection object

migrate_items_position = function(conn) {
  if ("position" %in% DBI::dbListFields(conn, "items")) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  if (!"position" %in% DBI::dbListFields(conn, "items")) {
    DBI::dbExecute(conn, "ALTER TABLE items ADD COLUMN position INTEGER")
    DBI::dbExecute(conn, "
      UPDATE items SET position = (
        SELECT ranked.position FROM (
          SELECT id,
                 ROW_NUMBER() OVER (
                   PARTITION BY question_name
                   ORDER BY (hotkey IS NULL), hotkey, id
                 ) AS position
          FROM items
        ) AS ranked
        WHERE ranked.id = items.id
      )
    ")
    set_metadata(conn, "schema_version", "2")
  }
  DBI::dbExecute(conn, "COMMIT")

  invisible(TRUE)
}

# Schema version 2 -> 3: add items.group_id to a database created by an
# earlier markermd (the item_groups table itself is created on first touch by
# create_tables_if_needed), then stamp the new schema version. Existing
# rubrics become all-ungrouped, so their scores are unchanged. Same shape as
# migrate_items_position(): runs on first touch of each database per session,
# is a no-op once the column exists, and holds an immediate transaction
# across the check and the ALTER.
#
# conn: DBI connection object

migrate_items_group = function(conn) {
  if ("group_id" %in% DBI::dbListFields(conn, "items")) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  if (!"group_id" %in% DBI::dbListFields(conn, "items")) {
    DBI::dbExecute(conn, "ALTER TABLE items ADD COLUMN group_id TEXT")
    set_metadata(conn, "schema_version", "3")
  }
  DBI::dbExecute(conn, "COMMIT")

  invisible(TRUE)
}

# Schema version 3 -> 4: add settings.optional to a database created by an
# earlier markermd, then stamp the new schema version. Existing questions
# become non-optional, so their scores and graded status are unchanged. Same
# shape as migrate_items_group(). A legacy database that had no settings
# table gets one with the column from create_tables_if_needed(), so there is
# nothing to add; it is stamped here all the same, since this is the last
# migration and the schema is now current.
#
# conn: DBI connection object

migrate_settings_optional = function(conn) {
  if ("optional" %in% DBI::dbListFields(conn, "settings")) {
    stored = get_metadata(conn, "schema_version")
    if (!is.null(stored) && utils::compareVersion(stored, "4") < 0) {
      set_metadata(conn, "schema_version", "4")
    }
    return(invisible(FALSE))
  }

  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  if (!"optional" %in% DBI::dbListFields(conn, "settings")) {
    DBI::dbExecute(conn, "ALTER TABLE settings ADD COLUMN optional INTEGER NOT NULL DEFAULT 0 CHECK (optional IN (0, 1))")
    set_metadata(conn, "schema_version", "4")
  }
  DBI::dbExecute(conn, "COMMIT")

  invisible(TRUE)
}

# Get current username for database records
#
# Returns: Character string with username

get_current_username = function() {
  username = Sys.getenv("USER", "")
  if (username == "") {
    username = Sys.getenv("USERNAME", "")
  }
  if (username == "") {
    username = "unknown"
  }
  return(username)
}

# Get current timestamp in ISO format
#
# Returns: Character string with current timestamp

get_current_timestamp = function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

# Current time as UTC ISO 8601 with milliseconds ("2026-09-29T23:55:40.123Z"),
# the format the snapshot tables use so ordering is unambiguous across
# graders and machines
#
# Returns: Character string

utc_timestamp = function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

# Parse UTC ISO 8601 timestamps written by utc_timestamp() into POSIXct in
# the session's time zone
#
# x: Character vector of timestamps (NA passes through)

parse_utc_timestamp = function(x) {
  out = as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  attr(out, "tzone") = ""
  out
}

# Safely close database connection
#
# conn: DBI connection object

close_database = function(conn) {
  if (!is.null(conn) && DBI::dbIsValid(conn)) {
    DBI::dbDisconnect(conn)
  }
}

# Execute query with automatic connection management
#
# collection_path: Path to collection directory
# query_func: Function that takes a connection and executes queries

with_database = function(collection_path, query_func) {
  conn = initialize_database(collection_path)
  on.exit(close_database(conn))
  query_func(conn)
}

# Upsert settings record
#
# conn: DBI connection object
# question_name: Character string
# grade_state: markermd_grade_state S7 object

upsert_settings = function(conn, question_name, grade_state) {
  timestamp = get_current_timestamp()

  # Single atomic statement (created_at is preserved on update); the previous
  # SELECT-then-write pattern was racy against a concurrent skill writer
  DBI::dbExecute(conn, "
    INSERT INTO settings (question_name, current_score, total_score, grading_mode,
                          bound_above_zero, bound_below_max, optional, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(question_name) DO UPDATE SET
      current_score = excluded.current_score,
      total_score = excluded.total_score,
      grading_mode = excluded.grading_mode,
      bound_above_zero = excluded.bound_above_zero,
      bound_below_max = excluded.bound_below_max,
      optional = excluded.optional,
      updated_at = excluded.updated_at
  ", params = list(
    question_name,
    grade_state@current_score,
    grade_state@total_score,
    grade_state@grading_mode,
    as.integer(grade_state@bound_above_zero),
    as.integer(grade_state@bound_below_max),
    as.integer(grade_state@optional),
    timestamp,
    timestamp
  ))
}

# Upsert items record
#
# conn: DBI connection object
# question_name: Character string
# item_id: Character string
# rubric_item: markermd_rubric_item S7 object
# position: Integer display position within the question, or NULL to leave
#   ordering alone: a new row is appended after the question's last positioned
#   item and an existing row keeps its stored position. Callers that reorder
#   (save_rubric_items, apply_rubric_import) pass it explicitly; edits to a
#   single item's points or description do not need to know it.

upsert_items = function(conn, question_name, item_id, rubric_item, position = NULL) {
  timestamp = get_current_timestamp()
  position = if (is.null(position)) NA_integer_ else as.integer(position)

  # Single atomic statement (created_at is preserved on update); the previous
  # SELECT-then-write pattern was racy against a concurrent skill writer
  DBI::dbExecute(conn, "
    INSERT INTO items (question_name, item_id, hotkey, position, group_id, points, description, created_at, updated_at)
    VALUES (
      ?, ?, ?,
      COALESCE(?, (SELECT COALESCE(MAX(position), 0) + 1 FROM items WHERE question_name = ?)),
      ?, ?, ?, ?, ?
    )
    ON CONFLICT(question_name, item_id) DO UPDATE SET
      hotkey = excluded.hotkey,
      position = COALESCE(?, items.position),
      group_id = excluded.group_id,
      points = excluded.points,
      description = excluded.description,
      updated_at = excluded.updated_at
  ", params = list(
    question_name,
    item_id,
    if (is.na(rubric_item@hotkey)) NA_integer_ else rubric_item@hotkey,
    position,
    question_name,
    if (is.na(rubric_item@group)) NA_character_ else rubric_item@group,
    rubric_item@points,
    rubric_item@description,
    timestamp,
    timestamp,
    position
  ))
}

# Upsert an item_groups record (created_at is preserved on update). The
# UNIQUE (question_name, label) constraint rejects a label already used by
# another group of the question; callers check availability first and this
# is the safety net.
#
# conn: DBI connection object
# question_name: Character string
# group_id: Character string
# group: markermd_rubric_group S7 object

upsert_item_group = function(conn, question_name, group_id, group) {
  timestamp = get_current_timestamp()
  DBI::dbExecute(conn, "
    INSERT INTO item_groups (question_name, group_id, label, floor, ceiling, exclusive, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(question_name, group_id) DO UPDATE SET
      label = excluded.label,
      floor = excluded.floor,
      ceiling = excluded.ceiling,
      exclusive = excluded.exclusive,
      updated_at = excluded.updated_at
  ", params = list(
    question_name,
    group_id,
    group@label,
    if (is.na(group@floor)) NA_real_ else as.numeric(group@floor),
    if (is.na(group@ceiling)) NA_real_ else as.numeric(group@ceiling),
    as.integer(group@exclusive),
    timestamp,
    timestamp
  ))
}

# Delete an item_groups record and ungroup its members, which keep their
# positions (they become ungrouped items in place). Grades are untouched.
#
# conn: DBI connection object
# question_name: Character string
# group_id: Character string

delete_item_group_records = function(conn, question_name, group_id) {
  DBI::dbExecute(conn, "
    UPDATE items SET group_id = NULL WHERE question_name = ? AND group_id = ?
  ", params = list(question_name, group_id))
  DBI::dbExecute(conn, "
    DELETE FROM item_groups WHERE question_name = ? AND group_id = ?
  ", params = list(question_name, group_id))
}

# Repositories with more than one of the given items selected (by the most
# recent grade event per item): the pairs that stand in the way of making a
# group exclusive, or of moving an item into an exclusive group. Empty when
# member_ids has fewer than two entries.
#
# conn: DBI connection object
# question_name: Character string
# member_ids: Character vector of the item ids that would share the group
# Returns: Data frame with assignment_repo and item_ids (a list column of
#   the selected member ids), one row per conflicting repository

group_selection_conflicts = function(conn, question_name, member_ids) {
  empty = data.frame(assignment_repo = character(0), stringsAsFactors = FALSE)
  empty$item_ids = list()
  if (length(member_ids) < 2) {
    return(empty)
  }

  grades = latest_rows(
    conn, "grades", c("assignment_repo", "item_id"),
    where = "question_name = ?", params = list(question_name)
  )
  grades = grades[grades$selected == 1 & grades$item_id %in% member_ids, , drop = FALSE]
  if (nrow(grades) == 0) {
    return(empty)
  }

  by_repo = split(grades$item_id, grades$assignment_repo)
  by_repo = by_repo[vapply(by_repo, length, integer(1)) > 1]
  if (length(by_repo) == 0) {
    return(empty)
  }

  out = data.frame(assignment_repo = names(by_repo), stringsAsFactors = FALSE)
  out$item_ids = unname(lapply(by_repo, function(ids) member_ids[member_ids %in% ids]))
  out
}

# Delete an items record along with its grade-selection events, so a repo no
# longer counts as graded via an item that no longer exists
#
# conn: DBI connection object
# question_name: Character string
# item_id: Character string

delete_item_records = function(conn, question_name, item_id) {
  DBI::dbExecute(conn, "
    DELETE FROM items WHERE question_name = ? AND item_id = ?
  ", params = list(question_name, item_id))
  DBI::dbExecute(conn, "
    DELETE FROM grades WHERE question_name = ? AND item_id = ?
  ", params = list(question_name, item_id))
}

# Insert grade record
#
# conn: DBI connection object
# question_name: Character string
# assignment_repo: Character string
# item_id: Character string
# selected: Logical

insert_grade = function(conn, question_name, assignment_repo, item_id, selected) {
  DBI::dbExecute(conn, "
    INSERT INTO grades (question_name, assignment_repo, item_id, selected, timestamp, username, action_id)
    VALUES (?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    question_name,
    assignment_repo,
    item_id,
    as.integer(selected),
    get_current_timestamp(),
    get_current_username(),
    current_action_id()
  ))
}

# Insert comment record
#
# conn: DBI connection object
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

insert_comment = function(conn, question_name, assignment_repo, comment_text) {
  DBI::dbExecute(conn, "
    INSERT INTO comments (question_name, assignment_repo, comment_text, timestamp, username, action_id)
    VALUES (?, ?, ?, ?, ?, ?)
  ", params = list(
    question_name,
    assignment_repo,
    comment_text,
    get_current_timestamp(),
    get_current_username(),
    current_action_id()
  ))
}

# Insert private comment record (grader-internal, never student-facing)
#
# conn: DBI connection object
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

insert_private_comment = function(conn, question_name, assignment_repo, comment_text) {
  DBI::dbExecute(conn, "
    INSERT INTO private_comments (question_name, assignment_repo, comment_text, timestamp, username, action_id)
    VALUES (?, ?, ?, ?, ?, ?)
  ", params = list(
    question_name,
    assignment_repo,
    comment_text,
    get_current_timestamp(),
    get_current_username(),
    current_action_id()
  ))
}

# Load all settings from database
#
# conn: DBI connection object
# Returns: Data frame with settings data

load_all_settings = function(conn) {
  DBI::dbGetQuery(conn, "SELECT * FROM settings")
}

# Load all items from database
#
# conn: DBI connection object
# Returns: Data frame with items data

load_all_items = function(conn) {
  DBI::dbGetQuery(conn, "SELECT * FROM items ORDER BY question_name, id")
}

# Load all item groups from database
#
# conn: DBI connection object
# Returns: Data frame with item_groups data

load_all_item_groups = function(conn) {
  DBI::dbGetQuery(conn, "SELECT * FROM item_groups ORDER BY question_name, id")
}

# Most recent row per group from an event-log table
#
# Recency is decided by the autoincrement id, not the timestamp: timestamps
# have 1-second resolution, so two quick events for the same group tie on
# MAX(timestamp) and a timestamp join would return both rows.
#
# conn: DBI connection object
# table: Event-log table name ("grades", "comments", "private_comments")
# group_cols: Character vector of columns defining a group
# where: Optional SQL condition restricting which rows are considered
# params: Query parameters for the where condition
# Returns: Data frame with the most recent full row per group

latest_rows = function(conn, table, group_cols, where = NULL, params = NULL) {
  where_clause = if (is.null(where)) "" else paste0("WHERE ", where, "\n      ")
  DBI::dbGetQuery(conn, glue::glue("
    SELECT t1.*
    FROM <<table>> t1
    INNER JOIN (
      SELECT MAX(id) AS max_id
      FROM <<table>>
      <<where_clause>>GROUP BY <<paste(group_cols, collapse = ', ')>>
    ) t2 ON t1.id = t2.max_id
  ", .open = "<<", .close = ">>"), params = params)
}

# Rows of a latest-comments data frame whose comment_text is non-empty
# (a blank or whitespace-only latest comment counts as "no comment")
#
# df: Data frame with a comment_text column

nonempty_comments = function(df) {
  df[!is.na(df$comment_text) & nchar(trimws(df$comment_text)) > 0, , drop = FALSE]
}

# Load most recent grades for all question/assignment combinations
#
# conn: DBI connection object
# Returns: Data frame with most recent grade data

load_most_recent_grades = function(conn) {
  latest_rows(conn, "grades", c("question_name", "assignment_repo", "item_id"))
}

# Load most recent comments for all question/assignment combinations
#
# conn: DBI connection object
# Returns: Data frame with most recent comment data

load_most_recent_comments = function(conn) {
  latest_rows(conn, "comments", c("question_name", "assignment_repo"))
}

# Load most recent private comments for all question/assignment combinations
#
# conn: DBI connection object
# Returns: Data frame with most recent private comment data

load_most_recent_private_comments = function(conn) {
  latest_rows(conn, "private_comments", c("question_name", "assignment_repo"))
}

# Get a metadata value by key
#
# conn: DBI connection object
# key: Character string
# Returns: Character scalar, or NULL when the key is absent

get_metadata = function(conn, key) {
  res = DBI::dbGetQuery(conn, "SELECT value FROM metadata WHERE key = ?", params = list(key))
  if (nrow(res) == 0) NULL else res$value[1]
}

# Upsert a metadata key/value pair
#
# conn: DBI connection object
# key: Character string
# value: Character string

set_metadata = function(conn, key, value) {
  DBI::dbExecute(conn, "
    INSERT INTO metadata (key, value, updated_at) VALUES (?, ?, ?)
    ON CONFLICT(key) DO UPDATE SET
      value = excluded.value,
      updated_at = excluded.updated_at
  ", params = list(key, value, get_current_timestamp()))
}

# Delete a metadata key
#
# conn: DBI connection object
# key: Character string

delete_metadata = function(conn, key) {
  DBI::dbExecute(conn, "DELETE FROM metadata WHERE key = ?", params = list(key))
}