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
#   7: the grading event tables' timestamps in UTC ISO 8601 (see
#      migrate_event_timestamps()); earlier rows were local wall-clock time
#      with no zone
#   8: grading keyed by question id: the questions registry and a
#      question_id column on settings, items, item_groups, grades, comments
#      and private_comments, which carries the keys question_name used to
#      (see migrate_question_ids())

markermd_db_version = function() "8"

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

  # Create connection; closed again if anything below fails, since the
  # caller never sees it
  conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  ready = FALSE
  on.exit(if (!ready) close_database(conn))

  # Enable foreign key constraints
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")

  # Wait up to 5s for a competing writer (e.g. a grading skill running while
  # mark() is open) instead of failing immediately with "database is locked".
  DBI::dbExecute(conn, "PRAGMA busy_timeout = 5000")

  # Write-ahead logging, so the app's reads and a concurrent import by an
  # agent do not block each other. Persistent once set; backups use VACUUM
  # INTO, which reads through the log.
  DBI::dbGetQuery(conn, "PRAGMA journal_mode = WAL")

  # Checked before anything is written, and on every connection (one
  # metadata read): a database written by a newer markermd must not be
  # migrated or touched by this one, and a concurrent skill run may have
  # upgraded the schema while this session held the table cache
  if (DBI::dbExistsTable(conn, "metadata")) {
    assert_db_compatible(conn)
  }

  if (fresh || !isTRUE(.initialized_dbs[[db_path]])) {
    create_tables_if_needed(conn, collection_path)
    .initialized_dbs[[db_path]] = TRUE
  }

  ready = TRUE
  conn
}

# Create database tables if they don't exist
#
# conn: DBI connection object
# collection_path: Project root the database belongs to, for the baseline
#   snapshot the schema 5 migration takes (NULL skips it)

create_tables_if_needed = function(conn, collection_path = NULL) {
  # The six grading tables (settings, items, item_groups and the three event
  # logs), keyed by question id; see question_table_ddl() for their layout and
  # migrate_question_ids() for how a database written before schema version 8
  # is rebuilt into it
  create_question_registry(conn)
  ddl = question_table_ddl()
  for (table in names(ddl)) {
    if (!DBI::dbExistsTable(conn, table)) {
      DBI::dbExecute(conn, ddl[[table]])
    }
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
  # The tables are rebuilt by id before the snapshot baseline reads them,
  # and stamped version 8 only once the version-gated migrations after it
  # have run
  migrate_question_ids(conn, stamp = FALSE)
  migrate_snapshots(conn, collection_path, baseline = snapshots_are_new)
  create_action_tables(conn)
  migrate_actions(conn)
  migrate_event_timestamps(conn)
  migrate_question_ids(conn)
}

# Schema version 6 -> 7: rewrite the grading event tables' timestamps from
# local wall-clock time ("2026-09-29 18:55:40", written by versions up to 6)
# to UTC ISO 8601 with milliseconds (event_timestamp()), the format the
# snapshot and action tables already use, so events from graders in
# different time zones order correctly and the activity log groups them
# right. SQLite's 'utc' modifier reads the stored text as local time of this
# machine, which is where the rows were written in the common case. Same
# shape as the earlier migrations: a no-op once stamped.
#
# conn: DBI connection object

migrate_event_timestamps = function(conn) {
  stored = get_metadata(conn, "schema_version")
  if (!is.null(stored) && utils::compareVersion(stored, "7") >= 0) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  stored = get_metadata(conn, "schema_version")
  if (is.null(stored) || utils::compareVersion(stored, "7") < 0) {
    for (table in c("grades", "comments", "private_comments")) {
      DBI::dbExecute(conn, glue::glue("
        UPDATE {table} SET timestamp = strftime('%Y-%m-%dT%H:%M:%fZ', timestamp, 'utc')
        WHERE timestamp NOT LIKE '%T%'
      "))
    }
    set_metadata(conn, "schema_version", "7")
  }
  DBI::dbExecute(conn, "COMMIT")

  invisible(TRUE)
}

# Questions registry (schema version 8). Every grading row carries the
# question_id of the question it belongs to, and question_id is the key every
# reader and writer uses. A row of the registry gives an id's current name
# (question_name in the grading tables is kept equal to it, for display and
# hand-reading); live marks the ids the grading state shows (the stored
# template's questions, or, before a template is stored, every name written
# to); templated marks ids that have been part of a stored template. The
# template's own question ids are the registry ids, so renaming a question in
# the template renames its id's rows and nothing else; a question removed from
# the template keeps its rows under its id (live = 0, an orphan) and a new
# question that reuses its name gets a new id, so the old grading does not
# come back unless the old question (its id) does. Names written to before
# any template names them get ids minted here (templated = 0); the first
# stored template that names them adopts their rows under its own ids (see
# sync_question_registry_conn()).
#
# conn: DBI connection object

create_question_registry = function(conn) {
  if (!DBI::dbExistsTable(conn, "questions")) {
    DBI::dbExecute(conn, "
      CREATE TABLE questions (
        question_id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        live INTEGER NOT NULL DEFAULT 1 CHECK (live IN (0, 1)),
        templated INTEGER NOT NULL DEFAULT 0 CHECK (templated IN (0, 1)),
        created_at TEXT NOT NULL
      )
    ")
    DBI::dbExecute(conn, "CREATE INDEX idx_questions_name ON questions (name, live)")
  }
}

# The grading tables keyed by question id

question_keyed_tables = function() c("settings", "items", "item_groups", "grades", "comments", "private_comments")

# The CREATE TABLE statement of each question-keyed grading table at schema
# version 8. question_id may be NULL only transiently: a row inserted without
# one (by hand, or by code that predates the column) is given its name's id by
# the trigger from create_question_indexes(). settings keeps an autoincrement
# id so such a row cannot take a rowid as its question id.

question_table_ddl = function() {
  list(
    settings = "
      CREATE TABLE settings (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_id INTEGER UNIQUE,
        question_name TEXT NOT NULL,
        current_score REAL NOT NULL,
        total_score REAL NOT NULL,
        grading_mode TEXT NOT NULL CHECK (grading_mode IN ('positive', 'negative')),
        bound_above_zero INTEGER NOT NULL CHECK (bound_above_zero IN (0, 1)),
        bound_below_max INTEGER NOT NULL CHECK (bound_below_max IN (0, 1)),
        optional INTEGER NOT NULL DEFAULT 0 CHECK (optional IN (0, 1)),
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )
    ",
    # position is the 1-based display order within the question and the only
    # ordering key; hotkey is derived from it (position <= 10) and kept in the
    # row so the table reads sensibly by hand. group_id names the item's group
    # in item_groups (NULL = ungrouped); a group's members are kept contiguous
    # in position order (normalize_item_order()), and the group's display
    # position is its first member's.
    items = "
      CREATE TABLE items (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_id INTEGER,
        question_name TEXT NOT NULL,
        item_id TEXT NOT NULL,
        hotkey INTEGER,
        position INTEGER,
        group_id TEXT,
        points REAL NOT NULL,
        description TEXT NOT NULL,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        UNIQUE (question_id, item_id)
      )
    ",
    # A named run of a question's items whose combined points are bounded by
    # floor (<= 0) and ceiling (>= 0), optionally exclusive. Labels are unique
    # within a question because the rubric import matches groups by label.
    item_groups = "
      CREATE TABLE item_groups (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_id INTEGER,
        question_name TEXT NOT NULL,
        group_id TEXT NOT NULL,
        label TEXT NOT NULL,
        floor REAL CHECK (floor IS NULL OR floor <= 0),
        ceiling REAL CHECK (ceiling IS NULL OR ceiling >= 0),
        exclusive INTEGER NOT NULL DEFAULT 0 CHECK (exclusive IN (0, 1)),
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        UNIQUE (question_id, group_id),
        UNIQUE (question_id, label)
      )
    ",
    grades = "
      CREATE TABLE grades (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_id INTEGER,
        question_name TEXT NOT NULL,
        assignment_repo TEXT NOT NULL,
        item_id TEXT NOT NULL,
        selected INTEGER NOT NULL CHECK (selected IN (0, 1)),
        timestamp TEXT NOT NULL,
        username TEXT NOT NULL,
        action_id TEXT
      )
    ",
    # The public, student-facing comment log; the most recent row per pair is
    # the current comment
    comments = "
      CREATE TABLE comments (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_id INTEGER,
        question_name TEXT NOT NULL,
        assignment_repo TEXT NOT NULL,
        comment_text TEXT NOT NULL,
        timestamp TEXT NOT NULL,
        username TEXT NOT NULL,
        action_id TEXT
      )
    ",
    # Grader and skill notes per pair, never student-facing
    private_comments = "
      CREATE TABLE private_comments (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_id INTEGER,
        question_name TEXT NOT NULL,
        assignment_repo TEXT NOT NULL,
        comment_text TEXT NOT NULL,
        timestamp TEXT NOT NULL,
        username TEXT NOT NULL,
        action_id TEXT
      )
    "
  )
}

# The indexes and triggers of the question-keyed tables (created after the
# tables, since a rebuild drops them with the old table). The trigger gives a
# row inserted without a question_id the id of its question_name's live
# registry row, minting one when the name is new, so rows written by hand or
# by older code still land under the right question.
#
# conn: DBI connection object

create_question_indexes = function(conn) {
  for (table in c("grades", "comments", "private_comments")) {
    cols = if (table == "grades") "question_id, assignment_repo, item_id, id" else "question_id, assignment_repo, id"
    DBI::dbExecute(conn, glue::glue("CREATE INDEX IF NOT EXISTS idx_{table}_lookup ON {table} ({cols})"))
    # migrate_actions() adds the column (and this index) to a table that
    # predates it
    if ("action_id" %in% DBI::dbListFields(conn, table)) {
      DBI::dbExecute(conn, glue::glue("CREATE INDEX IF NOT EXISTS idx_{table}_action ON {table} (action_id)"))
    }
  }
  for (table in question_keyed_tables()) {
    DBI::dbExecute(conn, glue::glue("
      CREATE TRIGGER IF NOT EXISTS trg_<<table>>_question_id
      AFTER INSERT ON <<table>> WHEN NEW.question_id IS NULL
      BEGIN
        INSERT INTO questions (name, live, templated, created_at)
          SELECT NEW.question_name, 1, 0, strftime('%Y-%m-%dT%H:%M:%fZ', 'now')
          WHERE NOT EXISTS (SELECT 1 FROM questions WHERE name = NEW.question_name AND live = 1);
        UPDATE <<table>> SET question_id = (
          SELECT question_id FROM questions
          WHERE name = NEW.question_name AND live = 1
          ORDER BY question_id DESC LIMIT 1
        ) WHERE id = NEW.id;
      END
    ", .open = "<<", .close = ">>"))
  }
}

# Schema version 7 -> 8: key the grading tables by question id. The registry
# is seeded with the stored template's questions (their own ids, live and
# templated); every other name found in the grading tables gets an id minted
# above the project's question id high-water mark, one per name: an orphan
# (not live) when a template is stored, since that name's question is no
# longer in it, otherwise a live, not yet templated question that the first
# stored template naming it adopts. Each table lacking question_id is rebuilt
# (SQLite cannot change a table's keys in place): renamed aside, recreated
# from question_table_ddl(), its rows copied with their ids (so event ids,
# which snapshots count from, are kept) and the question_id of their name,
# and dropped. Runs on first touch of each database per session like the
# earlier migrations, a no-op once every table has the column, inside one
# immediate transaction.
#
# conn: DBI connection object
# stamp: Whether to stamp schema version 8 (FALSE while the version-gated
#   migrations that follow have still to run)

migrate_question_ids = function(conn, stamp = TRUE) {
  needs = function(table) {
    DBI::dbExistsTable(conn, table) && !"question_id" %in% DBI::dbListFields(conn, table)
  }
  stamp_version = stamp
  stamp = function() {
    stored = get_metadata(conn, "schema_version")
    if (stamp_version && !is.null(stored) && utils::compareVersion(stored, "8") < 0) {
      set_metadata(conn, "schema_version", "8")
    }
  }
  if (!any(vapply(question_keyed_tables(), needs, logical(1)))) {
    create_question_indexes(conn)
    stamp()
    return(invisible(FALSE))
  }

  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  pending = Filter(needs, question_keyed_tables())
  if (length(pending) > 0) {
    template = stored_template_questions(conn)
    has_template = !is.null(template)
    for (i in seq_len(NROW(template))) {
      DBI::dbExecute(conn, "
        INSERT OR IGNORE INTO questions (question_id, name, live, templated, created_at) VALUES (?, ?, 1, 1, ?)
      ", params = list(template$id[i], template$name[i], utc_timestamp()))
    }
    raise_question_sequence(conn)
    seen = unique(unlist(lapply(pending, function(table) {
      DBI::dbGetQuery(conn, glue::glue("SELECT DISTINCT question_name FROM {table}"))$question_name
    })))
    for (name in setdiff(seen, template$name)) {
      DBI::dbExecute(conn, "
        INSERT INTO questions (name, live, templated, created_at) VALUES (?, ?, ?, ?)
      ", params = list(name, as.integer(!has_template), as.integer(has_template), utc_timestamp()))
    }

    ddl = question_table_ddl()
    for (table in pending) {
      old = paste0(table, "_v7")
      DBI::dbExecute(conn, glue::glue("ALTER TABLE {table} RENAME TO {old}"))
      DBI::dbExecute(conn, ddl[[table]])
      cols = setdiff(intersect(DBI::dbListFields(conn, old), DBI::dbListFields(conn, table)), c("question_id", "id"))
      if (table != "settings") cols = c("id", cols)
      col_list = paste(cols, collapse = ", ")
      DBI::dbExecute(conn, glue::glue("
        INSERT INTO {table} ({col_list}, question_id)
        SELECT {col_list}, (
          SELECT q.question_id FROM questions q WHERE q.name = {old}.question_name
          ORDER BY q.live DESC, q.question_id DESC LIMIT 1
        ) FROM {old}
      "))
      DBI::dbExecute(conn, glue::glue("DROP TABLE {old}"))
    }
  }
  create_question_indexes(conn)
  stamp()
  DBI::dbExecute(conn, "COMMIT")

  invisible(TRUE)
}

# The stored template's questions as a data frame of id and name, read from
# the template JSON in the metadata table without rebuilding the template
# (which would re-parse the assignment document), or NULL when no template is
# stored
#
# conn: DBI connection object

stored_template_questions = function(conn) {
  json = get_metadata(conn, "template")
  if (is.null(json)) {
    return(NULL)
  }
  questions = jsonlite::fromJSON(json, simplifyVector = FALSE)$questions
  data.frame(
    id = vapply(questions, function(q) as.integer(q$id), integer(1)),
    name = vapply(questions, function(q) as.character(q$name), character(1)),
    stringsAsFactors = FALSE
  )
}

# Raise the registry's id sequence to the project's question id high-water
# mark (the "question_id_max" metadata value) and any floor given, so an id
# minted for a name never collides with one the template editor has issued
#
# conn: DBI connection object
# floor: Optional integer the sequence must reach

raise_question_sequence = function(conn, floor = 0L) {
  recorded = suppressWarnings(as.integer(get_metadata(conn, "question_id_max")))
  registry = DBI::dbGetQuery(conn, "SELECT COALESCE(MAX(question_id), 0) AS m FROM questions")$m
  target = as.integer(max(c(0L, recorded[!is.na(recorded)], registry, floor)))
  present = DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM sqlite_sequence WHERE name = 'questions'")$n
  if (present == 0) {
    DBI::dbExecute(conn, "INSERT INTO sqlite_sequence (name, seq) VALUES ('questions', ?)", params = list(target))
  } else {
    DBI::dbExecute(conn, "
      UPDATE sqlite_sequence SET seq = ? WHERE name = 'questions' AND seq < ?
    ", params = list(target, target))
  }
  invisible(target)
}

# The question id a name refers to: its live registry row, minted (live, not
# yet templated) when the name is new and create is TRUE. Every grading
# reader and writer resolves its question name here once and keys its SQL on
# the id.
#
# conn: DBI connection object
# question_name: Character string
# create: Whether to mint an id for an unknown name (writers) or return NA
#   (readers, which then match no rows)

question_id_conn = function(conn, question_name, create = TRUE) {
  row = DBI::dbGetQuery(conn, "
    SELECT question_id FROM questions WHERE name = ? AND live = 1 ORDER BY question_id DESC LIMIT 1
  ", params = list(question_name))
  if (nrow(row) > 0) {
    return(as.integer(row$question_id[1]))
  }
  if (!create) {
    return(NA_integer_)
  }
  raise_question_sequence(conn)
  DBI::dbExecute(conn, "
    INSERT INTO questions (name, live, templated, created_at) VALUES (?, 1, 0, ?)
  ", params = list(question_name, utc_timestamp()))
  as.integer(DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id)
}

# SQL condition restricting a grading table's rows to live questions (the
# grading state; orphans are kept but not shown)

live_question_sql = "question_id IN (SELECT question_id FROM questions WHERE live = 1)"

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
# nothing to add; it is stamped here all the same so the later migrations
# find the version they expect.
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

# Current local time as "YYYY-MM-DD HH:MM:SS", for the records a person
# reads back (the project config, the settings and items rows' updated_at)
#
# Returns: Character string with current timestamp

get_current_timestamp = function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

# The timestamp written on every grading event row (grades, comments,
# private_comments): UTC ISO 8601 with milliseconds, like the snapshot and
# action tables, so events order across machines and zones

event_timestamp = function() {
  utc_timestamp()
}

# Parse grading event timestamps into POSIXct: the UTC ISO 8601 form
# written since schema version 7, or the zoneless local form of earlier
# rows (read in this session's zone)
#
# x: Character vector of timestamps

parse_event_timestamp = function(x) {
  iso = grepl("T", x, fixed = TRUE)
  out = as.POSIXct(rep(NA_character_, length(x)), tz = "")
  if (any(iso)) {
    out[iso] = parse_utc_timestamp(x[iso])
  }
  if (any(!iso)) {
    out[!iso] = as.POSIXct(x[!iso], format = "%Y-%m-%d %H:%M:%S", tz = "")
  }
  out
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

# Run `code` inside one transaction on an open connection: committed when it
# returns, rolled back when it errors, the error re-raised as "<what>: <its
# message>" without the handler's own call (which would read as a meaningless
# `value[[3L]]()`). Every bulk writer wraps its work in this.
#
# conn: DBI connection object
# what: What failed, e.g. "Failed to import rubric"
# code: The work (evaluated lazily, inside the transaction)

with_transaction = function(conn, what, code) {
  DBI::dbBegin(conn)
  tryCatch({
    result = code
    DBI::dbCommit(conn)
    result
  }, error = function(e) {
    DBI::dbRollback(conn)
    cli::cli_abort("{what}: {conditionMessage(e)}", call = NULL)
  })
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
    INSERT INTO settings (question_id, question_name, current_score, total_score, grading_mode,
                          bound_above_zero, bound_below_max, optional, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(question_id) DO UPDATE SET
      question_name = excluded.question_name,
      current_score = excluded.current_score,
      total_score = excluded.total_score,
      grading_mode = excluded.grading_mode,
      bound_above_zero = excluded.bound_above_zero,
      bound_below_max = excluded.bound_below_max,
      optional = excluded.optional,
      updated_at = excluded.updated_at
  ", params = list(
    question_id_conn(conn, question_name),
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
  question_id = question_id_conn(conn, question_name)
  DBI::dbExecute(conn, "
    INSERT INTO items (question_id, question_name, item_id, hotkey, position, group_id, points, description, created_at, updated_at)
    VALUES (
      ?, ?, ?, ?,
      COALESCE(?, (SELECT COALESCE(MAX(position), 0) + 1 FROM items WHERE question_id = ?)),
      ?, ?, ?, ?, ?
    )
    ON CONFLICT(question_id, item_id) DO UPDATE SET
      question_name = excluded.question_name,
      hotkey = excluded.hotkey,
      position = COALESCE(?, items.position),
      group_id = excluded.group_id,
      points = excluded.points,
      description = excluded.description,
      updated_at = excluded.updated_at
  ", params = list(
    question_id,
    question_name,
    item_id,
    if (is.na(rubric_item@hotkey)) NA_integer_ else rubric_item@hotkey,
    position,
    question_id,
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
    INSERT INTO item_groups (question_id, question_name, group_id, label, floor, ceiling, exclusive, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(question_id, group_id) DO UPDATE SET
      question_name = excluded.question_name,
      label = excluded.label,
      floor = excluded.floor,
      ceiling = excluded.ceiling,
      exclusive = excluded.exclusive,
      updated_at = excluded.updated_at
  ", params = list(
    question_id_conn(conn, question_name),
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
  question_id = question_id_conn(conn, question_name, create = FALSE)
  DBI::dbExecute(conn, "
    UPDATE items SET group_id = NULL WHERE question_id = ? AND group_id = ?
  ", params = list(question_id, group_id))
  DBI::dbExecute(conn, "
    DELETE FROM item_groups WHERE question_id = ? AND group_id = ?
  ", params = list(question_id, group_id))
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
    where = "question_id = ?", params = list(question_id_conn(conn, question_name, create = FALSE))
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
  question_id = question_id_conn(conn, question_name, create = FALSE)
  DBI::dbExecute(conn, "
    DELETE FROM items WHERE question_id = ? AND item_id = ?
  ", params = list(question_id, item_id))
  DBI::dbExecute(conn, "
    DELETE FROM grades WHERE question_id = ? AND item_id = ?
  ", params = list(question_id, item_id))
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
    INSERT INTO grades (question_id, question_name, assignment_repo, item_id, selected, timestamp, username, action_id)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    question_id_conn(conn, question_name),
    question_name,
    assignment_repo,
    item_id,
    as.integer(selected),
    event_timestamp(),
    get_current_username(),
    current_action_id()
  ))
}

# The two comment channels share one event-log layout: the public,
# student-facing "comments" table and the grader-internal
# "private_comments" table, which is never student-facing

comment_tables = function() c(public = "comments", private = "private_comments")

# Insert a comment event row into one of the comment tables
#
# conn: DBI connection object
# table: "comments" or "private_comments"
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

insert_comment_row = function(conn, table, question_name, assignment_repo, comment_text) {
  table = match.arg(table, comment_tables())
  DBI::dbExecute(conn, glue::glue("
    INSERT INTO {table} (question_id, question_name, assignment_repo, comment_text, timestamp, username, action_id)
    VALUES (?, ?, ?, ?, ?, ?, ?)
  "), params = list(
    question_id_conn(conn, question_name),
    question_name,
    assignment_repo,
    comment_text,
    event_timestamp(),
    get_current_username(),
    current_action_id()
  ))
}

# Insert comment record (public, student-facing)
#
# conn: DBI connection object
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

insert_comment = function(conn, question_name, assignment_repo, comment_text) {
  insert_comment_row(conn, "comments", question_name, assignment_repo, comment_text)
}

# Insert private comment record (grader-internal, never student-facing)
#
# conn: DBI connection object
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

insert_private_comment = function(conn, question_name, assignment_repo, comment_text) {
  insert_comment_row(conn, "private_comments", question_name, assignment_repo, comment_text)
}

# Load all settings from database
#
# conn: DBI connection object
# Returns: Data frame with settings data

load_all_settings = function(conn) {
  DBI::dbGetQuery(conn, paste("SELECT * FROM settings WHERE", live_question_sql))
}

# Load all items from database
#
# conn: DBI connection object
# Returns: Data frame with items data

load_all_items = function(conn) {
  DBI::dbGetQuery(conn, paste("SELECT * FROM items WHERE", live_question_sql, "ORDER BY question_name, id"))
}

# Load all item groups from database
#
# conn: DBI connection object
# Returns: Data frame with item_groups data

load_all_item_groups = function(conn) {
  DBI::dbGetQuery(conn, paste("SELECT * FROM item_groups WHERE", live_question_sql, "ORDER BY question_name, id"))
}

# Most recent row per group from an event-log table, among the rows of live
# questions (an orphaned question's rows are kept but are not grading state)
#
# Recency is decided by the autoincrement id, not the timestamp: timestamps
# have 1-second resolution, so two quick events for the same group tie on
# MAX(timestamp) and a timestamp join would return both rows. A question is
# grouped by its question_id whichever of question_id and question_name
# group_cols names, since the name is a display column.
#
# conn: DBI connection object
# table: Event-log table name ("grades", "comments", "private_comments")
# group_cols: Character vector of columns defining a group
# where: Optional SQL condition restricting which rows are considered
# params: Query parameters for the where condition
# Returns: Data frame with the most recent full row per group

latest_rows = function(conn, table, group_cols, where = NULL, params = NULL) {
  group_cols = unique(replace(group_cols, group_cols == "question_name", "question_id"))
  conditions = c(live_question_sql, where)
  where_clause = paste0("WHERE ", paste0("(", conditions, ")", collapse = " AND "), "\n      ")
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