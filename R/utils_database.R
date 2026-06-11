# Database utility functions for markermd SQLite storage
#
# collection_path: Path to collection directory

# Current grading-database schema version. Bumped whenever the SQLite schema
# changes incompatibly. assert_db_compatible() rejects a database written by a
# newer markermd than this.

markermd_db_version = function() "1"

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
  normalizePath(file.path(cache_dir, "markermd.sqlite"), mustWork = FALSE)
}

# Initialize database connection and create tables if needed
#
# collection_path: Path to collection directory

initialize_database = function(collection_path) {
  db_path = get_database_path(collection_path)
  
  # Create connection
  conn = DBI::dbConnect(RSQLite::SQLite(), db_path)
  
  # Enable foreign key constraints
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  
  # Create tables if they don't exist
  create_tables_if_needed(conn)

  assert_db_compatible(conn)

  return(conn)
}

# Create database tables if they don't exist
#
# conn: DBI connection object

create_tables_if_needed = function(conn) {
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
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )
    ")
  }
  
  # Items table - stores markermd_rubric_item properties (without selected state)
  if (!DBI::dbExistsTable(conn, "items")) {
    DBI::dbExecute(conn, "
      CREATE TABLE items (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question_name TEXT NOT NULL,
        item_id TEXT NOT NULL,
        hotkey INTEGER,
        points REAL NOT NULL,
        description TEXT NOT NULL,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        UNIQUE(question_name, item_id)
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
  
  # Check if record exists
  existing = DBI::dbGetQuery(conn, "
    SELECT question_name FROM settings WHERE question_name = ?
  ", params = list(question_name))
  
  if (nrow(existing) > 0) {
    # Update existing record
    DBI::dbExecute(conn, "
      UPDATE settings 
      SET current_score = ?, total_score = ?, grading_mode = ?, 
          bound_above_zero = ?, bound_below_max = ?, updated_at = ?
      WHERE question_name = ?
    ", params = list(
      grade_state@current_score,
      grade_state@total_score, 
      grade_state@grading_mode,
      as.integer(grade_state@bound_above_zero),
      as.integer(grade_state@bound_below_max),
      timestamp,
      question_name
    ))
  } else {
    # Insert new record
    DBI::dbExecute(conn, "
      INSERT INTO settings (question_name, current_score, total_score, grading_mode, 
                           bound_above_zero, bound_below_max, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      question_name,
      grade_state@current_score,
      grade_state@total_score,
      grade_state@grading_mode,
      as.integer(grade_state@bound_above_zero),
      as.integer(grade_state@bound_below_max),
      timestamp,
      timestamp
    ))
  }
}

# Upsert items record
#
# conn: DBI connection object
# question_name: Character string
# item_id: Character string
# rubric_item: markermd_rubric_item S7 object

upsert_items = function(conn, question_name, item_id, rubric_item) {
  timestamp = get_current_timestamp()
  
  # Check if record exists
  existing = DBI::dbGetQuery(conn, "
    SELECT question_name FROM items WHERE question_name = ? AND item_id = ?
  ", params = list(question_name, item_id))
  
  if (nrow(existing) > 0) {
    # Update existing record
    DBI::dbExecute(conn, "
      UPDATE items 
      SET hotkey = ?, points = ?, description = ?, updated_at = ?
      WHERE question_name = ? AND item_id = ?
    ", params = list(
      if (is.na(rubric_item@hotkey)) NA_integer_ else rubric_item@hotkey,
      rubric_item@points,
      rubric_item@description,
      timestamp,
      question_name,
      item_id
    ))
  } else {
    # Insert new record
    DBI::dbExecute(conn, "
      INSERT INTO items (question_name, item_id, hotkey, points, description, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      question_name,
      item_id,
      if (is.na(rubric_item@hotkey)) NA_integer_ else rubric_item@hotkey,
      rubric_item@points,
      rubric_item@description,
      timestamp,
      timestamp
    ))
  }
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
    INSERT INTO grades (question_name, assignment_repo, item_id, selected, timestamp, username)
    VALUES (?, ?, ?, ?, ?, ?)
  ", params = list(
    question_name,
    assignment_repo,
    item_id,
    as.integer(selected),
    get_current_timestamp(),
    get_current_username()
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
    INSERT INTO comments (question_name, assignment_repo, comment_text, timestamp, username)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(
    question_name,
    assignment_repo,
    comment_text,
    get_current_timestamp(),
    get_current_username()
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
    INSERT INTO private_comments (question_name, assignment_repo, comment_text, timestamp, username)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(
    question_name,
    assignment_repo,
    comment_text,
    get_current_timestamp(),
    get_current_username()
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

# Load most recent grades for all question/assignment combinations
#
# conn: DBI connection object
# Returns: Data frame with most recent grade data

load_most_recent_grades = function(conn) {
  # Most recent by autoincrement id; 1-second timestamps can tie
  DBI::dbGetQuery(conn, "
    SELECT g1.*
    FROM grades g1
    INNER JOIN (
      SELECT MAX(id) as max_id
      FROM grades
      GROUP BY question_name, assignment_repo, item_id
    ) g2 ON g1.id = g2.max_id
  ")
}

# Load most recent comments for all question/assignment combinations
#
# conn: DBI connection object
# Returns: Data frame with most recent comment data

load_most_recent_comments = function(conn) {
  # Most recent by autoincrement id; 1-second timestamps can tie
  DBI::dbGetQuery(conn, "
    SELECT c1.*
    FROM comments c1
    INNER JOIN (
      SELECT MAX(id) as max_id
      FROM comments
      GROUP BY question_name, assignment_repo
    ) c2 ON c1.id = c2.max_id
  ")
}

# Load most recent private comments for all question/assignment combinations
#
# conn: DBI connection object
# Returns: Data frame with most recent private comment data

load_most_recent_private_comments = function(conn) {
  # Most recent by autoincrement id; 1-second timestamps can tie
  DBI::dbGetQuery(conn, "
    SELECT c1.*
    FROM private_comments c1
    INNER JOIN (
      SELECT MAX(id) as max_id
      FROM private_comments
      GROUP BY question_name, assignment_repo
    ) c2 ON c1.id = c2.max_id
  ")
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
  timestamp = get_current_timestamp()
  existing = DBI::dbGetQuery(conn, "SELECT key FROM metadata WHERE key = ?", params = list(key))

  if (nrow(existing) > 0) {
    DBI::dbExecute(conn, "
      UPDATE metadata SET value = ?, updated_at = ? WHERE key = ?
    ", params = list(value, timestamp, key))
  } else {
    DBI::dbExecute(conn, "
      INSERT INTO metadata (key, value, updated_at) VALUES (?, ?, ?)
    ", params = list(key, value, timestamp))
  }
}

# Delete a metadata key
#
# conn: DBI connection object
# key: Character string

delete_metadata = function(conn, key) {
  DBI::dbExecute(conn, "DELETE FROM metadata WHERE key = ?", params = list(key))
}