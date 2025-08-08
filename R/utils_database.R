# Database utility functions for markermd SQLite storage
#
# collection_path: Path to collection directory

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
  
  # Comments table - stores question comments (future functionality)
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
  conn = NULL
  tryCatch({
    conn = initialize_database(collection_path)
    result = query_func(conn)
    return(result)
  }, error = function(e) {
    warning("Database operation failed: ", e$message)
    return(NULL)
  }, finally = {
    close_database(conn)
  })
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
  DBI::dbGetQuery(conn, "
    SELECT g1.*
    FROM grades g1
    INNER JOIN (
      SELECT question_name, assignment_repo, item_id, MAX(timestamp) as max_timestamp
      FROM grades
      GROUP BY question_name, assignment_repo, item_id
    ) g2 ON g1.question_name = g2.question_name 
        AND g1.assignment_repo = g2.assignment_repo
        AND g1.item_id = g2.item_id
        AND g1.timestamp = g2.max_timestamp
  ")
}

# Load most recent comments for all question/assignment combinations
#
# conn: DBI connection object
# Returns: Data frame with most recent comment data

load_most_recent_comments = function(conn) {
  DBI::dbGetQuery(conn, "
    SELECT c1.*
    FROM comments c1
    INNER JOIN (
      SELECT question_name, assignment_repo, MAX(timestamp) as max_timestamp
      FROM comments
      GROUP BY question_name, assignment_repo
    ) c2 ON c1.question_name = c2.question_name 
        AND c1.assignment_repo = c2.assignment_repo
        AND c1.timestamp = c2.max_timestamp
  ")
}