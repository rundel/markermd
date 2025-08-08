# Database integration functions for S7 class conversions and data operations
#
# These functions bridge between S7 objects and SQLite database operations

# Convert database settings row to markermd_grade_state S7 object
#
# settings_row: Single row data frame from settings table
# Returns: markermd_grade_state S7 object

db_row_to_grade_state = function(settings_row) {
  if (nrow(settings_row) == 0) {
    return(NULL)
  }
  
  markermd_grade_state(
    current_score = settings_row$current_score[1],
    total_score = settings_row$total_score[1], 
    grading_mode = settings_row$grading_mode[1],
    bound_above_zero = as.logical(settings_row$bound_above_zero[1]),
    bound_below_max = as.logical(settings_row$bound_below_max[1])
  )
}

# Convert database items row to markermd_rubric_item S7 object
#
# items_row: Single row data frame from items table
# selected: Logical indicating if item is selected (not stored in database)
# Returns: markermd_rubric_item S7 object

db_row_to_rubric_item = function(items_row, selected = FALSE) {
  if (nrow(items_row) == 0) {
    return(NULL)
  }
  
  markermd_rubric_item(
    hotkey = if (is.na(items_row$hotkey[1])) NA_integer_ else as.integer(items_row$hotkey[1]),
    points = items_row$points[1],
    description = items_row$description[1],
    selected = selected
  )
}

# Save grade state to database
#
# collection_path: Path to collection directory
# question_name: Character string
# grade_state: markermd_grade_state S7 object

save_grade_state = function(collection_path, question_name, grade_state) {
  with_database(collection_path, function(conn) {
    upsert_settings(conn, question_name, grade_state)
    return(TRUE)
  })
}

# Save rubric item to database
#
# collection_path: Path to collection directory
# question_name: Character string
# item_id: Character string (unique identifier for this item)
# rubric_item: markermd_rubric_item S7 object

save_rubric_item = function(collection_path, question_name, item_id, rubric_item) {
  with_database(collection_path, function(conn) {
    upsert_items(conn, question_name, item_id, rubric_item)
    return(TRUE)
  })
}

# Save grade selection event to database
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# item_id: Character string
# selected: Logical

save_grade_selection = function(collection_path, question_name, assignment_repo, item_id, selected) {
  with_database(collection_path, function(conn) {
    insert_grade(conn, question_name, assignment_repo, item_id, selected)
    return(TRUE)
  })
}

# Save comment to database
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

save_comment = function(collection_path, question_name, assignment_repo, comment_text) {
  with_database(collection_path, function(conn) {
    insert_comment(conn, question_name, assignment_repo, comment_text)
    return(TRUE)
  })
}

# Load grade state from database
#
# collection_path: Path to collection directory
# question_name: Character string
# Returns: markermd_grade_state S7 object or NULL if not found

load_grade_state = function(collection_path, question_name) {
  with_database(collection_path, function(conn) {
    settings_data = DBI::dbGetQuery(conn, "
      SELECT * FROM settings WHERE question_name = ?
    ", params = list(question_name))
    
    return(db_row_to_grade_state(settings_data))
  })
}

# Load all rubric items for a question from database
#
# collection_path: Path to collection directory
# question_name: Character string
# Returns: Named list of markermd_rubric_item S7 objects

load_rubric_items = function(collection_path, question_name) {
  with_database(collection_path, function(conn) {
    items_data = DBI::dbGetQuery(conn, "
      SELECT * FROM items WHERE question_name = ? ORDER BY id
    ", params = list(question_name))
    
    if (nrow(items_data) == 0) {
      return(list())
    }
    
    # Convert each row to rubric item
    items = list()
    for (i in seq_len(nrow(items_data))) {
      row = items_data[i, ]
      item_id = row$item_id
      items[[item_id]] = db_row_to_rubric_item(row, selected = FALSE)
    }
    
    return(items)
  })
}

# Load grade selections for a specific question and assignment
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# Returns: Named list with item_id -> selected status

load_grade_selections = function(collection_path, question_name, assignment_repo) {
  with_database(collection_path, function(conn) {
    grades_data = DBI::dbGetQuery(conn, "
      SELECT g1.*
      FROM grades g1
      INNER JOIN (
        SELECT item_id, MAX(timestamp) as max_timestamp
        FROM grades
        WHERE question_name = ? AND assignment_repo = ?
        GROUP BY item_id
      ) g2 ON g1.item_id = g2.item_id AND g1.timestamp = g2.max_timestamp
      WHERE g1.question_name = ? AND g1.assignment_repo = ?
    ", params = list(question_name, assignment_repo, question_name, assignment_repo))
    
    if (nrow(grades_data) == 0) {
      return(list())
    }
    
    # Convert to named list
    selections = list()
    for (i in seq_len(nrow(grades_data))) {
      row = grades_data[i, ]
      selections[[row$item_id]] = as.logical(row$selected)
    }
    
    return(selections)
  })
}

# Load comment for a specific question and assignment
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# Returns: Character string or NULL if no comment found

load_comment = function(collection_path, question_name, assignment_repo) {
  with_database(collection_path, function(conn) {
    comment_data = DBI::dbGetQuery(conn, "
      SELECT comment_text
      FROM comments
      WHERE question_name = ? AND assignment_repo = ?
      ORDER BY timestamp DESC
      LIMIT 1
    ", params = list(question_name, assignment_repo))
    
    if (nrow(comment_data) == 0) {
      return(NULL)
    }
    
    return(comment_data$comment_text[1])
  })
}

# Initialize database state for all questions from template
#
# collection_path: Path to collection directory
# template_obj: markermd_template S7 object
# Returns: List with loaded states organized by question

initialize_database_state = function(collection_path, template_obj) {
  if (is.null(template_obj)) {
    return(list())
  }
  
  question_names = sapply(template_obj@questions, function(q) q@name)
  
  # Initialize return structure
  state = list(
    grade_states = list(),
    rubric_items = list(),
    selections = list(),
    comments = list()
  )
  
  # Load data for each question
  for (question_name in question_names) {
    # Load grade state (or use defaults)
    loaded_grade_state = load_grade_state(collection_path, question_name)
    if (is.null(loaded_grade_state)) {
      # Use default grade state if none found
      loaded_grade_state = markermd_grade_state(current_score = 0, total_score = 10)
    }
    state$grade_states[[question_name]] = loaded_grade_state
    
    # Load rubric items 
    state$rubric_items[[question_name]] = load_rubric_items(collection_path, question_name)
    
    # Initialize empty selections and comments lists for this question
    state$selections[[question_name]] = list()
    state$comments[[question_name]] = list()
  }
  
  return(state)
}

# Load grade selections for a specific assignment across all questions
#
# collection_path: Path to collection directory
# assignment_repo: Character string
# question_names: Character vector of question names
# Returns: List organized by question name

load_assignment_selections = function(collection_path, assignment_repo, question_names) {
  selections = list()
  
  for (question_name in question_names) {
    selections[[question_name]] = load_grade_selections(collection_path, question_name, assignment_repo)
  }
  
  return(selections)
}

# Load comments for a specific assignment across all questions
#
# collection_path: Path to collection directory
# assignment_repo: Character string
# question_names: Character vector of question names
# Returns: List organized by question name

load_assignment_comments = function(collection_path, assignment_repo, question_names) {
  comments = list()
  
  for (question_name in question_names) {
    comments[[question_name]] = load_comment(collection_path, question_name, assignment_repo)
  }
  
  return(comments)
}

# Batch save multiple rubric items for a question
#
# collection_path: Path to collection directory
# question_name: Character string
# items_list: Named list of markermd_rubric_item S7 objects (names are item_ids)

batch_save_rubric_items = function(collection_path, question_name, items_list) {
  with_database(collection_path, function(conn) {
    # Use transaction for consistency
    DBI::dbBegin(conn)
    
    tryCatch({
      for (item_id in names(items_list)) {
        upsert_items(conn, question_name, item_id, items_list[[item_id]])
      }
      DBI::dbCommit(conn)
      return(TRUE)
    }, error = function(e) {
      DBI::dbRollback(conn)
      stop("Failed to batch save rubric items: ", e$message)
    })
  })
}

# Calculate grading progress for all assignments
#
# collection_path: Path to collection directory
# question_names: Character vector of question names
# assignment_repos: Character vector of assignment repository names
# Returns: Named list with assignment repos as names and graded question counts as values

calculate_grading_progress = function(collection_path, question_names, assignment_repos) {
  progress = stats::setNames(rep(0L, length(assignment_repos)), assignment_repos)
  
  # Use database connection to check grading status efficiently
  result = with_database(collection_path, function(conn) {
    progress_results = list()
    
    for (repo in assignment_repos) {
      graded_questions = 0L
      
      for (question in question_names) {
        is_graded = FALSE
        
        # Check if there are any selected rubric items for this question/assignment
        grade_query = DBI::dbGetQuery(conn, "
          SELECT g1.*
          FROM grades g1
          INNER JOIN (
            SELECT item_id, MAX(timestamp) as max_timestamp
            FROM grades
            WHERE question_name = ? AND assignment_repo = ?
            GROUP BY item_id
          ) g2 ON g1.item_id = g2.item_id AND g1.timestamp = g2.max_timestamp
          WHERE g1.question_name = ? AND g1.assignment_repo = ? AND g1.selected = 1
        ", params = list(question, repo, question, repo))
        
        # If any rubric items are selected, consider it graded
        if (nrow(grade_query) > 0) {
          is_graded = TRUE
        } else {
          # Check if there's a non-empty comment for this question/assignment
          comment_query = DBI::dbGetQuery(conn, "
            SELECT comment_text
            FROM comments
            WHERE question_name = ? AND assignment_repo = ?
            ORDER BY timestamp DESC
            LIMIT 1
          ", params = list(question, repo))
          
          # If there's a non-empty comment, consider it graded
          if (nrow(comment_query) > 0 && !is.na(comment_query$comment_text) && nchar(trimws(comment_query$comment_text)) > 0) {
            is_graded = TRUE
          }
        }
        
        if (is_graded) {
          graded_questions = graded_questions + 1L
        }
      }
      
      progress_results[[repo]] = graded_questions
    }
    
    return(progress_results)
  })
  
  # Convert to named integer vector
  return(unlist(result))
}