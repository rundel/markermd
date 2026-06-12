# Database integration functions for S7 class conversions and data operations
#
# These functions bridge between S7 objects and SQLite database operations

# SQL fragment selecting the most recent grade row per item_id for a given
# question_name / assignment_repo pair. Recency is decided by the
# autoincrement id, not the timestamp: timestamps have 1-second resolution, so
# two quick toggles of the same item tie on MAX(timestamp) and the join would
# return both rows. The outer query must alias the grades table as g1 and
# supply two pairs of (question_name, assignment_repo) params: one pair for
# this inner subquery and one for the outer WHERE clause.

most_recent_grade_join = "
  FROM grades g1
  INNER JOIN (
    SELECT MAX(id) as max_id
    FROM grades
    WHERE question_name = ? AND assignment_repo = ?
    GROUP BY item_id
  ) g2 ON g1.id = g2.max_id
  WHERE g1.question_name = ? AND g1.assignment_repo = ?"

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

# Delete rubric item from database, along with its grade-selection events
#
# collection_path: Path to collection directory
# question_name: Character string
# item_id: Character string (unique identifier for this item)

delete_rubric_item = function(collection_path, question_name, item_id) {
  with_database(collection_path, function(conn) {
    delete_item_records(conn, question_name, item_id)
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

# Save private comment to database (grader-internal, never student-facing)
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

save_private_comment = function(collection_path, question_name, assignment_repo, comment_text) {
  with_database(collection_path, function(conn) {
    insert_private_comment(conn, question_name, assignment_repo, comment_text)
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
    # The move/delete handlers keep hotkey == display position (1-10, NA
    # beyond), so ordering by hotkey reproduces the arrangement the grader
    # left; un-hotkeyed items keep insertion order at the tail
    items_data = DBI::dbGetQuery(conn, "
      SELECT * FROM items WHERE question_name = ?
      ORDER BY (hotkey IS NULL), hotkey, id
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
    grades_data = DBI::dbGetQuery(conn, glue::glue("
      SELECT g1.*
      <<most_recent_grade_join>>
    ", .open = "<<", .close = ">>"),
      params = list(question_name, assignment_repo, question_name, assignment_repo))

    if (nrow(grades_data) == 0) {
      return(list())
    }

    as.list(stats::setNames(as.logical(grades_data$selected), grades_data$item_id))
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
      ORDER BY id DESC
      LIMIT 1
    ", params = list(question_name, assignment_repo))
    
    if (nrow(comment_data) == 0) {
      return(NULL)
    }
    
    return(comment_data$comment_text[1])
  })
}

# Load private comment for a specific question and assignment
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# Returns: Character string or NULL if no private comment found

load_private_comment = function(collection_path, question_name, assignment_repo) {
  with_database(collection_path, function(conn) {
    comment_data = DBI::dbGetQuery(conn, "
      SELECT comment_text
      FROM private_comments
      WHERE question_name = ? AND assignment_repo = ?
      ORDER BY id DESC
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
    comments = list(),
    private_comments = list()
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
    state$private_comments[[question_name]] = list()
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

# Mint n fresh rubric item ids in the app's "item_<k>" style, continuing past
# the largest numeric suffix among the ids already in use so imports never
# reuse an id.
#
# existing_ids: Character vector of ids that must not be reused
# n: Number of ids to mint

next_item_ids = function(existing_ids, n) {
  if (n == 0) {
    return(character(0))
  }

  suffixes = sub("^item_", "", existing_ids[grepl("^item_[0-9]+$", existing_ids)])
  k = if (length(suffixes) == 0) 0L else max(as.integer(suffixes)) + 1L

  ids = character(0)
  while (length(ids) < n) {
    candidate = paste0("item_", k)
    if (!candidate %in% existing_ids) {
      ids = c(ids, candidate)
    }
    k = k + 1L
  }
  ids
}

# Apply a parsed rubric (the exchange list from read_rubric_yaml()) to a
# project's grading database in a single transaction.
#
# Mode "append" keeps each question's existing items and adds the file's items
# after them; "replace" deletes the existing items first, which also removes
# their per-repo grade-selection events (delete_item_records). In both modes
# the combined item list is renumbered to hotkeys 1-10 by display position (NA
# beyond ten), preserving the load_rubric_items() ordering invariant. Fresh
# item ids are minted avoiding every id in the database (across all questions,
# since the mark app's module-id namespace is shared) plus any reserved_ids
# the caller has ever bound.
#
# collection_path: Project root containing the grading database
# rubric: Exchange-list rubric (format_version + questions)
# mode: "append" or "replace"
# reserved_ids: Additional item ids that must not be reused
# Returns: Named list (by question name) of summaries with mode, n_existing,
#   new_ids, new_items (markermd_rubric_item objects as written, hotkeys
#   final), hotkey_changes (named integer vector for kept items whose hotkey
#   changed; NA means the hotkey was cleared) and scoring
#   (markermd_grade_state or NULL)

apply_rubric_import = function(collection_path, rubric, mode, reserved_ids = character(0)) {
  with_database(collection_path, function(conn) {
    DBI::dbBegin(conn)

    tryCatch({
      taken = union(load_all_items(conn)$item_id, reserved_ids)
      summaries = list()

      for (question in rubric$questions) {
        question_name = question$name

        existing = DBI::dbGetQuery(conn, "
          SELECT * FROM items WHERE question_name = ?
          ORDER BY (hotkey IS NULL), hotkey, id
        ", params = list(question_name))
        n_existing = nrow(existing)

        if (identical(mode, "replace")) {
          for (item_id in existing$item_id) {
            delete_item_records(conn, question_name, item_id)
          }
          existing = existing[0, ]
        }

        kept_ids = existing$item_id

        # Renumber kept items whose stored hotkey does not match their display
        # position (only possible when the database had non-contiguous hotkeys)
        hotkey_changes = integer(0)
        for (i in seq_along(kept_ids)) {
          hotkey = if (i <= 10) as.integer(i) else NA_integer_
          if (!identical(as.integer(existing$hotkey[i]), hotkey)) {
            item = db_row_to_rubric_item(existing[i, ])
            item@hotkey = hotkey
            upsert_items(conn, question_name, kept_ids[i], item)
            hotkey_changes[kept_ids[i]] = hotkey
          }
        }

        # Imported items take the positions after the kept ones
        new_ids = next_item_ids(taken, length(question$items))
        taken = union(taken, new_ids)

        new_items = lapply(seq_along(question$items), function(j) {
          item = question$items[[j]]
          pos = length(kept_ids) + j
          item@hotkey = if (pos <= 10) as.integer(pos) else NA_integer_
          item
        })
        for (j in seq_along(new_items)) {
          upsert_items(conn, question_name, new_ids[j], new_items[[j]])
        }

        if (!is.null(question$scoring)) {
          upsert_settings(conn, question_name, question$scoring)
        }

        summaries[[question_name]] = list(
          mode = mode,
          n_existing = n_existing,
          new_ids = new_ids,
          new_items = new_items,
          hotkey_changes = hotkey_changes,
          scoring = question$scoring
        )
      }

      DBI::dbCommit(conn)
      summaries
    }, error = function(e) {
      DBI::dbRollback(conn)
      stop("Failed to import rubric: ", e$message, call. = FALSE)
    })
  })
}

# Apply a resolved marks plan to a project's grading database in a single
# transaction. Each entry is one (repo, question) pair with fully-resolved
# selections (named logical keyed by item_id, covering every item of the
# question, so the pair's state is declarative; NULL to leave selections
# untouched) and optional public / private comment text appended to their
# event logs. The settings table is never touched: current_score is ephemeral
# state the mark app recomputes from live selections.
#
# collection_path: Project root containing the grading database
# plan: List of entries with repo, question, selections, comment, private_comment
# Returns: TRUE

apply_marks_import = function(collection_path, plan) {
  with_database(collection_path, function(conn) {
    DBI::dbBegin(conn)

    tryCatch({
      for (entry in plan) {
        for (item_id in names(entry$selections)) {
          insert_grade(conn, entry$question, entry$repo, item_id, entry$selections[[item_id]])
        }
        if (!is.null(entry$comment)) {
          insert_comment(conn, entry$question, entry$repo, entry$comment)
        }
        if (!is.null(entry$private_comment)) {
          insert_private_comment(conn, entry$question, entry$repo, entry$private_comment)
        }
      }

      DBI::dbCommit(conn)
      TRUE
    }, error = function(e) {
      DBI::dbRollback(conn)
      stop("Failed to import marks: ", e$message, call. = FALSE)
    })
  })
}

# All (question, repo) pairs that currently count as graded
#
# A question/repo pair is graded if it has a selected rubric item (using the
# most recent grade per item, by autoincrement id since timestamps can tie
# within a second) or a non-empty most recent comment. Two grouped queries
# cover all pairs at once rather than running a query per pair.
#
# collection_path: Path to collection directory
# Returns: Data frame with question_name and assignment_repo columns

graded_question_pairs = function(collection_path) {
  with_database(collection_path, function(conn) {
    # Pairs with a selected rubric item among the most recent grade per item
    selected_pairs = DBI::dbGetQuery(conn, "
      SELECT DISTINCT g1.question_name AS question_name, g1.assignment_repo AS assignment_repo
      FROM grades g1
      INNER JOIN (
        SELECT MAX(id) AS max_id
        FROM grades
        GROUP BY item_id, question_name, assignment_repo
      ) g2 ON g1.id = g2.max_id
      WHERE g1.selected = 1")

    # Most recent comment per pair, kept when it is non-empty
    latest_comments = DBI::dbGetQuery(conn, "
      SELECT c.question_name AS question_name, c.assignment_repo AS assignment_repo, c.comment_text AS comment_text
      FROM comments c
      INNER JOIN (
        SELECT MAX(id) AS max_id
        FROM comments
        GROUP BY question_name, assignment_repo
      ) latest ON c.id = latest.max_id")
    comment_pairs = latest_comments[
      !is.na(latest_comments$comment_text) & nchar(trimws(latest_comments$comment_text)) > 0,
      c("question_name", "assignment_repo"),
      drop = FALSE
    ]

    unique(rbind(
      selected_pairs[, c("question_name", "assignment_repo"), drop = FALSE],
      comment_pairs
    ))
  })
}

# All (question, repo) pairs that have any grading activity
#
# Broader than graded_question_pairs(): any grades event (even a deselection,
# which the app's graded definition deliberately ignores), or a non-empty most
# recent comment in either the public or the private channel, counts. This is
# the skip-protection check for programmatic marking (marks_import() /
# marks_set()), where "someone or something already touched this pair" is the
# question, not "does it display as graded".
#
# collection_path: Path to collection directory
# Returns: Data frame with question_name and assignment_repo columns

marked_question_pairs = function(collection_path) {
  with_database(collection_path, function(conn) {
    grade_pairs = DBI::dbGetQuery(conn, "
      SELECT DISTINCT question_name, assignment_repo FROM grades")

    nonempty_latest_pairs = function(table) {
      latest = DBI::dbGetQuery(conn, glue::glue("
        SELECT c.question_name AS question_name, c.assignment_repo AS assignment_repo, c.comment_text AS comment_text
        FROM <<table>> c
        INNER JOIN (
          SELECT MAX(id) AS max_id
          FROM <<table>>
          GROUP BY question_name, assignment_repo
        ) latest ON c.id = latest.max_id
      ", .open = "<<", .close = ">>"))
      latest[
        !is.na(latest$comment_text) & nchar(trimws(latest$comment_text)) > 0,
        c("question_name", "assignment_repo"),
        drop = FALSE
      ]
    }

    unique(rbind(
      grade_pairs,
      nonempty_latest_pairs("comments"),
      nonempty_latest_pairs("private_comments")
    ))
  })
}

# Per-repo per-question scores recomputed from the grading database
#
# Replicates the mark() rubric score: the points of the currently selected
# rubric items (most recent grade event per item, by autoincrement id) are
# summed and fed through the question's grading mode and bounds from the
# settings table, defaulting to the app's positive mode with a total of 10
# when a question has no settings row. The stored current_score is ephemeral
# app state and is never used. A pair that does not count as graded (see
# graded_question_pairs()) gets NA rather than a misleading 0 or full marks.
#
# collection_path: Path to collection directory
# question_names: Character vector of question names
# assignment_repos: Character vector of assignment repository names
# Returns: Data frame with question_name, assignment_repo, and score columns

collect_score_data = function(collection_path, question_names, assignment_repos) {
  data = with_database(collection_path, function(conn) {
    list(
      settings = load_all_settings(conn),
      items = load_all_items(conn),
      grades = load_most_recent_grades(conn)
    )
  })
  graded = graded_question_pairs(collection_path)
  graded_keys = paste(graded$question_name, graded$assignment_repo)

  selected = merge(
    data$grades[data$grades$selected == 1, c("question_name", "assignment_repo", "item_id"), drop = FALSE],
    data$items[, c("question_name", "item_id", "points"), drop = FALSE],
    by = c("question_name", "item_id")
  )

  rows = list()
  for (question_name in question_names) {
    state = db_row_to_grade_state(data$settings[data$settings$question_name == question_name, , drop = FALSE])
    if (is.null(state)) {
      state = markermd_grade_state(current_score = 0, total_score = 10)
    }

    for (repo_name in assignment_repos) {
      score = NA_real_
      if (paste(question_name, repo_name) %in% graded_keys) {
        points_sum = sum(selected$points[
          selected$question_name == question_name & selected$assignment_repo == repo_name
        ])
        score = if (state@grading_mode == "positive") points_sum else state@total_score + points_sum
        if (state@bound_above_zero && score < 0) {
          score = 0
        }
        if (state@bound_below_max && score > state@total_score) {
          score = state@total_score
        }
      }

      rows[[length(rows) + 1]] = data.frame(
        question_name = question_name,
        assignment_repo = repo_name,
        score = score,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      question_name = character(0), assignment_repo = character(0),
      score = numeric(0), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

# Calculate grading progress for all assignments
#
# Computes, for every requested repository, how many of the requested questions
# have grading data (see graded_question_pairs() for what counts as graded).
#
# collection_path: Path to collection directory
# question_names: Character vector of question names
# assignment_repos: Character vector of assignment repository names
# graded_pairs: Optional precomputed graded_question_pairs() result, so callers
#   that already hold the pairs avoid a second set of queries
# Returns: Named integer vector keyed by repo with graded question counts

calculate_grading_progress = function(collection_path, question_names, assignment_repos, graded_pairs = NULL) {
  if (length(question_names) == 0 || length(assignment_repos) == 0) {
    return(stats::setNames(integer(0), character(0)))
  }

  if (is.null(graded_pairs)) {
    graded_pairs = graded_question_pairs(collection_path)
  }

  graded_pairs = graded_pairs[
    graded_pairs$question_name %in% question_names &
      graded_pairs$assignment_repo %in% assignment_repos,
    ,
    drop = FALSE
  ]

  counts = table(factor(graded_pairs$assignment_repo, levels = assignment_repos))
  stats::setNames(as.integer(counts), assignment_repos)
}

# Calculate grading progress for a single question across all assignments
#
# collection_path: Path to collection directory
# question_name: Character string - name of the question
# assignment_repos: Character vector of assignment repository names
# Returns: List with graded_count and total_count

calculate_question_progress = function(collection_path, question_name, assignment_repos) {
  result = with_database(collection_path, function(conn) {
    graded_count = 0L

    for (repo in assignment_repos) {
      is_graded = FALSE

      # Check if there are any selected rubric items for this question/assignment
      grade_query = DBI::dbGetQuery(conn, glue::glue("
        SELECT COUNT(*) as selected_count
        <<most_recent_grade_join>> AND g1.selected = 1
      ", .open = "<<", .close = ">>"),
        params = list(question_name, repo, question_name, repo))

      # If any rubric items are selected, consider it graded
      if (grade_query$selected_count > 0) {
        is_graded = TRUE
      } else {
        # Check if there's a non-empty comment for this question/assignment
        comment_query = DBI::dbGetQuery(conn, "
          SELECT COUNT(*) as comment_count
          FROM comments
          WHERE question_name = ? AND assignment_repo = ? AND TRIM(comment_text) != ''
          ORDER BY timestamp DESC
          LIMIT 1
        ", params = list(question_name, repo))

        if (comment_query$comment_count > 0) {
          is_graded = TRUE
        }
      }

      if (is_graded) {
        graded_count = graded_count + 1L
      }
    }

    return(list(
      graded_count = graded_count,
      total_count = length(assignment_repos)
    ))
  })

  return(result)
}

# Save a grading template into the project database (its canonical store).
#
# Serializes the template with the format-agnostic template_to_list() converter
# and stores it as a JSON blob under the "template" metadata key.
#
# collection_path: Path to the project root
# template: markermd_template S7 object
# source_path: Assignment-document path recorded under source.path (optional;
#   prefer a root-relative path so it resolves on reload)

save_template_to_db = function(collection_path, template, source_path = NULL) {
  lst = template_to_list(template, source_path = source_path)
  json = as.character(jsonlite::toJSON(lst, auto_unbox = TRUE, null = "null"))
  with_database(collection_path, function(conn) {
    set_metadata(conn, "template", json)
    TRUE
  })
}

# Load the grading template from the project database.
#
# Returns the stored markermd_template S7 object, or NULL when the database has
# no template. base_dir / assignment / require_ast mirror read_template_yaml():
# the source document named in the stored template is re-parsed to rebuild the
# AST (never stored), resolved relative to base_dir (the project root) or an
# explicit assignment override.
#
# collection_path: Path to the project root
# base_dir: Directory used to resolve a relative source.path (defaults to root)
# assignment: Explicit assignment-document override (optional)
# require_ast: When TRUE an unresolvable source is an error; FALSE yields an empty AST

load_template_from_db = function(collection_path, base_dir = collection_path,
                                 assignment = NULL, require_ast = FALSE) {
  json = with_database(collection_path, function(conn) get_metadata(conn, "template"))
  if (is.null(json)) return(NULL)
  x = jsonlite::fromJSON(json, simplifyVector = FALSE)
  template_from_list(x, base_dir = base_dir, assignment = assignment, require_ast = require_ast)
}