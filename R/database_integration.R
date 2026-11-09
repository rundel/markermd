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
    bound_below_max = as.logical(settings_row$bound_below_max[1]),
    optional = isTRUE(as.logical(settings_row$optional[1]))
  )
}

# Hotkey for a rubric item at a given display position: positions 1-10 map to
# hotkeys 1-10 (shown as 1-9 and 0), anything past the tenth has none and is
# toggled by clicking. There is no limit on the number of items.
#
# position: Integer display position (1-based); NA gives NA

position_hotkey = function(position) {
  position = as.integer(position)
  if (length(position) != 1 || is.na(position) || position > 10L) NA_integer_ else position
}

# Convert database items row to markermd_rubric_item S7 object. The hotkey is
# derived from the row's display position; a row without one (see
# items_display_order_sql) falls back to its stored hotkey column.
#
# items_row: Single row data frame from items table
# selected: Logical indicating if item is selected (not stored in database)
# Returns: markermd_rubric_item S7 object

db_row_to_rubric_item = function(items_row, selected = FALSE) {
  if (nrow(items_row) == 0) {
    return(NULL)
  }

  position = items_row$position[1]
  hotkey = if (!is.null(position) && !is.na(position)) {
    position_hotkey(position)
  } else if (is.na(items_row$hotkey[1])) {
    NA_integer_
  } else {
    as.integer(items_row$hotkey[1])
  }

  group = items_row$group_id[1]
  group = if (is.null(group) || is.na(group)) NA_character_ else as.character(group)

  markermd_rubric_item(
    hotkey = hotkey,
    points = items_row$points[1],
    description = items_row$description[1],
    selected = selected,
    group = group
  )
}

# Convert a database item_groups row to a markermd_rubric_group S7 object
#
# group_row: Single row data frame from the item_groups table
# Returns: markermd_rubric_group S7 object

db_row_to_rubric_group = function(group_row) {
  if (nrow(group_row) == 0) {
    return(NULL)
  }
  markermd_rubric_group(
    label = group_row$label[1],
    floor = as.numeric(group_row$floor[1]),
    ceiling = as.numeric(group_row$ceiling[1]),
    exclusive = as.logical(group_row$exclusive[1])
  )
}

# Item groups for one question out of full item_groups and items data
# frames, as a named list of markermd_rubric_group keyed by group id in
# display order: a group sits where its first member does, and groups with
# no members come after every item, by insertion id.
#
# groups_df: Data frame of item_groups rows (see load_all_item_groups())
# items_df: Data frame of items rows (see load_all_items())
# question_name: Character string

groups_df_to_list = function(groups_df, items_df, question_name) {
  rows = groups_df[groups_df$question_name == question_name, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(list())
  }

  item_rows = items_df[items_df$question_name == question_name, , drop = FALSE]
  item_rows = item_rows[order(item_rows$position, item_rows$hotkey, item_rows$id), , drop = FALSE]
  first_member = vapply(rows$group_id, function(gid) {
    hit = which(!is.na(item_rows$group_id) & item_rows$group_id == gid)
    if (length(hit) == 0) Inf else min(hit)
  }, numeric(1))
  rows = rows[order(first_member, rows$id), , drop = FALSE]

  groups = list()
  for (i in seq_len(nrow(rows))) {
    groups[[rows$group_id[i]]] = db_row_to_rubric_group(rows[i, ])
  }
  groups
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

# Save a question's rubric items in display order, atomically: the list order
# (normalized so each group's members are contiguous) becomes each item's
# stored position (and the hotkey it implies), so a renumbering after a move
# or delete cannot be half-persisted, and it pays one connection rather than
# one per item. This is the app's only writer of display order.
#
# collection_path: Path to collection directory
# question_name: Character string
# items: Named list of markermd_rubric_item S7 objects keyed by item_id, in
#   display order

save_rubric_items = function(collection_path, question_name, items) {
  items = normalize_item_order(items)
  with_database(collection_path, function(conn) {
    with_transaction(conn, "Failed to save rubric items", {
      for (i in seq_along(items)) {
        item = items[[i]]
        item@hotkey = position_hotkey(i)
        upsert_items(conn, question_name, names(items)[i], item, position = i)
      }
    })
    invisible(TRUE)
  })
}

# Delete rubric item from database, along with its grade-selection events,
# optionally after an automatic snapshot taken in the same transaction (the
# marking app passes one when the item has recorded selections, the case
# where grading would otherwise be lost)
#
# collection_path: Path to collection directory
# question_name: Character string
# item_id: Character string (unique identifier for this item)
# snapshot: NULL, or a list with trigger and summary for snapshot_before_change()

delete_rubric_item = function(collection_path, question_name, item_id, snapshot = NULL) {
  with_database(collection_path, function(conn) {
    with_transaction(conn, "Failed to delete the rubric item", {
      snapshot_before_change(conn, collection_path, snapshot)
      delete_item_records(conn, question_name, item_id)
    })
    TRUE
  })
}

# How many repositories currently have an item selected (by the most recent
# grade event per repository), the marking app's test for whether deleting
# the item loses grading and so deserves an automatic snapshot
#
# collection_path: Path to collection directory
# question_name: Character string
# item_id: Character string

item_selection_count = function(collection_path, question_name, item_id) {
  with_database(collection_path, function(conn) {
    grades = latest_rows(
      conn, "grades", "assignment_repo",
      where = "question_id = ? AND item_id = ?", params = list(question_id_conn(conn, question_name, create = FALSE), item_id)
    )
    sum(grades$selected == 1)
  })
}

# How many repositories currently have each item selected (by the most
# recent grade event per repository and item), for the Rubric tab's editor
#
# collection_path: Path to collection directory
# Returns: Named integer vector keyed by "<question>\x1f<item_id>"; items
#   nobody has selected are absent

item_selection_counts = function(collection_path) {
  with_database(collection_path, function(conn) {
    grades = latest_rows(conn, "grades", c("question_name", "assignment_repo", "item_id"))
    grades = grades[grades$selected == 1, , drop = FALSE]
    counts = table(paste(grades$question_name, grades$item_id, sep = "\x1f"))
    stats::setNames(as.integer(counts), names(counts))
  })
}

# Save (insert or update) a rubric item group
#
# collection_path: Path to collection directory
# question_name: Character string
# group_id: Character string (unique identifier for this group)
# group: markermd_rubric_group S7 object

save_item_group = function(collection_path, question_name, group_id, group) {
  with_database(collection_path, function(conn) {
    upsert_item_group(conn, question_name, group_id, group)
    return(TRUE)
  })
}

# Delete a rubric item group, leaving its members ungrouped in place,
# optionally after an automatic snapshot taken in the same transaction
#
# collection_path: Path to collection directory
# question_name: Character string
# group_id: Character string
# snapshot: NULL, or a list with trigger and summary for snapshot_before_change()

delete_item_group = function(collection_path, question_name, group_id, snapshot = NULL) {
  with_database(collection_path, function(conn) {
    with_transaction(conn, "Failed to delete the item group", {
      snapshot_before_change(conn, collection_path, snapshot)
      delete_item_group_records(conn, question_name, group_id)
    })
    TRUE
  })
}

# Load a question's rubric item groups in display order
#
# collection_path: Path to collection directory
# question_name: Character string
# Returns: Named list of markermd_rubric_group keyed by group id (see
#   groups_df_to_list())

load_item_groups = function(collection_path, question_name) {
  with_database(collection_path, function(conn) {
    groups_df_to_list(load_all_item_groups(conn), load_all_items(conn), question_name)
  })
}

# Repositories with more than one of the given items selected (see
# group_selection_conflicts())
#
# collection_path: Path to collection directory
# question_name: Character string
# member_ids: Character vector of item ids

load_group_selection_conflicts = function(collection_path, question_name, member_ids) {
  with_database(collection_path, function(conn) {
    group_selection_conflicts(conn, question_name, member_ids)
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

# Save several grade selection events for one pair in one transaction (a
# toggle in an exclusive group writes the item and its deselected siblings
# together)
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# selections: Named logical vector or list keyed by item id

save_grade_selections = function(collection_path, question_name, assignment_repo, selections) {
  if (length(selections) == 0) {
    return(invisible(FALSE))
  }
  with_database(collection_path, function(conn) {
    with_transaction(conn, "Failed to save the selection", {
      for (item_id in names(selections)) {
        insert_grade(conn, question_name, assignment_repo, item_id, isTRUE(selections[[item_id]]))
      }
    })
    invisible(TRUE)
  })
}

# Save a comment event to one of the comment tables
#
# collection_path: Path to collection directory
# table: "comments" (public) or "private_comments"
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

save_comment_row = function(collection_path, table, question_name, assignment_repo, comment_text) {
  with_database(collection_path, function(conn) {
    insert_comment_row(conn, table, question_name, assignment_repo, comment_text)
    return(TRUE)
  })
}

# Save comment to database (public, student-facing)
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

save_comment = function(collection_path, question_name, assignment_repo, comment_text) {
  save_comment_row(collection_path, "comments", question_name, assignment_repo, comment_text)
}

# Save private comment to database (grader-internal, never student-facing)
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# comment_text: Character string

save_private_comment = function(collection_path, question_name, assignment_repo, comment_text) {
  save_comment_row(collection_path, "private_comments", question_name, assignment_repo, comment_text)
}

# Load grade state from database
#
# collection_path: Path to collection directory
# question_name: Character string
# Returns: markermd_grade_state S7 object or NULL if not found

load_grade_state = function(collection_path, question_name) {
  with_database(collection_path, function(conn) {
    settings_data = DBI::dbGetQuery(conn, "
      SELECT * FROM settings WHERE question_id = ?
    ", params = list(question_id_conn(conn, question_name, create = FALSE)))
    
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
    items_data = DBI::dbGetQuery(conn, paste0("
      SELECT * FROM items WHERE question_id = ?
      ORDER BY ", items_display_order_sql
    ), params = list(question_id_conn(conn, question_name, create = FALSE)))
    
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
    grades_data = latest_rows(
      conn, "grades", "item_id",
      where = "question_id = ? AND assignment_repo = ?",
      params = list(question_id_conn(conn, question_name, create = FALSE), assignment_repo)
    )

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
  load_comment_row(collection_path, "comments", question_name, assignment_repo)
}

# Load private comment for a specific question and assignment
#
# collection_path: Path to collection directory
# question_name: Character string
# assignment_repo: Character string
# Returns: Character string or NULL if no private comment found

load_private_comment = function(collection_path, question_name, assignment_repo) {
  load_comment_row(collection_path, "private_comments", question_name, assignment_repo)
}

# The latest comment of a pair in one of the comment tables, or NULL
#
# collection_path: Path to collection directory
# table: "comments" or "private_comments"
# question_name: Character string
# assignment_repo: Character string

load_comment_row = function(collection_path, table, question_name, assignment_repo) {
  table = match.arg(table, comment_tables())
  with_database(collection_path, function(conn) {
    comment_data = DBI::dbGetQuery(conn, glue::glue("
      SELECT comment_text
      FROM {table}
      WHERE question_id = ? AND assignment_repo = ?
      ORDER BY id DESC
      LIMIT 1
    "), params = list(question_id_conn(conn, question_name, create = FALSE), assignment_repo))

    if (nrow(comment_data) == 0) {
      return(NULL)
    }

    comment_data$comment_text[1]
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

  question_names = template_question_names(template_obj)

  # One connection for the whole load rather than three per question
  data = with_database(collection_path, function(conn) {
    list(
      settings = load_all_settings(conn),
      items = load_all_items(conn),
      groups = load_all_item_groups(conn)
    )
  })

  state = list(
    grade_states = list(),
    rubric_items = list(),
    rubric_groups = list()
  )

  for (question_name in question_names) {
    loaded_grade_state = db_row_to_grade_state(
      data$settings[data$settings$question_name == question_name, , drop = FALSE]
    )
    if (is.null(loaded_grade_state)) {
      # Use default grade state if none found
      loaded_grade_state = markermd_grade_state(current_score = 0, total_score = 10)
    }
    state$grade_states[[question_name]] = loaded_grade_state

    state$rubric_items[[question_name]] = items_df_to_rubric_list(data$items, question_name)
    state$rubric_groups[[question_name]] = groups_df_to_list(data$groups, data$items, question_name)
  }

  return(state)
}

# Rubric items for one question out of a full items-table data frame, as a
# named list of markermd_rubric_item in display order, matching
# load_rubric_items(). This is the R-side twin of items_display_order_sql:
# position first, then the legacy hotkey / insertion-id fallback for rows
# without one (order() already sorts NA last within each key).
#
# items_df: Data frame of items rows (see load_all_items())
# question_name: Character string

items_df_to_rubric_list = function(items_df, question_name) {
  rows = items_df[items_df$question_name == question_name, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(list())
  }

  rows = rows[order(rows$position, rows$hotkey, rows$id), , drop = FALSE]

  items = list()
  for (i in seq_len(nrow(rows))) {
    items[[rows$item_id[i]]] = db_row_to_rubric_item(rows[i, ], selected = FALSE)
  }
  items
}

# Mint n fresh ids in the app's "item_<k>" style (or "group_<k>" for item
# groups), continuing past the largest numeric suffix among the ids already
# in use so imports never reuse an id.
#
# existing_ids: Character vector of ids that must not be reused
# n: Number of ids to mint
# prefix: Id prefix, "item_" or "group_"

next_item_ids = function(existing_ids, n, prefix = "item_") {
  if (n == 0) {
    return(character(0))
  }

  pattern = paste0("^", prefix, "[0-9]+$")
  suffixes = substring(existing_ids[grepl(pattern, existing_ids)], nchar(prefix) + 1)
  k = if (length(suffixes) == 0) 0L else max(as.integer(suffixes)) + 1L

  ids = character(0)
  while (length(ids) < n) {
    candidate = paste0(prefix, k)
    if (!candidate %in% existing_ids) {
      ids = c(ids, candidate)
    }
    k = k + 1L
  }
  ids
}

# Repositories that would violate an exclusive group's rule once a rubric
# import has been applied in append mode: for every group the file marks
# exclusive that already exists (matched by label), the repositories with
# more than one of its current members selected. Replace mode deletes the
# existing items and their selections, so it can never conflict. Checked
# before anything is written.
#
# conn: DBI connection object
# rubric: Exchange-list rubric (format_version + questions)
# Returns: Data frame with question, group (label), assignment_repo and
#   items (the selected member descriptions, joined)

exclusive_import_conflicts = function(conn, rubric) {
  groups_df = load_all_item_groups(conn)
  items_df = load_all_items(conn)
  rows = list()

  for (question in rubric$questions) {
    for (group in question$groups) {
      if (!isTRUE(group@exclusive)) {
        next
      }
      db_id = groups_df$group_id[
        groups_df$question_name == question$name & groups_df$label == group@label
      ]
      if (length(db_id) != 1) {
        next
      }
      members = items_df[
        items_df$question_name == question$name &
          !is.na(items_df$group_id) & items_df$group_id == db_id,
        , drop = FALSE
      ]
      conflicts = group_selection_conflicts(conn, question$name, members$item_id)
      for (i in seq_len(nrow(conflicts))) {
        rows[[length(rows) + 1]] = data.frame(
          question = question$name,
          group = group@label,
          assignment_repo = conflicts$assignment_repo[i],
          items = paste(
            members$description[match(conflicts$item_ids[[i]], members$item_id)],
            collapse = "; "
          ),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      question = character(0), group = character(0),
      assignment_repo = character(0), items = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

# Apply a parsed rubric (the exchange list from read_rubric_yaml()) to a
# project's grading database in a single transaction.
#
# Mode "append" keeps each question's existing items and groups and adds the
# file's items after them; "replace" deletes the existing items (which also
# removes their per-repo grade-selection events, see delete_item_records)
# and groups first. The file's groups are matched to the question's existing
# groups by label: a match reuses the stored group (its floor, ceiling and
# exclusive flag are overwritten by the file's, just as scoring overwrites
# settings) and an unknown label creates a new group. In both modes the
# combined item list is written back with contiguous display positions
# (kept items first, in their current order, then the file's items in file
# order, normalized so an item imported into an existing group lands at the
# end of that group and a new group lands at the end of the question) and the
# hotkeys those positions imply (1-10, NA beyond). Fresh item and group ids
# are minted avoiding every id in the database (across all questions, since
# the mark app's module-id namespace is shared) plus any reserved_ids the
# caller has ever bound. Making an existing group exclusive while some
# repository has more than one of its items selected aborts before anything
# is written (exclusive_import_conflicts()).
#
# The import is one transaction, preceded by an automatic snapshot of the
# state it replaces when the caller asks for one (snapshot_before_change()),
# so the import and its snapshot commit or roll back together.
#
# collection_path: Project root containing the grading database
# rubric: Exchange-list rubric (format_version + questions)
# mode: "append" or "replace"
# reserved_ids: Additional item / group ids that must not be reused
# snapshot: NULL, or a list with trigger and summary for the automatic
#   snapshot taken before the import
# Returns: Named list (by question name) of summaries with mode, n_existing,
#   new_ids, new_items (markermd_rubric_item objects as written, hotkeys and
#   group ids final), items (every item of the question as written, a named
#   list in display order), hotkey_changes (named integer vector for kept
#   items whose hotkey changed; NA means the hotkey was cleared), group_ids
#   (the question's group ids after the import) and scoring
#   (markermd_grade_state or NULL)

apply_rubric_import = function(collection_path, rubric, mode, reserved_ids = character(0), snapshot = NULL) {
  with_database(collection_path, function(conn) {
    check_exclusive_import_conflicts(conn, rubric, mode)
    with_transaction(conn, "Failed to import rubric", {
      snapshot_before_change(conn, collection_path, snapshot)
      apply_rubric_import_conn(conn, rubric, mode, reserved_ids)
    })
  })
}

# Abort an append-mode import that would make a group exclusive while some
# repository has more than one of its items selected (see
# exclusive_import_conflicts()); replace mode can never conflict
#
# conn: DBI connection object
# rubric: Exchange-list rubric
# mode: "append" or "replace"

check_exclusive_import_conflicts = function(conn, rubric, mode) {
  if (!identical(mode, "append")) {
    return(invisible(NULL))
  }
  conflicts = exclusive_import_conflicts(conn, rubric)
  if (nrow(conflicts) > 0) {
    detail = glue::glue(
      "{conflicts$assignment_repo} / {conflicts$question}, group \"{conflicts$group}\": {conflicts$items}"
    )
    cli::cli_abort(c(
      "Cannot import the rubric: making a group exclusive while repositories have more than one of its items selected.",
      stats::setNames(as.character(detail), rep("x", length(detail))),
      "i" = "Resolve those selections first (in {.code mark()} or with {.code marks_set()}), or import with {.code mode = \"replace\"}."
    ))
  }
  invisible(NULL)
}

# The body of apply_rubric_import(), run inside the caller's transaction on
# an open connection (the snapshot restore runs it alongside the marks
# restore in one transaction). Same arguments and result as
# apply_rubric_import() without the connection management.
#
# conn: DBI connection object
# rubric: Exchange-list rubric (format_version + questions)
# mode: "append" or "replace"
# reserved_ids: Additional item / group ids that must not be reused

apply_rubric_import_conn = function(conn, rubric, mode, reserved_ids = character(0)) {
  taken = union(DBI::dbGetQuery(conn, "SELECT item_id FROM items")$item_id, reserved_ids)
  taken_groups = union(DBI::dbGetQuery(conn, "SELECT group_id FROM item_groups")$group_id, reserved_ids)
  summaries = list()

  for (question in rubric$questions) {
    question_name = question$name

    existing = DBI::dbGetQuery(conn, paste0("
      SELECT * FROM items WHERE question_id = ?
      ORDER BY ", items_display_order_sql
    ), params = list(question_id_conn(conn, question_name, create = FALSE)))
    existing_groups = DBI::dbGetQuery(conn, "
      SELECT * FROM item_groups WHERE question_id = ?
    ", params = list(question_id_conn(conn, question_name, create = FALSE)))
    n_existing = nrow(existing)

    if (identical(mode, "replace")) {
      for (item_id in existing$item_id) {
        delete_item_records(conn, question_name, item_id)
      }
      for (group_id in existing_groups$group_id) {
        delete_item_group_records(conn, question_name, group_id)
      }
      existing = existing[0, ]
      existing_groups = existing_groups[0, ]
    }

    # The file's groups, matched to stored groups by label; the exchange
    # list's transient group ids are mapped to database ids
    group_map = character(0)
    file_groups = if (is.null(question$groups)) list() else question$groups
    for (exchange_id in names(file_groups)) {
      group = file_groups[[exchange_id]]
      db_id = existing_groups$group_id[existing_groups$label == group@label]
      if (length(db_id) != 1) {
        db_id = next_item_ids(taken_groups, 1, prefix = "group_")
        taken_groups = union(taken_groups, db_id)
      }
      group_map[[exchange_id]] = db_id
      upsert_item_group(conn, question_name, db_id, group)
    }

    kept_items = items_df_to_rubric_list(existing, question_name)
    kept_ids = names(kept_items)

    # Descriptions identify items in every exchange format, so an append
    # must not create a second item with a stored description
    incoming = vapply(question$items, function(item) item@description, character(1))
    clashing = unique(incoming[incoming %in% existing$description])
    if (length(clashing) > 0) {
      cli::cli_abort(c(
        "Cannot append to question {.val {question_name}}: {cli::qty(length(clashing))}{?an item with this description is/items with these descriptions are} already stored: {.val {clashing}}.",
        "i" = "Reword or re-point a stored item with {.code rubric_item_set()}, or import only the new items (or use {.code mode = \"replace\"})."
      ))
    }

    new_ids = next_item_ids(taken, length(question$items))
    taken = union(taken, new_ids)
    new_items = lapply(question$items, function(item) {
      if (!is.na(item@group)) {
        item@group = group_map[[item@group]]
      }
      item
    })
    names(new_items) = new_ids

    # The final order: kept items, then the file's, pulled together by
    # group; every item is (re)written whose position, hotkey or group
    # differs from what is stored. hotkey_changes reports only the
    # visible part, for kept items.
    final = normalize_item_order(c(kept_items, new_items))
    hotkey_changes = integer(0)
    for (i in seq_along(final)) {
      item_id = names(final)[i]
      item = final[[i]]
      item@hotkey = position_hotkey(i)
      final[[i]] = item

      if (item_id %in% kept_ids) {
        row = existing[existing$item_id == item_id, , drop = FALSE]
        stored_hotkey = as.integer(row$hotkey[1])
        stored_position = as.integer(row$position[1])
        if (!identical(stored_hotkey, item@hotkey) || !identical(stored_position, i)) {
          upsert_items(conn, question_name, item_id, item, position = i)
        }
        if (!identical(stored_hotkey, item@hotkey)) {
          hotkey_changes[item_id] = item@hotkey
        }
      } else {
        upsert_items(conn, question_name, item_id, item, position = i)
      }
    }

    if (!is.null(question$scoring)) {
      upsert_settings(conn, question_name, question$scoring)
    }

    summaries[[question_name]] = list(
      mode = mode,
      n_existing = n_existing,
      new_ids = new_ids,
      new_items = unname(final[new_ids]),
      items = final,
      hotkey_changes = hotkey_changes,
      group_ids = union(existing_groups$group_id, unname(group_map)),
      scoring = question$scoring
    )
  }

  summaries
}

# Apply a resolved marks plan to a project's grading database in a single
# transaction, preceded by an automatic snapshot when the caller asks for
# one. Each entry is one (repo, question) pair with fully-resolved
# selections (named logical keyed by item_id, covering every item of the
# question, so the pair's state is declarative; NULL to leave selections
# untouched) and optional public / private comment text appended to their
# event logs. The settings table is never touched: current_score is ephemeral
# state the mark app recomputes from live selections.
#
# collection_path: Project root containing the grading database
# plan: List of entries with repo, question, selections, comment, private_comment
# snapshot: NULL, or a list with trigger and summary for the automatic
#   snapshot taken before the writes
# Returns: TRUE

apply_marks_import = function(collection_path, plan, snapshot = NULL) {
  with_database(collection_path, function(conn) {
    with_transaction(conn, "Failed to import marks", {
      snapshot_before_change(conn, collection_path, snapshot)
      apply_marks_import_conn(conn, plan)
    })
    TRUE
  })
}

# The writes of apply_marks_import(), on an open connection inside the
# caller's transaction
#
# conn: DBI connection object
# plan: List of entries with repo, question, selections, comment, private_comment

apply_marks_import_conn = function(conn, plan) {
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
  invisible(TRUE)
}

# All (question, repo) pairs that currently count as graded
#
# A question/repo pair is graded if it has a selected rubric item (using the
# most recent grade per item, by autoincrement id since timestamps can tie
# within a second) or a non-empty most recent comment. Two grouped queries
# cover all pairs at once rather than running a query per pair. When
# assignment_repos is given, every pair of an optional question (see
# markermd_grade_state) with one of those repos also counts as graded: an
# optional question with nothing recorded scores as if no item were selected.
#
# collection_path: Path to collection directory
# assignment_repos: Optional character vector of repository names whose
#   optional-question pairs count as graded; NULL leaves them out (callers
#   that do not know the repositories see only recorded grading)
# Returns: Data frame with question_name and assignment_repo columns

graded_question_pairs = function(collection_path, assignment_repos = NULL) {
  with_database(collection_path, function(conn) {
    graded_question_pairs_conn(conn, assignment_repos)
  })
}

# graded_question_pairs() on an open connection
#
# conn: DBI connection object
# assignment_repos: See graded_question_pairs()

graded_question_pairs_conn = function(conn, assignment_repos = NULL) {
  graded_pairs_from_frames(grading_frames_from_conn(conn), assignment_repos)
}

# The live database as a grading frame (see utils_mark_scores.R): every
# question with a settings row or an item, the most recent grade per item
# (by autoincrement id, since timestamps can tie within a second) where it
# selects an item that still exists (a grade row naming an item that no
# longer exists, written by a session whose rubric was replaced under it,
# does not count), and the pairs whose most recent public comment is
# non-empty. One read of each table, however many pairs are scored.
#
# conn: DBI connection object

grading_frames_from_conn = function(conn) {
  settings = load_all_settings(conn)
  items = load_all_items(conn)
  groups = load_all_item_groups(conn)

  question_names = unique(c(settings$question_name, items$question_name))
  questions = lapply(question_names, function(question_name) {
    state = db_row_to_grade_state(settings[settings$question_name == question_name, , drop = FALSE])
    list(
      items = items_df_to_rubric_list(items, question_name),
      groups = groups_df_to_list(groups, items, question_name),
      state = if (is.null(state)) default_grade_state() else state
    )
  })
  names(questions) = question_names

  latest_grades = load_most_recent_grades(conn)
  known = paste(latest_grades$question_name, latest_grades$item_id, sep = "\x1f") %in%
    paste(items$question_name, items$item_id, sep = "\x1f")
  selected = latest_grades[known & latest_grades$selected == 1, , drop = FALSE]

  comments = nonempty_comments(load_most_recent_comments(conn))

  list(
    questions = questions,
    selections = data.frame(
      question_name = as.character(selected$question_name),
      assignment_repo = as.character(selected$assignment_repo),
      item_id = as.character(selected$item_id),
      stringsAsFactors = FALSE
    ),
    comment_pairs = data.frame(
      question_name = as.character(comments$question_name),
      assignment_repo = as.character(comments$assignment_repo),
      stringsAsFactors = FALSE
    )
  )
}

# Elementwise TRUE test for an integer/logical column that may be absent
# (NULL) or contain NA, as settings$optional does in a database the
# optional-question migration has not yet touched
#
# x: Vector to test (may be NULL)

is_true_flag = function(x) {
  if (is.null(x)) return(logical(0))
  !is.na(x) & as.logical(x)
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
  with_database(collection_path, marked_question_pairs_conn)
}

# marked_question_pairs() on an open connection
#
# conn: DBI connection object

marked_question_pairs_conn = function(conn) {
  grade_pairs = DBI::dbGetQuery(conn, paste(
    "SELECT DISTINCT question_name, assignment_repo FROM grades WHERE", live_question_sql
  ))

  nonempty_latest_pairs = function(table) {
    nonempty_comments(latest_rows(conn, table, c("question_name", "assignment_repo")))[
      , c("question_name", "assignment_repo"), drop = FALSE
    ]
  }

  unique(rbind(
    grade_pairs,
    nonempty_latest_pairs("comments"),
    nonempty_latest_pairs("private_comments")
  ))
}

# Per-repo per-question scores recomputed from the grading database
#
# Replicates the mark() rubric score through score_question(): the currently
# selected rubric items (most recent grade event per item, by autoincrement
# id) are scored with the question's item groups, grading mode and bounds
# from the settings table, defaulting to the app's positive mode with a
# total of 10 when a question has no settings row. The stored current_score
# is ephemeral app state and is never used. A pair that does not count as
# graded (see graded_question_pairs()) gets NA rather than a misleading 0 or
# full marks. The same scorer, score_pairs_from_frames(), gives a snapshot
# its cached scores (score_snapshot_parts()).
#
# collection_path: Path to collection directory
# question_names: Character vector of question names
# assignment_repos: Character vector of assignment repository names
# Returns: Data frame with question_name, assignment_repo and score columns

collect_score_data = function(collection_path, question_names, assignment_repos) {
  with_database(collection_path, function(conn) {
    collect_score_data_conn(conn, question_names, assignment_repos)
  })
}

# collect_score_data() on an open connection
#
# conn: DBI connection object
# question_names: Character vector of question names
# assignment_repos: Character vector of assignment repository names

collect_score_data_conn = function(conn, question_names, assignment_repos) {
  score_pairs_from_frames(grading_frames_from_conn(conn), question_names, assignment_repos)
}

# Calculate grading progress for all assignments
#
# Computes, for every requested repository, how many of the requested questions
# have grading data (see graded_question_pairs() for what counts as graded).
#
# collection_path: Path to collection directory
# question_names: Character vector of question names
# assignment_repos: Character vector of assignment repository names
# graded_pairs: Optional precomputed graded_question_pairs() result (given the
#   same repositories, so optional questions count), so callers
#   that already hold the pairs avoid a second set of queries
# Returns: Named integer vector keyed by repo with graded question counts

calculate_grading_progress = function(collection_path, question_names, assignment_repos, graded_pairs = NULL) {
  if (length(question_names) == 0 || length(assignment_repos) == 0) {
    return(stats::setNames(integer(0), character(0)))
  }

  if (is.null(graded_pairs)) {
    graded_pairs = graded_question_pairs(collection_path, assignment_repos)
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

# Save a grading template into the project database (its canonical store).
#
# Serializes the template with the format-agnostic template_to_list() converter
# and stores it as a JSON blob under the "template" metadata key. The
# grading tables key on question names, so a question the new template
# renames (same id, new name) has its rubric, settings and marks carried
# over to the new name in the same transaction (see
# reconcile_question_names_conn()); the result says what was carried over
# and which removed questions still hold grading under their old name.
#
# collection_path: Path to the project root
# template: markermd_template S7 object
# source_path: Assignment-document path recorded under source.path (optional;
#   prefer a root-relative path so it resolves on reload)
# snapshot: NULL, or a list with trigger and summary for an automatic
#   snapshot of the state being overwritten, taken in the same transaction
# Returns: Invisibly, the list from reconcile_question_names_conn()

save_template_to_db = function(collection_path, template, source_path = NULL, snapshot = NULL) {
  lst = template_to_list(template, source_path = source_path)
  json = as.character(jsonlite::toJSON(lst, auto_unbox = TRUE, null = "null"))
  invisible(with_database(collection_path, function(conn) {
    with_transaction(conn, "Failed to save the template", {
      snapshot_before_change(conn, collection_path, snapshot)
      changes = reconcile_question_names_conn(conn, lst)
      note_question_ids_conn(conn, lst)
      set_metadata(conn, "template", json)
      changes
    })
  }))
}

# The highest question id the project has ever used: the recorded
# "question_id_max" metadata value, the highest id of the stored template (for
# a database written before the value was recorded) and the highest id in the
# questions registry (which also holds ids minted for names graded before a
# template named them). A new question takes an id above it, so the id of a
# question deleted and saved is never issued again (saving matches questions
# by id, and a reused id would read as a rename).
#
# conn: DBI connection object

question_id_high_water_conn = function(conn) {
  recorded = suppressWarnings(as.integer(get_metadata(conn, "question_id_max")))
  stored = db_template_list(conn)
  ids = vapply(stored$questions, function(q) as.integer(q$id), integer(1))
  registry = DBI::dbGetQuery(conn, "SELECT COALESCE(MAX(question_id), 0) AS m FROM questions")$m
  as.integer(max(c(0L, recorded[!is.na(recorded)], ids, registry)))
}

# The id for a question the template editor adds: one above both the
# session's highest id and the project's high-water mark. An unreadable
# database counts as no stored ids, so the editor keeps working.
#
# root: Project root, or NULL outside a project
# session_max: Highest question id already used in the editor session

next_question_id = function(root, session_max = 0L) {
  stored = 0L
  if (!is.null(root)) {
    stored = purrr::safely(function() with_database(root, question_id_high_water_conn))()$result
    if (is.null(stored)) stored = 0L
  }
  as.integer(max(session_max, stored) + 1L)
}

# Raise the recorded question id high-water mark to cover a template about to
# be stored (and the template it replaces, through
# question_id_high_water_conn()), and the registry's id sequence with it.
# Call before the template is written.
#
# conn: DBI connection object
# lst: Plain template list (template_to_list() shape) about to be stored

note_question_ids_conn = function(conn, lst) {
  ids = vapply(lst$questions, function(q) as.integer(q$id), integer(1))
  set_metadata(conn, "question_id_max", as.character(max(c(question_id_high_water_conn(conn), ids))))
  raise_question_sequence(conn)
}

# Whether any grading table holds rows for a question id
#
# conn: DBI connection object
# question_id: Integer

question_has_grading_conn = function(conn, question_id) {
  for (table in question_keyed_tables()) {
    n = DBI::dbGetQuery(
      conn, glue::glue("SELECT COUNT(*) AS n FROM {table} WHERE question_id = ?"), params = list(question_id)
    )$n
    if (n > 0) {
      return(TRUE)
    }
  }
  FALSE
}

# Move every grading row of one question id to another (the target holds no
# rows), and set the display name its rows carry
#
# conn: DBI connection object
# from: Integer question id the rows have now
# to: Integer question id they move to

rekey_question_rows = function(conn, from, to) {
  for (table in question_keyed_tables()) {
    DBI::dbExecute(conn, glue::glue("UPDATE {table} SET question_id = ? WHERE question_id = ?"), params = list(to, from))
  }
}

# Set the display name every grading row of a question id carries
#
# conn: DBI connection object
# question_id: Integer
# name: Character string

rename_question_rows = function(conn, question_id, name) {
  for (table in question_keyed_tables()) {
    DBI::dbExecute(
      conn, glue::glue("UPDATE {table} SET question_name = ? WHERE question_id = ? AND question_name != ?"),
      params = list(name, question_id, name)
    )
  }
}

# Bring the questions registry in line with a template about to be stored
# (or restored). Grading is keyed by question id, so a question whose name
# changed keeps its rows and only their display name is updated; renames
# within one save (a swap, a chain) need no ordering. A question of the
# template whose id is new to the registry adopts the rows of a name graded
# before any template named it (a minted, not yet templated id), and
# otherwise starts empty, even when an earlier question that left the
# template had the same name: that question's grading stays under its own id
# as an orphan, and comes back only with its id (restoring the old template
# from a snapshot). Minted ids that collide with a template id are moved out
# of the way first. Every registry row outside the template stops being live.
#
# conn: DBI connection object
# current: The template being stored, as a plain list (template_to_list())
# Returns: List with renamed (data frame with from, to and reused, whether
#   another question of the new template carries the old name; only
#   questions with grading are listed) and orphaned (character vector of the
#   names of questions that left the template holding grading)

reconcile_question_names_conn = function(conn, current) {
  out = list(
    renamed = data.frame(from = character(0), to = character(0), reused = logical(0), stringsAsFactors = FALSE),
    orphaned = character(0)
  )
  new_ids = vapply(current$questions, function(q) as.integer(q$id), integer(1))
  new_names = vapply(current$questions, function(q) as.character(q$name), character(1))
  now = utc_timestamp()
  registry = function() DBI::dbGetQuery(conn, "SELECT * FROM questions ORDER BY question_id")

  raise_question_sequence(conn, floor = max(c(0L, new_ids)))
  reg = registry()
  for (rid in reg$question_id[reg$templated == 0 & reg$question_id %in% new_ids]) {
    row = reg[reg$question_id == rid, , drop = FALSE]
    DBI::dbExecute(conn, "
      INSERT INTO questions (name, live, templated, created_at) VALUES (?, ?, 0, ?)
    ", params = list(row$name, row$live, row$created_at))
    fresh = as.integer(DBI::dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id)
    DBI::dbExecute(conn, "DELETE FROM questions WHERE question_id = ?", params = list(rid))
    rekey_question_rows(conn, rid, fresh)
  }

  reg = registry()
  gone = reg[reg$templated == 1 & reg$live == 1 & !reg$question_id %in% new_ids, , drop = FALSE]
  for (i in seq_len(nrow(gone))) {
    if (question_has_grading_conn(conn, gone$question_id[i])) {
      out$orphaned = c(out$orphaned, gone$name[i])
    }
  }

  from = character(0)
  to = character(0)
  for (k in seq_along(new_ids)) {
    id = new_ids[k]
    name = new_names[k]
    row = reg[reg$question_id == id, , drop = FALSE]
    if (nrow(row) > 0) {
      if (!identical(row$name, name) && question_has_grading_conn(conn, id)) {
        from = c(from, row$name)
        to = c(to, name)
      }
      DBI::dbExecute(conn, "UPDATE questions SET name = ?, live = 1, templated = 1 WHERE question_id = ?", params = list(name, id))
    } else {
      minted = reg[reg$templated == 0 & reg$name == name, , drop = FALSE]
      DBI::dbExecute(conn, "
        INSERT INTO questions (question_id, name, live, templated, created_at) VALUES (?, ?, 1, 1, ?)
      ", params = list(id, name, now))
      if (nrow(minted) > 0) {
        adopted = max(minted$question_id)
        DBI::dbExecute(conn, "DELETE FROM questions WHERE question_id = ?", params = list(adopted))
        rekey_question_rows(conn, adopted, id)
      }
    }
    rename_question_rows(conn, id, name)
  }

  if (length(new_ids) > 0) {
    DBI::dbExecute(conn, paste0(
      "UPDATE questions SET live = 0 WHERE live = 1 AND question_id NOT IN (", paste(new_ids, collapse = ", "), ")"
    ))
  } else {
    DBI::dbExecute(conn, "UPDATE questions SET live = 0 WHERE live = 1")
  }

  # A renamed question whose old name another question now carries is the
  # mark of a regenerated template whose ids shifted (see
  # report_question_name_changes())
  out$renamed = data.frame(from = from, to = to, reused = from %in% new_names, stringsAsFactors = FALSE)
  out
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