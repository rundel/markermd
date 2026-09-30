# Shiny module for the marking app's History tab: the project's snapshots
# (see snapshots.R), newest first, with a detail pane for the selected one
# (its fields and notes, what changed between it and the current state or
# another snapshot, and the scores it recorded) and the actions on it:
# taking a snapshot, restoring one, exporting it, pinning and deleting it,
# and backing up the database file. The tables use the same card and
# reactable conventions as the Assignments tab.

# Mark History UI
#
# id: Character. Module namespace ID

mark_history_ui = function(id) {
  ns = shiny::NS(id)

  bslib::layout_columns(
    col_widths = c(5, 7),
    class = "h-100",
    bslib::card(
      class = "h-100",
      bslib::card_header(
        class = "bg-light repo-table-controls",
        shiny::div(
          class = "d-flex justify-content-between align-items-center gap-2 w-100",
          shiny::span("Snapshots"),
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::div(
              style = "width: 120px;",
              shiny::selectInput(
                ns("kind_filter"), NULL,
                choices = c("All" = "all", "Manual" = "manual", "Automatic" = "auto"),
                width = "100%", selectize = FALSE
              )
            ),
            shiny::div(
              style = "width: 140px;",
              shiny::textInput(ns("search"), NULL, placeholder = "Search", width = "100%")
            ),
            bslib::tooltip(
              shiny::actionButton(
                ns("take_snapshot"), shiny::icon("camera"),
                class = "btn-primary btn-sm"
              ),
              "Take a snapshot: record the current template, rubric and marks with a label and a summary of what is about to change (shortcut: s)"
            ),
            bslib::tooltip(
              shiny::actionButton(
                ns("backup"), shiny::icon("hard-drive"),
                class = "btn-outline-secondary btn-sm"
              ),
              "Back up: copy the database file and the project config into the project's backups/ directory. Snapshots live inside the database, so this is the protection against losing the file itself."
            )
          )
        )
      ),
      bslib::card_body(
        class = "p-2",
        reactable::reactableOutput(ns("snapshot_table"), height = "100%")
      )
    ),
    bslib::card(
      class = "h-100",
      bslib::card_header(
        class = "bg-light",
        shiny::uiOutput(ns("detail_header"))
      ),
      bslib::card_body(
        class = "overflow-auto small",
        shiny::uiOutput(ns("detail_ui"))
      )
    ),
    history_hotkey_js(ns)
  )
}

# The "s" hotkey: opens the Take snapshot modal from any tab while no text
# field has focus and no modal is open (the rubric hotkeys leave "s" free)
#
# ns: The module's namespace function

history_hotkey_js = function(ns) {
  shiny::tags$script(shiny::HTML(glue::glue("
    document.addEventListener('keydown', function(e) {
      if (e.key !== 's' || e.metaKey || e.ctrlKey || e.altKey) return;
      if (document.body.classList.contains('modal-open')) return;
      var el = document.activeElement;
      if (el && (el.matches('input, textarea, select, [contenteditable=\"true\"]') || el.isContentEditable)) return;
      var btn = document.getElementById('<<ns(\"take_snapshot\")>>');
      if (btn) { e.preventDefault(); btn.click(); }
    });
  ", .open = "<<", .close = ">>")))
}

# The snapshot table's rows: the listing plus the change of the class mean
# total against the previous snapshot (NA when either side has none) and
# the text the search box matches (label, summary, trigger and notes)
#
# rows: Data frame from as_snapshot_list()
# notes_text: Named character vector of each snapshot's notes joined, keyed
#   by snapshot id

history_table_data = function(rows, notes_text = character(0)) {
  previous_mean = c(rows$mean_total[-1], NA_real_)
  rows$delta_mean = rows$mean_total - previous_mean
  rows$search_text = tolower(paste(
    ifelse(is.na(rows$label), "", rows$label),
    ifelse(is.na(rows$summary), "", rows$summary),
    ifelse(is.na(rows$trigger), "", rows$trigger),
    ifelse(is.na(notes_text[rows$snapshot_id]), "", notes_text[rows$snapshot_id])
  ))
  rows
}

# Choices for a select over snapshots: the ids, labelled with the local time
# and the label, trigger or kind. Empty (not a stray name) when there are no
# rows, which paste0()'s recycling of a zero-length vector would otherwise
# produce.
#
# rows: Data frame from as_snapshot_list()

snapshot_select_choices = function(rows) {
  if (nrow(rows) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  what = ifelse(!is.na(rows$label), rows$label, ifelse(!is.na(rows$trigger), rows$trigger, rows$kind))
  stats::setNames(rows$snapshot_id, paste0(format(rows$created_at, "%b %d %H:%M"), " · ", what))
}

# One-line text: whitespace collapsed and cut to width with an ellipsis
#
# text: Character vector (NA becomes "")
# width: Maximum number of characters

one_line_text = function(text, width = 80) {
  text = ifelse(is.na(text), "", gsub("\\s+", " ", trimws(text)))
  ifelse(nchar(text) > width, paste0(substr(text, 1, width - 3), "\u2026"), text)
}

# The snapshot table
#
# data: Data frame from history_table_data()
# selected: Row index to select initially, or NULL

history_reactable = function(data, selected = NULL) {
  kind_cell = function(value, index) {
    icon = if (identical(value, "manual")) "hand" else "gear"
    title = if (identical(value, "manual")) "Manual snapshot" else "Automatic snapshot"
    htmltools::tagList(
      shiny::icon(icon, class = "fa-fw text-muted", title = title),
      if (isTRUE(data$pinned[index])) shiny::icon("thumbtack", class = "fa-fw text-primary", title = "Pinned")
    )
  }
  reactable::reactable(
    data[, c("created_at", "kind", "label", "summary", "trigger", "username", "mean_total", "delta_mean", "pinned", "snapshot_id"), drop = FALSE],
    selection = "single",
    onClick = "select",
    defaultSelected = selected,
    highlight = TRUE,
    compact = TRUE,
    pagination = FALSE,
    sortable = TRUE,
    defaultSorted = list(created_at = "desc"),
    rowStyle = list(cursor = "pointer"),
    columns = list(
      created_at = reactable::colDef(
        name = "Time", minWidth = 110,
        cell = function(value, index) {
          htmltools::span(
            title = paste0(format(value, "%Y-%m-%d %H:%M:%S"), " (", data$created_tz[index], ")"),
            format(value, "%b %d %H:%M")
          )
        }
      ),
      kind = reactable::colDef(name = "", width = 48, cell = kind_cell, sortable = FALSE),
      label = reactable::colDef(name = "Label", minWidth = 110, cell = function(value) if (is.na(value)) "" else value),
      summary = reactable::colDef(
        name = "Summary", minWidth = 160,
        cell = function(value) {
          if (is.na(value)) return("")
          htmltools::span(title = value, one_line_text(value, 60))
        }
      ),
      trigger = reactable::colDef(
        name = "Trigger", minWidth = 120,
        cell = function(value) if (is.na(value)) "" else htmltools::span(class = "font-monospace", style = "font-size: 11px;", title = value, one_line_text(value, 40))
      ),
      username = reactable::colDef(name = "By", width = 80),
      mean_total = reactable::colDef(
        name = "Mean", width = 70, align = "right",
        cell = function(value, index) {
          if (is.na(value)) return("")
          htmltools::span(title = paste0(data$n_complete[index], " complete totals"), format_score(value))
        }
      ),
      delta_mean = reactable::colDef(
        name = "\u0394", width = 60, align = "right",
        cell = function(value) {
          if (is.na(value) || abs(value) < 1e-8) return("")
          sign = if (value > 0) "+" else ""
          htmltools::span(class = if (value > 0) "text-success" else "text-danger", paste0(sign, format_score(value)))
        }
      ),
      pinned = reactable::colDef(show = FALSE),
      snapshot_id = reactable::colDef(show = FALSE)
    ),
    theme = reactable::reactableTheme(cellPadding = "4px 8px", style = list(fontSize = "12px"))
  )
}

# A snapshot's overview as a definition list
#
# info: markermd_snapshot object

history_overview_ui = function(info) {
  field = function(name, value) {
    if (is.null(value) || (length(value) == 1 && is.na(value))) return(NULL)
    htmltools::tagList(shiny::tags$dt(class = "col-sm-3 fw-normal text-muted", name), shiny::tags$dd(class = "col-sm-9 mb-1", value))
  }
  totals = info@stats[info@stats$question_name == "(total)", , drop = FALSE]
  n_complete = sum(!is.na(totals$score))
  scores_text = if (nrow(totals) == 0) {
    "No repositories scored"
  } else if (n_complete == 0) {
    paste0("0 of ", nrow(totals), " repositories with a complete total")
  } else {
    paste0(
      n_complete, " of ", nrow(totals), " repositories complete; mean ",
      format_score(mean(totals$score, na.rm = TRUE)), ", range ",
      format_score(min(totals$score, na.rm = TRUE)), " to ", format_score(max(totals$score, na.rm = TRUE))
    )
  }
  shiny::tags$dl(
    class = "row mb-2",
    field("Created", paste0(format(info@created_at, "%Y-%m-%d %H:%M:%S"), " (", info@created_tz, ", ", format_age(info@created_at), ")")),
    field("By", info@username),
    field("Kind", if (info@pinned) paste(info@kind, "(pinned)") else info@kind),
    field("Trigger", if (is.na(info@trigger)) NULL else shiny::tags$code(info@trigger)),
    field("Summary", info@summary),
    field("Parent", if (is.na(info@parent_snapshot_id)) NULL else shiny::tags$code(info@parent_snapshot_id)),
    field("Scores", scores_text),
    field("Parts", paste0(info@parts$part, " (", info@parts$format_version, ", ", info@parts$size, " B)", collapse = "; ")),
    field("Versions", paste0("schema ", info@schema_version, ", markermd ", info@markermd_version))
  )
}

# A snapshot's notes as a list
#
# notes: Data frame from read_snapshot_notes()

history_notes_ui = function(notes) {
  if (nrow(notes) == 0) {
    return(shiny::div(class = "text-muted", "No notes yet."))
  }
  shiny::tags$ul(
    class = "list-unstyled mb-2",
    lapply(seq_len(nrow(notes)), function(i) {
      shiny::tags$li(
        class = "mb-1",
        shiny::span(class = "text-muted me-2", format(notes$timestamp[i], "%Y-%m-%d %H:%M"), notes$username[i]),
        if (!is.na(notes$key[i])) shiny::tags$code(class = "me-1", paste0(notes$key[i], " =")),
        notes$value[i]
      )
    })
  )
}

# A small table from a data frame, for the diff and totals views
#
# df: Data frame of character/numeric columns
# names: Column headers

history_small_table = function(df, names = colnames(df)) {
  shiny::tags$table(
    class = "table table-sm table-borderless mb-2",
    style = "font-size: 12px;",
    shiny::tags$thead(shiny::tags$tr(lapply(names, function(n) shiny::tags$th(class = "text-muted fw-normal", n)))),
    shiny::tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      shiny::tags$tr(lapply(seq_along(df), function(j) shiny::tags$td(as.character(df[[j]][i]))))
    }))
  )
}

# A markermd_snapshot_diff as HTML: the template, rubric and marks changes
# grouped the way the print method groups them, and the totals that changed
#
# diff: markermd_snapshot_diff object

snapshot_diff_ui = function(diff) {
  heading = function(text) shiny::h6(class = "mt-3 mb-1", text)
  none = function(text) shiny::div(class = "text-muted", text)

  template_ui = if (nrow(diff@template) > 0) {
    htmltools::tagList(heading("Template"), shiny::tags$ul(lapply(seq_len(nrow(diff@template)), function(i) {
      detail = if (nzchar(diff@template$detail[i])) paste0(" (", diff@template$detail[i], ")") else ""
      shiny::tags$li(shiny::strong(diff@template$question[i]), ": ", diff@template$change[i], detail)
    })))
  }

  rubric_ui = if (nrow(diff@rubric) == 0) {
    none("No rubric changes.")
  } else {
    lapply(unique(diff@rubric$question), function(question) {
      rows = diff@rubric[diff@rubric$question == question, , drop = FALSE]
      shiny::div(
        class = "mb-2",
        shiny::strong(question),
        shiny::tags$ul(class = "mb-1", lapply(seq_len(nrow(rows)), function(i) {
          shiny::tags$li(
            rows$change[i],
            if (!is.na(rows$item[i])) htmltools::tagList(" ", shiny::tags$code(rows$item[i])),
            if (nzchar(rows$detail[i])) paste0(": ", rows$detail[i])
          )
        }))
      )
    })
  }

  marks = diff@marks
  selection = marks[marks$change == "selection", , drop = FALSE]
  comments = marks[marks$change == "comment", , drop = FALSE]
  private = marks[marks$change == "private_comment", , drop = FALSE]
  marks_ui = if (nrow(marks) == 0) {
    none("No marks changes.")
  } else {
    htmltools::tagList(
      if (nrow(selection) > 0) htmltools::tagList(
        shiny::div(paste0(nrow(selection), " pair", if (nrow(selection) == 1) "" else "s", " with changed selections")),
        history_small_table(
          data.frame(selection$repo, selection$question, selection$before, selection$after, stringsAsFactors = FALSE),
          c("Repository", "Question", "Removed", "Added")
        )
      ),
      if (nrow(comments) > 0) htmltools::tagList(
        shiny::div(paste0(nrow(comments), " public comment", if (nrow(comments) == 1) "" else "s", " changed")),
        history_small_table(
          data.frame(comments$repo, comments$question, comments$before, comments$after, stringsAsFactors = FALSE),
          c("Repository", "Question", "Before", "After")
        )
      ),
      if (nrow(private) > 0) shiny::div(class = "text-muted", paste0(
        nrow(private), " private note", if (nrow(private) == 1) "" else "s", " changed (",
        paste(unique(paste0(private$repo, " / ", private$question)), collapse = ", "), ")"
      ))
    )
  }

  totals = diff@totals
  changed = totals[(is.na(totals$before) != is.na(totals$after)) | (!is.na(totals$change) & abs(totals$change) > 1e-8), , drop = FALSE]
  scores_ui = if (nrow(diff@scores) == 0) {
    none("No score changes.")
  } else {
    htmltools::tagList(
      shiny::div(paste0(nrow(diff@scores), " pair", if (nrow(diff@scores) == 1) "" else "s", " changed score")),
      if (nrow(changed) > 0) history_small_table(
        data.frame(
          changed$repo, format_score(changed$before), format_score(changed$after),
          ifelse(is.na(changed$change), "", ifelse(changed$change > 0, paste0("+", format_score(changed$change)), format_score(changed$change))),
          stringsAsFactors = FALSE
        ),
        c("Repository", "Total before", "Total after", "Change")
      )
    )
  }

  htmltools::tagList(
    template_ui,
    heading("Rubric"), rubric_ui,
    heading("Marks"), marks_ui,
    heading("Scores"), scores_ui
  )
}

# Parse the Take snapshot modal's metadata textarea ("key: value" per line)
# into a named list; blank lines are skipped, a line without a colon is a
# key with an empty value
#
# text: The textarea's contents

parse_metadata_lines = function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list())
  }
  lines = trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
  lines = lines[nzchar(lines)]
  out = list()
  for (line in lines) {
    parts = regmatches(line, regexec("^([^:]+):?\\s*(.*)$", line))[[1]]
    out[[trimws(parts[2])]] = trimws(parts[3])
  }
  out
}

# Mark History Server
#
# id: Character. Module namespace ID
# root: Project root
# question_names: Character vector of the template's question names
# refresh: Reactive whose changes (the app's navbar selection, grading
#   writes) re-read the snapshot listing
# reload_rubric: Function rebuilding the rubric module's state from the
#   database after a restore
# reserved_ids: Function returning the item and group ids the rubric module
#   has bound this session, which a restore must not reuse
# Returns: List with history_version (reactive, bumped on every snapshot
#   change made here), status (reactive snapshot_state_status() of the
#   live database) and open_take_snapshot (function opening the modal)

mark_history_server = function(id, root, question_names, refresh, reload_rubric, reserved_ids) {
  shiny::moduleServer(id, function(input, output, session) {
    proj = project_config(root)

    history_version = shiny::reactiveVal(0L)
    bump = function() history_version(shiny::isolate(history_version()) + 1L)

    # The listing, re-read on our own changes and on the app's refresh signal
    snapshots = shiny::reactive({
      history_version()
      refresh()
      with_database(root, function(conn) {
        rows = as_snapshot_list(snapshot_rows(conn))
        notes = DBI::dbGetQuery(conn, "SELECT snapshot_id, value FROM snapshot_notes")
        notes_text = vapply(split(notes$value, notes$snapshot_id), paste, character(1), collapse = " ")
        history_table_data(rows, notes_text)
      })
    })

    # Whether the live state differs from the latest snapshot, and how many
    # selection events were recorded since; the navbar indicator reads it
    status = shiny::reactive({
      history_version()
      refresh()
      with_database(root, snapshot_state_status)
    })

    search_text = shiny::debounce(shiny::reactive(input$search), 300)

    visible_rows = shiny::reactive({
      rows = snapshots()
      if (!identical(input$kind_filter, "all") && !is.null(input$kind_filter)) {
        rows = rows[rows$kind == input$kind_filter, , drop = FALSE]
      }
      needle = search_text()
      if (!is.null(needle) && nzchar(trimws(needle))) {
        rows = rows[grepl(tolower(trimws(needle)), rows$search_text, fixed = TRUE), , drop = FALSE]
      }
      rownames(rows) = NULL
      rows
    })

    selected_id = shiny::reactiveVal(NULL)

    output$snapshot_table = reactable::renderReactable({
      rows = visible_rows()
      current = shiny::isolate(selected_id())
      selected = if (!is.null(current) && current %in% rows$snapshot_id) match(current, rows$snapshot_id) else NULL
      history_reactable(rows, selected = selected)
    })

    shiny::observe({
      sel = reactable::getReactableState("snapshot_table", "selected")
      rows = shiny::isolate(visible_rows())
      selected_id(if (is.null(sel) || length(sel) == 0 || sel > nrow(rows)) NULL else rows$snapshot_id[sel])
    })

    # The selected snapshot's full record, re-read after every change here
    selected_info = shiny::reactive({
      history_version()
      id = selected_id()
      shiny::req(id)
      with_database(root, function(conn) {
        if (nrow(DBI::dbGetQuery(conn, "SELECT 1 FROM snapshots WHERE snapshot_id = ?", params = list(id))) == 0) {
          return(NULL)
        }
        snapshot_info_from_conn(conn, id)
      })
    })

    output$detail_header = shiny::renderUI({
      id = selected_id()
      if (is.null(id)) {
        return(shiny::span("Snapshot"))
      }
      info = selected_info()
      shiny::req(info)
      shiny::div(
        class = "d-flex justify-content-between align-items-center gap-2 w-100",
        shiny::span(
          shiny::tags$code(info@snapshot_id),
          if (!is.na(info@label)) shiny::span(class = "ms-2", info@label)
        ),
        shiny::div(
          class = "d-flex align-items-center gap-1",
          bslib::tooltip(
            shiny::actionButton(
              session$ns("toggle_pin"), shiny::icon("thumbtack"),
              class = paste("btn-sm", if (info@pinned) "btn-primary" else "btn-outline-secondary")
            ),
            if (info@pinned) {
              "Unpin: let snapshot_prune() remove this snapshot again and allow deleting it from here"
            } else {
              "Pin: keep this snapshot out of pruning and protect it from deletion"
            }
          ),
          bslib::tooltip(
            shiny::actionButton(session$ns("restore"), "Restore\u2026", icon = shiny::icon("rotate-left"), class = "btn-outline-danger btn-sm"),
            "Restore: write this snapshot's rubric and marks (or chosen questions and repositories) back into the database, undoing every change since. The current state is snapshotted first, so the restore can be undone."
          ),
          bslib::tooltip(
            shiny::actionButton(session$ns("export"), "Export\u2026", icon = shiny::icon("file-export"), class = "btn-outline-secondary btn-sm"),
            "Export: write this snapshot's template, rubric and marks as YAML files, with a snapshot.yaml sidecar, into a directory of your choice"
          ),
          if (!info@pinned) bslib::tooltip(
            shiny::actionButton(session$ns("delete"), shiny::icon("trash"), class = "btn-outline-secondary btn-sm"),
            "Delete: permanently remove this snapshot, its notes and its recorded scores (pin it to protect it)"
          )
        )
      )
    })

    output$detail_ui = shiny::renderUI({
      id = selected_id()
      if (is.null(id)) {
        return(shiny::div(
          class = "text-muted p-3",
          "Select a snapshot to see its details, what changed since, and the scores it recorded. ",
          "Snapshots are taken automatically before imports, restores and deletions; ",
          "press ", shiny::tags$kbd("s"), " or use the camera button to take one by hand."
        ))
      }
      info = selected_info()
      shiny::req(info)
      others = shiny::isolate(snapshots())
      others = others[others$snapshot_id != id, , drop = FALSE]
      compare_choices = c("Current state" = "current", snapshot_select_choices(others))
      bslib::navset_underline(
        id = session$ns("detail_tabs"),
        bslib::nav_panel(
          "Overview",
          shiny::div(class = "pt-2", history_overview_ui(info)),
          shiny::h6("Notes", class = "mt-2 mb-1"),
          history_notes_ui(info@notes),
          shiny::div(
            class = "d-flex gap-2 align-items-start",
            shiny::div(style = "flex: 1;", shiny::textInput(session$ns("new_note"), NULL, placeholder = "Add a note (or key: value)", width = "100%")),
            bslib::tooltip(
              shiny::actionButton(session$ns("add_note"), "Add", class = "btn-outline-secondary btn-sm"),
              "Append the text to this snapshot's note log with your name and the time; 'key: value' is recorded as metadata"
            )
          )
        ),
        bslib::nav_panel(
          "Changes",
          shiny::div(
            class = "d-flex align-items-center gap-2 pt-2",
            shiny::span(class = "text-muted", "Compare with"),
            shiny::div(style = "width: 280px;", shiny::selectInput(session$ns("compare_with"), NULL, choices = compare_choices, width = "100%", selectize = FALSE))
          ),
          shiny::uiOutput(session$ns("diff_ui"))
        ),
        bslib::nav_panel(
          "Scores",
          shiny::div(class = "pt-2", reactable::reactableOutput(session$ns("scores_table")))
        )
      )
    })

    output$diff_ui = shiny::renderUI({
      id = selected_id()
      shiny::req(id)
      history_version()
      refresh()
      to = if (is.null(input$compare_with)) "current" else input$compare_with
      diff = purrr::safely(snapshot_diff)(id, to, project = root)
      if (!is.null(diff$error)) {
        return(shiny::div(class = "text-danger", conditionMessage(diff$error)))
      }
      snapshot_diff_ui(diff$result)
    })

    output$scores_table = reactable::renderReactable({
      info = selected_info()
      shiny::req(info)
      wide = snapshot_scores_wide(info@stats, question_names)
      columns = stats::setNames(
        lapply(c(question_names, "Total"), function(column) {
          reactable::colDef(cell = function(value) format_score(value), align = "right", style = if (column == "Total") list(fontWeight = "bold"))
        }),
        c(question_names, "Total")
      )
      columns$Graded = reactable::colDef(show = FALSE)
      reactable::reactable(
        wide, compact = TRUE, pagination = FALSE, highlight = TRUE,
        columns = columns,
        theme = reactable::reactableTheme(cellPadding = "4px 8px", style = list(fontSize = "12px"))
      )
    })

    # A notification for a purrr::safely() result: the error, or a success
    # message; the warnings of a quietly-captured call are shown too
    notify_result = function(res, success) {
      if (!is.null(res$error)) {
        shiny::showNotification(conditionMessage(res$error), type = "error", duration = NULL)
        return(FALSE)
      }
      for (warning in res$result$warnings) {
        shiny::showNotification(warning, type = "warning", duration = 10)
      }
      shiny::showNotification(success, type = "message")
      TRUE
    }

    # --- Take snapshot ----------------------------------------------------------

    open_take_snapshot = function() {
      shiny::showModal(shiny::modalDialog(
        title = "Take snapshot",
        shiny::p(class = "small text-muted", "Records the current template, rubric and marks. The label and summary are yours to search for later."),
        shiny::textInput(session$ns("snapshot_label"), "Label", placeholder = "e.g. Before feedback review", width = "100%"),
        shiny::textAreaInput(session$ns("snapshot_summary"), "Summary", placeholder = "What is about to change, or why this state is worth keeping", width = "100%", rows = 3),
        shiny::textAreaInput(session$ns("snapshot_metadata"), "Metadata (one key: value per line)", placeholder = "review_doc: notes/review.md", width = "100%", rows = 2),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_snapshot"), "Take snapshot", class = "btn-primary")
        )
      ))
    }

    shiny::observe({
      open_take_snapshot()
    }) |> shiny::bindEvent(input$take_snapshot)

    shiny::observe({
      shiny::removeModal()
      blank_to_null = function(x) if (is.null(x) || !nzchar(trimws(x))) NULL else x
      res = purrr::safely(purrr::quietly(snapshot_create))(
        root,
        label = blank_to_null(input$snapshot_label),
        summary = blank_to_null(input$snapshot_summary),
        metadata = parse_metadata_lines(input$snapshot_metadata)
      )
      if (notify_result(res, "Snapshot taken.")) {
        bump()
        selected_id(res$result$result)
      }
    }) |> shiny::bindEvent(input$confirm_snapshot)

    # --- Notes and pinning --------------------------------------------------------

    shiny::observe({
      id = selected_id()
      shiny::req(id)
      text = trimws(input$new_note)
      shiny::req(nzchar(text))
      parsed = parse_metadata_lines(text)
      is_keyed = grepl("^[A-Za-z0-9_.-]+:\\s*\\S", text) && length(parsed) == 1
      res = purrr::safely(purrr::quietly(snapshot_annotate))(
        id, project = root,
        note = if (is_keyed) NULL else text,
        metadata = if (is_keyed) parsed else list()
      )
      if (notify_result(res, "Note added.")) {
        shiny::updateTextInput(session, "new_note", value = "")
        bump()
      }
    }) |> shiny::bindEvent(input$add_note)

    shiny::observe({
      info = selected_info()
      shiny::req(info)
      res = purrr::safely(purrr::quietly(snapshot_pin))(info@snapshot_id, pinned = !info@pinned, project = root)
      if (notify_result(res, if (info@pinned) "Snapshot unpinned." else "Snapshot pinned.")) {
        bump()
      }
    }) |> shiny::bindEvent(input$toggle_pin)

    # --- Restore ----------------------------------------------------------------

    shiny::observe({
      info = selected_info()
      shiny::req(info)
      parts = with_database(root, function(conn) read_snapshot_parts(conn, info@snapshot_id))
      snap_questions = vapply(parts$rubric$questions, function(q) as.character(q$name), character(1))
      snap_repos = vapply(parts$marks$repos, function(r) as.character(r$name), character(1))
      totals = info@stats[info@stats$question_name == "(total)" & !is.na(info@stats$score), , drop = FALSE]
      preview = if (nrow(totals) == 0) {
        "The snapshot recorded no complete totals."
      } else {
        paste0(
          "After restoring, ", nrow(totals), " repositor", if (nrow(totals) == 1) "y has" else "ies have",
          " a complete total (mean ", format_score(mean(totals$score)), ", range ",
          format_score(min(totals$score)), " to ", format_score(max(totals$score)), ")."
        )
      }
      shiny::showModal(shiny::modalDialog(
        title = "Restore snapshot",
        shiny::p(shiny::tags$code(info@snapshot_id), if (!is.na(info@label)) paste0(" (", info@label, ")")),
        shiny::checkboxGroupInput(
          session$ns("restore_parts"), "Restore",
          choices = c("Rubric (items, groups, scoring)" = "rubric", "Marks (selections and comments)" = "marks"),
          selected = c("rubric", "marks")
        ),
        shiny::p(class = "small text-muted mb-2", "The template is restored from R with ", shiny::tags$code("snapshot_restore(part = \"template\")"), " or from the template editor, since the open app was built for the current template."),
        shiny::selectizeInput(session$ns("restore_questions"), "Only these questions (optional)", choices = snap_questions, multiple = TRUE, width = "100%"),
        shiny::selectizeInput(session$ns("restore_repos"), "Only these repositories (optional)", choices = snap_repos, multiple = TRUE, width = "100%"),
        shiny::textAreaInput(session$ns("restore_summary"), "Why (required, recorded on the snapshot taken before restoring)", width = "100%", rows = 2),
        shiny::p(class = "small text-muted", preview),
        shiny::p(class = "text-danger small mb-0", "Every change since the snapshot to the chosen parts is undone. The current state is snapshotted first, so the restore can itself be undone."),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_restore"), "Restore", class = "btn-danger", disabled = TRUE)
        )
      ))
    }) |> shiny::bindEvent(input$restore)

    shiny::observe({
      ready = !is.null(input$restore_summary) && nzchar(trimws(input$restore_summary)) && length(input$restore_parts) > 0
      shinyjs::toggleState(session$ns("confirm_restore"), condition = ready, asis = TRUE)
    })

    shiny::observe({
      shiny::removeModal()
      id = selected_id()
      shiny::req(id)
      res = purrr::safely(purrr::quietly(restore_snapshot))(
        proj, id,
        part = input$restore_parts,
        question = if (length(input$restore_questions) > 0) input$restore_questions else NULL,
        repo = if (length(input$restore_repos) > 0) input$restore_repos else NULL,
        summary = trimws(input$restore_summary),
        reserved_ids = reserved_ids()
      )
      if (notify_result(res, paste0("Restored snapshot ", id, "."))) {
        reload_rubric()
        bump()
      }
    }) |> shiny::bindEvent(input$confirm_restore)

    # --- Export, delete, backup --------------------------------------------------

    shiny::observe({
      id = selected_id()
      shiny::req(id)
      shiny::showModal(shiny::modalDialog(
        title = "Export snapshot",
        shiny::p(class = "small text-muted", "Writes the snapshot's template, rubric and marks as YAML files plus a snapshot.yaml sidecar into a directory under the project root (or an absolute path)."),
        shiny::textInput(session$ns("export_dir"), "Directory", value = file.path("snapshots", id), width = "100%"),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_export"), "Export", class = "btn-primary")
        )
      ))
    }) |> shiny::bindEvent(input$export)

    shiny::observe({
      shiny::removeModal()
      id = selected_id()
      shiny::req(id, nzchar(trimws(input$export_dir)))
      dir = trimws(input$export_dir)
      if (!fs::is_absolute_path(dir)) {
        dir = fs::path(root, dir)
      }
      res = purrr::safely(purrr::quietly(snapshot_export))(id, dir, project = root)
      notify_result(res, paste0("Exported to ", dir, "."))
    }) |> shiny::bindEvent(input$confirm_export)

    shiny::observe({
      id = selected_id()
      shiny::req(id)
      shiny::showModal(shiny::modalDialog(
        title = "Delete snapshot?",
        shiny::p("Permanently deletes ", shiny::tags$code(id), ", its notes and its recorded scores. This cannot be undone."),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_delete"), "Delete", class = "btn-danger")
        )
      ))
    }) |> shiny::bindEvent(input$delete)

    shiny::observe({
      shiny::removeModal()
      id = selected_id()
      shiny::req(id)
      res = purrr::safely(purrr::quietly(snapshot_delete))(id, project = root, force = TRUE)
      if (notify_result(res, "Snapshot deleted.")) {
        selected_id(NULL)
        bump()
      }
    }) |> shiny::bindEvent(input$confirm_delete)

    shiny::observe({
      res = purrr::safely(purrr::quietly(project_backup))(root)
      if (notify_result(res, paste0("Database backed up to ", fs::path_rel(res$result$result, root), "."))) {
        bump()
      }
    }) |> shiny::bindEvent(input$backup)

    shiny::exportTestValues(
      snapshot_ids = snapshots()$snapshot_id,
      selected_snapshot = selected_id(),
      state_changed = status()$changed
    )

    list(
      history_version = shiny::reactive(history_version()),
      status = status,
      open_take_snapshot = open_take_snapshot
    )
  })
}
