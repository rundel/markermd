# Shiny module for the marking app's Activity tab: the project's activity log
# (see actions.R), newest first. The coarse view lists every recorded call of
# a markermd function (by an agent, a script or a person at an R prompt) and
# the app's own grading grouped into sessions; selecting an entry shows the
# fine view, what it changed per repository and question, plus the rubric
# and template changes of a call that replaced them. The layout and table
# conventions follow the History tab.

# Mark Activity UI
#
# id: Character. Module namespace ID

mark_activity_ui = function(id) {
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
          shiny::span("Activity"),
          shiny::div(
            style = "width: 170px;",
            shiny::selectInput(
              ns("actor_filter"), NULL,
              choices = c("Everything" = "all", "Agents and scripts" = "calls", "Grading in the app" = "app"),
              width = "100%", selectize = FALSE
            )
          )
        )
      ),
      bslib::card_body(
        class = "p-2",
        reactable::reactableOutput(ns("activity_table"), height = "100%")
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
    )
  )
}

# What an activity row did, in a few words: the function for a recorded
# call, "Grading in the app" for an app session
#
# rows: Data frame from activity_rows()

activity_what = function(rows) {
  ifelse(rows$kind == "app", "Grading in the app", rows$fn)
}

# What an activity row touched, in a few words: its pairs and the parts it
# replaced
#
# rows: Data frame from activity_rows()

activity_scope_text = function(rows) {
  vapply(seq_len(nrow(rows)), function(i) {
    n = rows$n_pairs[i]
    pieces = c(
      if (n > 0) paste0(n, " pair", if (n == 1) "" else "s"),
      if (length(rows$parts[[i]]) > 0) paste(rows$parts[[i]], collapse = " + ")
    )
    if (length(pieces) == 0) "" else paste(pieces, collapse = ", ")
  }, character(1))
}

# The activity table
#
# rows: Data frame from activity_rows()
# selected: Row index to select initially, or NULL

activity_reactable = function(rows, selected = NULL) {
  data = data.frame(
    created_at = rows$created_at,
    actor = rows$actor,
    what = activity_what(rows),
    summary = rows$summary,
    scope = activity_scope_text(rows),
    id = rows$id,
    stringsAsFactors = FALSE
  )
  reactable::reactable(
    data,
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
        name = "Time", minWidth = 100,
        cell = function(value, index) {
          started = rows$started_at[index]
          title = if (rows$kind[index] == "app" && !isTRUE(all.equal(started, value))) {
            paste0(format(started, "%Y-%m-%d %H:%M"), " to ", format(value, "%H:%M"))
          } else {
            format(value, "%Y-%m-%d %H:%M:%S")
          }
          htmltools::span(title = title, format(value, "%b %d %H:%M"))
        }
      ),
      actor = reactable::colDef(
        name = "Who", width = 80,
        cell = function(value, index) {
          icon = if (identical(value, "app")) "user" else if (value %in% c("claude", "codex")) "robot" else "terminal"
          htmltools::span(
            title = rows$username[index],
            shiny::icon(icon, class = "fa-fw text-muted me-1"), value
          )
        }
      ),
      what = reactable::colDef(
        name = "What", minWidth = 110,
        cell = function(value, index) {
          if (rows$kind[index] == "app") value else htmltools::span(class = "font-monospace", style = "font-size: 11px;", value)
        }
      ),
      summary = reactable::colDef(
        name = "Summary", minWidth = 150,
        cell = function(value) {
          if (is.na(value)) return("")
          htmltools::span(title = value, one_line_text(value, 60))
        }
      ),
      scope = reactable::colDef(name = "Scope", minWidth = 90),
      id = reactable::colDef(show = FALSE)
    ),
    theme = reactable::reactableTheme(cellPadding = "4px 8px", style = list(fontSize = "12px"))
  )
}

# An activity entry's fields as a definition list
#
# row: One-row data frame from activity_rows()

activity_overview_ui = function(row) {
  field = function(name, value) {
    if (is.null(value) || length(value) == 0 || (length(value) == 1 && (is.na(value) || !nzchar(value)))) {
      return(NULL)
    }
    htmltools::tagList(shiny::tags$dt(class = "col-sm-3 fw-normal text-muted", name), shiny::tags$dd(class = "col-sm-9 mb-1", value))
  }
  when = if (row$kind == "app") {
    paste0(format(row$started_at, "%Y-%m-%d %H:%M"), " to ", format(row$created_at, "%H:%M"), " (", format_age(row$created_at), ")")
  } else {
    paste0(format(row$created_at, "%Y-%m-%d %H:%M:%S"), " (", format_age(row$created_at), ")")
  }
  shiny::tags$dl(
    class = "row mb-2",
    field("When", when),
    field("Who", paste0(row$actor, " (", row$username, ")")),
    field("Call", if (!is.na(row$call)) shiny::tags$code(row$call)),
    field("Summary", row$summary),
    field("Repositories", paste(row$repos[[1]], collapse = ", ")),
    field("Questions", paste(row$questions[[1]], collapse = ", ")),
    field("Snapshot", if (!is.na(row$snapshot_id)) htmltools::tagList(
      shiny::tags$code(row$snapshot_id),
      shiny::span(class = "text-muted ms-1", "(the state before this change, in the History tab)")
    ))
  )
}

# Mark Activity Server
#
# id: Character. Module namespace ID
# root: Project root
# refresh: Reactive whose changes (grading writes, external changes, tab
#   visits) re-read the log

mark_activity_server = function(id, root, refresh) {
  shiny::moduleServer(id, function(input, output, session) {

    activity = shiny::reactive({
      refresh()
      with_database(root, function(conn) activity_rows(conn))
    })

    visible_rows = shiny::reactive({
      rows = activity()
      rows = switch(
        input$actor_filter,
        calls = rows[rows$kind == "action", , drop = FALSE],
        app = rows[rows$kind == "app", , drop = FALSE],
        rows
      )
      rownames(rows) = NULL
      rows
    })

    selected_id = shiny::reactiveVal(NULL)

    output$activity_table = reactable::renderReactable({
      rows = visible_rows()
      current = shiny::isolate(selected_id())
      selected = if (!is.null(current) && current %in% rows$id) match(current, rows$id) else NULL
      activity_reactable(rows, selected = selected)
    })

    shiny::observe({
      sel = reactable::getReactableState("activity_table", "selected")
      rows = shiny::isolate(visible_rows())
      selected_id(if (is.null(sel) || length(sel) == 0 || sel > nrow(rows)) NULL else rows$id[sel])
    })

    selected_row = shiny::reactive({
      id = selected_id()
      shiny::req(id)
      rows = activity()
      shiny::req(id %in% rows$id)
      rows[rows$id == id, , drop = FALSE]
    })

    output$detail_header = shiny::renderUI({
      id = selected_id()
      if (is.null(id)) {
        return(shiny::span("Details"))
      }
      row = selected_row()
      shiny::span(
        if (row$kind == "app") "Grading in the app" else shiny::tags$code(row$fn),
        shiny::span(class = "text-muted ms-2 small", row$actor, " · ", format_age(row$created_at))
      )
    })

    output$detail_ui = shiny::renderUI({
      if (is.null(selected_id())) {
        return(shiny::div(class = "text-muted", "Select an entry to see what it changed."))
      }
      row = selected_row()
      diff = with_database(root, function(conn) activity_diff_conn(conn, row$id))
      htmltools::tagList(
        activity_overview_ui(row),
        snapshot_diff_ui(diff, scores = FALSE, private_text = TRUE)
      )
    })

    list(selected_id = shiny::reactive(selected_id()))
  })
}
