# Rubric module: the Grade tab and Rubric tab views
#
# Both tabs draw the rubric from the same handles (mod_mark_rubric_state.R),
# each with its own id prefix ("grade-" and "editor-", see rx$view_id()), so
# both can be on the page at once and every event names its item or group
# id whichever tab sent it. The Grade tab's view is the current question's
# score widget and item list, drawn once and patched in place (toggle
# buttons, agent badges, group subtotals, the score); the Rubric tab's view is
# every question with its scoring settings and selection counts, drawn while
# that tab is shown. Each view redraws on structural changes (rx$redraw())
# and when its tab is entered, which picks up edits made in the other tab.
# rubric_views_wiring() is a plain wiring function like rubric_state_wiring().
#
# rx: The rubric module's shared context, after rubric_state_wiring()

rubric_views_wiring = function(rx) {
  session = rx$session
  input = rx$input
  output = rx$output
  root = rx$root
  repos = rx$repos
  question_names = rx$question_names

  # Bumped when a tab is entered, so its view redraws with what the other
  # tab changed meanwhile
  grade_entered = shiny::reactiveVal(0L)
  shiny::observe({
    if (identical(rx$active_tab(), "grade")) {
      grade_entered(shiny::isolate(grade_entered()) + 1L)
    }
  }) |>
    shiny::bindEvent(rx$active_tab(), ignoreInit = TRUE)

  # The question's items and groups as blocks (see rubric_blocks()):
  # ungrouped items and groups wrapping their members, then the groups with
  # no members
  question_blocks = function(question_name) {
    handles = rx$items[[question_name]]
    groups = rx$groups_of(question_name)
    list(
      handles = handles,
      groups = groups,
      blocks = rubric_blocks(names(handles), item_groups_of(handles), names(groups))
    )
  }

  group_ui = function(view, group_id, group, item_uis, subtotal = NULL) {
    mark_rubric_group_ui(
      rx$view_id(view, group_id), group, item_uis, subtotal = subtotal,
      group_id = group_id, events_id = session$ns("group_event")
    )
  }

  # --- The Grade tab ---------------------------------------------------------

  output$grade_ui = shiny::renderUI({
    shiny::req(input$question_select)
    grade_server = rx$grade_servers[[input$question_select]]
    if (!is.null(grade_server)) {
      result = rx$question_score()
      mark_grade_ui(
        session$ns(grade_server$id),
        grade_server$grade(),
        capped = result$score - result$without_groups,
        breakdown = shiny::uiOutput(session$ns("score_breakdown"))
      )
    }
  }) |>
    # Only re-render when the question changes, an import updates the
    # scoring setup (grade_redraw) or the tab is entered. Within a question
    # the score is updated in place (update_grade() -> update_score_display()),
    # so rubric item add / move / delete must not rebuild this widget.
    shiny::bindEvent(input$question_select, rx$grade_redraw(), grade_entered())
  shiny::outputOptions(output, "grade_ui", suspendWhenHidden = FALSE)

  # The cap badge's breakdown; it lives in a popover, so it is only rendered
  # while the popover is open
  output$score_breakdown = shiny::renderUI({
    shiny::req(input$question_select)
    score_breakdown_ui(rx$question_score(), rx$grade_servers[[input$question_select]]$grade())
  })

  # The current question's item list. Item rows keep their own markup, so
  # the hotkeys (matched by button label) are untouched by grouping. The
  # wrapper routes the rows' events (see rubric_item_container_attrs()).
  output$rubric_items_ui = shiny::renderUI({
    shiny::req(input$question_select)
    question_name = input$question_select
    q = question_blocks(question_name)
    result = rx$question_score()

    item_ui = function(item_id) {
      item = q$handles[[item_id]]$item()
      mark_rubric_item_ui(
        rx$view_id("grade", item_id), item,
        exclusive = !is.na(item@group) && isTRUE(q$groups[[item@group]]@exclusive)
      )
    }
    block_ui = function(block) {
      if (block$type == "item") {
        return(item_ui(block$id))
      }
      group_ui(
        "grade", block$group, q$groups[[block$group]], lapply(block$ids, item_ui),
        subtotal = result$groups[result$groups$group_id == block$group, , drop = FALSE]
      )
    }
    is_empty_group = vapply(q$blocks, function(block) {
      block$type == "group" && length(block$ids) == 0
    }, logical(1))

    do.call(shiny::div, c(
      list(
        id = session$ns("rubric_item_rows"),
        shiny::div(id = session$ns("rubric_blocks"), lapply(q$blocks[!is_empty_group], block_ui)),
        shiny::div(id = session$ns("rubric_empty_groups"), lapply(q$blocks[is_empty_group], block_ui))
      ),
      rubric_item_container_attrs(session$ns, "grade", q$groups)
    ))
  }) |>
    shiny::bindEvent(rx$redraw_ui(), input$question_select, grade_entered())
  shiny::outputOptions(output, "rubric_items_ui", suspendWhenHidden = FALSE)

  # Items of the pair on screen that a markermd call selected and nobody has
  # touched here since (see pair_agent_badges()); re-read on every grading
  # write here and on every change noticed outside
  agent_badges = shiny::reactive({
    shiny::req(input$question_select, rx$selected_repo())
    rx$grading_data_version()
    rx$external_version()
    with_database(root, function(conn) pair_agent_badges(conn, input$question_select, rx$selected_repo()))
  })

  # The item rows are patched in place after each flush (so a redraw in the
  # same flush cannot clobber a patch): the toggle buttons whenever an
  # item's selection or hotkey, or a group's exclusivity, changes, and the
  # agent badges whenever they change
  shiny::observe({
    shiny::req(input$question_select)
    rx$redraw_ui()
    grade_entered()
    question_name = input$question_select
    payload = lapply(unname(rx$items[[question_name]]), function(handle) {
      item = handle$item()
      c(
        list(id = rx$view_id("grade", handle$id, "hotkey_btn")),
        rubric_item_button_state(item, rx$group_is_exclusive(question_name, item@group))
      )
    })
    session$onFlushed(function() {
      session$sendCustomMessage("markermd_item_buttons", payload)
    }, once = TRUE)
  })

  shiny::observe({
    rx$redraw_ui()
    grade_entered()
    badges = agent_badges()
    titles = structure(list(), names = character(0))
    for (item_id in names(badges)) {
      value = badges[[item_id]]
      who = if (is.na(value$fn)) value$actor else paste0(value$actor, " (", value$fn, ")")
      titles[[rx$view_id("grade", item_id, "agent_badge")]] = paste0(
        "Selected by ", who, " and not yet reviewed; any change to this question clears it"
      )
    }
    session$onFlushed(function() {
      session$sendCustomMessage("markermd_agent_badges", list(
        container = session$ns("rubric_items_container"),
        icon = as.character(fa_icon("robot")),
        badges = titles
      ))
    }, once = TRUE)
  })

  # Patch the group headers' subtotals (and capped styling) in place after
  # the flush, the same way the score is patched, so no item row is
  # re-rendered and a question switch's stale render cannot clobber it
  send_group_subtotals = function(result) {
    rows = result$groups
    payload = lapply(seq_len(nrow(rows)), function(i) {
      list(
        id = rx$view_id("grade", rows$group_id[i], "container"),
        capped = !isTRUE(all.equal(rows$sum[i], rows$applied[i])),
        html = as.character(rubric_group_subtotal(rows$sum[i], rows$applied[i]))
      )
    })
    session$onFlushed(function() {
      session$sendCustomMessage("markermd_update_group_subtotals", payload)
    }, once = TRUE)
  }

  # Update the score, the cap badge and the group subtotals when the
  # selections (or the groups, or the scoring setup) change
  shiny::observe({
    shiny::req(input$question_select)
    grade_entered()
    grade_server = rx$grade_servers[[input$question_select]]
    if (!is.null(grade_server)) {
      grade_state = grade_server$grade()
      result = rx$question_score()
      capped = result$score - result$without_groups
      if (result$score != grade_state@current_score) {
        # The live score is per repository; it is shown, never persisted
        grade_server$set_score(result$score, capped = capped)
      } else {
        grade_server$refresh_display(capped)
      }
      send_group_subtotals(result)
    }
  })

  # --- The Rubric tab ----------------------------------------------------------

  # Every question with its scoring settings and its items and groups,
  # editable as in the Grade tab, each item showing how many repositories
  # selected it in place of its toggle. Drawn only while the tab is shown
  # (another tab keeps the last drawing, which the next visit replaces).
  output$rubric_editor_ui = shiny::renderUI({
    shiny::req(identical(rx$active_tab(), "rubric"), cancelOutput = TRUE)
    counts = item_selection_counts(root)
    add_button_style = "width: 26px; height: 26px; display: inline-flex; align-items: center; justify-content: center; padding: 0;"

    sections = lapply(seq_along(question_names), function(k) {
      question_name = question_names[k]
      q = question_blocks(question_name)
      item_ui = function(item_id) {
        n = counts[paste(question_name, item_id, sep = "\x1f")]
        mark_rubric_item_ui(
          rx$view_id("editor", item_id), q$handles[[item_id]]$item(),
          selected_count = if (is.na(n)) 0L else unname(n),
          repo_count = length(repos)
        )
      }
      block_ui = function(block) {
        if (block$type == "item") {
          return(item_ui(block$id))
        }
        group_ui("editor", block$group, q$groups[[block$group]], lapply(block$ids, item_ui))
      }
      grade_server = rx$grade_servers[[question_name]]

      bslib::card(
        id = session$ns(paste0("editor_question_", k)),
        class = "mb-3",
        bslib::card_header(
          class = "bg-light d-flex flex-wrap align-items-center gap-3",
          shiny::span(class = "fw-semibold", question_name),
          if (!is.null(grade_server)) {
            shiny::div(class = "ms-auto", mark_grade_settings_ui(session$ns(grade_server$id), grade_server$grade()))
          }
        ),
        bslib::card_body(
          class = "small",
          fillable = FALSE,
          if (length(q$blocks) == 0) shiny::div(class = "text-muted", "No items yet."),
          do.call(shiny::div, c(list(lapply(q$blocks, block_ui)), rubric_item_container_attrs(session$ns, "editor", q$groups))),
          shiny::div(
            class = "d-flex align-items-center gap-3 mt-2",
            shiny::span(
              shiny::actionButton(
                session$ns(paste0("editor_add_item_", k)), fa_icon("plus"),
                class = "btn-primary btn-sm rounded-circle", style = add_button_style, title = "Add Item"
              ),
              shiny::span("Add Item", class = "ms-1 text-dark")
            ),
            shiny::span(
              shiny::actionButton(
                session$ns(paste0("editor_add_group_", k)), fa_icon("layer-group"),
                class = "btn-outline-primary btn-sm rounded-circle", style = add_button_style, title = "Add Group"
              ),
              shiny::span("Add Group", class = "ms-1 text-dark")
            )
          )
        )
      )
    })
    shiny::div(class = "rubric-editor", sections)
  }) |>
    shiny::bindEvent(rx$redraw_ui(), rx$grade_redraw(), rx$active_tab(), rx$grading_data_version(), rx$external_version())
  shiny::outputOptions(output, "rubric_editor_ui", suspendWhenHidden = FALSE)

  # The editor's jump select scrolls to the chosen question's card, then
  # clears, so choosing the same question again scrolls again
  shiny::updateSelectizeInput(session, "editor_jump", choices = question_names, selected = "")
  shiny::observe({
    shiny::req(nzchar(input$editor_jump))
    k = match(input$editor_jump, question_names)
    session$sendCustomMessage("markermd_scroll_to", list(id = session$ns(paste0("editor_question_", k))))
    shiny::updateSelectizeInput(session, "editor_jump", selected = "")
  }) |>
    shiny::bindEvent(input$editor_jump)

  invisible(rx)
}
