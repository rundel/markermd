# Rubric module: persistence beyond item events
#
# The per-question comment autosave of the pair on screen, the rubric YAML
# import and export of both tabs, and the graded check marks in the question
# and repository selectors. rubric_persistence_wiring() is a plain wiring
# function like rubric_state_wiring(): it fills the module's shared context
# rx (adding refresh_comments(), the outside-change re-read).
#
# rx: The rubric module's shared context, after rubric_state_wiring()

rubric_persistence_wiring = function(rx) {
  session = rx$session
  input = rx$input
  output = rx$output
  root = rx$root
  repos = rx$repos
  question_names = rx$question_names

  # --- Per-question comment persistence -------------------------------------
  # Wires a comment textarea to debounced, navigation-flushed persistence
  # for the (question, repo) pair on screen. The textarea reflects one pair
  # at a time: pair records which, and stored the last value persisted (or
  # loaded) for it. Every edit is captured into pending_edit together with
  # the pair it was typed against, so a save (debounced, or flushed on
  # navigation) can never attribute text to a different question/repo even
  # when navigation outruns the textarea's update echo. Programmatic
  # updateTextAreaInput() echoes are consumed via pending_echoes so they are
  # never mistaken for edits. Returns the outside-change re-read.
  #
  # input_id: textarea input id within this module
  # load_fn: function(root, question, repo) returning the stored text or NULL
  # save_fn: function(root, question, repo, text) appending one event row
  # on_saved: zero-argument callback run after each persisted save

  bind_comment_autosave = function(input_id, load_fn, save_fn, on_saved) {
    pair = shiny::reactiveVal(NULL)
    stored = shiny::reactiveVal("")
    pending_edit = shiny::reactiveVal(NULL)
    pending_echoes = shiny::reactiveVal(character(0))

    save_pending = function() {
      edit = pending_edit()
      if (is.null(edit)) {
        return()
      }
      save_fn(root, edit$question, edit$repo, edit$text)
      current = pair()
      if (!is.null(current) && identical(current$question, edit$question) && identical(current$repo, edit$repo)) {
        stored(edit$text)
      }
      pending_edit(NULL)
      on_saved()
    }

    # Capture typing as a pending edit bound to the pair on screen; consume
    # programmatic echoes, and drop the pending edit when the text is typed
    # back to the stored value
    shiny::observe({
      val = input[[input_id]]
      echoes = pending_echoes()
      hit = match(val, echoes)
      if (!is.na(hit)) {
        pending_echoes(echoes[-hit])
        return()
      }
      current = pair()
      if (is.null(current)) {
        return()
      }
      if (identical(val, stored())) {
        pending_edit(NULL)
        return()
      }
      pending_edit(list(question = current$question, repo = current$repo, text = val))
    }) |>
      shiny::bindEvent(input[[input_id]], ignoreInit = TRUE)

    # On question/repo change: flush the pending edit (to its own pair), then
    # load the new pair's stored comment
    shiny::observe({
      shiny::req(input$question_select, rx$selected_repo())
      save_pending()
      pair(list(question = input$question_select, repo = rx$selected_repo()))
      loaded = load_fn(root, input$question_select, rx$selected_repo())
      if (is.null(loaded)) loaded = ""
      stored(loaded)
      if (!identical(loaded, input[[input_id]])) {
        # The update only echoes back when it changes the client value; cap
        # the outstanding-echo list so an unmatched entry cannot linger forever
        pending_echoes(utils::tail(c(pending_echoes(), loaded), 8))
        shiny::updateTextAreaInput(session, input_id, value = loaded)
      }
    }) |>
      shiny::bindEvent(input$question_select, rx$selected_repo())

    # Debounced autosave while typing; comments are an event log, so one row
    # per pause rather than per keystroke
    text_debounced = shiny::debounce(shiny::reactive(input[[input_id]]), 1000)
    shiny::observe({
      save_pending()
    }) |>
      shiny::bindEvent(text_debounced(), ignoreInit = TRUE)

    # Re-read the pair on screen after a change made outside this session. An
    # unsaved edit is kept (it is saved on the next pause, over the outside
    # change); returns TRUE when it was
    function() {
      current = shiny::isolate(pair())
      if (is.null(current)) {
        return(FALSE)
      }
      if (!is.null(shiny::isolate(pending_edit()))) {
        return(TRUE)
      }
      loaded = load_fn(root, current$question, current$repo)
      if (is.null(loaded)) loaded = ""
      if (!identical(loaded, shiny::isolate(stored()))) {
        stored(loaded)
        pending_echoes(utils::tail(c(shiny::isolate(pending_echoes()), loaded), 8))
        shiny::updateTextAreaInput(session, input_id, value = loaded)
      }
      FALSE
    }
  }

  # Only the public channel feeds grading progress: a non-empty public
  # comment counts the pair as graded, while private notes never do
  refresh_comment = bind_comment_autosave("question_comment", load_comment, save_comment, on_saved = rx$bump_grading_version)
  refresh_private_comment = bind_comment_autosave("question_private_comment", load_private_comment, save_private_comment, on_saved = rx$on_commit)
  rx$refresh_comments = function() {
    any(c(refresh_comment(), refresh_private_comment()))
  }

  # --- Rubric YAML import/export --------------------------------------------
  # Exports read the database via collect_rubric_data(): every item and
  # scoring edit is persisted at once, so the database is never behind the UI.

  write_all = function(file) {
    write_rubric_yaml(collect_rubric_data(root, question_names), file)
  }
  output$export_question = shiny::downloadHandler(
    filename = function() {
      paste0("rubric_", gsub("[^A-Za-z0-9_-]+", "_", input$question_select), ".yaml")
    },
    content = function(file) {
      write_rubric_yaml(collect_rubric_data(root, input$question_select), file)
    },
    contentType = "text/yaml"
  )
  output$export_all = shiny::downloadHandler(
    filename = function() "markermd_rubric.yaml", content = write_all, contentType = "text/yaml"
  )
  output$editor_export_all = shiny::downloadHandler(
    filename = function() "markermd_rubric.yaml", content = write_all, contentType = "text/yaml"
  )

  # A parsed rubric import awaiting confirmation while the modal is open
  pending_rubric_import = shiny::reactiveVal(NULL)

  # Import: a menu button (in either tab) triggers its hidden file picker;
  # parse and validate the upload, then confirm the mode (append / replace)
  # before applying
  handle_rubric_upload = function(file) {
    parsed = purrr::safely(read_rubric_yaml)(file$datapath)
    if (!is.null(parsed$error)) {
      shiny::showNotification(paste0("Could not import rubric: ", conditionMessage(parsed$error)), type = "error")
      return()
    }

    rubric = parsed$result
    yaml_names = purrr::map_chr(rubric$questions, "name")
    if (length(yaml_names) == 0) {
      shiny::showNotification("The rubric file contains no questions.", type = "warning")
      return()
    }
    unknown = setdiff(yaml_names, question_names)
    if (length(unknown) > 0) {
      shiny::showNotification(
        glue::glue(
          "Could not import rubric: question(s) {paste0('\"', unknown, '\"', collapse = ', ')} ",
          "are not in this project's template ({paste0('\"', question_names, '\"', collapse = ', ')})."
        ),
        type = "error"
      )
      return()
    }

    summary_items = lapply(rubric$questions, function(q) {
      n_existing = length(load_rubric_items(root, q$name))
      n_groups = length(q$groups)
      groups_note = if (n_groups == 0) "" else glue::glue(", {n_groups} group{if (n_groups == 1) '' else 's'}")
      scoring_note = if (is.null(q$scoring)) "" else "; updates scoring"
      shiny::tags$li(glue::glue(
        "{q$name}: {length(q$items)} item{if (length(q$items) == 1) '' else 's'}{groups_note} ",
        "({n_existing} existing){scoring_note}"
      ))
    })

    pending_rubric_import(list(rubric = rubric, name = file$name))
    shiny::showModal(shiny::modalDialog(
      title = "Import rubric items?",
      shiny::p(glue::glue("Importing \"{file$name}\" affects:")),
      shiny::tags$ul(summary_items),
      shiny::radioButtons(
        session$ns("rubric_import_mode"),
        "Import mode:",
        choices = c("Append to existing items" = "append", "Replace existing items" = "replace"),
        selected = "append"
      ),
      shiny::textInput(
        session$ns("rubric_import_summary"),
        "Summary (recorded on the snapshot taken before importing)",
        placeholder = "What this import changes and why",
        width = "100%"
      ),
      shiny::p(
        class = "text-danger small mb-0",
        "Replace deletes the existing rubric items and groups for these questions, and any",
        "recorded selections of the items, for every repository. A snapshot of the current",
        "rubric and marks is taken first, so the import can be undone from the History tab."
      ),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("confirm_rubric_import"), "Import", class = "btn-primary")
      )
    ))
  }

  shiny::observe({
    handle_rubric_upload(input$rubric_import_file)
  }) |>
    shiny::bindEvent(input$rubric_import_file)

  shiny::observe({
    handle_rubric_upload(input$editor_import_file)
  }) |>
    shiny::bindEvent(input$editor_import_file)

  # Replace deletes and re-records; the confirm button says so
  shiny::observe({
    shinyjs::toggleClass(
      session$ns("confirm_rubric_import"), "btn-danger", asis = TRUE,
      condition = identical(input$rubric_import_mode, "replace")
    )
    shinyjs::toggleClass(
      session$ns("confirm_rubric_import"), "btn-primary", asis = TRUE,
      condition = !identical(input$rubric_import_mode, "replace")
    )
  }) |>
    shiny::bindEvent(input$rubric_import_mode)

  # Apply a confirmed import, then bring the handles back in line with the
  # database
  shiny::observe({
    shiny::removeModal()
    pending = pending_rubric_import()
    pending_rubric_import(NULL)
    shiny::req(pending)
    mode = input$rubric_import_mode

    summary = if (nzchar(trimws(input$rubric_import_summary))) trimws(input$rubric_import_summary)
    applied = purrr::safely(function() {
      rx$app_action(
        "rubric_import",
        call = glue::glue("mark(): rubric import (mode = '{mode}')"),
        summary = summary, parts = "rubric",
        apply_rubric_import(
          root, pending$rubric, mode,
          snapshot = list(trigger = glue::glue("mark(): Rubric import ({mode})"), summary = summary)
        )
      )
    })()
    if (!is.null(applied$error)) {
      shiny::showNotification(
        paste0("Could not import rubric: ", conditionMessage(applied$error)),
        type = "error", duration = NULL
      )
      return()
    }
    summaries = applied$result

    scoring_updated = FALSE
    for (question_name in names(summaries)) {
      s = summaries[[question_name]]
      # Groups first, so the items' group ids resolve to live handles
      if (identical(s$mode, "replace")) {
        # Rebuild the question's handles from the database
        rx$loaded_questions = union(rx$loaded_questions, question_name)
        rx$sync_question_groups(question_name)
        rx$rebuild_question_items(question_name, load_rubric_items(root, question_name))
      } else {
        # Keep the existing handles (their live selections and any edit in
        # flight); add handles for just the imported items, then take the
        # written order, hotkeys and group ids. A question not loaded yet is
        # loaded from the database, which already holds the imported items.
        rx$ensure_question_loaded(question_name)
        rx$sync_question_groups(question_name)
        for (item_id in names(s$items)) {
          written = s$items[[item_id]]
          handle = rx$items[[question_name]][[item_id]]
          if (is.null(handle)) {
            rx$add_item_handle(question_name, item_id, written)
          } else {
            item = handle$item()
            item@hotkey = written@hotkey
            item@group = written@group
            handle$update_item(item)
          }
        }
        rx$items[[question_name]] = rx$items[[question_name]][names(s$items)]
      }

      if (!is.null(s$scoring)) {
        scoring_updated = TRUE
        grade_server = rx$grade_servers[[question_name]]
        if (!is.null(grade_server)) {
          grade_server$update_grade(s$scoring)
        }
      }
    }

    rx$apply_saved_selections()
    rx$redraw()
    if (scoring_updated) {
      rx$grade_redraw(rx$grade_redraw() + 1)
    }
    # Progress checkmarks re-read the database (a replace stops the replaced
    # items' selections counting)
    rx$bump_grading_version()

    n_new = sum(purrr::map_int(summaries, ~ length(.x$new_ids)))
    shiny::showNotification(
      glue::glue(
        "Imported {n_new} item{if (n_new == 1) '' else 's'} into ",
        "{length(summaries)} question{if (length(summaries) == 1) '' else 's'} from {pending$name}."
      ),
      type = "message"
    )
  }) |>
    shiny::bindEvent(input$confirm_rubric_import)

  # --- Graded check marks ----------------------------------------------------

  # Graded (question, repo) pairs re-read from the database after each write
  graded_pairs_r = shiny::reactive({
    rx$grading_data_version()
    graded_question_pairs(root, repos)
  })

  # Which questions count as graded for the current repo
  graded_questions = shiny::reactive({
    shiny::req(rx$selected_repo())
    pairs = graded_pairs_r()
    repo_graded = pairs$question_name[pairs$assignment_repo == rx$selected_repo()]
    stats::setNames(question_names %in% repo_graded, question_names)
  })

  # Prefix graded questions with a check in the selector; only push an
  # update when the labels actually change so open menus are not rebuilt
  last_question_labels = shiny::reactiveVal(NULL)
  shiny::observe({
    shiny::req(input$question_select)
    status = graded_questions()
    labels = ifelse(status[question_names], paste0("✓ ", question_names), question_names)
    if (identical(labels, last_question_labels())) return()
    last_question_labels(labels)
    shiny::updateSelectInput(
      session, "question_select",
      choices = stats::setNames(question_names, labels),
      selected = shiny::isolate(input$question_select)
    )
  }) |>
    shiny::bindEvent(graded_questions())

  # Prefix fully graded repos with a check in the content pane's repo
  # selector, through the content module's label setter
  last_repo_labels = shiny::reactiveVal(NULL)
  shiny::observe({
    shiny::req(rx$selected_repo())
    counts = calculate_grading_progress(root, question_names, repos, graded_pairs = graded_pairs_r())
    done = length(question_names) > 0 & counts[repos] >= length(question_names)
    labels = ifelse(done, paste0("✓ ", repos), repos)
    if (identical(labels, last_repo_labels())) return()
    last_repo_labels(labels)
    rx$set_repo_labels(
      choices = stats::setNames(repos, labels),
      selected = shiny::isolate(rx$selected_repo())
    )
  }) |>
    shiny::bindEvent(graded_pairs_r())

  invisible(rx)
}
