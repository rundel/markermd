# Template persistence for the template app: dirty tracking against a saved
# snapshot, the save button/status UI, saving into the project database, YAML
# download/export, and the upload/confirm import flow.
#
# This is deliberately NOT a Shiny module: it shares the app's root namespace
# because its input/output ids (save_template, save_to_project,
# export_template, import_file, confirm_import, template_status_ui,
# save_button_ui) are referenced by the app's static UI and by the shinytest2
# tests. It is a plain wiring function taking the server's input/output/session
# plus the reactives and functions it needs, following the same shape as
# ast_preview_observers().

# Wire the template persistence outputs and observers onto the app's session
#
# input, output, session: The template app's server objects
# project: markermd_project S7 object, or NULL for standalone sessions
# source_path: Character. Path to the assignment document, or NULL
# question_modules: Reactive. The app's question-module registry
# build_current_template: Function. Assembles a markermd_template from the
#   live question modules
# load_questions_into_editor: Function(questions). Replaces the editor's
#   questions with the supplied list
#
# Returns list(dirty = reactive dirty flag, mark_saved = function resetting
# the saved snapshot to the current serialized state)

template_persistence = function(input, output, session,
                                project, source_path,
                                question_modules,
                                build_current_template,
                                load_questions_into_editor) {

  # Serialized form of the current template, used for unsaved-change
  # detection. Metadata is dropped (created_at is stamped fresh on every
  # build); NULL until every question module has a live server so a loaded
  # template is not compared against a half-built editor.
  current_template_serialized = shiny::reactive({
    modules = question_modules()
    if (length(modules) > 0 && any(vapply(modules, function(m) is.null(m$server), logical(1)))) {
      return(NULL)
    }
    lst = template_to_list(build_current_template())
    lst$metadata = NULL
    lst
  })

  # Snapshot as of the last save, baselined once the initial state is built
  saved_template_snapshot = shiny::reactiveVal(NULL)

  shiny::observe({
    cur = current_template_serialized()
    if (is.null(saved_template_snapshot()) && !is.null(cur)) {
      saved_template_snapshot(cur)
    }
  })

  # Reset the snapshot to the current serialized state, marking the editor
  # clean; used by both save paths and exposed for any future save path
  mark_saved = function() {
    saved_template_snapshot(current_template_serialized())
  }

  # Debounced so the serialization does not run per keystroke, and held in a
  # reactiveVal so the save controls (and their popover DOM) only re-render
  # when the dirty flag actually flips
  current_template_serialized_debounced = shiny::debounce(current_template_serialized, 500)
  template_dirty = shiny::reactiveVal(FALSE)

  shiny::observe({
    snap = saved_template_snapshot()
    cur = current_template_serialized_debounced()
    dirty = !is.null(snap) && !is.null(cur) && !identical(cur, snap)
    if (!identical(dirty, template_dirty())) {
      template_dirty(dirty)
    }
  })

  # Question count and unsaved-changes indicator shown above the save controls
  output$template_status_ui = shiny::renderUI({
    modules = question_modules()
    if (length(modules) == 0) {
      return(NULL)
    }
    n = 0
    for (m in modules) {
      if (!is.null(m$server)) {
        n = n + 1
      }
    }
    shiny::div(
      class = "small text-muted mb-1",
      glue::glue("{n} question{if (n == 1) '' else 's'}"),
      if (template_dirty()) {
        shiny::span(
          class = "text-warning-emphasis ms-2",
          shiny::icon("circle-exclamation"),
          " unsaved changes"
        )
      }
    )
  })

  # Dynamic save button UI; the save button turns warning-colored while the
  # editor has diverged from the last saved state
  output$save_button_ui = shiny::renderUI({
    has_questions = length(question_modules()) > 0
    save_class = if (template_dirty()) "btn-warning" else "btn-success"
    if (!is.null(project)) {
      # Project sessions: save to the project database. The adjacent
      # import/export popover is static UI (see the card footer).
      shiny::actionButton(
        "save_to_project",
        "Save to Project",
        class = paste(save_class, "btn-sm"),
        disabled = !has_questions
      )
    } else if (!has_questions) {
      shiny::actionButton(
        "save_disabled",
        "Save Template",
        class = "btn-secondary btn-sm",
        disabled = TRUE
      )
    } else {
      shiny::downloadButton(
        "save_template",
        "Save Template",
        class = paste(save_class, "btn-sm")
      )
    }
  })

  # Import is available even with no questions; exporting an empty template
  # is not, so the static export button is soft-disabled via its class
  if (!is.null(project)) {
    shiny::observe({
      shinyjs::toggleClass(
        id = "export_template",
        class = "disabled",
        condition = length(question_modules()) == 0
      )
    })
  }

  # Save template functionality (file download for non-project sessions)
  output$save_template = shiny::downloadHandler(
    filename = function() {
      timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("template_", timestamp, ".yaml")
    },
    content = function(file) {
      # Save as YAML, recording the assignment source so the template can be
      # re-opened and re-validated later.
      write_template_yaml(build_current_template(), file, source_path = source_path)
      mark_saved()
    },
    contentType = "text/yaml"
  )

  # Save into the project database, the canonical template store (project
  # sessions). The assignment source is recorded root-relative so it resolves
  # on reload; YAML remains available via the Export button. An automatic
  # snapshot of the state being overwritten is taken in the same transaction
  # (skipped when nothing changed since the latest snapshot).
  shiny::observe({
    src_rel = if (!is.null(source_path)) as.character(fs::path_rel(source_path, project@root)) else NULL
    save_template_to_db(
      project@root, build_current_template(), source_path = src_rel,
      snapshot = list(trigger = "template(): save")
    )
    mark_saved()

    shiny::showNotification(
      "Saved template to the project database.",
      type = "message"
    )
  }) |> shiny::bindEvent(input$save_to_project)

  # Snapshots (project sessions): take a named snapshot of the project's
  # grading state, or load the template a snapshot recorded into the editor
  # (saving then writes it back, through the usual automatic snapshot). The
  # snapshot records the saved database state, not the editor's unsaved edits.
  if (!is.null(project)) {
    template_snapshot_controls(input, output, session, project, question_modules, template_dirty, load_questions_into_editor)
  }

  # Export the current template to a YAML file (project sessions). The source
  # is recorded root-relative so the exported file re-opens against the key.
  output$export_template = shiny::downloadHandler(
    filename = function() "markermd_template.yaml",
    content = function(file) {
      src = if (!is.null(project) && !is.null(source_path)) {
        as.character(fs::path_rel(source_path, project@root))
      } else {
        source_path
      }
      write_template_yaml(build_current_template(), file, source_path = src)
    },
    contentType = "text/yaml"
  )

  apply_import = function(questions, file_name) {
    load_questions_into_editor(questions)
    shiny::showNotification(
      glue::glue("Imported {length(questions)} question{if (length(questions) == 1) '' else 's'} from {file_name}."),
      type = "message"
    )
  }

  # A parsed import awaiting confirmation while the replace modal is open
  pending_import = shiny::reactiveVal(NULL)

  # Import: the Import button triggers the hidden #import_file picker; parse
  # the uploaded file and load it into the editor (replacing its questions).
  # When the editor already has questions, confirm before replacing them so a
  # stray import does not silently lose authoring work.
  shiny::observe({
    file = input$import_file
    parsed = purrr::safely(read_template_yaml)(file$datapath, require_ast = FALSE)

    if (!is.null(parsed$error)) {
      shiny::showNotification(
        paste0("Could not import template: ", conditionMessage(parsed$error)),
        type = "error"
      )
      return()
    }

    questions = parsed$result@questions
    n_existing = length(question_modules())
    if (n_existing > 0) {
      pending_import(list(questions = questions, name = file$name))
      shiny::showModal(shiny::modalDialog(
        title = "Replace existing questions?",
        glue::glue(
          "Importing \"{file$name}\" replaces the {n_existing} ",
          "question{if (n_existing == 1) '' else 's'} currently in the editor. ",
          "This cannot be undone."
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_import", "Replace", class = "btn-danger")
        )
      ))
    } else {
      apply_import(questions, file$name)
    }
  }) |> shiny::bindEvent(input$import_file)

  shiny::observe({
    shiny::removeModal()
    pending = pending_import()
    pending_import(NULL)
    apply_import(pending$questions, pending$name)
  }) |> shiny::bindEvent(input$confirm_import)

  list(
    dirty = template_dirty,
    mark_saved = mark_saved
  )
}

# The template editor's snapshot controls (project sessions): the modals
# behind the Snapshots popover's "Take snapshot" and "Restore template"
# buttons. Snapshots are of the project database (template, rubric and
# marks); restoring here loads a snapshot's template into the editor rather
# than writing the database, so the user reviews it and saves.
#
# input, output, session: The template app's server objects
# project: markermd_project S7 object
# question_modules: Reactive. The app's question-module registry
# template_dirty: Reactive. Whether the editor has unsaved changes
# load_questions_into_editor: Function(questions). Replaces the editor's
#   questions with the supplied list

template_snapshot_controls = function(input, output, session, project, question_modules, template_dirty,
                                      load_questions_into_editor) {
  shiny::observe({
    shiny::showModal(shiny::modalDialog(
      title = "Take snapshot",
      shiny::p(
        class = "small text-muted",
        "Records the project's current template, rubric and marks as stored in its database.",
        if (template_dirty()) shiny::span(class = "text-warning-emphasis", " The editor has unsaved changes; save first to include them.")
      ),
      shiny::textInput("snapshot_label", "Label", placeholder = "e.g. Template before adding Q4", width = "100%"),
      shiny::textAreaInput("snapshot_summary", "Summary", placeholder = "What is about to change, or why this state is worth keeping", width = "100%", rows = 3),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("confirm_snapshot", "Take snapshot", class = "btn-primary")
      )
    ))
  }) |> shiny::bindEvent(input$take_snapshot)

  shiny::observe({
    shiny::removeModal()
    blank_to_null = function(x) if (is.null(x) || !nzchar(trimws(x))) NULL else x
    res = purrr::safely(purrr::quietly(snapshot_create))(
      project@root,
      label = blank_to_null(input$snapshot_label),
      summary = blank_to_null(input$snapshot_summary)
    )
    if (!is.null(res$error)) {
      shiny::showNotification(conditionMessage(res$error), type = "error", duration = NULL)
    } else {
      shiny::showNotification(paste0("Snapshot ", res$result$result, " taken."), type = "message")
    }
  }) |> shiny::bindEvent(input$confirm_snapshot)

  shiny::observe({
    choices = template_snapshot_choices(project@root)
    if (length(choices) == 0) {
      shiny::showNotification("No snapshot holds a template yet.", type = "warning")
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = "Restore a template snapshot",
      shiny::p(
        class = "small text-muted",
        "Loads the template a snapshot recorded into the editor, replacing the questions shown here.",
        "Nothing is written until you save. Only snapshots whose template differs from the one before are listed."
      ),
      shiny::selectInput("restore_snapshot_id", "Snapshot", choices = choices, width = "100%", selectize = FALSE),
      if (length(question_modules()) > 0) shiny::p(
        class = "text-danger small mb-0",
        "The ", length(question_modules()), " question", if (length(question_modules()) == 1) "" else "s",
        " currently in the editor will be replaced.", if (template_dirty()) " Unsaved changes are lost."
      ),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("confirm_restore_snapshot", "Load into editor", class = "btn-danger")
      )
    ))
  }) |> shiny::bindEvent(input$restore_snapshot)

  shiny::observe({
    shiny::removeModal()
    id = input$restore_snapshot_id
    shiny::req(id)
    part = with_database(project@root, function(conn) read_snapshot_part(conn, id, "template"))
    parsed = purrr::safely(function() lapply(part$questions, question_from_list))()
    if (!is.null(parsed$error)) {
      shiny::showNotification(
        paste0("Could not load the snapshot's template: ", conditionMessage(parsed$error)),
        type = "error", duration = NULL
      )
      return()
    }
    load_questions_into_editor(parsed$result)
    shiny::showNotification(
      glue::glue("Loaded {length(parsed$result)} question{if (length(parsed$result) == 1) '' else 's'} from snapshot {id}. Save to keep them."),
      type = "message"
    )
  }) |> shiny::bindEvent(input$confirm_restore_snapshot)
}
