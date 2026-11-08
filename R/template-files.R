# The template editor's files: the Files button in the Questions card's
# title bar, the read-only view of the active entry's file in the left pane,
# and the Add files modal listing what the key directory holds. Like
# template_persistence(), this is a plain wiring function, not a module, so
# its ids stay in the app's root namespace.

# The Files button, for the Questions card's title bar

template_files_button_ui = function() {
  shiny::actionButton(
    "add_files", "Files",
    icon = shiny::icon("folder-plus"),
    class = "btn-outline-secondary btn-sm",
    title = "Choose which of the assignment's other files (scripts, data, images) entries can cover"
  )
}

# The editor's left pane: the document tree, or the read-only view of the
# active entry's file when it is on another file. The hidden text output
# carries that file ("" for the document) for the conditional panels.
#
# ast_panel: The AST module's UI (the document tree card)

template_files_pane_ui = function(ast_panel) {
  shiny::div(
    class = "h-100",
    shiny::div(class = "d-none", shiny::textOutput("template_shown_file")),
    shiny::conditionalPanel(
      "!output.template_shown_file",
      class = "h-100",
      ast_panel
    ),
    shiny::conditionalPanel(
      "output.template_shown_file",
      class = "h-100",
      bslib::card(
        class = "h-100",
        bslib::card_body(
          class = "p-0 overflow-auto",
          shiny::uiOutput("file_view", class = "h-100")
        )
      )
    )
  )
}

# Wire the file view and the Add files modal onto the app's session
#
# input, output, session: The template app's server objects
# files_root: The key directory the files are listed from, or NULL when the
#   editor has no directory (a template object without its source)
# doc_rel: The Quarto document's path relative to files_root, never listed
#   as a file
# template_files: reactiveVal holding the template's files
# question_files: Function returning a named character vector, the file of
#   each question (by name) that is on one
# shown_file: Reactive giving the active entry's file, "" for the document

template_files_server = function(input, output, session, files_root, doc_rel,
                                 template_files, question_files, shown_file) {

  output$template_shown_file = shiny::renderText(shown_file())
  shiny::outputOptions(output, "template_shown_file", suspendWhenHidden = FALSE)

  output$file_view = shiny::renderUI({
    file = shown_file()
    shiny::req(nzchar(file))
    path = if (is.null(files_root)) NA_character_ else file.path(files_root, file)
    file_view_ui(
      if (!is.na(path) && file.exists(path)) path else NA_character_,
      file, editor_id = "markermd-template-file-editor"
    )
  })

  # The Add files modal: a checkbox list of the key directory's files, the
  # template's files checked, with a switch revealing the filtered ones
  candidates = shiny::reactive({
    shiny::req(files_root)
    input$add_files
    assignment_file_candidates(files_root, exclude = doc_rel)
  })

  shiny::observe({
    if (is.null(files_root)) {
      shiny::showNotification("This template has no assignment directory to list files from.", type = "warning")
      return()
    }
    shiny::showModal(shiny::modalDialog(
      title = "Assignment files",
      size = "l",
      easyClose = TRUE,
      shiny::p(
        class = "small text-muted",
        "Questions can cover any of these files as a whole: grading shows the file from each repository, and validation checks that the repository has it. The Quarto document is always included."
      ),
      bslib::input_switch("add_files_show_all", "Show hidden and filtered files", value = FALSE),
      shiny::div(
        class = "overflow-auto border rounded p-2",
        style = "max-height: 55vh;",
        shiny::uiOutput("add_files_list")
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("apply_files", "Apply", class = "btn-primary")
      )
    ))
  }) |> shiny::bindEvent(input$add_files)

  output$add_files_list = shiny::renderUI({
    rows = candidates()
    show_all = isTRUE(input$add_files_show_all)
    current = shiny::isolate(template_files())
    checked = shiny::isolate(input$add_files_selected)
    checked = if (is.null(checked)) current else checked
    visible = rows[show_all | !rows$filtered | rows$path %in% union(current, checked), , drop = FALSE]
    missing = setdiff(current, rows$path)
    paths = c(visible$path, missing)
    if (length(paths) == 0) {
      return(shiny::div(class = "text-muted small", "No other files in the assignment directory."))
    }
    labels = lapply(seq_along(paths), function(i) {
      path = paths[i]
      reason = if (path %in% missing) "not in the assignment directory" else visible$reason[i]
      shiny::span(
        shiny::icon(file_icon_name(path), class = "fa-fw text-muted me-1"),
        shiny::tags$code(path),
        if (!is.na(reason)) shiny::span(class = "text-muted small ms-1", paste0("(", reason, ")"))
      )
    })
    shiny::checkboxGroupInput(
      "add_files_selected", NULL,
      choiceNames = labels, choiceValues = paths,
      selected = intersect(checked, paths), width = "100%"
    )
  })

  # Apply: the checked files become the template's files, in the listing's
  # order after the files kept from before. A file a question covers stays.
  shiny::observe({
    chosen = input$add_files_selected
    chosen = if (is.null(chosen)) character(0) else chosen
    used = question_files()
    kept = intersect(setdiff(template_files(), chosen), used)
    if (length(kept) > 0) {
      who = names(used)[used %in% kept]
      shiny::showNotification(
        paste0(
          "Kept ", paste(kept, collapse = ", "), ": covered by ",
          paste(who, collapse = ", "), ". Move those questions off the file first."
        ),
        type = "warning", duration = 10
      )
    }
    previous = template_files()
    files = c(intersect(previous, union(chosen, kept)), setdiff(chosen, previous))
    template_files(files)
    shiny::removeModal()
  }) |> shiny::bindEvent(input$apply_files)

  invisible(NULL)
}
