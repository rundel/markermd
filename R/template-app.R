# Main Shiny application for creating assignment templates
#
# ast: Reactive. The parsed AST object
# template_obj: markermd_template S7 object. Optional template to load on startup
# source_path: Character. Path to the assignment document, recorded in saved
#   templates so they can be re-opened and re-validated later
# project: markermd_project S7 object. When set, "Save Template" writes the
#   template into the project and records it in the config (via project_set())
#   rather than offering a browser download

template_app = function(ast, template_obj = NULL, source_path = NULL, project = NULL) {

  # Where the assignment's other files are listed from: the project's key
  # directory, else the Quarto document's directory; the document's path
  # relative to it is never offered as a file
  files_root = if (!is.null(project) && !is.na(project@key)) {
    as.character(fs::path(project@root, project@key))
  } else if (!is.null(source_path)) {
    dirname(source_path)
  }
  doc_rel = if (!is.null(files_root) && !is.null(source_path)) {
    as.character(fs::path_rel(normalizePath(source_path, winslash = "/", mustWork = FALSE), normalizePath(files_root, winslash = "/")))
  } else {
    character(0)
  }
  doc_label = if (length(doc_rel) == 1) doc_rel else if (!is.null(source_path)) basename(source_path) else "Assignment"

  # UI. A tagList (not a wrapper div) so the layout_columns is a direct fill
  # item of the enclosing fillable page and sizes itself to the remaining
  # viewport instead of a hard-coded vh formula.
  ui = shiny::tagList(
    app_loading_overlay("Loading the template editor\u2026"),
    # Initialize shinyjs
    shinyjs::useShinyjs(),
    markermd_modal_css(),
    fa_dependency(),

      shiny::tags$style(shiny::HTML("
      /* Rule form controls */
      .rule-item select,
      .rule-item .form-control,
      .rule-item input[type='number'],
      .rule-item .input-group-addon {
        font-size: 12px !important;
        height: 32px !important;
        padding: 4px 8px !important;
      }
      
      .rule-item {
        position: relative;
        z-index: 100;
        width: 100% !important;
        max-width: 100% !important;
        overflow: visible !important;
      }
      
      /* The question header's file label: smaller text than the name */
      .question-file-label { font-size: 12px; }

      .rule-item .form-group,
      .filter-condition .form-group,
      .filter-group .form-group { margin-bottom: 0 !important; }
      .filter-condition .checkbox,
      .filter-group .checkbox { margin: 0 !important; min-height: 0 !important; }

      /* Per-condition negate toggle: stack the not label under the checkbox
         so the toggle stays no taller than the adjacent select inputs */
      .filter-not-toggle .checkbox label {
        display: flex !important;
        flex-direction: column;
        align-items: center;
        margin: 0 !important;
        padding: 0 !important;
        font-size: 10px;
        line-height: 1.2;
        color: var(--bs-secondary-color, #6c757d);
      }
      .filter-not-toggle .checkbox input[type='checkbox'] {
        position: static !important;
        margin: 0 !important;
      }
      .rule-item .input-group-addon { line-height: 1.2 !important; }
      .rule-item select:focus { z-index: 1000; }

      /* Multi-select node-type control (selectize), used by both rule rows
         and filter conditions. Shiny copies .form-control onto the selectize
         wrapper, so the fixed-height/border/padding rule above lands on it
         and clips its items (causing the box to overflow onto the rule
         below). Neutralise the wrapper and treat the inner .selectize-input
         as the form-control-like box so the control grows with its items. */
      .rule-item .selectize-control,
      .filter-condition .selectize-control { margin: 0 !important; }
      .rule-item .selectize-control.form-control,
      .filter-condition .selectize-control.form-control {
        height: auto !important;
        min-height: 0 !important;
        padding: 0 !important;
        border: 0 !important;
        background: transparent !important;
      }
      .rule-item .selectize-control.multi .selectize-input,
      .filter-condition .selectize-control.multi .selectize-input {
        min-height: 32px !important;
        padding: 2px 6px !important;
        font-size: 12px !important;
        line-height: 1.5 !important;
        /* match the native rule selects, whose border is $input-border-color
           ($gray-500) in this Bootstrap 5 theme */
        border: 1px solid var(--bs-gray-500, #8d959e) !important;
        border-radius: var(--bs-border-radius, 0.375rem) !important;
        box-shadow: none !important;
      }
      .rule-item .selectize-control.multi .selectize-input > .item,
      .filter-condition .selectize-control.multi .selectize-input > .item {
        font-size: 12px !important;
        line-height: 1.4 !important;
        padding: 0 4px !important;
        margin: 0 3px 0 0 !important;
      }
      /* dropdownParent='body' renders the menu on <body> */
      .selectize-dropdown { font-size: 12px !important; }

      /* Question cards: inactive cards collapse to their header plus a
         one-line summary; the active card shows its full body. Toggled via
         the question-active class on the card wrapper. */
      #dynamic_questions_container .question-card:not(.question-active) .question-body {
        display: none;
      }
      #dynamic_questions_container .question-card.question-active .question-summary {
        display: none;
      }
      #dynamic_questions_container .question-card:not(.question-active) {
        cursor: pointer;
      }

      #dynamic_questions_container,
      #questions_container .card {
        width: 100% !important;
        max-width: 100% !important;
        box-sizing: border-box !important;
      }
      
      #questions_container .card {
        margin-bottom: 10px !important;
        overflow: visible !important;
      }
      
      #questions_container .bslib-card {
        margin-bottom: 0 !important;
      }
      
      #questions_container .bslib-card .card-body {
        padding-bottom: 8px !important;
      }
      
      #questions_container .bslib-card .card-body > div:last-child {
        margin-bottom: 0 !important;
      }
      
      #questions_container .card * {
        box-sizing: border-box !important;
      }
    ")),

    bslib::layout_columns(
      col_widths = c(6, 6),
      class = "h-100",
      template_files_pane_ui(ast_module_ui(
        "ast_panel",
        header_extra = shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::uiOutput("parse_warnings_badge", inline = TRUE),
          shiny::uiOutput("active_question_badge", inline = TRUE)
        )
      )),
      bslib::card(
        class = "h-100",
        bslib::card_header(
          class = "bg-light d-flex justify-content-between align-items-center",
          "Questions",
          template_files_button_ui()
        ),
        bslib::card_body(
          class = "flex-fill overflow-auto p-0",
          shiny::div(
            id = "questions_container",
            class = "p-3 w-100",
            shiny::uiOutput("questions_ui")
          )
        ),
        bslib::card_footer(
          class = "text-center",
          shiny::uiOutput("template_status_ui"),
          if (!is.null(project)) {
            # Project sessions: the import/export popover is static so its
            # download link binds exactly once; only the save button re-renders
            # (see save_button_ui)
            shiny::div(
              class = "d-flex gap-2 justify-content-center",
              shiny::uiOutput("save_button_ui"),
              io_menu_ui(
                id = "io_menu",
                title = "Template YAML",
                export_buttons = list(
                  shiny::downloadButton(
                    "export_template",
                    "Export",
                    class = "btn-outline-secondary btn-sm"
                  )
                ),
                import_input_id = "import_file"
              ),
              # Snapshots of the project's grading state: take one, or load
              # the template a snapshot recorded (see template_snapshot_controls())
              bslib::popover(
                shiny::tags$button(
                  fa_icon("clock-rotate-left"),
                  id = "snapshot_menu",
                  type = "button",
                  class = "btn btn-outline-secondary btn-sm",
                  title = "Snapshots: record the project's grading state, or load the template an earlier snapshot recorded"
                ),
                shiny::div(
                  class = "d-grid gap-2",
                  bslib::tooltip(
                    shiny::actionButton("take_snapshot", "Take snapshot", class = "btn-outline-secondary btn-sm"),
                    "Record the project's stored template, rubric and marks as a named snapshot in its database (save first to include unsaved edits)"
                  ),
                  bslib::tooltip(
                    shiny::actionButton("restore_snapshot", "Restore template\u2026", class = "btn-outline-secondary btn-sm"),
                    "Choose a snapshot and load the template it recorded into the editor, replacing the questions shown here; nothing is written until you save"
                  )
                ),
                title = "Snapshots"
              )
            )
          } else {
            shiny::uiOutput("save_button_ui")
          },
          # Hidden file input the Import button triggers directly. Bound once
          # here so the upload survives re-renders.
          if (!is.null(project)) shiny::div(
            class = "visually-hidden",
            shiny::fileInput("import_file", NULL, accept = c(".yaml", ".yml", "text/yaml"))
          )
        )
      )
    )
  )
  
  # Server
  server = function(input, output, session) {
    
    # Global state management
    current_question_id = shiny::reactiveVal(1)
    last_question_id = shiny::reactiveVal(0)
    
    # Question modules storage
    question_modules = shiny::reactiveVal(list())

    # The template's other files (questions can cover one each), the File
    # selects' choices, and the file pane over the document tree
    template_files = shiny::reactiveVal(if (is.null(template_obj)) character(0) else template_obj@files)
    file_choices = shiny::reactive(question_file_choices(doc_label, template_files()))

    # The file of each question on one, named by question
    question_files = function() {
      out = character(0)
      for (m in shiny::isolate(question_modules())) {
        q = if (!is.null(m$server)) shiny::isolate(m$server$question()) else m$initial_question
        if (is_file_question(q)) out[[q@name]] = q@file
      }
      out
    }

    # The active entry's file ("" for the document), which the left pane
    # shows: the document tree, or the file's source
    shown_file = shiny::reactive({
      current_module = question_modules()[[as.character(current_question_id())]]
      if (is.null(current_module) || is.null(current_module$server)) {
        return("")
      }
      file = current_module$server$question()@file
      if (is.na(file)) "" else file
    })

    template_files_server(
      input, output, session, files_root, doc_rel,
      template_files, question_files, shown_file
    )
    
    # Reactive value for pending node clicks
    pending_node_click = shiny::reactiveVal(NULL)

    # Memoised flattened document tree. ast() is write-once, so this computes the
    # structure (q2r_flatten over the whole document) once and is reused by the
    # node-click handlers instead of re-flattening on every interaction.
    tree_items_memo = shiny::reactive(build_ast_tree_structure(ast()))

    # Load template on startup if provided
    template_loaded = shiny::reactiveVal(FALSE)
    
    shiny::observe({
      if (!is.null(template_obj) && !template_loaded()) {
        # Load questions from template
        for (question in template_obj@questions) {
          id = question@id

          # Create initial question object in the same format as add_new_question
          module_id = paste0("question_", id)

          # Store the module data (server will be created when UI is inserted)
          modules = question_modules()
          modules[[as.character(id)]] = list(
            id = id,
            module_id = module_id,
            initial_question = question,
            server = NULL  # Will be created when UI is inserted
          )
          question_modules(modules)

          # Update last question ID if this is higher
          if (id > last_question_id()) {
            last_question_id(id)
          }
        }

        # Set current question to first question if any exist
        if (length(template_obj@questions) > 0) {
          current_question_id(template_obj@questions[[1]]@id)
        }

        template_loaded(TRUE)
      }
    }) |>
      shiny::bindEvent(template_obj, once = TRUE, ignoreNULL = TRUE)
    
    # The name a new question gets when the Add modal leaves it blank
    default_question_name = function(scope, file, id) {
      base = if (!is.na(file)) basename(file) else if (scope == "document") "Document" else paste("Question", id)
      taken = character(0)
      for (m in shiny::isolate(question_modules())) {
        q = if (!is.null(m$server)) shiny::isolate(m$server$question()) else m$initial_question
        taken = c(taken, q@name)
      }
      name = base
      k = 2
      while (name %in% taken) {
        name = paste0(base, " (", k, ")")
        k = k + 1
      }
      name
    }

    # Add a new question to the modules list
    #
    # scope: "sections" for a question over selected headings/divs, or
    #   "document" for a document-level question (see markermd_question)
    # file: One of the template's files for a question covering it, or NA for
    #   a question on the Quarto document
    # name: The question's name, or NULL for the default ("Document" for a
    #   document-scope question, the file's name for one on a file, else
    #   "Question <id>")
    add_new_question = function(scope = "sections", file = NA_character_, name = NULL) {
      next_id = last_question_id() + 1
      last_question_id(next_id)

      if (is.null(name)) {
        name = default_question_name(scope, file, next_id)
      }

      # Create initial question object
      initial_question = markermd_question(
        id = as.integer(next_id),
        name = name,
        selected_nodes = markermd_node_selection(),
        rules = list(),
        scope = scope,
        file = file
      )

      # Create question module
      module_id = paste0("question_", next_id)

      # Update question modules (store the initial question, server will be created when UI is inserted)
      modules_list = question_modules()
      modules_list[[as.character(next_id)]] = list(
        id = next_id,
        module_id = module_id,
        initial_question = initial_question,
        server = NULL  # Will be created when UI is inserted
      )
      question_modules(modules_list)

      # Set as current question
      current_question_id(next_id)

      return(next_id)
    }

    # Ensure current question exists in the modules list
    #
    ensure_current_question_exists = function() {
      current_q_id = current_question_id()
      modules_list = question_modules()

      if (is.null(modules_list[[as.character(current_q_id)]])) {
        # Create new question
        add_new_question()
      }
    }
    
    # Get selected nodes for current question
    selected_nodes = shiny::reactive({
      current_q_id = current_question_id()
      result_nodes = integer(0)
      modules_list = question_modules()

      if (length(modules_list) > 0 && !is.null(modules_list[[as.character(current_q_id)]])) {
        current_module = modules_list[[as.character(current_q_id)]]
        if (!is.null(current_module$server)) {
          nodes_result = current_module$server$get_selected_nodes()
          if (!is.null(nodes_result)) {
            result_nodes = nodes_result
          }
        }
      }

      return(result_nodes)
    })
    
    # Tree indices excluded by the current question's filters, drawn in red
    filtered_nodes = shiny::reactive({
      current_q_id = current_question_id()
      modules_list = question_modules()
      current_module = modules_list[[as.character(current_q_id)]]

      if (is.null(current_module) || is.null(current_module$server)) {
        return(integer(0))
      }

      question_filtered_indices(ast(), current_module$server$question())
    })

    # Initialize AST selectable module
    ast_result = ast_module_server("ast_panel", ast, selected_nodes, filtered_nodes, interactive = TRUE)

    # Toggle a node in the current question's selection: a no-op when the node
    # sits under an already-selected ancestor, a removal when itself selected,
    # otherwise an add that first clears any selected descendants. Shared by the
    # pending-click and direct-click observers so the two never drift.
    apply_node_toggle = function(current_module, node_index) {
      # A document entry's selection is the whole tree and cannot change;
      # add_node() tells the user so
      if (is_document_question(current_module$server$question())) {
        current_module$server$add_node(node_index)
        return(invisible())
      }
      current_selected = current_module$server$get_selected_nodes()
      tree_items = tree_items_memo()

      if (has_selected_ancestor(tree_items, node_index, current_selected)) {
        return(invisible())
      }

      if (node_index %in% current_selected) {
        current_module$server$remove_node(node_index)
      } else {
        descendants_to_remove = find_selected_descendants(tree_items, node_index, current_selected)
        for (desc in descendants_to_remove) {
          current_module$server$remove_node(desc)
        }
        current_module$server$add_node(node_index)
      }
    }

    # Handle pending node clicks when question modules change
    shiny::observe({
      click_data = pending_node_click()
      if (is.null(click_data)) return()

      current_q_id = current_question_id()
      modules_list = question_modules()
      current_module = modules_list[[as.character(current_q_id)]]

      # Only process if we now have a server
      if (!is.null(current_module) && !is.null(current_module$server)) {
        # Clear the pending click
        pending_node_click(NULL)

        # Apply the node selection
        apply_node_toggle(current_module, click_data$node_index)
      }
    }) |>
      shiny::bindEvent(list(question_modules(), pending_node_click()))
    
    # Handle node selection events from AST module
    shiny::observe({
      click_data = ast_result$node_clicked()
      if (is.null(click_data)) return()

      current_q_id = current_question_id()

      # Check if question exists
      modules_list = question_modules()
      current_module = modules_list[[as.character(current_q_id)]]
      question_exists = !is.null(current_module)

      # If question doesn't exist, create it and store the pending click
      if (!question_exists) {
        ensure_current_question_exists()
        pending_node_click(click_data)
        return()
      }

      # If question exists but server doesn't exist yet, store as pending
      if (is.null(current_module$server)) {
        pending_node_click(click_data)
        return()
      }

      # Process the click immediately
      apply_node_toggle(current_module, click_data$node_index)
    }) |>
      shiny::bindEvent(ast_result$node_clicked(), ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # The document's parser warnings: a badge in the tree's header that
    # opens them in a modal (q2r keeps them on the AST rather than printing
    # them to the console)
    output$parse_warnings_badge = shiny::renderUI({
      n = length(document_parse_warnings(ast()))
      if (n == 0) {
        return(NULL)
      }
      shiny::actionLink(
        "show_parse_warnings",
        paste0(n, " parser warning", if (n == 1) "" else "s"),
        icon = fa_icon("triangle-exclamation"),
        class = "badge text-bg-warning fw-normal text-decoration-none"
      )
    })

    shiny::observe({
      shiny::showModal(shiny::modalDialog(
        title = "Parser warnings",
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        shiny::p(class = "small text-muted", "The document parsed, but the parser flagged these. Rendering may differ from what was intended."),
        lapply(format_parse_diagnostics(document_parse_warnings(ast())), function(text) {
          shiny::tags$pre(class = "small bg-body-tertiary border rounded p-2", style = "white-space: pre-wrap;", text)
        })
      ))
    }) |> shiny::bindEvent(input$show_parse_warnings)

    # Show which question tree selections will land on, since the tree's
    # highlights always reflect the active question
    output$active_question_badge = shiny::renderUI({
      current_q_id = current_question_id()
      modules_list = question_modules()
      current_module = modules_list[[as.character(current_q_id)]]

      if (is.null(current_module) || is.null(current_module$server)) {
        return(shiny::span(
          "Click a heading to start a question",
          class = "badge text-bg-secondary fw-normal"
        ))
      }

      shiny::span(
        glue::glue("Editing: {current_module$server$question()@name}"),
        class = "badge text-bg-primary fw-normal"
      )
    })

    # Create persistent question container
    output$questions_ui = shiny::renderUI({
      shiny::div(
        id = "questions_container_content",
        shiny::div(id = "dynamic_questions_container"),
        shiny::uiOutput("add_question_button")
      )
    })
    
    # Render add question button, with first-run guidance while no questions
    # exist (the primary gesture, clicking a heading in the tree, is otherwise
    # undiscoverable)
    output$add_question_button = shiny::renderUI({
      shiny::tagList(
        if (length(question_modules()) == 0) {
          shiny::div(
            class = "text-center text-muted py-4 px-3",
            shiny::p(
              fa_icon("arrow-left", class = "me-2"),
              "Click a heading in the Document Structure to start your first question; the heading and the section beneath it become the question's nodes."
            ),
            shiny::p(
              "You can also add an empty question below and select headings afterwards.",
              class = "small mb-0"
            )
          )
        },
        shiny::div(
          class = "text-center mb-3",
          shiny::actionButton(
            "add_question",
            "Add Question",
            icon = fa_icon("plus"),
            class = "btn-outline-primary btn-sm"
          )
        )
      )
    })
    
    # Use insertUI/removeUI approach for stable question modules
    # Lazily create a server for any question module that lacks one. Doing this
    # in an observer (not inside the question_items render) keeps the render pure
    # and avoids the re-entrant output state errors that a render writing its own
    # reactive dependency would cause.
    shiny::observe({
      modules_list = question_modules()
      needs_server = vapply(modules_list, function(m) is.null(m$server), logical(1))
      if (!any(needs_server)) {
        return()
      }

      for (q_id_str in names(modules_list)[needs_server]) {
        modules_list[[q_id_str]]$server = question_server(
          modules_list[[q_id_str]]$module_id,
          ast,
          initial_question = modules_list[[q_id_str]]$initial_question,
          name_available = local({
            own = q_id_str
            function(name) !name %in% question_names_except(shiny::isolate(question_modules()), own)
          })
        )
      }
      question_modules(modules_list)
    })

    # Track which question cards have been inserted into the DOM
    inserted_questions = shiny::reactiveVal(character(0))

    # Sync the question cards to the modules list with insertUI/removeUI so that
    # existing cards are never rebuilt. The previous full re-render rebuilt the
    # rule controls and dropped in-progress picker edits whenever a question was
    # added/removed or the active card changed. Servers are created lazily above,
    # so an inserted card's outputs wire up immediately. The active-border
    # highlight is toggled separately via shinyjs.
    shiny::observe({
      modules_list = question_modules()
      already = inserted_questions()
      module_ids = names(modules_list)

      for (q_id_str in setdiff(already, module_ids)) {
        shiny::removeUI(selector = paste0("#question_wrapper_", q_id_str))
      }

      for (q_id_str in setdiff(module_ids, already)) {
        q_id = as.numeric(q_id_str)
        question_module = modules_list[[q_id_str]]
        card_class = if (q_id == shiny::isolate(current_question_id())) {
          "question-card question-active border border-primary border-2"
        } else {
          "question-card border"
        }

        shiny::insertUI(
          selector = "#dynamic_questions_container",
          where = "beforeEnd",
          ui = shiny::div(
            id = paste0("question_wrapper_", q_id),
            style = "margin-bottom: 15px; width: 100%; max-width: 100%; box-sizing: border-box;",
            # priority: 'event' so re-clicking a card whose value is already
            # stored still fires (the active question can move programmatically
            # via add/delete/import, and a deduped click would silently leave
            # tree selections landing on the wrong question)
            onclick = paste0("Shiny.setInputValue('select_question', ", q_id, ", {priority: 'event'});"),
            shiny::div(
              id = paste0("question_card_", q_id),
              class = card_class,
              style = "border-radius: 0.375rem;",
              question_ui(
                question_module$module_id, q_id,
                name = question_module$initial_question@name,
                file_label = if (is.na(question_module$initial_question@file)) doc_label else question_module$initial_question@file,
                file = question_module$initial_question@file
              )
            )
          )
        )
      }

      if (!identical(already, module_ids)) {
        inserted_questions(module_ids)
      }
    })
    
    # Add new question handler: a new entry starts on the document; its File
    # select moves it onto another file
    # Add Question asks which file the entry covers and, on the Quarto
    # document, whether it covers selected sections or the entire document
    # (the latter grades what belongs to no single question: the YAML header,
    # render settings). The file and coverage are fixed once added.
    shiny::observe({
      doc_js = jsonlite::toJSON(doc_label, auto_unbox = TRUE)
      shiny::showModal(shiny::modalDialog(
        title = "Add a question",
        easyClose = TRUE,
        shiny::textInput("new_entry_name", "Name", placeholder = "Default: Question N, Document, or the file name", width = "100%"),
        shiny::selectInput("new_entry_file", "File", choices = file_choices(), width = "100%", selectize = FALSE),
        shiny::conditionalPanel(
          paste0("input.new_entry_file == ", doc_js),
          shiny::radioButtons(
            "new_entry_coverage", "Covers",
            choices = c("Selected sections" = "sections", "The entire document" = "document"),
            selected = "sections"
          )
        ),
        shiny::div(
          class = "small text-muted",
          "Other files are covered entire and take no rules; the Files button in the Questions header adds files to choose from."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_add_entry", "Add", class = "btn-primary")
        )
      ))
    }) |>
      shiny::bindEvent(input$add_question)

    shiny::observe({
      file = input$new_entry_file
      on_document = is.null(file) || identical(file, doc_label)
      scope = if (on_document && identical(input$new_entry_coverage, "document")) "document" else "sections"
      file = if (on_document) NA_character_ else file
      name = trimws(input$new_entry_name)
      if (nzchar(name)) {
        taken = question_names_except(question_modules(), NULL)
        if (name %in% taken) {
          shiny::showNotification(paste0("There is already a question named \"", name, "\"."), type = "error")
          return()
        }
      }
      shiny::removeModal()
      add_new_question(scope = scope, file = file, name = if (nzchar(name)) name else NULL)
    }) |>
      shiny::bindEvent(input$confirm_add_entry)

    # Reorder a question one position up or down. The modules list order is
    # the saved template order; the DOM wrapper is moved in place because
    # re-inserting the UI would rebuild the inputs and drop in-progress edits.
    move_question = function(q_id_str, direction) {
      modules_list = question_modules()
      ids = names(modules_list)
      pos = match(q_id_str, ids)
      new_pos = pos + direction
      if (is.na(pos) || new_pos < 1 || new_pos > length(ids)) {
        return()
      }

      other_id = ids[new_pos]
      new_order = seq_along(ids)
      new_order[c(pos, new_pos)] = new_order[c(new_pos, pos)]
      question_modules(modules_list[new_order])

      verb = if (direction < 0) "insertBefore" else "insertAfter"
      shinyjs::runjs(glue::glue(
        "$('#question_wrapper_<<q_id_str>>').<<verb>>('#question_wrapper_<<other_id>>');",
        .open = "<<", .close = ">>"
      ))
    }

    # Wire each question's move up/down signals once its server exists
    wired_move_handlers = shiny::reactiveVal(character(0))

    shiny::observe({
      modules_list = question_modules()
      done = wired_move_handlers()
      for (q_id_str in setdiff(names(modules_list), done)) {
        m = modules_list[[q_id_str]]
        if (is.null(m$server)) next
        local({
          qid = q_id_str
          srv = m$server
          shiny::observe({
            move_question(qid, -1L)
          }) |>
            shiny::bindEvent(srv$move_up_clicked(), ignoreInit = TRUE)
          shiny::observe({
            move_question(qid, 1L)
          }) |>
            shiny::bindEvent(srv$move_down_clicked(), ignoreInit = TRUE)
        })
        done = c(done, q_id_str)
      }
      if (!identical(done, wired_move_handlers())) {
        wired_move_handlers(done)
      }
    })

    # Handle question selection. The click fires as an event on every click
    # (see the insertUI onclick), so dedupe here against the authoritative
    # current id, which also reflects programmatic moves (add/delete/import).
    shiny::observe({
      if (!identical(as.numeric(input$select_question), as.numeric(current_question_id()))) {
        current_question_id(input$select_question)
      }
    }) |>
      shiny::bindEvent(input$select_question)

    # Move the active-card border highlight without re-rendering question_items
    # (see the isolate() above). Toggling the class in place leaves the rule
    # controls untouched so picker edits survive switching cards.
    shiny::observe({
      cur = current_question_id()
      for (q_id_str in names(shiny::isolate(question_modules()))) {
        selector = paste0("#question_card_", q_id_str)
        if (as.numeric(q_id_str) == cur) {
          shinyjs::addClass(selector = selector, class = "border-primary border-2 question-active")
        } else {
          shinyjs::removeClass(selector = selector, class = "border-primary border-2 question-active")
        }
      }
    }) |>
      shiny::bindEvent(current_question_id())
    
    
    # Handle question deletion
    shiny::observe({
      modules_list = question_modules()
      questions_to_remove = c()
      
      for (q_id_str in names(modules_list)) {
        question_module = modules_list[[q_id_str]]
        if (!is.null(question_module$server)) {
          delete_count = question_module$server$delete_clicked()
          
          if (!is.null(delete_count) && delete_count > 0) {
            questions_to_remove = c(questions_to_remove, q_id_str)
          }
        }
      }
      
      if (length(questions_to_remove) > 0) {
        # Remove questions
        modules_list = question_modules()
        
        for (q_id_str in questions_to_remove) {
          modules_list[[q_id_str]] = NULL
        }
        
        question_modules(modules_list)
        
        # Update current question if needed
        current_q_id = current_question_id()
        if (as.character(current_q_id) %in% questions_to_remove) {
          if (length(modules_list) > 0) {
            current_question_id(as.numeric(names(modules_list)[1]))
          } else {
            current_question_id(1)
          }
        }
      }
    })
    
    # Build a markermd_template from the current question modules, stamped with
    # the current format version.
    build_current_template = function() {
      questions_list = list()

      modules = question_modules()
      for (i in seq_along(modules)) {
        module_data = modules[[i]]

        if (!is.null(module_data) && !is.null(module_data$server) && is.list(module_data$server)) {
          if (!is.null(module_data$server$question) && is.function(module_data$server$question)) {
            question_obj = module_data$server$question()

            if (!is.null(question_obj)) {
              # Ensure question ID matches the module ID for uniqueness
              if (question_obj@id != module_data$id) {
                question_obj@id = as.integer(module_data$id)
              }

              questions_list[[length(questions_list) + 1]] = question_obj
            }
          }
        }
      }

      markermd_template(
        original_ast = ast(),
        questions = questions_list,
        files = template_files(),
        metadata = markermd_metadata(version = markermd_template_version())
      )
    }

    # Replace the editor's questions (and files) with those from an imported
    # template, renumbering ids so the question cards fully refresh.
    #
    # questions: List of markermd_question objects
    # files: The template's files; the files the questions cover are kept
    #   in any case
    load_questions_into_editor = function(questions, files = character(0)) {
      covered = stats::na.omit(vapply(questions, function(q) q@file, character(1)))
      template_files(union(files, covered))
      modules = list()
      start = last_question_id()
      for (k in seq_along(questions)) {
        q = questions[[k]]
        new_id = start + k
        q@id = as.integer(new_id)
        modules[[as.character(new_id)]] = list(
          id = new_id,
          module_id = paste0("question_", new_id),
          initial_question = q,
          server = NULL
        )
      }
      last_question_id(start + length(questions))
      question_modules(modules)
      if (length(questions) > 0) current_question_id(start + 1)
    }

    # Dirty tracking, save/status UI, project-database saves, YAML
    # export/import (see template-persistence.R). Reactives and functions are
    # passed as-is, never their values, so nothing captures a stale closure.
    persistence = template_persistence(
      input, output, session,
      project = project,
      source_path = source_path,
      question_modules = question_modules,
      build_current_template = build_current_template,
      load_questions_into_editor = load_questions_into_editor
    )

    # Export values for testing
    shiny::exportTestValues(
      n_questions = {
        length(question_modules())
      },
      question_names = {
        modules = question_modules()
        purrr::map_chr(modules, function(m) {
          if (!is.null(m$server) && !is.null(m$server$question)) {
            m$server$question()@name
          } else {
            NA_character_
          }
        })
      },
      n_rules_per_question = {
        modules = question_modules()
        purrr::map_int(modules, function(m) {
          if (!is.null(m$server) && !is.null(m$server$question)) {
            length(m$server$question()@rules)
          } else {
            0L
          }
        })
      },
      rules_per_question = {
        modules = question_modules()
        purrr::map(modules, function(m) {
          if (!is.null(m$server) && !is.null(m$server$question)) {
            lapply(m$server$question()@rules, function(r) {
              list(node_type = r@node_type, verb = r@verb, values = r@values)
            })
          } else {
            NULL
          }
        })
      },
      selected_nodes = {
        modules = question_modules()
        purrr::map(modules, function(m) {
          if (!is.null(m$server) && !is.null(m$server$question)) {
            m$server$question()@selected_nodes@node_ids
          } else {
            character(0)
          }
        })
      },
      filter_exprs_per_question = {
        modules = question_modules()
        purrr::map(modules, function(m) {
          if (!is.null(m$server) && !is.null(m$server$question)) {
            filters_expr_text(m$server$question()@filters)
          } else {
            NULL
          }
        })
      },
      template_dirty = {
        persistence$dirty()
      }
    )
  }

  return(list(ui = ui, server = server))
}

# Template app wrapped in navbar for standalone use
#
# ast: Reactive. The parsed AST object
# template_obj: markermd_template S7 object. Optional template to load on startup
# assignment_path: Character. Path to display in footer
# source_path: Character. Path to the assignment document, recorded in saved templates
# project: markermd_project S7 object. When set, the app saves into the project
#   and records the template in its config instead of offering a file download

template_app_standalone = function(ast, template_obj = NULL, assignment_path = NULL, source_path = NULL, project = NULL) {

  # Get the base template app components
  app_components = template_app(ast, template_obj, source_path = source_path, project = project)

  # A simple fillable page with a plain header bar: the app has a single view,
  # so a navbar whose only tab duplicates the page title earns nothing
  ui = bslib::page_fillable(
    title = "markermd - Template Creation",
    theme = bslib::bs_theme(version = 5),
    padding = 0,
    gap = 0,
    shiny::div(
      class = "bg-light border-bottom px-3 py-2 fs-5",
      "markermd - Template Creation"
    ),
    shiny::div(
      class = "flex-grow-1 overflow-hidden p-3",
      app_components$ui
    ),
    # Footer with assignment path (truncated rather than wrapped on narrow
    # windows)
    if (!is.null(assignment_path)) {
      shiny::div(
        class = "bg-light border-top text-center text-muted p-2 fs-6 text-truncate flex-shrink-0",
        shiny::strong("Assignment path:"), " ",
        shiny::code(assignment_path, class = "bg-light px-1 rounded small")
      )
    }
  )

  return(list(ui = ui, server = app_components$server))
}

# The names of the editor's questions, from the live servers where they
# exist, leaving out one module (the question being renamed)
#
# modules: The question-module registry (a list keyed by question id)
# except: The key to leave out, or NULL

question_names_except = function(modules, except = NULL) {
  modules = modules[setdiff(names(modules), except)]
  vapply(modules, function(m) {
    q = if (!is.null(m$server)) m$server$question() else m$initial_question
    q@name
  }, character(1), USE.NAMES = FALSE)
}

#' Launch the markermd Template Creation Application
#'
#' @param assignment_path Assignment source or existing template. Can be:
#'   - Character path to an initialized markermd project directory (one
#'     containing `.markermd/config.yml`; see [init_project()]). The template is
#'     authored against the project's key (solution) repository, any configured
#'     template is preloaded for editing, and saving writes the template into the
#'     project and records it in the config.
#'   - Character path to local directory containing assignment
#'   - Character GitHub repo in format "owner/repo"
#'   - Character path to a saved template (`.yaml`/`.yml`)
#'   - markermd_template S7 object
#' @param local_dir Character string. Local directory for cloning (required for remote GitHub repos, ignored for templates)
#' @param filename Character string. Glob pattern to match Rmd/qmd file to grade (ignored for templates). Default glob matches any .Rmd or .qmd file.
#' @param assignment Character string. Optional path to the assignment document,
#'   used when loading a template whose stored `source.path` cannot be located.
#' @param ... Additional arguments passed to shiny::shinyApp()
#'
#' @return A Shiny app object (from [shiny::shinyApp()]); printing it at an
#'   interactive console runs the template editor.
#' @export
#'
#' @examples
#' \dontrun{
#' # Author a template for an initialized project (uses the key repo)
#' template("/path/to/project")
#'
#' # Local assignment with default pattern
#' template("/path/to/assignment")
#'
#' # Local assignment with specific filename
#' template("/path/to/assignment", filename = "homework.Rmd")
#' 
#' # Remote GitHub repo
#' template("username/repo-name", local_dir = "/tmp/grading", filename = "assignment.qmd")
#' 
#' # Load existing template from file
#' template("/path/to/saved_template.yaml")
#'
#' # Load existing template from S7 object
#' my_template = read_template_yaml("/path/to/template.yaml")
#' template(my_template)
#' }
template = function(assignment_path, local_dir = NULL, filename = "*.[Rq]md", assignment = NULL, ...) {
  
  # Validate inputs
  if (missing(assignment_path)) {
    cli::cli_abort("assignment_path is required")
  }
  
  # Determine what type of input we have
  template_obj = NULL
  is_template_mode = FALSE
  project = NULL
  app = NULL

  if (S7::S7_inherits(assignment_path, markermd_template)) {
    # Input is an S7 template object
    template_obj = assignment_path
    is_template_mode = TRUE

  } else if (is_markermd_project(assignment_path)) {
    # Input is an initialized markermd project: author the template against the
    # project's key (solution) repo and preload any configured template.
    app = template_app_from_project(assignment_path, filename, assignment)

  } else if (is.character(assignment_path) && length(assignment_path) == 1) {
    # Input is a character string - could be assignment path or template file

    if (grepl("\\.ya?ml$", assignment_path, ignore.case = TRUE)) {
      # Input is a saved template YAML file
      if (!file.exists(assignment_path)) {
        cli::cli_abort("Template file does not exist: {assignment_path}")
      }
      template_obj = read_template_yaml(assignment_path, assignment = assignment, require_ast = TRUE)
      is_template_mode = TRUE

    } else {
      # Input is an assignment path (local directory or GitHub repo)
      is_template_mode = FALSE
    }

  } else {
    input_desc = if (is.character(assignment_path)) paste0("a length-", length(assignment_path), " character vector") else paste0("an object of class ", class(assignment_path)[1])
    cli::cli_abort("assignment_path must be a single character string (an assignment file, a directory containing one, a saved template .yaml, or a GitHub '<owner>/<repo>') or a markermd_template object, not {input_desc}.")
  }
  
  # Handle template mode vs assignment mode (project mode already built `app`)
  if (is.null(app) && is_template_mode) {
    assert_template_compatible(template_obj)

    # Template mode: use AST from template, ignore filename/local_dir
    ast = template_obj@original_ast

    # Source path resolved when loading a YAML template, preserved on re-save
    source_path = attr(template_obj, "markermd_source_path")

    # Create footer path - show original input for template objects, full path for files
    footer_path = if (S7::S7_inherits(assignment_path, markermd_template)) {
      "Template Object"
    } else {
      as.character(assignment_path)  # Show full path for template files
    }

    # Create app with template data
    app = template_app_standalone(shiny::reactiveVal(ast), template_obj, footer_path, source_path = source_path)

  } else if (is.null(app)) {
    # Assignment mode: a single assignment file, a directory containing one, or
    # a GitHub "<owner>/<repo>" to clone. A name with a document or template
    # extension is a mistyped local path, never a repository, even though it
    # matches the owner/repo shape.
    looks_like_file = tolower(fs::path_ext(assignment_path)) %in% c("qmd", "rmd", "yaml", "yml")
    is_github_repo = grepl("^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$", assignment_path) &&
      !file.exists(assignment_path) && !looks_like_file

    if (is_github_repo) {
      if (is.null(local_dir)) {
        cli::cli_abort("local_dir is required when cloning the GitHub repository '{assignment_path}'.")
      }
      repo_path = setup_assignment_repo(assignment_path, local_dir, is_github_repo = TRUE)
      file_path = resolve_assignment_file(repo_path, filename)

    } else if (dir.exists(assignment_path)) {
      file_path = resolve_assignment_file(assignment_path, filename)

    } else if (file.exists(assignment_path)) {
      if (!tolower(fs::path_ext(assignment_path)) %in% c("qmd", "rmd")) {
        cli::cli_abort("Assignment file must be a .qmd or .Rmd document: {assignment_path}")
      }
      file_path = normalizePath(assignment_path)

    } else {
      cli::cli_abort("assignment_path not found locally: '{assignment_path}'.\nPass a path to an assignment file (.qmd/.Rmd), a directory containing one, a saved template (.yaml), a markermd project directory, or a GitHub repository as '<owner>/<repo>'.")
    }

    ast = parse_assignment_document(file_path)
    app = template_app_standalone(shiny::reactiveVal(ast), NULL, assignment_path, source_path = file_path)
  }

  shiny::shinyApp(ui=app$ui, server=app$server, ...)
}

# Build the standalone template app for an initialized markermd project.
#
# Authors the template against the project's key (solution) repository and
# preloads the template stored in the project database (if any) for editing. The
# app saves directly back into the project database (see template_app()).
#
# path: project root directory
# filename: glob used to locate the assignment document within the key repo
# assignment: optional assignment-document override (unused for the DB preload,
#   which resolves the source against the key repo)

template_app_from_project = function(path, filename, assignment) {
  project = project_config(path)
  root = project@root

  if (is.na(project@key)) {
    cli::cli_abort(c(
      "No key (solution) repository is configured for this project.",
      "i" = "Record one with {.code markermd::project_set(\"{root}\", key = \"<key-dir>\")}."
    ))
  }
  key_dir = fs::path(root, project@key)
  if (!fs::dir_exists(key_dir)) {
    cli::cli_abort("Configured key repository does not exist: {.path {key_dir}}")
  }
  key_doc = resolve_assignment_file(key_dir, filename)

  template_obj = load_template_from_db(root, base_dir = root, assignment = key_doc, require_ast = TRUE)
  if (!is.null(template_obj)) {
    assert_template_compatible(template_obj)
  }

  if (!is.null(template_obj)) {
    ast = template_obj@original_ast
    source_path = attr(template_obj, "markermd_source_path")
    if (is.null(source_path)) source_path = key_doc
  } else {
    ast = parse_assignment_document(key_doc)
    source_path = key_doc
  }

  template_app_standalone(
    shiny::reactiveVal(ast), template_obj, key_doc,
    source_path = source_path, project = project
  )
}