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

  # UI. A tagList (not a wrapper div) so the layout_columns is a direct fill
  # item of the enclosing fillable page and sizes itself to the remaining
  # viewport instead of a hard-coded vh formula.
  ui = shiny::tagList(
    # Initialize shinyjs
    shinyjs::useShinyjs(),

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

      /* Modal styling */
      .modal-header { padding: 8px 15px !important; }
      .modal-title { margin: 0 !important; padding: 0 !important; line-height: 1.2 !important; }
      
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
      ast_module_ui(
        "ast_panel",
        header_extra = shiny::uiOutput("active_question_badge", inline = TRUE)
      ),
      bslib::card(
        class = "h-100",
        bslib::card_header("Questions", class = "bg-light"),
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
          shiny::uiOutput("save_button_ui"),
          # Hidden file input the Import button triggers directly (see
          # save_button_ui). Bound once here so the upload survives re-renders.
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
    
    # Add a new question to the modules list
    #
    add_new_question = function() {
      next_id = last_question_id() + 1
      last_question_id(next_id)

      # Create initial question object
      initial_question = markermd_question(
        id = as.integer(next_id),
        name = paste("Question", next_id),
        selected_nodes = markermd_node_selection(),
        rules = list()
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
              shiny::icon("arrow-left", class = "me-2"),
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
            icon = shiny::icon("plus"),
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
          initial_question = modules_list[[q_id_str]]$initial_question
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
              question_ui(question_module$module_id, q_id, name = question_module$initial_question@name)
            )
          )
        )
      }

      if (!identical(already, module_ids)) {
        inserted_questions(module_ids)
      }
    })
    
    # Add new question handler
    shiny::observe({
      add_new_question()
    }) |>
      shiny::bindEvent(input$add_question)

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
        metadata = markermd_metadata(version = markermd_template_version())
      )
    }

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
        # Project sessions: save to the project database, plus YAML
        # export/import. Import is available even with no questions; saving and
        # exporting an empty template is not.
        shiny::div(
          class = "d-flex gap-2 justify-content-center",
          shiny::actionButton(
            "save_to_project",
            "Save to Project",
            class = paste(save_class, "btn-sm"),
            disabled = !has_questions
          ),
          # Import/Export tucked into a popover off an exchange-arrows icon
          bslib::popover(
            shiny::tags$button(
              shiny::icon("right-left"),
              id = "io_menu",
              type = "button",
              class = "btn btn-outline-secondary btn-sm",
              title = "Import / Export"
            ),
            shiny::div(
              class = "d-grid gap-2",
              shiny::downloadButton(
                "export_template",
                "Export",
                class = paste(c("btn-outline-secondary btn-sm", if (!has_questions) "disabled"), collapse = " ")
              ),
              shiny::tags$button(
                "Import",
                type = "button",
                class = "btn btn-outline-secondary btn-sm",
                onclick = "document.getElementById('import_file').click();"
              )
            ),
            title = "Template YAML"
          )
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
        saved_template_snapshot(current_template_serialized())
      },
      contentType = "text/yaml"
    )

    # Save into the project database, the canonical template store (project
    # sessions). The assignment source is recorded root-relative so it resolves
    # on reload; YAML remains available via the Export button.
    shiny::observe({
      src_rel = if (!is.null(source_path)) as.character(fs::path_rel(source_path, project@root)) else NULL
      save_template_to_db(project@root, build_current_template(), source_path = src_rel)
      saved_template_snapshot(current_template_serialized())

      shiny::showNotification(
        "Saved template to the project database.",
        type = "message"
      )
    }) |> shiny::bindEvent(input$save_to_project)

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

    # Replace the editor's questions with those from an imported template,
    # renumbering ids so the question cards fully refresh.
    load_questions_into_editor = function(questions) {
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
#' @return Launches Shiny application for template creation
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
    stop("assignment_path is required")
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
        stop("Template file does not exist: ", assignment_path, call. = FALSE)
      }
      template_obj = read_template_yaml(assignment_path, assignment = assignment, require_ast = TRUE)
      is_template_mode = TRUE

    } else {
      # Input is an assignment path (local directory or GitHub repo)
      is_template_mode = FALSE
    }

  } else {
    stop(
      "assignment_path must be a single character string (an assignment file, a ",
      "directory containing one, a saved template .yaml, or a GitHub '<owner>/<repo>') ",
      "or a markermd_template object, not ",
      if (is.character(assignment_path)) paste0("a length-", length(assignment_path), " character vector") else paste0("an object of class ", class(assignment_path)[1]),
      ".",
      call. = FALSE
    )
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
    # a GitHub "<owner>/<repo>" to clone.
    is_github_repo = grepl("^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$", assignment_path) &&
      !file.exists(assignment_path)

    if (is_github_repo) {
      if (is.null(local_dir)) {
        stop("local_dir is required when cloning the GitHub repository '", assignment_path, "'.", call. = FALSE)
      }
      repo_path = setup_assignment_repo(assignment_path, local_dir, is_github_repo = TRUE)
      file_path = resolve_assignment_file(repo_path, filename)

    } else if (dir.exists(assignment_path)) {
      file_path = resolve_assignment_file(assignment_path, filename)

    } else if (file.exists(assignment_path)) {
      if (!tolower(tools::file_ext(assignment_path)) %in% c("qmd", "rmd")) {
        stop("Assignment file must be a .qmd or .Rmd document: ", assignment_path, call. = FALSE)
      }
      file_path = normalizePath(assignment_path)

    } else {
      stop(
        "assignment_path not found: '", assignment_path, "'.\n",
        "Pass a path to an assignment file (.qmd/.Rmd), a directory containing one, ",
        "a saved template (.yaml), a markermd project directory, or a GitHub repository as '<owner>/<repo>'.",
        call. = FALSE
      )
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