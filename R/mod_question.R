# Question Module
#
# Shiny module for managing a single question with its rules

# Question UI
#
# id: Character. Module namespace ID
# name_id: Integer. The question ID used to build the fallback default name
# name: Character. The question's name to show; defaults to "Question <name_id>"
#   so a freshly added question keeps its positional default, while a loaded or
#   imported question shows its own saved name
# file_label: The name of the file the question covers, shown beside its
#   name (the Quarto document's name for a question on the document)
# file: The question's file, or NA for the Quarto document. A question on
#   another file covers the whole file and has no body

question_ui = function(id, name_id, name = paste("Question", name_id), file_label, file = NA_character_) {
  ns = shiny::NS(id)

  bslib::card(
    style = "margin: 0; width: 100%; max-width: 100%; box-sizing: border-box;",
    # The bslib card-header is itself a horizontal flex row, so these controls are
    # added directly as its flex children. space-between spreads the title and
    # delete button evenly, keeping the delete button hard-right; the title has
    # a modest, shrinkable width so it does not stretch across the whole header.
    bslib::card_header(
      class = "bg-light",
      style = "gap: 0.75rem; justify-content: space-between;",

      # Question name input, and beside it the file the question covers
      shiny::div(
        class = "d-flex align-items-center gap-3",
        style = "flex: 1 1 auto; min-width: 0;",
        shiny::div(
          style = "flex: 0 1 14rem; min-width: 0;",
          shiny::textInput(
            ns("question_name"),
            NULL,
            value = name,
            width = "100%",
            updateOn = "blur"
          )
        ),
        # The file the question covers, fixed when it is added
        shiny::div(
          class = "question-file-label d-flex align-items-center gap-1 text-muted",
          style = "min-width: 0;",
          fa_icon("file-lines", class = "fs-5", title = "File the question covers"),
          shiny::span(class = "text-truncate", file_label)
        )
      ),

      # Reorder and delete controls (right aligned)
      shiny::div(
        style = "flex: 0 0 auto;",
        class = "d-flex align-items-center gap-1",
        shiny::actionButton(
          ns("move_question_up"),
          fa_icon("chevron-up"),
          class = "btn btn-sm p-1 border-0 text-secondary",
          style = "line-height: 1;",
          title = "Move question up"
        ),
        shiny::actionButton(
          ns("move_question_down"),
          fa_icon("chevron-down"),
          class = "btn btn-sm p-1 border-0 text-secondary",
          style = "line-height: 1;",
          title = "Move question down"
        ),
        shiny::actionButton(
          ns("delete_question"),
          fa_icon("times"),
          class = "btn-danger btn-sm",
          style = "font-size: 12px; padding: 2px 6px; border-radius: 20%; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; line-height: 1;",
          title = "Delete Question"
        )
      )
    ),
    
    # One-line summary shown while the card is collapsed (inactive); the
    # question-summary / question-body visibility swap is CSS-driven off the
    # wrapper's question-active class (see template-app.R)
    bslib::card_body(
      class = "question-summary py-1 small text-muted",
      shiny::uiOutput(ns("summary_line"))
    ),

    # Question content (full body, shown when the card is active)
    bslib::card_body(
      class = "question-body",
      style = "padding: 0;",

      # A question on another file covers the whole file: it has no nodes,
      # filters or rules, so its body stays empty (the padding lives on this
      # div so an empty body takes no space)
      if (is.na(file)) shiny::div(
        style = "padding: 12px;",

      # Selected nodes display with an adjacent clear control
      shiny::div(
        style = "margin-bottom: 2px;",
        class = "d-flex align-items-baseline justify-content-between gap-2",
        shiny::div(
          shiny::strong("Selected nodes: "),
          shiny::uiOutput(ns("selected_nodes_display"), inline = TRUE)
        ),
        shiny::uiOutput(ns("clear_nodes_button"), inline = TRUE)
      ),

      # Filters section
      shiny::div(
        # Filters header with matching-semantics help and add button
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-top: -2px; margin-bottom: 8px;",
          shiny::div(
            class = "d-flex align-items-center gap-1",
            shiny::strong("Filters: "),
            shiny::uiOutput(ns("filters_status"), inline = TRUE),
            bslib::popover(
              fa_icon("circle-question", class = "text-muted"),
              title = "Filter matching",
              shiny::tags$ul(
                class = "small mb-0 ps-3",
                shiny::tags$li(shiny::strong("node type"), ": matches any of the selected kinds"),
                shiny::tags$li(shiny::strong("has class / has id / has engine"), ": exact match"),
                shiny::tags$li(shiny::strong("has text"), ": regular expression"),
                shiny::tags$li(shiny::strong("has label"), ": glob pattern (e.g. fig-*)"),
                shiny::tags$li(shiny::strong("has option"), ": \"eval\" tests key presence, \"eval: false\" tests its value")
              )
            )
          ),
          shiny::actionButton(
            ns("add_filter_group"),
            "Add Filter",
            icon = fa_icon("plus"),
            class = "btn-outline-primary btn-sm",
            style = "font-size: 11px; padding: 2px 8px;"
          )
        ),

        # Filter groups container
        shiny::div(
          id = ns("filters_container"),
          style = "margin-bottom: 0;",
          shiny::uiOutput(ns("filters_ui"))
        ),

        # Live preview of the constructed q2r filter expression, plus
        # warnings for values that would silently misfire
        shiny::uiOutput(ns("filter_preview")),
        shiny::uiOutput(ns("filter_warnings"))
      ),

      # Rules section
      shiny::div(
        # Rules header with add button
        shiny::div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-top: -2px; margin-bottom: 8px;",
          shiny::div(
            shiny::strong("Validation rules: "),
            shiny::uiOutput(ns("rules_status"))
          ),
          shiny::actionButton(
            ns("add_rule"),
            "Add Rule",
            icon = fa_icon("plus"),
            class = "btn-outline-primary btn-sm",
            style = "font-size: 11px; padding: 2px 8px;"
          )
        ),

        # Rules container
        shiny::div(
          id = ns("rules_container"),
          style = "position: relative; z-index: 1; margin-bottom: 0;",
          shiny::uiOutput(ns("rules_ui"))
        ),

        # Warnings for rule values the inputs hold but the rule cannot take
        # (an invalid count or range), which keep the stored values
        shiny::uiOutput(ns("rule_warnings"))
      )
      )
    )
  )
}

# Choices for the Add entry modal's File select: the Quarto document (its
# value is its own name, so it shows like any other choice) and then the
# template's other files
#
# doc_label: The Quarto document's name
# files: The template's files

question_file_choices = function(doc_label, files) {
  c(stats::setNames(doc_label, doc_label), stats::setNames(files, files))
}

# Tell the user a question on a file has no document nodes

notify_file_question = function(file) {
  shiny::showNotification(
    paste0("This question covers ", file, " as a whole, so document nodes cannot be selected for it."),
    type = "message"
  )
}

# Tell the user a document entry's nodes cannot be changed: it covers the
# entire document

notify_document_entry = function() {
  shiny::showNotification(
    "This entry covers the entire document, so headings cannot be selected for it.",
    type = "message"
  )
}

# Question Server
#
# id: Character. Module namespace ID
# ast: Reactive. The parsed AST object for building tree structure
# initial_question: S7 markermd_question object. Optional initial question state
# name_available: Function(name) telling whether no other question has the
#   name; a rename to a taken (or blank) name is refused and the input put
#   back, since the template's questions must be uniquely named

question_server = function(id, ast, initial_question = NULL, name_available = function(name) TRUE) {
  shiny::moduleServer(id, function(input, output, session) {

    # Question state - use initial_question if provided, otherwise default
    state = shiny::reactiveVal({
      if (!is.null(initial_question)) {
        initial_question
      } else {
        markermd_question(1L, "default", markermd_node_selection(), list())
      }
    })
    
    # Memoised flattened tree. ast() is write-once, so this is built once and
    # reused instead of re-flattening the whole document on every selection change.
    question_tree_items = shiny::reactive({
      if (is.null(ast())) return(list())
      build_ast_tree_structure(ast())
    })

    # One-line summary shown while the card is collapsed
    output$summary_line = shiny::renderUI({
      q = state()
      n_nodes = length(q@selected_nodes@node_ids)
      n_filters = length(q@filters)
      n_rules = length(q@rules)
      # An entry covering a whole file (the Quarto document's entire-document
      # entry, or any other file) has no nodes to count
      nodes_text = if (is_file_question(q) || is_document_question(q)) {
        "Entire document \u00b7 "
      } else {
        glue::glue("{n_nodes} node{if (n_nodes == 1) '' else 's'} selected \u00b7 ")
      }
      shiny::span(glue::glue(
        "{nodes_text}",
        "{n_filters} filter{if (n_filters == 1) '' else 's'} \u00b7 ",
        "{n_rules} rule{if (n_rules == 1) '' else 's'}"
      ))
    })

    # Clear control next to the selected-nodes display, shown only when there
    # is a selection to clear
    output$clear_nodes_button = shiny::renderUI({
      if (length(state()@selected_nodes@node_ids) == 0) {
        return(NULL)
      }
      shiny::actionButton(
        session$ns("clear_nodes_btn"),
        "Clear",
        class = "btn-link btn-sm p-0 text-decoration-none",
        title = "Clear this question's selected nodes"
      )
    })

    shiny::observe({
      cur_state = state()
      cur_state@selected_nodes = markermd_node_selection(node_ids = character(0))
      state(cur_state)
    }) |>
      shiny::bindEvent(input$clear_nodes_btn)

    # Render selected nodes display: each node-id selector followed by the
    # number of nodes it covers (the selected heading/div and its descendants).
    output$selected_nodes_display = shiny::renderUI({
      ids = state()@selected_nodes@node_ids
      if (is_document_question(state())) {
        shiny::span("Entire document", class = "text-success")
      } else if (length(ids) == 0) {
        shiny::span("None", class = "text-muted")
      } else {
        tree_items = question_tree_items()
        labels = vapply(ids, function(id) {
          n = length(compute_all_selected_nodes(tree_items, node_ids_to_indices(ast(), id)))
          paste0("#", id, " (", n, " node", if (n != 1) "s" else "", ")")
        }, character(1))

        shiny::span(paste(labels, collapse = ", "), class = "text-success")
      }
    })
        
    # The name is committed on blur; a blank or taken name is refused
    # rather than stored (the template class would reject it when built)
    shiny::observe({
      shiny::req(!is.null(input$question_name))
      cur_state = state()
      name = trimws(input$question_name)
      if (identical(name, cur_state@name)) {
        return()
      }
      if (!nzchar(name)) {
        shiny::showNotification("A question needs a name.", type = "warning")
        shiny::updateTextInput(session, "question_name", value = cur_state@name)
        return()
      }
      if (!name_available(name)) {
        shiny::showNotification(paste0("There is already a question named \"", name, "\"."), type = "warning")
        shiny::updateTextInput(session, "question_name", value = cur_state@name)
        return()
      }
      cur_state@name = name
      state(cur_state)
    }) |>
      shiny::bindEvent(input$question_name)

    # Confirm before deleting a question so a stray click does not lose its rules.
    # The actual removal is driven off the confirm button via delete_clicked().
    shiny::observe({
      shiny::showModal(shiny::modalDialog(
        title = "Delete question?",
        glue::glue("Delete \"{state()@name}\" and its validation rules? This cannot be undone."),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_delete_question"), "Delete", class = "btn-danger")
        )
      ))
    }) |>
      shiny::bindEvent(input$delete_question)

    shiny::observe({
      shiny::removeModal()
    }) |>
      shiny::bindEvent(input$confirm_delete_question)
    
    # The rules and filters row machinery (structural twins; see
    # mod_question_rules.R / mod_question_filters.R). Each owns its keyed-list
    # bookkeeping internally and writes into the shared question state. Rules
    # first, so observer creation order (and same-priority flush order)
    # matches the pre-split layout.
    question_rules_server(input, output, session, state, ast)
    question_filters_server(input, output, session, state, ast)

    # Return reactive question data and methods
    return(list(
      # Reactive data
      question = shiny::reactive({
        state()
      }),
      
      # Node management methods. The public interface stays index-based (the tree
      # interaction works in index space), but selections are stored as the
      # nodes' q2r ids (heading or div ids), converted at this boundary.
      add_node = function(node_index) {
        # A document-scope question covers the entire document and never
        # selects nodes, nor does a question on another file
        if (is_document_question(state())) {
          notify_document_entry()
          return(invisible(NULL))
        }
        if (is_file_question(state())) {
          notify_file_question(state()@file)
          return(invisible(NULL))
        }
        id = node_id_for_index(ast(), node_index)
        if (nchar(id) == 0) {
          return(invisible(NULL))
        }
        if (id %in% duplicate_node_ids(ast())) {
          shiny::showNotification(
            paste0("The id #", id, " is shared by several headings or divs, so it cannot be selected. Give each a unique {#id} in the document."),
            type = "warning"
          )
          return(invisible(NULL))
        }
        cur_state = state()
        cur_ids = cur_state@selected_nodes@node_ids
        if (!id %in% cur_ids) {
          cur_state@selected_nodes = markermd_node_selection(node_ids = c(cur_ids, id))
          state(cur_state)
        }
      },

      remove_node = function(node_index) {
        if (is_document_question(state())) {
          notify_document_entry()
          return(invisible(NULL))
        }
        if (is_file_question(state())) {
          notify_file_question(state()@file)
          return(invisible(NULL))
        }
        id = node_id_for_index(ast(), node_index)
        cur_state = state()
        cur_state@selected_nodes = markermd_node_selection(
          node_ids = setdiff(cur_state@selected_nodes@node_ids, id)
        )
        state(cur_state)
      },

      # A document-scope question reports the tree's document root (index 0),
      # so the tree highlights every node beneath it
      get_selected_nodes = shiny::reactive({
        if (is_document_question(state())) {
          return(0L)
        }
        if (is_file_question(state())) {
          return(integer(0))
        }
        node_ids_to_indices(ast(), state()@selected_nodes@node_ids)
      }),

      delete_clicked = shiny::reactive({
        input$confirm_delete_question
      }),

      move_up_clicked = shiny::reactive({
        input$move_question_up
      }),

      move_down_clicked = shiny::reactive({
        input$move_question_down
      })
    ))
    })
}
