# AST Module
#
# A single Shiny module for displaying an AST tree. The rendering itself lives in
# render_ast_tree() (utils_tree.R) and is shared with the read-only tree on the
# mark side; this module adds the reactivity: the preview-modal observers and,
# in interactive mode, the node-selection click handling.

# Wire preview-modal observers for a set of nodes
#
# Binds an observer to each node's preview button (preview_<id_prefix>_<index>,
# or preview_<index> when id_prefix is NULL) that opens a read-only Monaco
# editor showing the node rendered back to qmd. Shared by the template app and
# the mark validation display so the preview behaviour is defined once.
#
# input: The module's input object
# nodes: List of q2r nodes in tree order (index i is node i)
# id_prefix: Optional prefix matching node_preview_button()'s button ids

ast_preview_observers = function(input, nodes, id_prefix = NULL) {
  for (i in seq_along(nodes)) {
    local({
      node_index = i
      button_id = if (!is.null(id_prefix)) {
        paste0("preview_", id_prefix, "_", node_index)
      } else {
        paste0("preview_", node_index)
      }

      shiny::observe({
        node = nodes[[node_index]]

        content = node_to_qmd(node) |>
          as.character() |>
          paste(collapse = "\n")

        node_type = sub("^q2r::", "", class(node)[1])
        editor_id = paste0("monaco-editor-", if (is.null(id_prefix)) "ast" else id_prefix, "-", node_index)

        shiny::showModal(
          shiny::modalDialog(
            title = shiny::span(node_type, style = "font-size: 16px; font-weight: bold;"),
            size = "l",
            easyClose = TRUE,
            footer = NULL,
            shiny::div(
              style = "height: 400px;",
              shiny::div(
                id = editor_id,
                style = "height: 100%; width: 100%; border: 1px solid #e1e5e9;"
              )
            )
          )
        )

        render_monaco_editor(editor_id, content, monaco_language_for_node(node))
      }) |>
        shiny::bindEvent(input[[button_id]], ignoreInit = TRUE)
    })
  }
}

# AST module UI
#
# id: Character. Module namespace ID
# title: Character. Panel title (default: "Document Structure")
# show_clear_button: Logical. Whether to show the clear-selections button

ast_module_ui = function(id, title = "Document Structure", show_clear_button = FALSE) {
  ns = shiny::NS(id)

  bslib::card(
    class = "h-100",
    bslib::card_header(title, class = "bg-light"),
    bslib::card_body(
      class = "flex-fill overflow-auto p-0",
      shiny::div(
        id = ns("ast_tree_container"),
        class = "bg-light p-3 h-100 overflow-auto",
        shiny::uiOutput(ns("ast_tree_ui"))
      )
    ),
    if (show_clear_button) {
      bslib::card_footer(
        class = "text-center",
        shiny::actionButton(ns("clear_selections"), "Clear Question", class = "btn-secondary btn-sm")
      )
    }
  )
}

# AST module server
#
# id: Character. Module namespace ID
# ast: Reactive. The parsed AST object
# selected_nodes: Reactive. Currently selected node indices (optional)
# filtered_nodes: Reactive. Node indices excluded by the current question's
#   filters, drawn in red (optional; see question_filtered_indices())
# interactive: Logical. TRUE for a selectable tree, FALSE for read-only
# enable_preview: Logical. Whether to wire the preview-modal observers

ast_module_server = function(id, ast, selected_nodes = shiny::reactive(integer(0)), filtered_nodes = shiny::reactive(integer(0)), interactive = TRUE, enable_preview = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {

    # Flattened AST nodes in tree order (index i aligns with the tree's node i)
    ast_nodes = shiny::reactive({
      if (is.null(ast())) return(NULL)
      lapply(q2r_flatten(ast()), function(record) record$node)
    })

    # Memoised flattened tree. ast() is write-once, so the structure is built once
    # and reused across re-renders (which fire on every selection change) instead
    # of re-flattening the whole document each time.
    ast_tree_items = shiny::reactive({
      if (is.null(ast())) return(list())
      build_ast_tree_structure(ast())
    })

    output$ast_tree_ui = shiny::renderUI({
      if (is.null(ast())) {
        return(shiny::p("No document loaded"))
      }

      tree_items = ast_tree_items()
      if (length(tree_items) == 0) {
        return(shiny::p("No document structure available"))
      }

      render_ast_tree(
        tree_items,
        session$ns,
        ast_render_opts(
          mode = if (interactive) "interactive" else "readonly",
          selected = selected_nodes(),
          filtered = filtered_nodes()
        )
      )
    })

    # Click events from the selectable tree (interactive mode only)
    node_clicked = shiny::reactiveVal(NULL)

    shiny::observe({
      shiny::req(ast_nodes())
      nodes = ast_nodes()

      if (enable_preview) {
        ast_preview_observers(input, nodes)
      }

      if (interactive) {
        for (i in seq_along(nodes)) {
          local({
            node_index = i
            if (!node_is_selectable(nodes[[node_index]])) return()

            register_toggle = function(input_id) {
              shiny::observe({
                node_clicked(list(
                  node_index = node_index,
                  action = "toggle",
                  timestamp = Sys.time()
                ))
              }) |>
                shiny::bindEvent(input[[input_id]])
            }

            # The text label and its circle marker both toggle the node.
            register_toggle(paste0("select_", node_index))
            register_toggle(paste0("select_children_", node_index))
          })
        }
      }
    })

    result = list(
      ast_nodes = ast_nodes,
      clear_clicked = shiny::reactive({
        input$clear_selections
      })
    )

    if (interactive) {
      result$node_clicked = shiny::reactive({
        node_clicked()
      })
    }

    return(result)
  })
}
