# AST Module
#
# A single Shiny module for displaying an AST tree. The rendering itself lives in
# render_ast_tree() (utils_tree.R) and is shared with the read-only tree on the
# mark side; this module adds the reactivity: the preview-modal observers and,
# in interactive mode, the node-selection click handling.

# Open the read-only Monaco preview modal for a single q2r node
#
# node: A q2r pandoc node

show_node_preview = function(node) {
  content = node_to_qmd(node) |>
    as.character() |>
    paste(collapse = "\n")

  language = monaco_language_for_node(node)
  if (!identical(language, "markdown")) {
    # Engine-language previews: drop the surrounding fence lines so the
    # engine grammar is not applied to the ``` markers themselves
    lines = strsplit(content, "\n", fixed = TRUE)[[1]]
    fence = grepl("^\\s*(`{3,}|~{3,})", lines)
    if (length(lines) >= 2 && fence[1] && fence[length(lines)]) {
      content = paste(lines[-c(1, length(lines))], collapse = "\n")
    }
  }

  # One fixed editor id: only one preview modal is open at a time, and
  # the shared init disposes the previous editor before creating anew
  editor_id = "markermd-preview-editor"

  shiny::showModal(
    shiny::modalDialog(
      # Echo the tree row that was clicked rather than an internal class
      # name, so the modal speaks the same vocabulary as the tree
      title = shiny::span(q2r_node_label(node), class = "fs-6 fw-bold"),
      size = "l",
      easyClose = TRUE,
      footer = shiny::modalButton("Close"),
      shiny::div(
        style = "height: 400px;",
        shiny::div(
          id = editor_id,
          class = "h-100 w-100 border rounded"
        )
      )
    )
  )

  render_monaco_editor(editor_id, content, language)
}

# Wire preview-modal observers for a set of nodes
#
# Binds an observer to each node's preview button (preview_<id_prefix>_<index>,
# or preview_<index> when id_prefix is NULL) that opens a read-only Monaco
# editor showing the node rendered back to qmd. Shared by the template app and
# the mark validation display so the preview behaviour is defined once.
#
# When node_at is supplied the clicked node is resolved lazily at click time
# (used by the mark validation display, where the same button index must show
# the currently selected repo's node rather than a node captured at wiring
# time); otherwise the static nodes list is indexed.
#
# input: The module's input object
# nodes: List of q2r nodes in tree order (index i is node i), or NULL when
#   node_at is supplied
# id_prefix: Optional prefix matching node_preview_button()'s button ids
# node_at: Optional function(index) returning the current node for that index
# indices: Button indices to wire (defaults to seq_along(nodes))

ast_preview_observers = function(input, nodes, id_prefix = NULL, node_at = NULL, indices = NULL) {
  resolve = if (!is.null(node_at)) node_at else function(k) nodes[[k]]
  wire = if (!is.null(indices)) indices else seq_along(nodes)

  for (i in wire) {
    local({
      node_index = i
      button_id = if (!is.null(id_prefix)) {
        paste0("preview_", id_prefix, "_", node_index)
      } else {
        paste0("preview_", node_index)
      }

      shiny::observe({
        node = resolve(node_index)
        if (!is.null(node)) {
          show_node_preview(node)
        }
      }) |>
        shiny::bindEvent(input[[button_id]], ignoreInit = TRUE)
    })
  }
}

# AST module UI
#
# id: Character. Module namespace ID
# title: Character. Panel title (default: "Document Structure")
# header_extra: Tag. Optional content shown on the right of the card header
#   (the template app shows the active-question badge there)
# show_clear_button: Logical. Whether to show the clear-selections footer
#   button (the template app now clears from the question card instead)

ast_module_ui = function(id, title = "Document Structure", header_extra = NULL, show_clear_button = FALSE) {
  ns = shiny::NS(id)

  bslib::card(
    class = "h-100",
    bslib::card_header(
      class = "bg-light d-flex justify-content-between align-items-center",
      title,
      header_extra
    ),
    bslib::card_body(
      class = "flex-fill overflow-auto p-0",
      shiny::div(
        id = ns("ast_tree_container"),
        class = "bg-light p-3",
        shiny::uiOutput(ns("ast_tree_ui"))
      )
    ),
    if (show_clear_button) {
      bslib::card_footer(
        class = "text-center",
        shiny::actionButton(ns("clear_selections"), "Clear selected nodes", class = "btn-secondary btn-sm")
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
