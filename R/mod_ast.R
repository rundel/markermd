# AST Module
#
# The template editor's selectable document tree. The rendering itself lives in
# render_ast_tree() (utils_tree.R) and is shared with the read-only trees on the
# mark side. This module renders the tree once per document, patches its
# highlight in place when the active question's selection or filters change
# (the markermd_tree_state message, see ast_tree_script()), and takes every
# heading toggle and preview click through one delegated input, tree_event, so
# nothing is bound per node.

# Open the read-only Monaco preview modal for a single q2r node
#
# node: A q2r pandoc node
# back_id: Optional full input id of a Back button in the footer, for a
#   preview opened from another modal (which the preview replaces)

show_node_preview = function(node, back_id = NULL) {
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
      footer = htmltools::tagList(
        if (!is.null(back_id)) shiny::actionButton(back_id, "Back", icon = fa_icon("arrow-left"), class = "btn-outline-secondary"),
        shiny::modalButton("Close")
      ),
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
# back_id: Optional full input id of a Back button for the preview modal (see
#   show_node_preview())

ast_preview_observers = function(input, nodes, id_prefix = NULL, node_at = NULL, indices = NULL, back_id = NULL) {
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
          show_node_preview(node, back_id = back_id)
        }
      }) |>
        shiny::bindEvent(input[[button_id]], ignoreInit = TRUE)
    })
  }
}

# A reactive over source() that only invalidates its dependents when the value
# actually changes. Shiny reactives re-run their dependents on every
# invalidation, so a value derived from the question state (its selection, its
# filtered nodes) would otherwise redraw the tree on each rule edit or rename.
#
# source: A reactive expression
# track: Optional function given the observer, so a caller that tears its
#   observers down (question_server()'s tracker) can register it

deduplicated_reactive = function(source, track = invisible) {
  value = shiny::reactiveVal(NULL)
  observer = shiny::observe({
    new_value = source()
    if (!identical(new_value, shiny::isolate(value()))) {
      value(new_value)
    }
  })
  track(observer)
  shiny::reactive(value())
}

# The page-head script behind the interactive tree: one delegated click handler
# reporting toggles and previews to the tree's input ({index, action, nonce};
# the input id is the data-tree-input attribute of the tree's root list), and
# the markermd_tree_state handler that patches every node's highlight from
# tree_selection_state()'s vectors. The patch builds each class string in the
# order tree_node_view() does, so a patched node matches a rendered one. The
# last state per tree is kept and re-applied when the tree output renders,
# since a message can arrive before the tree's markup does.

ast_tree_script = function() {
  icons = jsonlite::toJSON(
    list(check = as.character(tree_node_icon("check")), ellipsis = as.character(tree_node_icon("ellipsis"))),
    auto_unbox = TRUE
  )
  shiny::tags$head(shiny::tags$script(shiny::HTML(glue::glue(
    "
    (function() {
      var icons = <<icons>>;
      var lastState = {};
      var nonce = 0;

      function highlight(selected, filtered) {
        return selected ? (filtered ? ' selected filtered' : ' selected') : '';
      }

      function applyState(container, state) {
        var selected = new Set(state.selected);
        var direct = new Set(state.direct);
        var filtered = new Set(state.filtered);
        var locked = new Set(state.locked);
        container.querySelectorAll('[data-tree-index]').forEach(function(node) {
          var index = parseInt(node.getAttribute('data-tree-index'), 10);
          var selectable = node.getAttribute('data-tree-selectable') === '1';
          var isSelected = selected.has(index);
          var isFiltered = isSelected && filtered.has(index);
          var open = selectable && !locked.has(index);
          var hl = highlight(isSelected, isFiltered);
          var icon = direct.has(index) ? 'check' : (isSelected ? 'ellipsis' : '');
          var title = open ? 'Toggle this node and its children' :
            (isFiltered ? 'Excluded by question filters' :
              (isSelected ? 'Selected via parent selection' : 'Non-selectable node'));

          node.className = 'tree-node-content' + (selectable && !open ? ' tree-locked' : '');
          var marker = node.querySelector('.tree-toggle-btn');
          if (marker) {
            marker.className = marker.getAttribute('data-tree-base') + (open ? '' : ' tree-marker-square') + hl;
            marker.title = title;
            if (marker.getAttribute('data-tree-icon') !== icon) {
              marker.innerHTML = icon ? icons[icon] : '';
              marker.setAttribute('data-tree-icon', icon);
            }
          }
          node.querySelectorAll('.tree-node-info [data-tree-base]').forEach(function(label) {
            label.className = label.getAttribute('data-tree-base') + hl;
          });
        });
      }

      Shiny.addCustomMessageHandler('markermd_tree_state', function(state) {
        lastState[state.id] = state;
        var container = document.getElementById(state.id);
        if (container) applyState(container, state);
      });

      $(document).on('shiny:value', function(event) {
        var state = lastState[event.name];
        if (!state) return;
        setTimeout(function() {
          var container = document.getElementById(event.name);
          if (container) applyState(container, state);
        }, 0);
      });

      $(document).on('click', '[data-tree-input] [data-tree-action]', function(event) {
        var node = this.closest('[data-tree-index]');
        var tree = this.closest('[data-tree-input]');
        if (!node || !tree) return;
        var action = this.getAttribute('data-tree-action');
        if (action === 'toggle' && node.classList.contains('tree-locked')) return;
        event.preventDefault();
        Shiny.setInputValue(tree.getAttribute('data-tree-input'), {
          index: parseInt(node.getAttribute('data-tree-index'), 10),
          action: action,
          nonce: ++nonce
        }, {priority: 'event'});
      });
    })();
    ",
    .open = "<<",
    .close = ">>"
  ))))
}

# AST module UI
#
# id: Character. Module namespace ID
# title: Character. Panel title (default: "Document Structure")
# header_extra: Tag. Optional content shown on the right of the card header
#   (the template app shows the active-question badge there)

ast_module_ui = function(id, title = "Document Structure", header_extra = NULL) {
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
    )
  )
}

# AST module server
#
# Returns list(node_clicked = reactive) reporting heading toggles as
# list(node_index, action = "toggle", timestamp); previews are handled here.
#
# id: Character. Module namespace ID
# ast: Reactive. The parsed AST object
# selected_nodes: Reactive. Currently selected node indices
# filtered_nodes: Reactive. Node indices excluded by the current question's
#   filters, drawn in red (see question_filtered_indices())

ast_module_server = function(id, ast, selected_nodes = shiny::reactive(integer(0)), filtered_nodes = shiny::reactive(integer(0))) {
  shiny::moduleServer(id, function(input, output, session) {

    # Flattened AST nodes in tree order (index i aligns with the tree's node i)
    ast_nodes = shiny::reactive({
      if (is.null(ast())) return(NULL)
      lapply(q2r_flatten(ast()), function(record) record$node)
    })

    ast_tree_items = shiny::reactive({
      if (is.null(ast())) return(list())
      build_ast_tree_structure(ast())
    })

    children_index = shiny::reactive(tree_children_index(ast_tree_items()))

    # The highlight follows the deduplicated selection and filter vectors, so
    # an edit that changes neither sends nothing
    selected = deduplicated_reactive(shiny::reactive(sort(unique(as.integer(selected_nodes())))))
    filtered = deduplicated_reactive(shiny::reactive(sort(unique(as.integer(filtered_nodes())))))
    tree_state = shiny::reactive({
      tree_selection_state(ast_tree_items(), selected(), filtered(), children_index())
    })

    # Drawn once per document; the highlight is patched in place below
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
        ast_render_opts(mode = "interactive"),
        state = shiny::isolate(tree_state())
      )
    })

    shiny::observe({
      state = tree_state()
      session$sendCustomMessage("markermd_tree_state", list(
        id = session$ns("ast_tree_ui"),
        selected = as.list(state$selected),
        direct = as.list(state$direct),
        filtered = as.list(state$filtered),
        locked = as.list(state$locked)
      ))
    }) |>
      shiny::bindEvent(tree_state())

    node_clicked = shiny::reactiveVal(NULL)

    shiny::observe({
      event = input$tree_event
      index = suppressWarnings(as.integer(event$index))
      items = ast_tree_items()
      if (length(index) != 1 || is.na(index) || index < 1 || index + 1 > length(items)) {
        return()
      }
      if (identical(event$action, "toggle") && tree_item_selectable(items[[index + 1]])) {
        node_clicked(list(node_index = index, action = "toggle", timestamp = Sys.time()))
      } else if (identical(event$action, "preview")) {
        show_node_preview(ast_nodes()[[index]])
      }
    }) |>
      shiny::bindEvent(input$tree_event)

    list(node_clicked = shiny::reactive(node_clicked()))
  })
}
