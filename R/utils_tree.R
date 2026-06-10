# Tree Display Utilities
# Functions for creating hierarchical tree displays of AST structures.
#
# There is a single render core: render_ast_tree() turns the tree_items produced
# by build_ast_tree_structure() into HTML, parameterised by ast_render_opts().
# Both the interactive template module (mod_ast) and the non-interactive mark
# validation display (mod_mark_validate) call it, so the selectable and read-only
# trees share one renderer, one CSS generator, and one recursive level builder.

# Build a hierarchical tree structure from a q2r document
#
# Produces the tree_items contract the selection and render code depends on:
# a document-root entry at index 0 followed by one entry per flattened node,
# each with index, type, depth (root = 0), parent_index (0 = root),
# description, and prefix. Node index i is stored at tree_items[[i + 1]].
#
# ast: q2r pandoc AST object

build_ast_tree_structure = function(ast) {
  if (is.null(ast)) {
    return(list())
  }

  records = q2r_flatten(ast)
  if (length(records) == 0) {
    return(list())
  }

  tree_items = list()
  tree_items[[1]] = list(
    index = 0,
    type = "document_root",
    depth = 0,
    parent_index = NULL,
    description = "Document",
    detail = "",
    prefix = ""
  )

  for (record in records) {
    node = record$node
    node_id = if (S7::S7_inherits(node, q2r::pandoc_header) || S7::S7_inherits(node, q2r::pandoc_div)) {
      node@attr@id
    } else {
      ""
    }
    tree_items[[record$index + 1]] = list(
      index = record$index,
      type = record$type,
      node_id = node_id,
      depth = record$depth,
      parent_index = record$parent,
      description = record$label,
      detail = record$detail,
      prefix = "\\u251c\\u2500\\u2500 "
    )
  }

  tree_items
}

# Options controlling how render_ast_tree() draws a tree
#
# A plain validated list (rather than an S7 object) since this is transient UI
# render configuration, not persisted domain data.
#
# mode: "interactive" (selectable, used by the template app) or "readonly"
#   (used by the mark validation display). Highlighting is data, not a mode:
#   passing a non-empty `selected` in readonly mode marks those nodes.
# selected: Integer vector of directly selected node indices
# filtered: Integer vector of node indices excluded by the current question's
#   filters (see question_filtered_indices()), drawn in red instead of green
# id_prefix: Optional prefix for preview-button ids, to namespace multiple trees
#   that share a module (e.g. one per question on the mark side)
# start_depth: Depth level to start rendering from (0 = document root)
# drop_root: When TRUE, removes the index-0 document-root item and reparents any
#   node pointing at it to a top-level node. Used by the mark side to show a
#   question's content without the synthetic document root.
# enable_preview: Whether preview observers are wired by the caller (does not
#   affect rendering; the preview buttons are always drawn for content nodes)

ast_render_opts = function(mode = c("interactive", "readonly"),
                           selected = integer(0),
                           filtered = integer(0),
                           id_prefix = NULL,
                           start_depth = 0L,
                           drop_root = FALSE,
                           enable_preview = TRUE) {
  mode = match.arg(mode)
  start_depth = as.integer(start_depth)

  if (start_depth < 0L) {
    stop("`start_depth` must be >= 0", call. = FALSE)
  }
  if (mode == "interactive" && (drop_root || start_depth != 0L)) {
    stop("interactive mode requires `drop_root = FALSE` and `start_depth = 0`", call. = FALSE)
  }

  list(
    mode = mode,
    selected = selected,
    filtered = filtered,
    id_prefix = id_prefix,
    start_depth = start_depth,
    drop_root = drop_root,
    enable_preview = enable_preview
  )
}

# Render a tree of AST nodes to HTML
#
# The single render entry point, shared by the interactive and read-only trees.
#
# tree_items: List of tree items from build_ast_tree_structure
# ns: Shiny namespace function
# opts: Render options from ast_render_opts()

render_ast_tree = function(tree_items, ns, opts = ast_render_opts()) {
  if (length(tree_items) == 0) {
    return(shiny::p("No document structure available"))
  }

  if (opts$drop_root) {
    tree_items = lapply(
      Filter(function(x) x$index != 0, tree_items),
      function(x) {
        if (!is.null(x$parent_index) && x$parent_index == 0) {
          x$parent_index = NULL
        }
        x
      }
    )
  }

  if (length(tree_items) == 0) {
    return(shiny::p("No document structure available"))
  }

  css_class = if (opts$mode == "interactive") {
    "ast-tree"
  } else if (opts$start_depth > 0) {
    "ast-tree-readonly-nested"
  } else {
    "ast-tree-readonly"
  }

  all_selected_nodes = compute_all_selected_nodes(tree_items, opts$selected)

  tree_html = build_ast_tree_level(tree_items, opts$start_depth, NULL, opts, all_selected_nodes, ns)

  shiny::tagList(
    ast_tree_css(css_class, opts),
    shiny::tags$ul(class = css_class, tree_html)
  )
}

# CSS for a rendered tree
#
# css_class: Character. The tree's root class name
# opts: Render options from ast_render_opts()

ast_tree_css = function(css_class, opts) {
  font_size = if (opts$mode == "interactive") "13px" else "11px"

  base_css = glue::glue(
    "
    .<<css_class>> {
      --spacing: 1.5rem;
      --radius: 10px;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      font-size: <<font_size>>;
      list-style: none;
      padding: 0;
      margin: 0;
    }

    .<<css_class>> li {
      display: block;
      position: relative;
      padding-left: calc(2 * var(--spacing) - var(--radius) - 2px);
    }

    .<<css_class>> ul {
      margin-left: calc(var(--radius) - var(--spacing));
      padding-left: 0;
      list-style: none;
    }

    .<<css_class>> ul li {
      border-left: 2px solid #ddd;
    }

    .<<css_class>> ul li:last-child {
      border-color: transparent;
    }

    .<<css_class>> ul li::before {
      content: '';
      display: block;
      position: absolute;
      top: calc(var(--spacing) / -2);
      left: -2px;
      width: calc(var(--spacing) + 2px);
      height: calc(var(--spacing) + 1px);
      border: solid #ddd;
      border-width: 0 0 2px 2px;
    }

    .<<css_class>> .tree-node-content {
      display: flex;
      align-items: center;
      justify-content: flex-start;
      padding: 2px 8px;
      border-radius: 3px;
      margin-bottom: 2px;
      min-height: var(--spacing);
    }

    .<<css_class>> .tree-node-info {
      display: flex;
      align-items: center;
      flex-grow: 1;
    }

    .<<css_class>> .tree-node-description {
      margin-right: 12px;
      line-height: 1.4;
      color: #333;
    }
    ",
    .open = "<<",
    .close = ">>"
  )

  nested_css = if (opts$start_depth > 0) {
    glue::glue(
      "
      .<<css_class>> > li {
        border-left: 2px solid #ddd;
      }

      .<<css_class>> > li::before {
        content: '';
        display: block;
        position: absolute;
        top: calc(var(--spacing) / -2);
        left: -2px;
        width: calc(var(--spacing) + 2px);
        height: calc(var(--spacing) + 1px);
        border: solid #ddd;
        border-width: 0 0 2px 2px;
      }

      .<<css_class>> > li:last-child {
        border-color: transparent;
      }
      ",
      .open = "<<",
      .close = ">>"
    )
  } else {
    ""
  }

  mode_css = if (opts$mode == "interactive") {
    glue::glue(
      "
      .<<css_class>> .tree-toggle-btn {
        position: absolute;
        top: calc(var(--spacing) / 2 - var(--radius));
        left: calc(var(--spacing) - var(--radius) - 1px);
        width: calc(2 * var(--radius));
        height: calc(2 * var(--radius));
        border-radius: 50%;
        background: #ddd;
        border: 1px solid #bbb;
        padding: 0;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 8px;
        cursor: pointer;
        z-index: 10;
      }

      .<<css_class>> .tree-toggle-btn:hover {
        background: #007bff;
        color: white;
        border-color: #0056b3;
      }

      .<<css_class>> .tree-toggle-btn:focus {
        outline: 2px solid #28a745;
        outline-offset: 1px;
      }

      .<<css_class>> .tree-toggle-btn.selected {
        background: #28a745;
        color: white;
        border-color: #1e7e34;
      }

      /* Non-selectable nodes use a square instead of the selectable circle.
         Background/border inherit the circle's styling from .tree-toggle-btn. */
      .<<css_class>> .tree-toggle-btn.tree-marker-square {
        border-radius: 0;
        transform: scale(0.75);
      }

      .<<css_class>> .tree-toggle-btn.tree-marker-square.selected {
        background: #28a745;
      }

      .<<css_class>> .tree-node-description.selected {
        background-color: #28a745;
        color: white;
        padding-left: 4px;
        padding-right: 4px;
        border-radius: 3px;
      }

      .<<css_class>> .tree-node-description-btn {
        margin-right: 12px;
        line-height: 1.4;
        text-decoration: none;
        outline: none;
        box-shadow: none;
      }

      .<<css_class>> .tree-node-description-btn:hover {
        background-color: #f8f9fa !important;
        border-radius: 3px;
      }

      .<<css_class>> .tree-node-description-btn:focus {
        outline: 2px solid #28a745;
        outline-offset: 1px;
      }

      .<<css_class>> .tree-node-description-btn.selected {
        background-color: #28a745 !important;
        color: white !important;
        padding-left: 4px;
        padding-right: 4px;
        border-radius: 3px;
      }

      /* Nodes excluded by the current question's filters: red overrides the
         selection green */
      .<<css_class>> .tree-toggle-btn.selected.filtered {
        background: #dc3545;
        border-color: #b02a37;
      }

      .<<css_class>> .tree-marker-square.selected.filtered {
        background: #dc3545;
      }

      .<<css_class>> .tree-node-description.selected.filtered {
        background-color: #dc3545;
      }

      .<<css_class>> .tree-node-description-btn.selected.filtered {
        background-color: #dc3545 !important;
      }
      ",
      .open = "<<",
      .close = ">>"
    )
  } else {
    glue::glue(
      "
      .<<css_class>> li::after {
        content: '';
        display: block;
        position: absolute;
        top: calc(var(--spacing) / 2 - var(--radius));
        left: calc(var(--spacing) - var(--radius) - 1px);
        width: calc(2 * var(--radius));
        height: calc(2 * var(--radius));
        border-radius: 50%;
        background: #ddd;
      }

      .<<css_class>> li.document-root::after {
        content: '';
        background: #ddd;
        border-radius: 50%;
        width: calc(2 * var(--radius));
        height: calc(2 * var(--radius));
      }

      .<<css_class>> .tree-node-description.selected {
        background-color: #28a745;
        color: white;
        padding-left: 4px;
        padding-right: 4px;
        border-radius: 3px;
      }

      .<<css_class>> .tree-node-description.selected.filtered {
        background-color: #dc3545;
      }
      ",
      .open = "<<",
      .close = ">>"
    )
  }

  shiny::tags$style(shiny::HTML(paste0(base_css, nested_css, mode_css)))
}

# Preview button for a content node (NULL for headings and the document root)
#
# Single source of the preview-button markup and id scheme shared by both modes.
#
# item: A tree item
# ns: Shiny namespace function
# id_prefix: Optional prefix to namespace the button id

node_preview_button = function(item, ns, id_prefix = NULL) {
  if (item$type == "document_root" || item$type == "pandoc_header") {
    return(NULL)
  }

  button_id = if (!is.null(id_prefix)) {
    paste0("preview_", id_prefix, "_", item$index)
  } else {
    paste0("preview_", item$index)
  }

  shiny::actionButton(
    ns(button_id),
    shiny::icon("search"),
    class = "btn-outline-info",
    style = "font-size: 8px; padding: 1px 4px; min-width: 18px; height: 18px; border-width: 1px; margin-left: 8px;",
    title = "Preview content"
  )
}

# Whether a tree item is directly selectable as a question target
#
# Headings are always selectable; divs only when they carry an explicit id. The
# tree-item twin of node_is_selectable() (which operates on a live q2r node); the
# two must stay in lockstep.
#
# item: A tree item

tree_item_selectable = function(item) {
  item$type == "pandoc_header" ||
    (item$type == "pandoc_div" && !is.null(item$node_id) && nzchar(item$node_id))
}

# Build the .tree-node-content block for a single tree item
#
# item: A tree item
# opts: Render options from ast_render_opts()
# tree_items: All tree items (for ancestor lookup in interactive mode)
# ns: Shiny namespace function
# is_selected: Whether the item is selected (directly or via an ancestor)
# is_directly_selected: Whether the item itself was selected
# is_indirectly_selected: Whether the item is selected only via an ancestor
# is_filtered: Whether the item is excluded by the current question's filters
#   (drawn red instead of green; only meaningful for selected items)

ast_tree_node_content = function(item, opts, tree_items, ns,
                                 is_selected, is_directly_selected, is_indirectly_selected,
                                 is_filtered = FALSE) {

  # Selected nodes are green; those excluded by the question's filters are red
  highlight_class = function(base) {
    if (is_selected && is_filtered) {
      paste(base, "selected filtered")
    } else if (is_selected) {
      paste(base, "selected")
    } else {
      base
    }
  }

  if (item$type == "document_root") {
    label = shiny::span(
      item$description,
      class = "tree-node-description",
      style = "font-weight: bold; color: #333;"
    )

    if (opts$mode == "interactive") {
      # The root is non-selectable, so it uses the square marker like other
      # non-selectable nodes.
      return(shiny::div(
        class = "tree-node-content",
        shiny::div(
          class = "tree-toggle-btn tree-marker-square",
          style = "cursor: default; pointer-events: none;"
        ),
        shiny::div(class = "tree-node-info", label)
      ))
    }

    return(shiny::div(
      class = "tree-node-content",
      shiny::div(class = "tree-node-info", label)
    ))
  }

  preview_btn = node_preview_button(item, ns, opts$id_prefix)

  if (opts$mode == "interactive") {
    is_selectable = tree_item_selectable(item) &&
      !has_selected_ancestor(tree_items, item$index, opts$selected)

    if (is_selectable) {
      button_icon = if (is_directly_selected) {
        shiny::icon("check")
      } else if (is_indirectly_selected) {
        shiny::icon("ellipsis-v")
      } else {
        ""
      }

      toggle_button = shiny::actionButton(
        ns(paste0("select_children_", item$index)),
        button_icon,
        class = highlight_class("tree-toggle-btn"),
        title = "Toggle this node and its children"
      )

      return(shiny::div(
        class = "tree-node-content",
        toggle_button,
        shiny::div(
          class = "tree-node-info",
          shiny::actionButton(
            ns(paste0("select_", item$index)),
            item$description,
            class = highlight_class("tree-node-description-btn"),
            style = "background: none; border: none; padding: 0; margin: 0; font: inherit; cursor: pointer; text-align: left; color: inherit;"
          ),
          preview_btn
        )
      ))
    }

    indicator_icon = if (is_indirectly_selected) {
      shiny::icon("ellipsis-v")
    } else if (is_directly_selected) {
      shiny::icon("check")
    } else {
      ""
    }

    selection_indicator = shiny::div(
      class = highlight_class("tree-toggle-btn tree-marker-square"),
      style = "cursor: default; pointer-events: none;",
      indicator_icon,
      title = if (is_selected && is_filtered) {
        "Excluded by question filters"
      } else if (is_selected) {
        "Selected via parent selection"
      } else {
        "Non-selectable node"
      }
    )

    text_class = highlight_class("tree-node-description")

    return(shiny::div(
      class = "tree-node-content",
      selection_indicator,
      shiny::div(
        class = "tree-node-info",
        tree_node_label_ui(item$description, item$detail, text_class),
        preview_btn
      )
    ))
  }

  # Read-only mode - no interactive elements except preview
  text_class = highlight_class("tree-node-description")

  shiny::div(
    class = "tree-node-content",
    shiny::div(
      class = "tree-node-info",
      tree_node_label_ui(item$description, item$detail, text_class),
      preview_btn
    )
  )
}

# Build the nested <li> elements for one depth level, recursing into children
#
# tree_items: All tree items
# target_depth: Current depth level to build
# parent_index: Parent node index (NULL for the level's roots)
# opts: Render options from ast_render_opts()
# all_selected_nodes: Vector of all selected node indices (direct + descendants)
# ns: Shiny namespace function

build_ast_tree_level = function(tree_items, target_depth, parent_index, opts, all_selected_nodes, ns) {

  level_items = tree_items[sapply(tree_items, function(x) {
    x$depth == target_depth &&
    ((is.null(parent_index) && is.null(x$parent_index)) ||
     (!is.null(parent_index) && !is.null(x$parent_index) && x$parent_index == parent_index))
  })]

  if (length(level_items) == 0) {
    return(list())
  }

  lapply(level_items, function(item) {

    is_selected = item$index %in% all_selected_nodes
    is_directly_selected = item$index %in% opts$selected
    is_indirectly_selected = is_selected && !is_directly_selected
    is_filtered = item$index %in% opts$filtered

    children = tree_items[sapply(tree_items, function(x) {
      !is.null(x$parent_index) && x$parent_index == item$index
    })]
    has_children = length(children) > 0

    node_content = ast_tree_node_content(
      item, opts, tree_items, ns,
      is_selected, is_directly_selected, is_indirectly_selected,
      is_filtered
    )

    li_class = if (item$type == "document_root") "document-root" else NULL

    if (has_children) {
      child_elements = build_ast_tree_level(tree_items, target_depth + 1, item$index, opts, all_selected_nodes, ns)
      shiny::tags$li(
        class = li_class,
        node_content,
        shiny::tags$ul(child_elements)
      )
    } else {
      shiny::tags$li(
        class = li_class,
        node_content
      )
    }
  })
}

# Node label UI: the description, with an optional smaller content line beneath
#
# description: Character. The node's primary label
# detail: Character. Optional content preview ("" or NULL for none)
# text_class: Character. CSS class(es) for the primary label

tree_node_label_ui = function(description, detail, text_class) {
  if (is.null(detail) || !nzchar(detail)) {
    return(shiny::span(description, class = text_class, style = "line-height: 1.4;"))
  }

  shiny::div(
    style = "margin-right: 12px;",
    shiny::div(shiny::span(description, class = text_class)),
    shiny::div(detail, style = "font-size: 0.8em; color: #6c757d; line-height: 1.3; margin-top: 1px;")
  )
}

# Find all child nodes for a given parent node
#
# tree_items: List of tree items
# parent_node_index: Index of parent node

find_node_children = function(tree_items, parent_node_index) {

  children = integer(0)

  if (is.null(parent_node_index)) {
    return(children)
  }

  for (i in seq_along(tree_items)) {
    item = tree_items[[i]]

    if (!is.null(item$parent_index) && !is.na(item$parent_index) &&
        item$parent_index == parent_node_index) {
      children = c(children, item$index)
    }
  }

  return(children)
}

# Find all descendant nodes (children, grandchildren, etc.) for a given parent node
#
# tree_items: List of tree items
# parent_node_index: Index of parent node

find_all_descendants = function(tree_items, parent_node_index) {

  all_descendants = integer(0)

  if (is.null(parent_node_index)) {
    return(all_descendants)
  }

  direct_children = find_node_children(tree_items, parent_node_index)

  if (length(direct_children) > 0) {
    all_descendants = c(all_descendants, direct_children)

    for (child_index in direct_children) {
      child_descendants = find_all_descendants(tree_items, child_index)
      all_descendants = c(all_descendants, child_descendants)
    }
  }

  return(unique(all_descendants))
}

# Check if any ancestor node of the given node is in the directly selected list
#
# tree_items: List of tree items
# node_index: Index of the node to check
# directly_selected_nodes: Vector of directly selected node indices

has_selected_ancestor = function(tree_items, node_index, directly_selected_nodes) {

  if (is.null(node_index) || length(directly_selected_nodes) == 0) {
    return(FALSE)
  }

  node_item = NULL
  for (item in tree_items) {
    if (item$index == node_index) {
      node_item = item
      break
    }
  }

  if (is.null(node_item)) {
    return(FALSE)
  }

  current_parent = node_item$parent_index

  while (!is.null(current_parent) && current_parent != 0) {  # 0 is document root
    if (current_parent %in% directly_selected_nodes) {
      return(TRUE)
    }

    parent_item = NULL
    for (item in tree_items) {
      if (item$index == current_parent) {
        parent_item = item
        break
      }
    }

    if (is.null(parent_item)) {
      break
    }

    current_parent = parent_item$parent_index
  }

  return(FALSE)
}

# Given a list of directly selected nodes, compute the full list including all descendants
#
# tree_items: List of tree items
# directly_selected_nodes: Vector of directly selected node indices

compute_all_selected_nodes = function(tree_items, directly_selected_nodes) {

  if (length(directly_selected_nodes) == 0) {
    return(integer(0))
  }

  all_selected = integer(0)

  for (node_index in directly_selected_nodes) {
    all_selected = c(all_selected, node_index)

    descendants = find_all_descendants(tree_items, node_index)
    all_selected = c(all_selected, descendants)
  }

  return(unique(sort(all_selected)))
}

# Given a list of currently selected nodes and a new node, find which selected
# nodes are descendants of the new node (and should be removed)
#
# tree_items: List of tree items
# new_node_index: Index of the newly selected node
# current_selected: Vector of currently selected node indices

find_selected_descendants = function(tree_items, new_node_index, current_selected) {

  if (length(current_selected) == 0) {
    return(integer(0))
  }

  all_descendants = find_all_descendants(tree_items, new_node_index)

  selected_descendants = intersect(current_selected, all_descendants)

  return(selected_descendants)
}
