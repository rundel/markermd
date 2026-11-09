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
# A heading inside a div also carries `section`: the indices of the in-div
# blocks its section covers (see in_div_heading_sections()). The display tree
# nests a div's blocks under the div, not under such a heading, so selection
# highlighting and node counts follow `section` to match what grading takes.
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
    detail = ""
  )

  sections = in_div_heading_sections(records)

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
      section = sections[[as.character(record$index)]]
    )
  }

  tree_items
}

# The section of every heading that sits inside a div: the indices of its
# following sibling blocks in the same div up to the next heading of the same
# or higher level, the same resolution heading_section_indices() uses for
# grading. Keyed by the heading's index as a character string; top-level
# headings are absent, since the display tree already nests their sections
# beneath them.
#
# records: q2r_flatten() output for the AST

in_div_heading_sections = function(records) {
  out = list()
  container_of = vapply(records, function(r) as.integer(r$container), integer(1))
  for (record in records) {
    node = record$node
    if (record$container == 0L || !S7::S7_inherits(node, q2r::pandoc_header)) {
      next
    }
    siblings = which(container_of == record$container)
    following = siblings[siblings > record$index]
    covered = integer(0)
    for (s in following) {
      sib = records[[s]]$node
      if (S7::S7_inherits(sib, q2r::pandoc_header) && sib@level <= node@level) {
        break
      }
      covered = c(covered, s)
    }
    if (length(covered) > 0) {
      out[[as.character(record$index)]] = covered
    }
  }
  out
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
    cli::cli_abort("`start_depth` must be >= 0")
  }
  if (mode == "interactive" && (drop_root || start_depth != 0L)) {
    cli::cli_abort("interactive mode requires `drop_root = FALSE` and `start_depth = 0`")
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
# state: Optional highlight state from tree_selection_state(); computed from
#   opts$selected / opts$filtered when NULL. The interactive tree passes the
#   state it later patches in place, so the first render and the patches agree.

render_ast_tree = function(tree_items, ns, opts = ast_render_opts(), state = NULL) {
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

  # One children-by-parent index for the whole render, so each level looks up
  # its items directly; per-level rescans of all items made large documents
  # quadratic
  children_index = tree_children_index(tree_items)

  if (is.null(state)) {
    state = if (opts$mode == "interactive") {
      tree_selection_state(tree_items, opts$selected, opts$filtered, children_index)
    } else {
      all_selected = compute_all_selected_nodes(tree_items, opts$selected, children_index)
      list(
        selected = all_selected,
        direct = as.integer(opts$selected),
        filtered = intersect(as.integer(opts$filtered), all_selected),
        locked = integer(0)
      )
    }
  }

  tree_html = build_ast_tree_level(tree_items, opts$start_depth, NULL, opts, state, ns, children_index)

  # The interactive tree reports every click through one input named on its
  # root list (see ast_tree_script())
  shiny::tagList(
    ast_tree_css(css_class, opts),
    shiny::tags$ul(
      class = css_class,
      `data-tree-input` = if (opts$mode == "interactive") ns("tree_event"),
      tree_html
    )
  )
}

# The highlight state of an interactive tree for one selection, as index
# vectors: `selected` is the direct selection plus everything it covers
# (compute_all_selected_nodes()), `direct` the direct selection, `filtered` the
# covered nodes the question's filters exclude (drawn red), and `locked` the
# selectable nodes beneath a directly selected ancestor, drawn as indicators
# rather than toggles. The document root (index 0, a whole-document question's
# selection) never locks its descendants, matching has_selected_ancestor().
# The same state drives the first render and every in-place patch
# (ast_tree_script()).
#
# tree_items: Tree items from build_ast_tree_structure()
# selected: Integer vector of directly selected node indices
# filtered: Integer vector of node indices excluded by the question's filters
# children_index: Optional children-by-parent index (see tree_children_index())

tree_selection_state = function(tree_items, selected, filtered, children_index = NULL) {
  if (is.null(children_index)) {
    children_index = tree_children_index(tree_items)
  }
  direct = sort(unique(as.integer(selected)))
  all_selected = compute_all_selected_nodes(tree_items, direct, children_index)

  under_selection = unlist(lapply(setdiff(direct, 0L), function(s) tree_descendant_indices(children_index, s)))
  locked = integer(0)
  for (index in unique(as.integer(under_selection))) {
    if (tree_item_selectable(tree_items[[index + 1]])) {
      locked = c(locked, index)
    }
  }

  list(
    selected = as.integer(all_selected),
    direct = direct,
    filtered = sort(intersect(as.integer(filtered), as.integer(all_selected))),
    locked = sort(locked)
  )
}

# How one interactive node is drawn for its highlight state: the marker and
# label classes, the marker's icon ("check", "ellipsis" or "") and its title.
# The class strings are built in the same order as the in-place patch in
# ast_tree_script() builds them, so a patched node matches a rendered one.
#
# selectable: Whether the node can be a question target (tree_item_selectable())
# selected, direct, filtered, locked: The node's membership in each of the
#   tree_selection_state() vectors
# marker_base, label_base: The marker and label elements' fixed classes

tree_node_view = function(selectable, selected, direct, filtered, locked, marker_base, label_base) {
  open = selectable && !locked
  highlight = if (selected && filtered) " selected filtered" else if (selected) " selected" else ""
  list(
    open = open,
    marker_class = paste0(marker_base, if (!open) " tree-marker-square" else "", highlight),
    label_class = paste0(label_base, highlight),
    icon = if (direct) "check" else if (selected) "ellipsis" else "",
    title = if (open) {
      "Toggle this node and its children"
    } else if (selected && filtered) {
      "Excluded by question filters"
    } else if (selected) {
      "Selected via parent selection"
    } else {
      "Non-selectable node"
    }
  )
}

# The icon markup for a tree_node_view() icon name
#
# name: "check", "ellipsis" or ""

tree_node_icon = function(name) {
  switch(name, check = fa_icon("check"), ellipsis = fa_icon("ellipsis-v"), "")
}

# CSS for a rendered tree
#
# css_class: Character. The tree's root class name
# opts: Render options from ast_render_opts()

ast_tree_css = function(css_class, opts) {
  # Same base size in both modes: the read-only validation tree carries the
  # content graders actually scan, so it must not render smaller than the
  # interactive template tree
  font_size = "13px"

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

      /* A selectable node under a selected ancestor is an indicator, not a
         toggle, until the ancestor is deselected */
      .<<css_class>> .tree-locked .tree-toggle-btn,
      .<<css_class>> .tree-locked .tree-node-description-btn {
        cursor: default;
        pointer-events: none;
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

node_preview_button = function(item, ns, id_prefix = NULL, delegated = FALSE) {
  if (item$type == "document_root" || item$type == "pandoc_header") {
    return(NULL)
  }

  button_id = if (!is.null(id_prefix)) {
    paste0("preview_", id_prefix, "_", item$index)
  } else {
    paste0("preview_", item$index)
  }

  # The interactive tree's buttons are plain markup reported through its one
  # delegated input; the read-only trees bind one input per button
  if (delegated) {
    return(shiny::tags$button(
      id = ns(button_id),
      type = "button",
      class = "btn btn-default btn-outline-info",
      style = "font-size: 10px; padding: 2px 6px; min-width: 24px; height: 22px; border-width: 1px; margin-left: 8px;",
      title = "Preview content",
      `data-tree-action` = "preview",
      fa_icon("search")
    ))
  }

  shiny::actionButton(
    ns(button_id),
    fa_icon("search"),
    class = "btn-outline-info",
    style = "font-size: 10px; padding: 2px 6px; min-width: 24px; height: 22px; border-width: 1px; margin-left: 8px;",
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
# An interactive node carries its index and selectability as data attributes
# and plain buttons, so ast_tree_script() can patch its highlight in place and
# report its clicks through the tree's one input; the read-only trees draw the
# highlight once.
#
# item: A tree item
# opts: Render options from ast_render_opts()
# ns: Shiny namespace function
# state: Highlight state (see tree_selection_state())

ast_tree_node_content = function(item, opts, ns, state) {
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

  is_selected = item$index %in% state$selected
  is_filtered = item$index %in% state$filtered

  if (opts$mode == "interactive") {
    return(ast_tree_interactive_node(item, ns, state, is_selected, is_filtered))
  }

  text_class = paste0("tree-node-description", if (is_selected && is_filtered) " selected filtered" else if (is_selected) " selected" else "")

  shiny::div(
    class = "tree-node-content",
    shiny::div(
      class = "tree-node-info",
      tree_node_label_ui(item$description, item$detail, text_class),
      node_preview_button(item, ns, opts$id_prefix)
    )
  )
}

# The interactive tree's markup for one non-root node: a marker (a toggle
# button for a selectable node, a square indicator otherwise), its label and,
# for content nodes, a preview button. A selectable node keeps its buttons
# while locked under a selected ancestor (the .tree-locked class turns them
# into indicators), so a patch never has to replace elements.
#
# item: A tree item
# ns: Shiny namespace function
# state: Highlight state (see tree_selection_state())
# is_selected, is_filtered: The node's highlight

ast_tree_interactive_node = function(item, ns, state, is_selected, is_filtered) {
  selectable = tree_item_selectable(item)
  marker_base = if (selectable) "btn btn-default tree-toggle-btn" else "tree-toggle-btn"
  label_base = if (selectable) "btn btn-default tree-node-description-btn" else "tree-node-description"
  view = tree_node_view(
    selectable, is_selected, item$index %in% state$direct, is_filtered,
    item$index %in% state$locked, marker_base, label_base
  )
  preview_btn = node_preview_button(item, ns, delegated = TRUE)

  if (selectable) {
    marker = shiny::tags$button(
      id = ns(paste0("select_children_", item$index)),
      type = "button",
      class = view$marker_class,
      title = view$title,
      `data-tree-action` = "toggle",
      `data-tree-base` = marker_base,
      `data-tree-icon` = view$icon,
      tree_node_icon(view$icon)
    )
    label = shiny::tags$button(
      id = ns(paste0("select_", item$index)),
      type = "button",
      class = view$label_class,
      style = "background: none; border: none; padding: 0; margin: 0; font: inherit; cursor: pointer; text-align: left; color: inherit;",
      `data-tree-action` = "toggle",
      `data-tree-base` = label_base,
      item$description
    )
  } else {
    marker = shiny::div(
      class = view$marker_class,
      style = "cursor: default; pointer-events: none;",
      title = view$title,
      `data-tree-base` = marker_base,
      `data-tree-icon` = view$icon,
      tree_node_icon(view$icon)
    )
    label = tree_node_label_ui(item$description, item$detail, view$label_class, label_base)
  }

  shiny::div(
    class = paste0("tree-node-content", if (selectable && !view$open) " tree-locked" else ""),
    `data-tree-index` = item$index,
    `data-tree-selectable` = if (selectable) "1" else "0",
    marker,
    shiny::div(class = "tree-node-info", label, preview_btn)
  )
}

# Children of each tree item, keyed by parent index as a character string
# ("root" for items with no parent). Built once per render so each level looks
# up its items directly instead of rescanning the whole list.
#
# tree_items: All tree items

tree_children_index = function(tree_items) {
  index = new.env(parent = emptyenv())
  for (item in tree_items) {
    key = if (is.null(item$parent_index)) "root" else as.character(item$parent_index)
    index[[key]] = c(index[[key]], list(item))
  }
  index
}

# Build the nested <li> elements for one depth level, recursing into children
#
# tree_items: All tree items
# target_depth: Current depth level to build
# parent_index: Parent node index (NULL for the level's roots)
# opts: Render options from ast_render_opts()
# state: Highlight state (see tree_selection_state())
# ns: Shiny namespace function
# children_index: Children-by-parent index (see tree_children_index())

build_ast_tree_level = function(tree_items, target_depth, parent_index, opts, state, ns, children_index = NULL) {
  if (is.null(children_index)) {
    children_index = tree_children_index(tree_items)
  }

  key = if (is.null(parent_index)) "root" else as.character(parent_index)
  candidates = children_index[[key]]
  level_items = Filter(function(x) x$depth == target_depth, candidates)

  if (length(level_items) == 0) {
    return(list())
  }

  lapply(level_items, function(item) {
    node_content = ast_tree_node_content(item, opts, ns, state)
    li_class = if (item$type == "document_root") "document-root" else NULL

    if (length(children_index[[as.character(item$index)]]) > 0) {
      child_elements = build_ast_tree_level(tree_items, target_depth + 1, item$index, opts, state, ns, children_index)
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
# base_class: Optional fixed classes of the label, recorded for the
#   interactive tree's in-place patch (see ast_tree_script())

tree_node_label_ui = function(description, detail, text_class, base_class = NULL) {
  if (is.null(detail) || !nzchar(detail)) {
    return(shiny::span(description, class = text_class, `data-tree-base` = base_class, style = "line-height: 1.4;"))
  }

  shiny::div(
    style = "margin-right: 12px;",
    shiny::div(shiny::span(description, class = text_class, `data-tree-base` = base_class)),
    shiny::div(detail, class = "text-secondary", style = "font-size: 0.85em; line-height: 1.3; margin-top: 1px;")
  )
}

# Indices of every tree descendant of a node (children, grandchildren, and so
# on), walking the children-by-parent index so the cost is linear in the
# subtree; a scan of every item per node made a click on a large top-level
# heading quadratic in the document
#
# children_index: Children-by-parent index (see tree_children_index())
# node_index: Index of the node whose descendants are wanted

tree_descendant_indices = function(children_index, node_index) {
  found = list()
  stack = as.integer(node_index)
  while (length(stack) > 0) {
    kids = children_index[[as.character(stack[[1]])]]
    stack = stack[-1]
    if (length(kids) > 0) {
      ids = vapply(kids, function(k) as.integer(k$index), integer(1))
      found[[length(found) + 1]] = ids
      stack = c(ids, stack)
    }
  }
  as.integer(unlist(found))
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

  # Only called for interactive trees, which never drop the root, so the
  # "node index i is stored at tree_items[[i + 1]]" invariant holds and the
  # parent chain can be walked by direct indexing
  current_parent = tree_items[[node_index + 1]]$parent_index

  while (!is.null(current_parent) && current_parent != 0) {  # 0 is document root
    if (current_parent %in% directly_selected_nodes) {
      return(TRUE)
    }
    current_parent = tree_items[[current_parent + 1]]$parent_index
  }

  return(FALSE)
}

# Given a list of directly selected nodes, compute the full list including all descendants
#
# tree_items: List of tree items
# directly_selected_nodes: Vector of directly selected node indices
# children_index: Optional children-by-parent index (see tree_children_index())

compute_all_selected_nodes = function(tree_items, directly_selected_nodes, children_index = NULL) {

  if (length(directly_selected_nodes) == 0) {
    return(integer(0))
  }

  if (is.null(children_index)) {
    children_index = tree_children_index(tree_items)
  }

  sections = list()
  for (item in tree_items) {
    if (length(item$section) > 0) {
      sections[[as.character(item$index)]] = item$section
    }
  }

  # A seen flag per index (offset by one for the root) instead of a growing
  # %in% check, which made a whole-document selection quadratic
  max_index = max(vapply(tree_items, function(item) as.integer(item$index), integer(1)), as.integer(directly_selected_nodes))
  seen = logical(max_index + 1L)
  stack = as.integer(directly_selected_nodes)
  while (length(stack) > 0) {
    node_index = stack[[1]]
    stack = stack[-1]
    if (seen[node_index + 1L]) {
      next
    }
    seen[node_index + 1L] = TRUE

    kids = children_index[[as.character(node_index)]]
    if (length(kids) > 0) {
      stack = c(vapply(kids, function(k) as.integer(k$index), integer(1)), stack)
    }
    stack = c(as.integer(sections[[as.character(node_index)]]), stack)
  }

  which(seen) - 1L
}

# Given a list of currently selected nodes and a new node, find which selected
# nodes are descendants of the new node (and should be removed)
#
# tree_items: List of tree items
# new_node_index: Index of the newly selected node
# current_selected: Vector of currently selected node indices
# children_index: Optional children-by-parent index (see tree_children_index())

find_selected_descendants = function(tree_items, new_node_index, current_selected, children_index = NULL) {
  if (length(current_selected) == 0) {
    return(integer(0))
  }
  if (is.null(children_index)) {
    children_index = tree_children_index(tree_items)
  }
  intersect(current_selected, tree_descendant_indices(children_index, new_node_index))
}
