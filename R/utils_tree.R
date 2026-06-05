# Tree Display Utilities
# Functions for creating hierarchical tree displays of AST structures

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
    prefix = ""
  )

  for (record in records) {
    tree_items[[record$index + 1]] = list(
      index = record$index,
      type = record$type,
      depth = record$depth,
      parent_index = record$parent,
      description = record$label,
      prefix = "\\u251c\\u2500\\u2500 "
    )
  }

  tree_items
}

# Create a tree view for AST nodes supporting different selection modes
#
# tree_items: List of tree items from build_ast_tree_structure
# selected_nodes: Vector of selected node indices
# ns: Shiny namespace function
# selection_mode: Character. Selection mode ("interactive", "readonly", "highlight_only")
# id_prefix: Character. Optional prefix for button IDs to avoid collisions

create_unified_tree = function(tree_items, selected_nodes, ns, selection_mode = "readonly", id_prefix = NULL) {
  
  if (length(tree_items) == 0) {
    return(shiny::p("No document structure available"))
  }
  
  # Choose appropriate CSS class based on mode
  css_class = switch(selection_mode,
    "interactive" = "ast-tree",
    "readonly" = "ast-tree-readonly", 
    "highlight_only" = "ast-tree-readonly"
  )
  
  # Create unified tree CSS 
  tree_css = create_unified_tree_css(css_class, selection_mode)
  
  # Compute all selected nodes from directly selected ones
  all_selected_nodes = compute_all_selected_nodes(tree_items, selected_nodes)
  
  # Build nested tree structure
  tree_html = build_unified_tree_level(tree_items, 0, NULL, all_selected_nodes, ns, selected_nodes, selection_mode, id_prefix)
  
  shiny::tagList(
    tree_css,
    shiny::tags$ul(class = css_class, tree_html)
  )
}

# Find all child nodes for a given parent node
#
# tree_items: List of tree items
# parent_node_index: Index of parent node

find_node_children = function(tree_items, parent_node_index) {
  
  children = integer(0)
  
  # Handle NULL parent_node_index
  if (is.null(parent_node_index)) {
    return(children)
  }
  
  # Find all tree items that have this node as their parent
  for (i in seq_along(tree_items)) {
    item = tree_items[[i]]
    
    # Check if this item has the specified parent
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
  
  # Handle NULL parent_node_index
  if (is.null(parent_node_index)) {
    return(all_descendants)
  }
  
  # Find direct children
  direct_children = find_node_children(tree_items, parent_node_index)
  
  if (length(direct_children) > 0) {
    # Add direct children to the result
    all_descendants = c(all_descendants, direct_children)
    
    # Recursively find descendants of each child
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
  
  # Handle edge cases
  if (is.null(node_index) || length(directly_selected_nodes) == 0) {
    return(FALSE)
  }
  
  # Find the tree item for this node
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
  
  # Walk up the parent chain checking for selected ancestors
  current_parent = node_item$parent_index
  
  while (!is.null(current_parent) && current_parent != 0) {  # 0 is document root
    # Check if this parent is directly selected
    if (current_parent %in% directly_selected_nodes) {
      return(TRUE)
    }
    
    # Find the parent item to continue walking up
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
    # Add the directly selected node
    all_selected = c(all_selected, node_index)
    
    # Add all its descendants
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
  
  # Get all descendants of the new node
  all_descendants = find_all_descendants(tree_items, new_node_index)
  
  # Find which currently selected nodes are in the descendants list
  selected_descendants = intersect(current_selected, all_descendants)
  
  return(selected_descendants)
}

# Create a simple flat tree view for AST nodes without selection functionality,
# starting at a specific depth
#
# tree_items: List of tree items from build_ast_tree_structure
# ns: Shiny namespace function
# repo_id: Character. Unique identifier for the repository to avoid ID collisions
# start_depth: Integer. The depth level to start rendering from

create_simple_tree_readonly_at_depth = function(tree_items, ns, repo_id = NULL, start_depth = 0) {
  
  if (length(tree_items) == 0) {
    return(shiny::p("No document structure available"))
  }
  
  # Create tree CSS using iamkate.com styling (without interactive elements)
  # Add special handling for trees starting at depth > 0
  css_class = if (start_depth > 0) "ast-tree-readonly-nested" else "ast-tree-readonly"
  
  tree_css = shiny::tags$style(shiny::HTML(paste0("
    .", css_class, " {
      --spacing: 1.5rem;
      --radius: 10px;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      font-size: 11px;
      list-style: none;
      padding: 0;
      margin: 0;
    }
    
    .", css_class, " li {
      display: block;
      position: relative;
      padding-left: calc(2 * var(--spacing) - var(--radius) - 2px);
    }
    
    .", css_class, " ul {
      margin-left: calc(var(--radius) - var(--spacing));
      padding-left: 0;
      list-style: none;
    }
    
    .", css_class, " ul li {
      border-left: 2px solid #ddd;
    }
    
    .", css_class, " ul li:last-child {
      border-color: transparent;
    }
    
    .", css_class, " ul li::before {
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
    
    /* For nested trees (start_depth > 0), add branch lines to root level items */
    ", if (start_depth > 0) paste0("
    .", css_class, " > li {
      border-left: 2px solid #ddd;
    }
    
    .", css_class, " > li::before {
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
    
    .", css_class, " > li:last-child {
      border-color: transparent;
    }
    ") else "", "
    
    /* Simple dots for all nodes (no interaction) */
    .", css_class, " li::after {
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
    
    /* Document icon for root node */
    .", css_class, " li.document-root::after {
      content: '';
      background: #ddd;
      border-radius: 50%;
      width: calc(2 * var(--radius));
      height: calc(2 * var(--radius));
    }
    
    .", css_class, " .tree-node-content {
      display: flex;
      align-items: center;
      justify-content: flex-start;
      padding: 4px 8px;
      border-radius: 3px;
      margin-bottom: 2px;
      min-height: calc(2 * var(--radius));
    }
    
    .", css_class, " .tree-node-info {
      display: flex;
      align-items: center;
      flex-grow: 1;
    }
    
    .", css_class, " .tree-node-description {
      margin-right: 12px;
      line-height: 1.4;
      color: #333;
    }
  ")))
  
  # Build nested tree structure (read-only) starting at the specified depth
  tree_html = build_simple_tree_level_readonly(tree_items, start_depth, NULL, ns, repo_id)
  
  shiny::tagList(
    tree_css,
    shiny::tags$ul(class = css_class, tree_html)
  )
}

# Build a nested tree structure for display without any interactive elements
#
# tree_items: All tree items
# target_depth: Current depth level to build
# parent_index: Parent node index (NULL for root)
# ns: Shiny namespace function
# repo_id: Character. Unique identifier for the repository to avoid ID collisions

build_simple_tree_level_readonly = function(tree_items, target_depth, parent_index, ns, repo_id = NULL) {
  
  # Find items at this depth with the specified parent
  level_items = tree_items[sapply(tree_items, function(x) {
    x$depth == target_depth && 
    ((is.null(parent_index) && is.null(x$parent_index)) || 
     (!is.null(parent_index) && !is.null(x$parent_index) && x$parent_index == parent_index))
  })]
  
  if (length(level_items) == 0) {
    return(list())
  }
  
  lapply(level_items, function(item) {
    
    # Find children for this item
    children = tree_items[sapply(tree_items, function(x) {
      !is.null(x$parent_index) && x$parent_index == item$index
    })]
    
    has_children = length(children) > 0
    
    # Create node content (read-only with preview)
    if (item$type == "document_root") {
      # Document root node - no buttons, not selectable
      # For read-only tree, we need to use the dot positioning from the CSS
      node_content = shiny::div(
        class = "tree-node-content",
        shiny::div(
          class = "tree-node-info",
          shiny::span(
            item$description, 
            class = "tree-node-description",
            style = "font-weight: bold; color: #333;"
          )
        )
      )
    } else {
      # Regular nodes with preview button only
      # Create unique button ID by including repo_id to avoid collisions
      button_id = if (!is.null(repo_id)) {
        paste0("preview_", repo_id, "_", item$index)
      } else {
        paste0("preview_", item$index)
      }
      
      preview_btn = shiny::actionButton(
        ns(button_id),
        shiny::icon("search"),
        class = "btn-outline-info",
        style = "font-size: 8px; padding: 1px 4px; min-width: 18px; height: 18px; border-width: 1px; margin-left: 8px;",
        title = "Preview content"
      )
      
      node_content = shiny::div(
        class = "tree-node-content",
        shiny::div(
          class = "tree-node-info",
          shiny::span(
            item$description, 
            class = "tree-node-description"
          ),
          preview_btn
        )
      )
    }
    
    if (has_children) {
      # Create nested structure
      child_elements = build_simple_tree_level_readonly(tree_items, target_depth + 1, item$index, ns, repo_id)
      
      li_class = if (item$type == "document_root") "document-root" else NULL
      shiny::tags$li(
        class = li_class,
        node_content,
        shiny::tags$ul(child_elements)
      )
    } else {
      # Leaf node - just the content
      li_class = if (item$type == "document_root") "document-root" else NULL
      shiny::tags$li(
        class = li_class,
        node_content
      )
    }
  })
}

# Create CSS styling for unified tree display
#
# css_class: Character. CSS class name to use
# selection_mode: Character. Selection mode for mode-specific styling

create_unified_tree_css = function(css_class, selection_mode) {
  
  # Base CSS common to all modes
  base_css = paste0("
    .", css_class, " {
      --spacing: 1.5rem;
      --radius: 10px;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      font-size: ", if (selection_mode == "interactive") "13px" else "11px", ";
      list-style: none;
      padding: 0;
      margin: 0;
    }
    
    .", css_class, " li {
      display: block;
      position: relative;
      padding-left: calc(2 * var(--spacing) - var(--radius) - 2px);
    }
    
    .", css_class, " ul {
      margin-left: calc(var(--radius) - var(--spacing));
      padding-left: 0;
      list-style: none;
    }
    
    .", css_class, " ul li {
      border-left: 2px solid #ddd;
    }
    
    .", css_class, " ul li:last-child {
      border-color: transparent;
    }
    
    .", css_class, " ul li::before {
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
    
    .", css_class, " .tree-node-content {
      display: flex;
      align-items: center;
      justify-content: flex-start;
      padding: 4px 8px;
      border-radius: 3px;
      margin-bottom: 2px;
      min-height: calc(2 * var(--radius));
    }
    
    .", css_class, " .tree-node-info {
      display: flex;
      align-items: center;
      flex-grow: 1;
    }
    
    .", css_class, " .tree-node-description {
      margin-right: 12px;
      line-height: 1.4;
      color: #333;
    }
  ")
  
  # Mode-specific CSS
  mode_css = switch(selection_mode,
    "interactive" = paste0("
      /* Interactive mode styling */
      .", css_class, " .tree-toggle-btn {
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
      
      .", css_class, " .tree-toggle-btn:hover {
        background: #007bff;
        color: white;
        border-color: #0056b3;
      }
      
      .", css_class, " .tree-toggle-btn:focus {
        outline: 2px solid #28a745;
        outline-offset: 1px;
      }
      
      .", css_class, " .tree-toggle-btn.selected {
        background: #28a745;
        color: white;
        border-color: #1e7e34;
      }
      
      .", css_class, " .tree-node-description.selected {
        background-color: #28a745;
        color: white;
        padding-left: 4px;
        padding-right: 4px;
        border-radius: 3px;
      }
      
      .", css_class, " .tree-node-description-btn {
        margin-right: 12px;
        line-height: 1.4;
        text-decoration: none;
        outline: none;
        box-shadow: none;
      }
      
      .", css_class, " .tree-node-description-btn:hover {
        background-color: #f8f9fa !important;
        border-radius: 3px;
      }
      
      .", css_class, " .tree-node-description-btn:focus {
        outline: 2px solid #28a745;
        outline-offset: 1px;
      }
      
      .", css_class, " .tree-node-description-btn.selected {
        background-color: #28a745 !important;
        color: white !important;
        padding-left: 4px;
        padding-right: 4px;
        border-radius: 3px;
      }
    "),
    "readonly" = paste0("
      /* Read-only mode styling */
      .", css_class, " li::after {
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
      
      /* Document icon for root node */
      .", css_class, " li.document-root::after {
        content: '';
        background: #ddd;
        border-radius: 50%;
        width: calc(2 * var(--radius));
        height: calc(2 * var(--radius));
      }
    "),
    "highlight_only" = paste0("
      /* Highlight-only mode styling */
      .", css_class, " li::after {
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
      
      .", css_class, " .tree-node-description.selected {
        background-color: #28a745;
        color: white;
        padding-left: 4px;
        padding-right: 4px;
        border-radius: 3px;
      }
      
      /* Document icon for root node */
      .", css_class, " li.document-root::after {
        content: '';
        background: #ddd;
        border-radius: 50%;
        width: calc(2 * var(--radius));
        height: calc(2 * var(--radius));
      }
    ")
  )
  
  shiny::tags$style(shiny::HTML(paste0(base_css, mode_css)))
}

# Build a nested tree structure supporting different selection modes
#
# tree_items: All tree items
# target_depth: Current depth level to build
# parent_index: Parent node index (NULL for root)
# all_selected_nodes: Vector of all selected node indices
# ns: Shiny namespace function
# directly_selected_nodes: Vector of directly selected node indices
# selection_mode: Character. Selection mode
# id_prefix: Character. Optional prefix for button IDs

build_unified_tree_level = function(tree_items, target_depth, parent_index, all_selected_nodes, ns, directly_selected_nodes = integer(0), selection_mode = "readonly", id_prefix = NULL) {
  
  # Find items at this depth with the specified parent
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
    is_directly_selected = item$index %in% directly_selected_nodes
    is_indirectly_selected = is_selected && !is_directly_selected
    
    # Find children for this item
    children = tree_items[sapply(tree_items, function(x) {
      !is.null(x$parent_index) && x$parent_index == item$index
    })]
    
    has_children = length(children) > 0
    
    # Create node content based on mode and node type
    if (item$type == "document_root") {
      # Document root node
      if (selection_mode == "interactive") {
        # Interactive mode - positioned icon similar to tree-toggle-btn
        document_icon = shiny::div(
          class = "tree-toggle-btn",
          style = "background: #ddd; border: 1px solid #bbb; cursor: default; pointer-events: none;"
        )
        
        node_content = shiny::div(
          class = "tree-node-content",
          document_icon,
          shiny::div(
            class = "tree-node-info",
            shiny::span(
              item$description, 
              class = "tree-node-description",
              style = "font-weight: bold; color: #333;"
            )
          )
        )
      } else {
        # Read-only modes - use CSS ::after for icon
        node_content = shiny::div(
          class = "tree-node-content",
          shiny::div(
            class = "tree-node-info",
            shiny::span(
              item$description, 
              class = "tree-node-description",
              style = "font-weight: bold; color: #333;"
            )
          )
        )
      }
    } else {
      # Regular nodes
      # Create preview button (always available)
      preview_button_id = if (!is.null(id_prefix)) {
        paste0("preview_", id_prefix, "_", item$index)
      } else {
        paste0("preview_", item$index)
      }
      
      preview_btn = shiny::actionButton(
        ns(preview_button_id),
        shiny::icon("search"),
        class = "btn-outline-info",
        style = "font-size: 8px; padding: 1px 4px; min-width: 18px; height: 18px; border-width: 1px; margin-left: 8px;",
        title = "Preview content"
      )
      
      if (selection_mode == "interactive") {
        # Interactive mode - full selection functionality
        is_selectable_heading = item$type == "pandoc_header" && !has_selected_ancestor(tree_items, item$index, directly_selected_nodes)
        
        if (is_selectable_heading) {
          # Create toggle button for tree structure  
          button_icon = if(is_directly_selected) {
            shiny::icon("check")
          } else if(is_indirectly_selected) {
            shiny::icon("ellipsis-v") 
          } else {
            ""
          }
          
          toggle_button = shiny::actionButton(
            ns(paste0("select_children_", item$index)),
            button_icon,
            class = paste("tree-toggle-btn", if(is_selected) "selected" else ""),
            title = "Toggle this node and its children"
          )
          
          node_content = shiny::div(
            class = "tree-node-content",
            toggle_button,
            shiny::div(
              class = "tree-node-info",
              shiny::actionButton(
                ns(paste0("select_", item$index)),
                item$description,
                class = paste("tree-node-description-btn", if(is_selected) "selected" else ""),
                style = "background: none; border: none; padding: 0; margin: 0; font: inherit; cursor: pointer; text-align: left; color: inherit;"
              ),
              preview_btn
            )
          )
        } else {
          # Non-selectable nodes in interactive mode
          indicator_icon = if(is_indirectly_selected) {
            shiny::icon("ellipsis-v")
          } else if(is_directly_selected) {
            shiny::icon("check") 
          } else {
            ""
          }
          
          selection_indicator = shiny::div(
            class = paste("tree-toggle-btn", if(is_selected) "selected" else ""),
            style = "cursor: default; pointer-events: none;",
            indicator_icon,
            title = if(is_selected) "Selected via parent heading" else "Non-selectable node"
          )
          
          text_class = paste("tree-node-description", if(is_selected) "selected" else "")
          
          node_content = shiny::div(
            class = "tree-node-content",
            selection_indicator,
            shiny::div(
              class = "tree-node-info",
              shiny::span(
                item$description,
                class = text_class,
                style = "margin-right: 12px; line-height: 1.4;"
              ),
              preview_btn
            )
          )
        }
      } else {
        # Read-only modes - no interactive elements except preview
        text_class = paste("tree-node-description", if(is_selected) "selected" else "")
        
        node_content = shiny::div(
          class = "tree-node-content",
          shiny::div(
            class = "tree-node-info",
            shiny::span(
              item$description, 
              class = text_class
            ),
            preview_btn
          )
        )
      }
    }
    
    if (has_children) {
      # Create nested structure
      child_elements = build_unified_tree_level(tree_items, target_depth + 1, item$index, all_selected_nodes, ns, directly_selected_nodes, selection_mode, id_prefix)
      
      li_class = if (item$type == "document_root") "document-root" else NULL
      shiny::tags$li(
        class = li_class,
        node_content,
        shiny::tags$ul(child_elements)
      )
    } else {
      # Leaf node - just the content
      li_class = if (item$type == "document_root") "document-root" else NULL
      shiny::tags$li(
        class = li_class,
        node_content
      )
    }
  })
}

