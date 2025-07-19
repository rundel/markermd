#' Tree Display Utilities
#'
#' Functions for creating hierarchical tree displays of AST structures

#' Build Tree Structure from AST
#'
#' Create a hierarchical tree structure similar to parsermd's print output
#'
#' @param ast parsermd AST object
#'
#' @return List with tree structure information
#'
build_ast_tree_structure = function(ast) {
  
  if (is.null(ast)) {
    return(list())
  }
  
  # Handle new parsermd structure with nodes slot
  nodes = if (inherits(ast, "rmd_ast") && !is.null(ast@nodes)) {
    ast@nodes
  } else {
    ast
  }
  
  if (length(nodes) == 0) {
    return(list())
  }
  
  tree_items = list()
  heading_stack = list() # Stack to track heading hierarchy
  
  # Add a visual root node for display purposes (not selectable)
  document_root = list(
    index = 0,  # Use index 0 to distinguish from actual nodes
    type = "document_root",
    depth = 0,
    parent_index = NULL,
    description = "Document",
    prefix = ""
  )
  tree_items[[1]] = document_root
  
  for (i in seq_along(nodes)) {
    node = nodes[[i]]
    node_type = class(node)[1]
    
    # Use parsermd's tree_node method to get proper description
    node_info = parsermd:::tree_node(node)
    # Remove ASCII/ANSI escape codes from the text components
    clean_text = gsub("\033\\[[0-9;]*m", "", node_info$text)
    clean_label = gsub("\033\\[[0-9;]*m", "", node_info$label)
    description = paste(clean_text, clean_label)
    
    # Determine hierarchy depth and parent relationships
    if (grepl("heading", node_type, ignore.case = TRUE)) {
      heading_level = if (!is.null(node@level)) node@level else 1
      
      # Pop headings from stack that are at same or higher level
      while (length(heading_stack) > 0 && heading_stack[[length(heading_stack)]]$level >= heading_level) {
        heading_stack = heading_stack[-length(heading_stack)]
      }
      
      # Current heading depth is based on remaining stack + 1 (for document root)
      depth = length(heading_stack) + 1
      parent_index = if (length(heading_stack) > 0) heading_stack[[length(heading_stack)]]$index else 0  # Document root has index 0
      
      # Add current heading to stack
      heading_stack[[length(heading_stack) + 1]] = list(level = heading_level, index = i, depth = depth)
      
    } else {
      # Non-heading nodes go under the most recent heading
      if (length(heading_stack) > 0) {
        depth = heading_stack[[length(heading_stack)]]$depth + 1
        parent_index = heading_stack[[length(heading_stack)]]$index
      } else {
        depth = 1  # Direct children of document root
        parent_index = 0  # Document root has index 0
      }
    }
    
    # Create tree item using direct node index
    item = list(
      index = i,  # Use direct node index
      type = node_type,
      depth = depth,
      parent_index = parent_index,
      description = description,
      prefix = "├── " # Will be updated later
    )
    
    tree_items[[i + 1]] = item  # Store at i+1 because document root is at index 1
  }
  
  # Set appropriate prefixes for all items
  for (i in seq_along(tree_items)) {
    if (tree_items[[i]]$type == "document_root") {
      tree_items[[i]]$prefix = "📄 "  # Document icon for root
    } else {
      tree_items[[i]]$prefix = "├── "
    }
  }
  
  return(tree_items)
}

#' Create Simple Tree View using ul/li elements
#'
#' Create a simple flat tree view for AST nodes
#'
#' @param tree_items List of tree items from build_ast_tree_structure
#' @param selected_nodes Vector of selected node indices
#' @param ns Shiny namespace function
#'
#' @return Shiny UI element with simple tree
#'
create_simple_tree = function(tree_items, selected_nodes, ns) {
  
  if (length(tree_items) == 0) {
    return(shiny::p("No document structure available"))
  }
  
  # Create tree CSS using iamkate.com styling (without collapsible functionality)
  tree_css = shiny::tags$style(shiny::HTML("
    .ast-tree {
      --spacing: 1.5rem;
      --radius: 10px;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      font-size: 13px;
      list-style: none;
      padding: 0;
      margin: 0;
    }
    
    .ast-tree li {
      display: block;
      position: relative;
      padding-left: calc(2 * var(--spacing) - var(--radius) - 2px);
    }
    
    .ast-tree ul {
      margin-left: calc(var(--radius) - var(--spacing));
      padding-left: 0;
      list-style: none;
    }
    
    .ast-tree ul li {
      border-left: 2px solid #ddd;
    }
    
    .ast-tree ul li:last-child {
      border-color: transparent;
    }
    
    .ast-tree ul li::before {
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
    
    /* Style for tree toggle buttons */
    .ast-tree .tree-toggle-btn {
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
    
    .ast-tree .tree-toggle-btn:hover {
      background: #007bff;
      color: white;
      border-color: #0056b3;
    }
    
    .ast-tree .tree-toggle-btn:focus {
      outline: 2px solid #28a745;
      outline-offset: 1px;
    }
    
    .ast-tree .tree-toggle-btn.selected {
      background: #28a745;
      color: white;
      border-color: #1e7e34;
    }
    
    .ast-tree .tree-node-content {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 4px 8px;
      border-radius: 3px;
      margin-bottom: 2px;
      min-height: calc(2 * var(--radius));
    }
    
    .ast-tree .tree-node-info {
      display: flex;
      align-items: center;
      flex-grow: 1;
    }
    
    .ast-tree .tree-node-description {
      margin-right: 12px;
      line-height: 1.4;
    }
    
    .ast-tree .tree-node-description.selected {
      background-color: #28a745;
      color: white;
      padding-left: 4px;
      padding-right: 4px;
      border-radius: 3px;
    }
    
    .ast-tree .tree-node-description-btn {
      margin-right: 12px;
      line-height: 1.4;
      text-decoration: none;
      outline: none;
      box-shadow: none;
    }
    
    .ast-tree .tree-node-description-btn:hover {
      background-color: #f8f9fa !important;
      border-radius: 3px;
    }
    
    .ast-tree .tree-node-description-btn:focus {
      outline: 2px solid #28a745;
      outline-offset: 1px;
    }
    
    .ast-tree .tree-node-description-btn.selected {
      background-color: #28a745 !important;
      color: white !important;
      padding-left: 4px;
      padding-right: 4px;
      border-radius: 3px;
    }
    
  "))
  
  # Build nested tree structure (but without collapsible functionality)
  tree_html = build_simple_tree_level(tree_items, 0, NULL, selected_nodes, ns)
  
  shiny::tagList(
    tree_css,
    shiny::tags$ul(class = "ast-tree", tree_html)
  )
}

#' Build Simple Tree Level (Nested but not collapsible)
#'
#' Build a nested tree structure for proper CSS styling without collapsible functionality
#'
#' @param tree_items All tree items
#' @param target_depth Current depth level to build
#' @param parent_index Parent node index (NULL for root)
#' @param selected_nodes Vector of selected node indices
#' @param ns Shiny namespace function
#'
#' @return List of HTML elements for this level
#'
build_simple_tree_level = function(tree_items, target_depth, parent_index, selected_nodes, ns) {
  
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
    
    is_selected = item$index %in% selected_nodes
    
    # Find children for this item
    children = tree_items[sapply(tree_items, function(x) {
      !is.null(x$parent_index) && x$parent_index == item$index
    })]
    
    has_children = length(children) > 0
    
    # Create node content - special handling for document root
    if (item$type == "document_root") {
      # Document root node - no action buttons, not selectable
      # Create a positioned icon similar to the tree-toggle-btn
      document_icon = shiny::div(
        class = "tree-toggle-btn",
        style = "background: transparent; border: none; cursor: default; pointer-events: none; font-size: 16px;",
        shiny::HTML("📄")
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
      # Create toggle button for tree structure
      toggle_button = shiny::actionButton(
        ns(paste0("select_children_", item$index)),
        if(is_selected) shiny::icon("check") else "",
        class = paste("tree-toggle-btn", if(is_selected) "selected" else ""),
        title = "Toggle this node and its children"
      )
      
      # Create preview button
      preview_btn = shiny::actionButton(
        ns(paste0("preview_", item$index)),
        shiny::icon("search"),
        class = "btn-outline-info",
        style = "font-size: 8px; padding: 1px 4px; min-width: 18px; height: 18px; border-width: 1px; margin-left: 8px;",
        title = "Preview content"
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
    }
    
    if (has_children) {
      # Create nested structure (but not collapsible)
      child_elements = build_simple_tree_level(tree_items, target_depth + 1, item$index, selected_nodes, ns)
      
      shiny::tags$li(
        node_content,
        shiny::tags$ul(child_elements)
      )
    } else {
      # Leaf node - just the content
      shiny::tags$li(node_content)
    }
  })
}


#' Create Tree Connector using CSS/SVG
#'
#' Create visual tree connectors using CSS borders and positioning
#'
#' @param depth Integer depth of the node
#' @param is_last_child Logical indicating if this is the last child
#'
#' @return Shiny UI element with tree connectors
#'
#' Create Tree Connector with Proper Vertical Connectivity
#'
#' Create visual tree connectors with proper vertical line management
#'
#' @param item Tree item with depth and parent info
#' @param tree_items Full list of tree items for context
#' @param current_index Current item's position in the list
#'
#' @return Shiny UI element with tree connectors
#'
create_tree_connector = function(item, tree_items, current_index) {
  
  depth = item$depth
  
  if (depth == 0) {
    # Root level nodes - simple horizontal line
    return(shiny::div(
      style = "width: 20px; height: 20px; position: relative; flex-shrink: 0;",
      shiny::div(
        style = "position: absolute; left: 8px; top: 10px; width: 12px; height: 2px; background-color: #8e9aaf;"
      )
    ))
  }
  
  # Calculate connector width based on depth
  connector_width = depth * 24 + 20
  connectors = list()
  
  # Build the connector by analyzing the text prefix that was already calculated correctly
  prefix = item$prefix
  prefix_chars = strsplit(prefix, "")[[1]]
  
  # The prefix structure is: "│   │   ├── " (no leading space)
  # Each level is 4 characters: either "│   " (continuation) or "    " (gap)
  # Final level ends with "├── " or "└── "
  
  # Find the positions of the tree characters
  continuation_levels = c()
  branch_char = ""
  
  # Parse the prefix to identify vertical continuation lines and branch type  
  if (depth > 0 && length(prefix_chars) > 0) {
    # Check continuation lines for levels 1 to (depth-1)
    if (depth > 1) {
      for (level in 1:(depth-1)) {
        pos = (level - 1) * 4 + 1  # Position for this level
        
        if (pos <= length(prefix_chars)) {
          char = prefix_chars[pos]
          if (char == "│") {
            continuation_levels = c(continuation_levels, level)
          }
        }
      }
    }
    
    # Check branch character at the final position
    branch_pos = depth * 4 + 1  # Branch character position
    if (branch_pos <= length(prefix_chars)) {
      char = prefix_chars[branch_pos]
      if (char %in% c("├", "└")) {
        branch_char = char
      }
    }
  }
  
  # Draw vertical continuation lines for intermediate levels
  for (level in continuation_levels) {
    x_pos = (level - 1) * 24 + 8
    connectors[[length(connectors) + 1]] = shiny::div(
      style = paste0(
        "position: absolute; left: ", x_pos, "px; top: 0; width: 2px; height: 20px; ",
        "background-color: #8e9aaf;"
      )
    )
  }
  
  # Draw the branch connector for the current level
  if (depth > 0 && branch_char != "") {
    x_pos = (depth - 1) * 24 + 8
    
    # Top half - connecting up to parent
    connectors[[length(connectors) + 1]] = shiny::div(
      style = paste0(
        "position: absolute; left: ", x_pos, "px; top: 0; width: 2px; height: 10px; ",
        "background-color: #8e9aaf;"
      )
    )
    
    # Bottom half - only if this is not the last child (├ vs └)
    if (branch_char == "├") {
      connectors[[length(connectors) + 1]] = shiny::div(
        style = paste0(
          "position: absolute; left: ", x_pos, "px; top: 10px; width: 2px; height: 10px; ",
          "background-color: #8e9aaf;"
        )
      )
    }
    
    # Horizontal line to the node
    connectors[[length(connectors) + 1]] = shiny::div(
      style = paste0(
        "position: absolute; left: ", x_pos, "px; top: 10px; width: 14px; height: 2px; ",
        "background-color: #8e9aaf;"
      )
    )
  }
  
  shiny::div(
    style = paste0("width: ", connector_width, "px; height: 20px; position: relative; flex-shrink: 0;"),
    shiny::tagList(connectors)
  )
}

#' Find Children of Node
#'
#' Find all child nodes for a given parent node
#'
#' @param tree_items List of tree items
#' @param parent_index Index of parent node
#'
#' @return Vector of child node indices
#'
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

#' Create Simple Tree View for Read-Only Display
#'
#' Create a simple flat tree view for AST nodes without selection functionality
#'
#' @param tree_items List of tree items from build_ast_tree_structure
#' @param ns Shiny namespace function
#' @param repo_id Character. Unique identifier for the repository to avoid ID collisions
#'
#' @return Shiny UI element with simple tree (read-only)
#'
create_simple_tree_readonly = function(tree_items, ns, repo_id = NULL) {
  
  if (length(tree_items) == 0) {
    return(shiny::p("No document structure available"))
  }
  
  # Create tree CSS using iamkate.com styling (without interactive elements)
  tree_css = shiny::tags$style(shiny::HTML("
    .ast-tree-readonly {
      --spacing: 1.5rem;
      --radius: 10px;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      font-size: 13px;
      list-style: none;
      padding: 0;
      margin: 0;
    }
    
    .ast-tree-readonly li {
      display: block;
      position: relative;
      padding-left: calc(2 * var(--spacing) - var(--radius) - 2px);
    }
    
    .ast-tree-readonly ul {
      margin-left: calc(var(--radius) - var(--spacing));
      padding-left: 0;
      list-style: none;
    }
    
    .ast-tree-readonly ul li {
      border-left: 2px solid #ddd;
    }
    
    .ast-tree-readonly ul li:last-child {
      border-color: transparent;
    }
    
    .ast-tree-readonly ul li::before {
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
    
    /* Simple dots for all nodes (no interaction) */
    .ast-tree-readonly li::after {
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
    .ast-tree-readonly li.document-root::after {
      content: '📄';
      background: transparent;
      border-radius: 0;
      width: calc(2 * var(--radius));
      height: calc(2 * var(--radius));
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 16px;
    }
    
    .ast-tree-readonly .tree-node-content {
      display: flex;
      align-items: center;
      justify-content: flex-start;
      padding: 4px 8px;
      border-radius: 3px;
      margin-bottom: 2px;
      min-height: calc(2 * var(--radius));
    }
    
    .ast-tree-readonly .tree-node-info {
      display: flex;
      align-items: center;
      flex-grow: 1;
    }
    
    .ast-tree-readonly .tree-node-description {
      margin-right: 12px;
      line-height: 1.4;
      color: #333;
    }
  "))
  
  # Build nested tree structure (read-only)
  tree_html = build_simple_tree_level_readonly(tree_items, 0, NULL, ns, repo_id)
  
  shiny::tagList(
    tree_css,
    shiny::tags$ul(class = "ast-tree-readonly", tree_html)
  )
}

#' Build Simple Tree Level (Read-Only)
#'
#' Build a nested tree structure for display without any interactive elements
#'
#' @param tree_items All tree items
#' @param target_depth Current depth level to build
#' @param parent_index Parent node index (NULL for root)
#' @param ns Shiny namespace function
#' @param repo_id Character. Unique identifier for the repository to avoid ID collisions
#'
#' @return List of HTML elements for this level
#'
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

