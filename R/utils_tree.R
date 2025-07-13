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
  heading_stack = list() # Stack to track heading hierarchy: list(level=1, index=2, depth=0)
  
  # Add the root AST node as the first item (using 1-based indexing)
  ast_root = list(
    index = 1,  # Use index 1 for the root AST node
    type = "rmd_ast",
    depth = 0,
    parent_index = NULL,
    description = "Document",
    icon = "document",
    prefix = ""
  )
  tree_items[[1]] = ast_root
  
  for (i in seq_along(nodes)) {
    node = nodes[[i]]
    node_type = class(node)[1]
    
    # Set description and icon based on node type
    if (grepl("yaml", node_type, ignore.case = TRUE)) {
      yaml_fields = if (!is.null(node@yaml)) length(node@yaml) else 0
      description = paste0("YAML [", yaml_fields, " fields]")
      icon = "file-code"
      
    } else if (grepl("heading", node_type, ignore.case = TRUE)) {
      heading_level = if (!is.null(node@level)) node@level else 1
      heading_name = if (!is.null(node@name)) node@name else "Heading"
      description = paste0("Heading [h", heading_level, "] - ", heading_name)
      icon = "header"
      
    } else if (grepl("chunk", node_type, ignore.case = TRUE)) {
      engine = if (!is.null(node@engine)) node@engine else "r"
      options_count = if (!is.null(node@options)) length(node@options) else 0
      code_lines = if (!is.null(node@code)) length(node@code) else 0
      chunk_name = if (!is.null(node@name)) node@name else "unnamed"
      description = paste0("Chunk [", engine, ", ", options_count, " options, ", code_lines, " lines] - ", chunk_name)
      icon = "code"
      
    } else if (grepl("markdown", node_type, ignore.case = TRUE)) {
      line_count = if (!is.null(node@lines)) length(node@lines) else 0
      description = paste0("Markdown [", line_count, " line", if(line_count != 1) "s" else "", "]")
      icon = "file-text"
      
    } else {
      description = node_type
      icon = "file"
    }
    
    # Determine hierarchy depth and parent relationships
    # All original nodes are now at least depth 1 (children of the AST root)
    if (grepl("heading", node_type, ignore.case = TRUE)) {
      heading_level = if (!is.null(node@level)) node@level else 1
      
      # Pop headings from stack that are at same or higher level
      while (length(heading_stack) > 0 && heading_stack[[length(heading_stack)]]$level >= heading_level) {
        heading_stack = heading_stack[-length(heading_stack)]
      }
      
      # Current heading depth is based on remaining stack + 1 (for AST root)
      depth = length(heading_stack) + 1
      parent_index = if (length(heading_stack) > 0) heading_stack[[length(heading_stack)]]$index else 1  # AST root has index 1
      
      # Add current heading to stack (with adjusted index)
      heading_stack[[length(heading_stack) + 1]] = list(level = heading_level, index = i + 1, depth = depth)
      
    } else {
      # Non-heading nodes go under the most recent heading
      if (length(heading_stack) > 0) {
        depth = heading_stack[[length(heading_stack)]]$depth + 1
        parent_index = heading_stack[[length(heading_stack)]]$index
      } else {
        depth = 1  # Direct children of AST root
        parent_index = 1  # AST root has index 1
      }
    }
    
    # Create tree item (with adjusted index)
    item = list(
      index = i + 1,  # Adjust index to account for AST root at index 1
      type = node_type,
      depth = depth,
      parent_index = parent_index,
      description = description,
      icon = icon,
      prefix = "├── " # Will be updated later
    )
    
    tree_items[[i + 1]] = item  # +1 because AST root is at index 1
  }
  
  # Calculate proper prefixes based on tree structure
  for (i in seq_along(tree_items)) {
    item = tree_items[[i]]
    depth = item$depth
    
    # Find if this is the last child at its depth under its parent
    is_last_child = TRUE
    
    for (j in (i + 1):length(tree_items)) {
      if (j > length(tree_items)) break
      next_item = tree_items[[j]]
      
      # If we find another item at same depth with same parent, this is not the last
      if (next_item$depth == depth) {
        # Check if they have the same parent
        if ((is.null(item$parent_index) && is.null(next_item$parent_index)) ||
            (!is.null(item$parent_index) && !is.null(next_item$parent_index) && 
             item$parent_index == next_item$parent_index)) {
          is_last_child = FALSE
          break
        }
      }
      
      # If we encounter an item at lower depth, we've moved to a different section
      if (next_item$depth < depth) {
        break
      }
    }
    
    # Build prefix based on depth and position
    if (depth == 0) {
      prefix = if (is_last_child) "└── " else "├── "
    } else {
      # Build the indentation string
      indent_parts = character(depth)
      
      # Look back through the tree to determine which levels need vertical bars
      current_parent = item$parent_index
      current_depth = depth
      
      while (current_depth > 0 && !is.null(current_parent)) {
        # Check if the parent at this level has more siblings after it
        parent_has_more_siblings = FALSE
        
        if (current_parent < length(tree_items)) {
          parent_item = tree_items[[current_parent]]
          parent_depth = parent_item$depth
          parent_parent = parent_item$parent_index
          
          for (k in (current_parent + 1):length(tree_items)) {
            if (k > length(tree_items)) break
            check_item = tree_items[[k]]
            
            if (check_item$depth == parent_depth) {
              # Check if same parent
              if ((is.null(parent_parent) && is.null(check_item$parent_index)) ||
                  (!is.null(parent_parent) && !is.null(check_item$parent_index) && 
                   parent_parent == check_item$parent_index)) {
                parent_has_more_siblings = TRUE
                break
              }
            } else if (check_item$depth < parent_depth) {
              break
            }
          }
        }
        
        indent_parts[current_depth] = if (parent_has_more_siblings) "│   " else "    "
        
        # Move up to parent's parent
        if (current_parent <= length(tree_items)) {
          parent_item = tree_items[[current_parent]]
          current_parent = parent_item$parent_index
          current_depth = current_depth - 1
        } else {
          break
        }
      }
      
      # Combine indentation with final branch
      prefix = paste0(paste(indent_parts, collapse = ""), if (is_last_child) "└── " else "├── ")
    }
    
    tree_items[[i]]$prefix = prefix
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
    
    /* Add simple dots for all nodes (no expand/collapse functionality) */
    .ast-tree li::after {
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
      background-color: #007bff;
      color: white;
      padding-left: 4px;
      padding-right: 4px;
      border-radius: 3px;
    }
    
    .ast-tree .tree-node-actions {
      display: flex;
      align-items: center;
      gap: 4px;
      opacity: 0.7;
      transition: opacity 0.2s ease;
    }
    
    .ast-tree .tree-node-content:hover .tree-node-actions {
      opacity: 1;
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
    
    # Create node content - special handling for AST root
    if (item$type == "rmd_ast") {
      # AST root node - no action buttons, not selectable
      node_content = shiny::div(
        class = "tree-node-content",
        shiny::div(
          class = "tree-node-info",
          shiny::span(
            item$description, 
            class = "tree-node-description"
          )
        )
      )
    } else {
      # Regular nodes with action buttons
      action_buttons = create_tree_action_buttons(item, has_children, ns)
      node_content = shiny::div(
        class = "tree-node-content",
        shiny::div(
          class = "tree-node-info",
          shiny::span(
            item$description, 
            class = paste("tree-node-description", if(is_selected) "selected" else "")
          )
        ),
        shiny::div(class = "tree-node-actions", action_buttons)
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

#' Create Tree Action Buttons
#'
#' Create action buttons for tree nodes (select, select+children, preview)
#'
#' @param item Tree item
#' @param has_children Whether the node has children
#' @param ns Shiny namespace function
#'
#' @return List of action button elements
#'
create_tree_action_buttons = function(item, has_children, ns) {
  
  select_btn = shiny::actionButton(
    ns(paste0("select_", item$index)),
    shiny::icon("check"),
    class = "btn-outline-primary",
    style = "font-size: 8px; padding: 1px 4px; min-width: 18px; height: 18px; border-width: 1px;",
    title = "Toggle selection"
  )
  
  select_children_btn = if (has_children) {
    shiny::actionButton(
      ns(paste0("select_children_", item$index)),
      shiny::icon("check-double"),
      class = "btn-outline-success",
      style = "font-size: 8px; padding: 1px 4px; min-width: 18px; height: 18px; border-width: 1px;",
      title = "Toggle this node and its children"
    )
  } else {
    NULL
  }
  
  preview_btn = shiny::actionButton(
    ns(paste0("preview_", item$index)),
    shiny::icon("search"),
    class = "btn-outline-info",
    style = "font-size: 8px; padding: 1px 4px; min-width: 18px; height: 18px; border-width: 1px;",
    title = "Preview content"
  )
  
  list(select_btn, select_children_btn, preview_btn)
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
find_node_children = function(tree_items, parent_index) {
  
  if (parent_index > length(tree_items)) {
    return(integer(0))
  }
  
  parent_item = tree_items[[parent_index]]
  children = integer(0)
  
  # Look for nodes that come after the parent and have higher depth
  for (i in (parent_index + 1):length(tree_items)) {
    if (i > length(tree_items)) break
    
    item = tree_items[[i]]
    
    # If we encounter a node at same or lower depth, we've moved past children
    if (item$depth <= parent_item$depth) {
      break
    }
    
    # This is a child node (directly or indirectly)
    children = c(children, item$index)
  }
  
  return(children)
}

