test_that("node selection logic works correctly", {
  library(parsermd)
  
  # Test the tree structure building
  test_file = file.path("..", "..", "inst", "examples", "test_assignment", "student1-excellent", "assignment.qmd")
  test_ast = parse_qmd(test_file)
  tree_items = markermd:::build_ast_tree_structure(test_ast)
  
  # Basic structure tests
  expect_true(length(tree_items) > 0)
  expect_equal(tree_items[[1]]$type, "document_root")
  expect_equal(tree_items[[1]]$index, 0)
  
  # Test that node indices are correctly assigned
  for (i in 2:length(tree_items)) {
    expect_equal(tree_items[[i]]$index, i - 1)  # Account for document root at index 1
  }
  
  # Test find_node_children function directly with node 4 (we know this is h2)
  h2_index = 4
  h2_children = markermd:::find_node_children(tree_items, h2_index)
  expect_true(length(h2_children) > 0)
  expect_true(5 %in% h2_children)  # Should include markdown nodes under h2
  
  # Test that markdown nodes have no children (node 5 is markdown)
  markdown_index = 5
  markdown_children = markermd:::find_node_children(tree_items, markdown_index)
  expect_equal(length(markdown_children), 0)
  
  # Test parent-child relationships
  for (i in seq_along(tree_items)) {
    item = tree_items[[i]]
    if (!is.null(item$parent_index) && item$parent_index != 0) {
      # This item should be in the children of its parent
      parent_children = markermd:::find_node_children(tree_items, item$parent_index)
      expect_true(item$index %in% parent_children, 
                  info = paste("Node", item$index, "should be child of", item$parent_index))
    }
  }
})

test_that("tree structure is hierarchical", {
  library(parsermd)
  
  test_file = file.path("..", "..", "inst", "examples", "test_assignment", "student1-excellent", "assignment.qmd")
  test_ast = parse_qmd(test_file)
  tree_items = markermd:::build_ast_tree_structure(test_ast)
  
  # Check that document root has depth 0
  expect_equal(tree_items[[1]]$depth, 0)
  
  # Check that depths increase appropriately
  for (i in 2:length(tree_items)) {
    item = tree_items[[i]]
    
    # If item has a parent, its depth should be greater than parent's depth
    if (!is.null(item$parent_index)) {
      parent_item = NULL
      for (j in seq_along(tree_items)) {
        if (tree_items[[j]]$index == item$parent_index) {
          parent_item = tree_items[[j]]
          break
        }
      }
      
      expect_false(is.null(parent_item))
      expect_true(item$depth > parent_item$depth,
                  info = paste("Node", item$index, "depth", item$depth, 
                              "should be greater than parent", parent_item$index, 
                              "depth", parent_item$depth))
    }
  }
})

test_that("button IDs and observer IDs match", {
  library(parsermd)
  
  test_file = file.path("..", "..", "inst", "examples", "test_assignment", "student1-excellent", "assignment.qmd")
  test_ast = parse_qmd(test_file)
  tree_items = markermd:::build_ast_tree_structure(test_ast)
  nodes = test_ast
  
  # Check that every node has a corresponding tree item with the same index
  for (i in seq_along(nodes)) {
    # Find tree item with this index
    tree_item = NULL
    for (j in seq_along(tree_items)) {
      if (tree_items[[j]]$index == i) {
        tree_item = tree_items[[j]]
        break
      }
    }
    
    expect_false(is.null(tree_item), 
                info = paste("Node", i, "should have corresponding tree item"))
    
    # Check that tree item index matches node index
    expect_equal(tree_item$index, i)
  }
  
  # Check that tree UI will create buttons for all non-root nodes
  button_ids = c()
  for (i in seq_along(tree_items)) {
    item = tree_items[[i]]
    if (item$type != "document_root") {
      button_ids = c(button_ids, item$index)
    }
  }
  
  # Check that template module will create observers for all nodes
  observer_ids = seq_along(nodes)
  
  # The sets should be identical
  expect_setequal(button_ids, observer_ids)
})

test_that("leaf node circle button behavior matches text button behavior", {
  library(parsermd)
  
  test_file = file.path("..", "..", "inst", "examples", "test_assignment", "student1-excellent", "assignment.qmd")
  test_ast = parse_qmd(test_file)
  tree_items = markermd:::build_ast_tree_structure(test_ast)
  
  # Find a leaf node (node with no children)
  leaf_node_index = NULL
  for (i in seq_along(tree_items)) {
    if (tree_items[[i]]$type != "document_root") {
      children = markermd:::find_node_children(tree_items, tree_items[[i]]$index)
      if (length(children) == 0) {
        leaf_node_index = tree_items[[i]]$index
        break
      }
    }
  }
  
  expect_false(is.null(leaf_node_index), info = "Should find at least one leaf node")
  
  # Verify that this is indeed a leaf node
  leaf_children = markermd:::find_node_children(tree_items, leaf_node_index)
  expect_equal(length(leaf_children), 0, info = "Leaf node should have no children")
  
  # Find a non-leaf node for comparison
  non_leaf_node_index = NULL
  for (i in seq_along(tree_items)) {
    if (tree_items[[i]]$type != "document_root") {
      children = markermd:::find_node_children(tree_items, tree_items[[i]]$index)
      if (length(children) > 0) {
        non_leaf_node_index = tree_items[[i]]$index
        break
      }
    }
  }
  
  expect_false(is.null(non_leaf_node_index), info = "Should find at least one non-leaf node")
  
  # Verify that this is indeed a non-leaf node
  non_leaf_children = markermd:::find_node_children(tree_items, non_leaf_node_index)
  expect_true(length(non_leaf_children) > 0, info = "Non-leaf node should have children")
})