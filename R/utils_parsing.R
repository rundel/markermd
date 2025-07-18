#' Document Parsing Utilities
#'
#' Functions for parsing Rmd/qmd documents using parsermd

#' Parse Assignment Document
#'
#' Parse an Rmd or qmd document into an AST using parsermd
#'
#' @param file_path Character. Path to the assignment file
#'
#' @return parsermd AST object
#'
parse_assignment_document = function(file_path) {
  
  if (!file.exists(file_path)) {
    stop("Assignment file does not exist: ", file_path)
  }
  
  # Load parsermd package (required for internal functions)
  if (!requireNamespace("parsermd", quietly = TRUE)) {
    stop("parsermd package is required but not available")
  }
  
  # Parse the document
  tryCatch({
    # Load parsermd to make internal functions available
    library(parsermd, quietly = TRUE)
    
    # Use appropriate parser based on file extension
    if (grepl("\\.qmd$", file_path, ignore.case = TRUE)) {
      ast = parsermd::parse_qmd(file_path)
    } else {
      ast = parsermd::parse_rmd(file_path)
    }
    return(ast)
  }, error = function(e) {
    stop("Failed to parse assignment document: ", e$message)
  })
}

#' Get Document Summary
#'
#' Extract basic information about the parsed document
#'
#' @param ast parsermd AST object
#'
#' @return List with document summary information
#'
get_document_summary = function(ast) {
  
  if (is.null(ast)) {
    return(list(
      total_nodes = 0,
      node_types = character(0),
      has_yaml = FALSE,
      code_chunks = 0,
      markdown_sections = 0
    ))
  }
  
  # Handle new parsermd structure with nodes slot
  nodes = if (inherits(ast, "rmd_ast") && !is.null(ast@nodes)) {
    ast@nodes
  } else {
    ast
  }
  
  # Count different node types
  node_types = sapply(nodes, function(x) class(x)[1])
  
  summary_info = list(
    total_nodes = length(nodes),
    node_types = table(node_types),
    has_yaml = any(grepl("yaml", node_types, ignore.case = TRUE)),
    code_chunks = sum(grepl("chunk", node_types, ignore.case = TRUE)),
    markdown_sections = sum(grepl("markdown", node_types, ignore.case = TRUE))
  )
  
  return(summary_info)
}

#' Create AST Tree Display
#'
#' Create a text representation of the AST similar to parsermd::print_tree()
#'
#' @param ast parsermd AST object
#'
#' @return Character vector with tree representation
#'
create_ast_tree_display = function(ast) {
  
  if (is.null(ast)) {
    return("No document loaded")
  }
  
  # Use the tree structure builder
  tree_items = build_ast_tree_structure(ast)
  
  if (length(tree_items) == 0) {
    return("No document loaded")
  }
  
  # Create text representation similar to parsermd output
  tree_lines = sapply(tree_items, function(item) {
    paste0(item$prefix, item$description)
  })
  
  return(tree_lines)
}