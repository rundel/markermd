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
    ast = parsermd::parse_rmd(file_path)
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
  
  # Handle new parsermd structure with nodes slot
  nodes = if (inherits(ast, "rmd_ast") && !is.null(ast@nodes)) {
    ast@nodes
  } else {
    ast
  }
  
  if (length(nodes) == 0) {
    return("No document loaded")
  }
  
  # This is a placeholder - in a real implementation we would
  # create a detailed tree view similar to parsermd::print_tree()
  tree_lines = character()
  
  for (i in seq_along(nodes)) {
    node = nodes[[i]]
    node_type = class(node)[1]
    
    # Create a simple representation
    if (grepl("yaml", node_type, ignore.case = TRUE)) {
      tree_lines = c(tree_lines, paste0(i, ". YAML Header"))
    } else if (grepl("chunk", node_type, ignore.case = TRUE)) {
      engine = if (!is.null(node@engine)) node@engine else "r"
      label = if (!is.null(node@name)) node@name else "unnamed"
      tree_lines = c(tree_lines, paste0(i, ". Code Chunk (", engine, "): ", label))
    } else if (grepl("markdown", node_type, ignore.case = TRUE)) {
      # Get first line of markdown for preview
      first_line = if (!is.null(node@lines) && length(node@lines) > 0) node@lines[1] else ""
      preview = if (nchar(first_line) > 50) {
        paste0(substr(first_line, 1, 47), "...")
      } else {
        first_line
      }
      tree_lines = c(tree_lines, paste0(i, ". Markdown: ", preview))
    } else if (grepl("heading", node_type, ignore.case = TRUE)) {
      heading_text = if (!is.null(node@name)) node@name else "Heading"
      level = if (!is.null(node@level)) node@level else 1
      tree_lines = c(tree_lines, paste0(i, ". Heading [h", level, "]: ", heading_text))
    } else {
      tree_lines = c(tree_lines, paste0(i, ". ", node_type))
    }
  }
  
  return(tree_lines)
}