#' Read-Only AST Module
#'
#' AST tree display without selection capabilities

#' Read-Only AST UI
#'
#' @param id Character. Module namespace ID
#' @param title Character. Panel title (default: "Document Structure")
#'
ast_readonly_ui = function(id, title = "Document Structure") {
  ast_base_ui(id, title = title, show_clear_button = FALSE)
}

#' Read-Only AST Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param selected_nodes Reactive. Optional selected nodes to highlight (default: none)
#' @param enable_preview Logical. Whether to enable preview functionality (default: TRUE)
#'
ast_readonly_server = function(id, ast, selected_nodes = shiny::reactive(integer(0)), enable_preview = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Use base AST server with read-only functionality
    base_result = ast_base_server(id, ast, selected_nodes, enable_preview = enable_preview)
    
    # Return the base results (no additional functionality needed for read-only)
    return(base_result)
  })
}