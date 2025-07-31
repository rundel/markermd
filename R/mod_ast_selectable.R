#' Selectable AST Module
#'
#' AST tree display with node selection capabilities

#' Selectable AST UI
#'
#' @param id Character. Module namespace ID
#' @param title Character. Panel title (default: "Document Structure")
#'
ast_selectable_ui = function(id, title = "Document Structure") {
  ast_base_ui(id, title = title, show_clear_button = TRUE)
}

#' Selectable AST Server
#'
#' @param id Character. Module namespace ID
#' @param ast Reactive. The parsed AST object
#' @param selected_nodes Reactive. Currently selected node indices
#'
ast_selectable_server = function(id, ast, selected_nodes = shiny::reactive(integer(0))) {
  
  # Use base AST server with interactive mode
  ast_base_server(id, ast, selected_nodes, enable_preview = TRUE, selection_mode = "interactive")
}