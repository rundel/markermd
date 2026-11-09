# Add a question in the template app through the Add question modal
#
# app: The shinytest2 AppDriver
# file: The file to cover, or NULL for the Quarto document
# coverage: "sections" or "document" for a question on the Quarto document
# name: The question's name, or NULL for the default

# Click a node's selection circle in the template app's document tree. The
# tree's buttons are plain markup reported through one delegated input, so the
# click goes through the DOM rather than set_inputs().
#
# app: The shinytest2 AppDriver
# index: The node's tree index

click_tree_node = function(app, index) {
  app$click(selector = paste0("#ast_panel-select_children_", index))
  app$wait_for_idle()
}

# The template app's document tree as the browser currently shows it, after
# any in-place highlight patches (the tree output itself is rendered once)
#
# app: The shinytest2 AppDriver

tree_dom_html = function(app) {
  app$get_js("document.getElementById('ast_panel-ast_tree_ui').innerHTML")
}

add_entry = function(app, file = NULL, coverage = "sections", name = NULL) {
  app$wait_for_js("!!document.querySelector('#add_question.shiny-bound-input')")
  app$click("add_question")
  app$wait_for_js("!!document.getElementById('confirm_add_entry')")
  if (!is.null(file)) app$set_inputs(new_entry_file = file)
  if (!identical(coverage, "sections")) app$set_inputs(new_entry_coverage = coverage)
  if (!is.null(name)) app$set_inputs(new_entry_name = name)
  app$click("confirm_add_entry")
  app$wait_for_js("!document.getElementById('confirm_add_entry')")
  app$wait_for_idle()
}
