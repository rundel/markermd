# Add a question in the template app through the Add question modal
#
# app: The shinytest2 AppDriver
# file: The file to cover, or NULL for the Quarto document
# coverage: "sections" or "document" for a question on the Quarto document
# name: The question's name, or NULL for the default

add_entry = function(app, file = NULL, coverage = "sections", name = NULL) {
  app$click("add_question")
  app$wait_for_js("!!document.getElementById('confirm_add_entry')")
  if (!is.null(file)) app$set_inputs(new_entry_file = file)
  if (!identical(coverage, "sections")) app$set_inputs(new_entry_coverage = coverage)
  if (!is.null(name)) app$set_inputs(new_entry_name = name)
  app$click("confirm_add_entry")
  app$wait_for_js("!document.getElementById('confirm_add_entry')")
  app$wait_for_idle()
}
