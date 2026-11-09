# Change one field of a group's settings popover the way a user does: set the
# control's value and fire its change event, which the delegated handler
# (rubric_groups_js()) reports as a group_event. The controls exist in the
# page whether or not the popover is open.
#
# app: AppDriver
# group: The group id
# field: "label", "floor", "ceiling" or "exclusive"
# value: The new value (logical for exclusive, NA or "" for a blank bound)
# view: "grade" or "editor"
group_setting = function(app, group, field, value, view = "grade") {
  selector = paste0('[data-settings-for="rubric_module-', view, "-", group, '-container"] [data-field="', field, '"]')
  set = if (is.logical(value) && !is.na(value)) {
    paste0("el.checked = ", tolower(value), ";")
  } else {
    paste0("el.value = ", jsonlite::toJSON(if (is.na(value)) "" else as.character(value), auto_unbox = TRUE), ";")
  }
  app$run_js(paste0(
    "var el = document.querySelector('", selector, "'); ", set,
    " el.dispatchEvent(new Event('change', {bubbles: true}));"
  ))
  app$wait_for_idle()
}

# A template with the given question names and a trivial AST, for tests that
# need a markermd_template but not a real document (the group tests' initial
# database state and Assignments table checks)
mt_template_for_groups = function(names) {
  qs = lapply(seq_along(names), function(i) markermd_question(id = as.integer(i), name = names[i]))
  markermd_template(original_ast = q2r::parse_qmd("# x", quiet = TRUE), questions = qs)
}
