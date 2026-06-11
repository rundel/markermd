# Filter Module
#
# UI builders for question filters. Filters are groups of conditions: the
# conditions within a group are ANDed, groups are ORed, and the resulting q2r
# predicate narrows the question's node set before its rules evaluate. Like
# the rule UI (mod_rule.R) these are direct-input renderers - inputs are
# namespaced with structured ids and managed by the question module's server.

# Build the UI row for a single filter condition
#
# condition: markermd_filter_condition S7 object used to set selected values
# input_id: Function. Maps a bare field name (e.g. "type") to the final input
#   id used for that control.
# show_and: Logical. Whether to show the leading "and" connector label (TRUE
#   for every condition after the first in a group)

filter_condition_ui = function(condition, input_id, show_and = FALSE) {
  shiny::div(
    class = "filter-condition",
    style = "margin: 2px 0; padding: 2px 8px; font-size: 12px;",

    shiny::div(
      style = "display: flex; align-items: center; gap: 4px; width: 100%;",

      # "and" connector between conditions in a group
      shiny::div(
        style = "flex: 0 0 28px; text-align: right;",
        if (show_and) shiny::span("and", class = "text-muted small")
      ),

      # Negation toggle (logical NOT in front of this condition), rendered as
      # a checkbox with its "not" label stacked underneath (see the
      # .filter-not-toggle CSS) so the row stays as short as the selects
      bslib::tooltip(
        shiny::div(
          class = "filter-not-toggle",
          style = "flex: 0 0 30px;",
          shiny::checkboxInput(
            input_id("negate"),
            "not",
            value = condition@negate,
            width = "100%"
          )
        ),
        "Excludes nodes matching this condition (logical NOT)"
      ),

      # Condition type selection; the labels carry each type's matching
      # semantics (exact / regex / glob / option syntax)
      shiny::div(
        style = "flex: 0 0 30%;",
        shiny::selectInput(
          input_id("type"),
          NULL,
          choices = filter_condition_type_choices(),
          selected = condition@type,
          width = "100%",
          selectize = FALSE
        )
      ),

      # Type-dependent value input
      shiny::div(
        style = "flex: 1;",
        create_filter_value_input(condition@type, condition@value, input_id("value"))
      ),

      # Delete condition button
      shiny::div(
        style = "flex: 0 0 auto;",
        shiny::actionButton(
          input_id("delete"),
          shiny::icon("trash-alt"),
          class = "btn-outline-danger btn-sm",
          style = "font-size: 10px; padding: 0; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center;",
          title = "Delete Condition"
        )
      )
    )
  )
}

# Create the value input control for a filter condition type
#
# type: Character. The condition type
# value: Character. Current condition value
# value_id: Character. The fully-resolved input id for the value control

create_filter_value_input = function(type, value, value_id) {
  input_wrapper = function(input_element) {
    shiny::div(
      style = "font-size: 12px;",
      input_element
    )
  }

  if (type == "node type") {
    # Multi-select: the selected kinds are ORed within this one condition,
    # mirroring the rule row's node-type control (the kinds are pairwise
    # disjoint, so a second ANDed node-type condition could never match)
    input_wrapper(
      shiny::selectizeInput(
        value_id,
        NULL,
        choices = setdiff(get_allowed_node_types(), "Any node"),
        selected = value,
        multiple = TRUE,
        width = "100%",
        options = list(
          placeholder = "node types (any of)",
          # Render the menu on <body> so it floats above the question card
          # instead of being clipped by the scrolling questions container.
          dropdownParent = "body",
          plugins = list("remove_button")
        )
      )
    )
  } else {
    placeholder = switch(type,
      "has class" = "class name",
      "has id" = "id",
      "has text" = "regex pattern",
      "has label" = "glob pattern",
      "has option" = "eval or eval: false",
      "has engine" = "engine (e.g. r)",
      ""
    )
    input_wrapper(
      shiny::textInput(
        value_id,
        NULL,
        value = value,
        width = "100%",
        placeholder = placeholder
      )
    )
  }
}

# Build the UI for one filter group: its condition rows plus a footer with the
# add-condition (AND) button, the group negation toggle, and the delete-group
# button
#
# group_id: Character. The group ID used in input naming
# group: Keyed group entry: list(negate = logical, conditions = named list of
#   markermd_filter_condition objects keyed by condition id "1".."k")
# ns: Function. Namespace function for proper input scoping

filter_group_ui = function(group_id, group, ns) {
  conditions = group$conditions
  cond_ids = names(conditions)

  cond_uis = lapply(seq_along(cond_ids), function(i) {
    input_id = function(name) ns(paste0("filter_", group_id, "_", cond_ids[i], "-", name))
    filter_condition_ui(conditions[[cond_ids[i]]], input_id, show_and = i > 1)
  })

  shiny::div(
    class = "filter-group border rounded p-1 mb-1",

    # Group header: negation toggle and delete button
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center; padding: 0 8px;",

      # Group negation toggle (logical NOT around the whole group)
      bslib::tooltip(
        shiny::div(
          style = "font-size: 12px;",
          shiny::checkboxInput(
            ns(paste0("filter_group_", group_id, "-negate")),
            "not group",
            value = group$negate,
            width = "100%"
          )
        ),
        "Excludes nodes matching ALL conditions below (negates the whole group)"
      ),

      # Outline X distinguishes deleting the whole group from both the
      # per-condition trash buttons and the question's filled delete button
      shiny::actionButton(
        ns(paste0("filter_group_", group_id, "-delete")),
        shiny::icon("times"),
        class = "btn-outline-danger btn-sm",
        style = "font-size: 12px; padding: 2px 6px; border-radius: 20%; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center; line-height: 1;",
        title = "Delete Filter Group"
      )
    ),

    shiny::tagList(cond_uis),

    shiny::div(
      style = "display: flex; align-items: center; justify-content: center; padding: 0 8px;",
      shiny::actionButton(
        ns(paste0("filter_group_", group_id, "-add_condition")),
        "and",
        icon = shiny::icon("plus"),
        class = "btn-outline-primary btn-sm",
        style = "font-size: 11px; padding: 2px 8px;",
        title = "Add a condition that must also match (AND)"
      )
    )
  )
}

# Render all of a question's filter groups, interleaved with "or" separators
#
# groups: Named list of keyed group entries (each list(negate, conditions)),
#   keyed by group id ("1".."g")
# ns: Function. Namespace function for proper input scoping

filters_ui_direct = function(groups, ns) {
  group_ids = names(groups)

  parts = list()
  for (i in seq_along(group_ids)) {
    if (i > 1) {
      parts[[length(parts) + 1]] = shiny::div(
        class = "text-center text-muted small my-1",
        "or"
      )
    }
    parts[[length(parts) + 1]] = filter_group_ui(group_ids[i], groups[[group_ids[i]]], ns)
  }

  shiny::tagList(parts)
}
