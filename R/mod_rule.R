# Rule Module
#
# Shiny module for managing a validation rule

# Build the rule item UI for a single validation rule
#
# rule: markermd_rule S7 object. Rule used to set selected values (optional)
# input_id: Function. Maps a bare field name (e.g. "verb") to the final
#   input id used for that control. Allows the same UI to be used both as a
#   Shiny module (namespaced field names) and as directly managed inputs
#   (prefixed field names).
# verb_inputs: Tag. The content rendered in the dynamic verb-inputs region.
#   For the module variant this is a uiOutput placeholder populated by the
#   server; for the direct variant it is the statically rendered inputs.

rule_item_ui = function(rule = NULL, input_id, verb_inputs) {
  if (!is.null(rule) && S7::S7_inherits(rule, markermd_rule)) {
    selected_node_type = rule@node_type
    selected_verb = rule@verb
  } else {
    selected_node_type = get_allowed_node_types()[1]
    selected_verb = get_allowed_rule_verbs()[1]
  }

  shiny::div(
    class = "rule-item",
    style = "margin: 2px 0; padding: 2px 8px; font-size: 12px;",

    # Rule configuration with flexbox layout
    shiny::div(
      style = "display: flex; align-items: center; gap: 4px; width: 100%; position: relative; z-index: 1000;",

      # Node types selection
      shiny::div(
        style = "flex: 0 0 35%; position: relative;",
        shiny::selectInput(
          input_id("node_types"),
          NULL,
          choices = get_allowed_node_types(),
          selected = selected_node_type,
          multiple = FALSE,
          width = "100%",
          selectize = FALSE
        )
      ),

      # Verb selection
      shiny::div(
        style = "flex: 0 0 25%; position: relative;",
        shiny::selectInput(
          input_id("verb"),
          NULL,
          choices = get_allowed_rule_verbs(),
          selected = selected_verb,
          width = "100%",
          selectize = FALSE
        )
      ),

      # Dynamic verb inputs
      shiny::div(
        style = "flex: 1; position: relative;",
        verb_inputs
      ),

      # Delete button
      shiny::div(
        style = "flex: 0 0 auto;",
        shiny::actionButton(
          input_id("delete"),
          shiny::icon("trash-alt"),
          class = "btn-outline-danger btn-sm",
          style = "font-size: 10px; padding: 0; width: 24px; height: 24px; display: flex; align-items: center; justify-content: center;",
          title = "Delete Rule"
        )
      )
    )
  )
}

# Rule UI without server module (direct input handling)
#
# id: Character. The rule ID for input naming
# rule: markermd_rule S7 object. The rule to display
# ns: Function. Namespace function for proper input scoping (optional)

rule_ui_direct = function(id, rule, ns = NULL) {
  ns_func = if (is.null(ns)) function(x) x else ns
  input_id = function(name) ns_func(paste0(id, "-", name))
  rule_item_ui(
    rule,
    input_id = input_id,
    verb_inputs = create_rule_verb_inputs(rule@verb, rule@values, input_id("values"))
  )
}

# Create dynamic verb input UI based on rule verb type
#
# verb: Character. The rule verb type
# value: Current value(s) for the rule
# values_id: Character. The fully-resolved input id for the values control

create_rule_verb_inputs = function(verb, value, values_id) {

  # Wrap all inputs in a div with smaller text styling
  input_wrapper = function(input_element) {
    shiny::div(
      style = "font-size: 12px;",
      input_element
    )
  }

  switch(verb,
    "has between" = {
      input_wrapper(
        shinyWidgets::numericRangeInput(
          values_id,
          NULL,
          value = value,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        )
      )
    },

    "has at least" = ,
    "has at most" = {
      input_wrapper(
        shiny::numericInput(
          values_id,
          NULL,
          value = value,
          min = 0,
          max = 1000,
          step = 1,
          width = "100%"
        )
      )
    },

    "has content" = {
      input_wrapper(
        shiny::textInput(
          values_id,
          NULL,
          value = value,
          width = "100%"
        )
      )
    },

    "lacks content" = {
      input_wrapper(
        shiny::textInput(
          values_id,
          NULL,
          value = value,
          width = "100%"
        )
      )
    },

    "has name" = {
      input_wrapper(
        shiny::textInput(
          values_id,
          NULL,
          value = value,
          width = "100%"
        )
      )
    },

    # Default case
    NULL
  )
}
