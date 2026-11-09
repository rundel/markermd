# Mark Rubric Group Module
#
# Shiny module for one rubric item group in the marking app's Rubric pane: a
# header row (icon, label, a bounds or pick-one chip, the live subtotal, and
# the move and settings actions) wrapping its member items' rows. The items
# stay ordinary item rows (see mod_mark_rubric_item.R); the group's
# settings are edited in a popover off the header and saved on blur, and its
# structural actions (move up/down, ungroup) are signals the rubric module
# acts on. The subtotal is patched in
# place by the rubric module (markermd_update_group_subtotals) rather than
# re-rendered, so in-progress item edits survive a selection change.

# Chips describing a group's bounds ("floor -2", "ceiling 3") and, for an
# exclusive group, "pick one"
#
# group: markermd_rubric_group S7 object

rubric_group_chips = function(group) {
  chips = list()
  if (!is.na(group@floor)) {
    chips[[length(chips) + 1]] = shiny::span(
      class = "rubric-group-chip", paste("floor", format_score(group@floor))
    )
  }
  if (!is.na(group@ceiling)) {
    chips[[length(chips) + 1]] = shiny::span(
      class = "rubric-group-chip", paste("ceiling", format_score(group@ceiling))
    )
  }
  if (isTRUE(group@exclusive)) {
    chips[[length(chips) + 1]] = shiny::span(
      class = "rubric-group-chip rubric-group-chip-excl",
      fa_icon("circle-dot"), " pick one"
    )
  }
  shiny::tagList(chips)
}

# Subtotal markup for a group: the applied value, preceded by the raw sum
# struck through when a bound changed it. Nothing changes visually while
# the group is within its bounds.
#
# sum: Numeric. Sum of the group's selected items' points
# applied: Numeric. That sum after the group's floor and ceiling

rubric_group_subtotal = function(sum, applied) {
  capped = !isTRUE(all.equal(sum, applied))
  shiny::tagList(
    if (capped) shiny::span(class = "raw", format_points_signed(sum)),
    shiny::span(class = "eff", format_points_signed(applied))
  )
}

# Creates UI for a rubric item group: its header and its member item rows
#
# id: Character. Module namespace ID
# group: markermd_rubric_group S7 object
# item_uis: List of the member items' UI (mark_rubric_item_ui()), in order
# subtotal: One-row data frame from score_question()'s groups (sum,
#   applied), or NULL for a group with nothing selected

mark_rubric_group_ui = function(id, group, item_uis, subtotal = NULL) {
  ns = shiny::NS(id)
  sum = if (is.null(subtotal) || nrow(subtotal) == 0) 0 else subtotal$sum[1]
  applied = if (is.null(subtotal) || nrow(subtotal) == 0) 0 else subtotal$applied[1]
  capped = !isTRUE(all.equal(sum, applied))

  action_style = "border: none; background: transparent; color: #6c757d; font-size: 12px; line-height: 1;"

  shiny::div(
    id = ns("container"),
    class = paste("rubric-group", if (capped) "rubric-group-capped"),
    shiny::div(
      class = "rubric-group-head d-flex align-items-center gap-2 position-relative",
      fa_icon("layer-group", class = "text-muted", style = "font-size: 11px;"),
      shiny::span(class = "rubric-group-name", group@label),
      shiny::span(class = "rubric-group-chips d-inline-flex gap-1", rubric_group_chips(group)),
      shiny::span(class = "rubric-group-subtotal", rubric_group_subtotal(sum, applied)),
      shiny::div(
        class = "rubric-action-btns position-absolute",
        style = "top: 2px; right: 8px;",
        shiny::div(
          style = "display: flex; gap: 2px;",
          shiny::actionButton(
            ns("move_up_btn"),
            label = NULL,
            icon = fa_icon("chevron-up"),
            class = "btn btn-sm p-1",
            style = action_style,
            title = "Move group up"
          ),
          shiny::actionButton(
            ns("move_down_btn"),
            label = NULL,
            icon = fa_icon("chevron-down"),
            class = "btn btn-sm p-1",
            style = action_style,
            title = "Move group down"
          ),
          bslib::popover(
            shiny::actionButton(
              ns("settings_btn"),
              label = NULL,
              icon = fa_icon("sliders"),
              class = "btn btn-sm p-1",
              style = action_style,
              title = "Group settings"
            ),
            title = "Group settings",
            placement = "bottom",
            id = ns("settings_popover"),
            options = list(customClass = "rubric-wide-popover"),
            shiny::div(
              class = "rubric-group-settings",
              style = "min-width: 270px;",
              shiny::textInput(
                ns("label"), "Label",
                value = group@label, width = "100%", updateOn = "blur"
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                shiny::numericInput(
                  ns("floor"), "Floor",
                  value = if (is.na(group@floor)) NULL else group@floor,
                  max = 0, step = 0.5, width = "100%", updateOn = "blur"
                ),
                shiny::numericInput(
                  ns("ceiling"), "Ceiling",
                  value = if (is.na(group@ceiling)) NULL else group@ceiling,
                  min = 0, step = 0.5, width = "100%", updateOn = "blur"
                )
              ),
              shiny::div(
                class = "text-muted mb-2",
                style = "font-size: 11px;",
                "Bounds the sum of this group's selected items. Floor \u2264 0 \u2264 ceiling; blank means no bound."
              ),
              shiny::uiOutput(ns("settings_feedback")),
              bslib::input_switch(
                ns("exclusive"), "Only one item can be selected",
                value = isTRUE(group@exclusive)
              ),
              shiny::div(
                class = "d-flex justify-content-between align-items-center gap-2 mt-1",
                shiny::span(
                  class = "text-muted",
                  style = "font-size: 11px;",
                  "Applies to all repositories. Labels may appear in feedback when group headings are exported."
                ),
                shiny::actionButton(
                  ns("ungroup_btn"), "Ungroup",
                  class = "btn-outline-danger btn-sm flex-shrink-0"
                )
              )
            )
          )
        )
      )
    ),
    shiny::div(id = ns("items"), item_uis)
  )
}

# Client-side handlers for the group displays, included once by
# mark_rubric_ui(): patch the subtotals (and the capped styling) in place,
# patch a renamed or re-bounded group's header and menu entries, and open a
# popover (used to open a freshly added group's settings once it renders;
# the bslib popover element takes the same message its Shiny input binding
# does).

rubric_groups_js = function() {
  shiny::tags$script(shiny::HTML("
    $(document).ready(function() {
      Shiny.addCustomMessageHandler('markermd_update_group_subtotals', function(msg) {
        msg.forEach(function(g) {
          var el = document.getElementById(g.id);
          if (!el) return;
          el.classList.toggle('rubric-group-capped', !!g.capped);
          var sub = el.querySelector('.rubric-group-subtotal');
          if (sub) sub.innerHTML = g.html;
        });
      });

      Shiny.addCustomMessageHandler('markermd_update_group', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) {
          var name = el.querySelector('.rubric-group-name');
          if (name) name.textContent = msg.label;
          var chips = el.querySelector('.rubric-group-chips');
          if (chips) chips.innerHTML = msg.chips;
        }
        // The item rows' group menus are built from their container's
        // groups list when they open (see rubric_item_container_attrs())
        document.querySelectorAll('[data-groups]').forEach(function(box) {
          var groups = JSON.parse(box.dataset.groups || '[]');
          var changed = false;
          groups.forEach(function(g) {
            if (g.id === msg.group) {
              g.label = msg.label;
              changed = true;
            }
          });
          if (changed) box.dataset.groups = JSON.stringify(groups);
        });
      });

      Shiny.addCustomMessageHandler('markermd_show_popover', function(msg) {
        var el = document.getElementById(msg.id);
        if (!el) return;
        el.scrollIntoView({block: 'nearest'});
        if (typeof el.receiveMessage === 'function') {
          el.receiveMessage(el, {method: 'toggle', value: 'show'});
        }
      });
    });
  "))
}

# Server logic for a rubric item group. Settings changes are validated,
# saved to the database and exposed through group() and changed(); the
# structural actions are signals for the rubric module.
#
# id: Character. Module namespace ID (the group id)
# initial_group: markermd_rubric_group S7 object with initial state
# collection_path: Character string. Path to collection directory
# question_name: Character string. Name of the question
# group_id: Character string. Unique identifier for this group
# label_available: function(label) returning whether no other group of the
#   question uses that label
# exclusive_conflicts: function() returning the repositories with more than
#   one of the group's items selected (data frame with assignment_repo and
#   items), which block turning the exclusive switch on
# app_action: Function(fn, code, call, summary, parts) recording a change as
#   an action of the app (see mark-app.R); the default just runs the code

mark_rubric_group_server = function(id, initial_group, collection_path, question_name, group_id,
                                    label_available, exclusive_conflicts, app_action = function(fn, code, ...) code) {
  shiny::moduleServer(id, function(input, output, session) {

    group_state = shiny::reactiveVal(initial_group)
    feedback = shiny::reactiveVal(NULL)
    changed = shiny::reactiveVal(0)

    output$settings_feedback = shiny::renderUI({
      message = feedback()
      if (is.null(message)) {
        return(NULL)
      }
      shiny::div(class = "text-danger mb-2", style = "font-size: 11px;", message)
    })

    commit = function(new_group) {
      group_state(new_group)
      app_action(
        "edit item group",
        call = glue::glue("mark(): edit group \"{new_group@label}\" of {question_name}"),
        parts = "rubric",
        save_item_group(collection_path, question_name, group_id, new_group)
      )
      feedback(NULL)
      changed(changed() + 1)
    }

    shiny::observe({
      current = group_state()
      label = trimws(input$label)
      if (identical(label, current@label)) {
        return()
      }
      if (!nzchar(label)) {
        feedback("A group needs a label.")
        shiny::updateTextInput(session, "label", value = current@label)
        return()
      }
      if (!label_available(label)) {
        feedback(glue::glue("Another group is already named \"{label}\"."))
        shiny::updateTextInput(session, "label", value = current@label)
        return()
      }
      current@label = label
      commit(current)
    }) |>
      shiny::bindEvent(input$label, ignoreInit = TRUE)

    # A bound: blank (NA) means none; the sign rule is enforced inline and an
    # invalid value is left in the input, unsaved, with the reason shown
    bound_observer = function(field, valid, message) {
      shiny::observe({
        current = group_state()
        value = input[[field]]
        value = if (is.null(value) || is.na(value)) NA_real_ else as.numeric(value)
        if (identical(value, as.numeric(S7::prop(current, field)))) {
          return()
        }
        if (!is.na(value) && !valid(value)) {
          feedback(message)
          return()
        }
        S7::prop(current, field) = value
        commit(current)
      }) |>
        shiny::bindEvent(input[[field]], ignoreInit = TRUE)
    }
    bound_observer("floor", function(x) x <= 0, "The floor must be 0 or negative (or blank for none).")
    bound_observer("ceiling", function(x) x >= 0, "The ceiling must be 0 or positive (or blank for none).")

    # Exclusivity is refused while any repository has more than one of the
    # group's items selected: deselecting across repositories is a grading
    # decision the app must not make silently
    shiny::observe({
      current = group_state()
      value = isTRUE(input$exclusive)
      if (identical(value, current@exclusive)) {
        return()
      }
      if (value) {
        conflicts = exclusive_conflicts()
        if (nrow(conflicts) > 0) {
          bslib::update_switch("exclusive", value = FALSE, session = session)
          shiny::showModal(rubric_group_conflict_modal(
            glue::glue("Cannot make \"{current@label}\" exclusive"),
            conflicts
          ))
          return()
        }
      }
      current@exclusive = value
      commit(current)
    }) |>
      shiny::bindEvent(input$exclusive, ignoreInit = TRUE)

    move_up_signal = shiny::reactiveVal(0)
    move_down_signal = shiny::reactiveVal(0)
    ungroup_signal = shiny::reactiveVal(0)

    shiny::observe({
      move_up_signal(move_up_signal() + 1)
    }) |>
      shiny::bindEvent(input$move_up_btn)

    shiny::observe({
      move_down_signal(move_down_signal() + 1)
    }) |>
      shiny::bindEvent(input$move_down_btn)

    # Confirm before ungrouping: the bounds stop applying for every
    # repository, which changes recorded scores across the class
    shiny::observe({
      current = group_state()
      bslib::toggle_popover("settings_popover", show = FALSE, session = session)
      shiny::showModal(shiny::modalDialog(
        title = "Ungroup these items?",
        glue::glue(
          "The items of \"{current@label}\" stay in the rubric as ungrouped items, ",
          "but the group's bounds no longer apply to any repository's score."
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_ungroup"), "Ungroup", class = "btn-danger")
        )
      ))
    }) |>
      shiny::bindEvent(input$ungroup_btn)

    shiny::observe({
      shiny::removeModal()
      ungroup_signal(ungroup_signal() + 1)
    }) |>
      shiny::bindEvent(input$confirm_ungroup)

    return(list(
      id = id,
      group = shiny::reactive(group_state()),
      changed = shiny::reactive(changed()),
      move_up_signal = move_up_signal,
      move_down_signal = move_down_signal,
      ungroup_signal = ungroup_signal,
      update_group = function(new_group) {
        group_state(new_group)
      }
    ))
  })
}

# Modal listing the repositories that block an exclusivity change (or a
# move into an exclusive group), so the grader can resolve them first
#
# title: Modal title
# conflicts: Data frame with assignment_repo and items (the selected member
#   descriptions, joined)

rubric_group_conflict_modal = function(title, conflicts) {
  shiny::modalDialog(
    title = title,
    shiny::p(
      "An exclusive group allows one selected item, but these repositories",
      "have more than one of the group's items selected. Resolve them first:"
    ),
    shiny::tags$ul(
      lapply(seq_len(nrow(conflicts)), function(i) {
        shiny::tags$li(shiny::strong(conflicts$assignment_repo[i]), ": ", conflicts$items[i])
      })
    ),
    easyClose = TRUE,
    footer = shiny::modalButton("Close")
  )
}
