# Mark Rubric Groups
#
# The markup and client behavior of the rubric's item groups. A group has no
# module server of its own: its state lives in a rubric_group_handle() held
# by the rubric module, and every interaction with it (the move buttons, the
# settings popover's label, bounds and exclusive switch, Ungroup) is reported
# by one delegated handler (rubric_groups_js()) to the rubric module's single
# group_event input as {group, action, value}, the way item rows report to
# item_event. A group renders as a header row (icon, label, a bounds or
# pick-one chip, the live subtotal, and the move and settings actions)
# wrapping its member items' rows. The subtotal and header are patched in
# place by the rubric module (markermd_update_group_subtotals,
# markermd_update_group) rather than re-rendered, so in-progress item edits
# survive a selection change.

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

# The live state of one item group: a reactive holder in place of a module
# server, read with group() and replaced with update_group()
#
# id: The group id
# initial_group: markermd_rubric_group S7 object

rubric_group_handle = function(id, initial_group) {
  state = shiny::reactiveVal(initial_group)
  list(
    id = id,
    group = function() state(),
    update_group = function(new_group) state(new_group)
  )
}

# The values a group's settings controls show, for the in-place header and
# settings patch (markermd_update_group): blank bounds for none
#
# group: markermd_rubric_group S7 object

rubric_group_settings_values = function(group) {
  list(
    label = group@label,
    floor = if (is.na(group@floor)) "" else format(group@floor),
    ceiling = if (is.na(group@ceiling)) "" else format(group@ceiling),
    exclusive = isTRUE(group@exclusive)
  )
}

# Creates UI for a rubric item group: its header and its member item rows.
# The settings controls are plain markup with no Shiny input bindings; their
# changes reach the server through group_event (see rubric_groups_js()).
# The popover moves its content to the page body while open, so the
# settings block carries its own routing attributes.
#
# id: Character. The group's namespaced id for this view (the rubric
#   module's ns("<view>-<group id>"))
# group: markermd_rubric_group S7 object
# item_uis: List of the member items' UI (mark_rubric_item_ui()), in order
# subtotal: One-row data frame from score_question()'s groups (sum,
#   applied), or NULL for a group with nothing selected
# group_id: The group id reported in its events, or NULL for markup that
#   reports nothing
# events_id: The rubric module's group_event input id, or NULL

mark_rubric_group_ui = function(id, group, item_uis, subtotal = NULL, group_id = NULL, events_id = NULL) {
  ns = shiny::NS(id)
  sum = if (is.null(subtotal) || nrow(subtotal) == 0) 0 else subtotal$sum[1]
  applied = if (is.null(subtotal) || nrow(subtotal) == 0) 0 else subtotal$applied[1]
  capped = !isTRUE(all.equal(sum, applied))
  values = rubric_group_settings_values(group)

  action_btn = function(action, icon, title) {
    shiny::tags$button(
      id = ns(paste0(action, "_btn")),
      type = "button",
      class = "btn btn-sm p-1 rubric-act-btn",
      `data-group-action` = action,
      title = title,
      `aria-label` = title,
      fa_icon(icon)
    )
  }
  routing = list(`data-group-id` = group_id, `data-group-events` = events_id)

  shiny::div(
    id = ns("container"),
    class = paste("rubric-group", if (capped) "rubric-group-capped"),
    !!!routing,
    shiny::div(
      class = "rubric-group-head d-flex align-items-center gap-2 position-relative",
      fa_icon("layer-group", class = "text-muted", style = "font-size: 11px;"),
      shiny::span(class = "rubric-group-name", group@label),
      shiny::span(class = "rubric-group-chips d-inline-flex gap-1", rubric_group_chips(group)),
      shiny::span(class = "rubric-group-subtotal", rubric_group_subtotal(sum, applied)),
      shiny::div(
        class = "rubric-action-btns position-absolute d-flex",
        style = "top: 2px; right: 8px; gap: 2px;",
        action_btn("move_up", "chevron-up", "Move group up"),
        action_btn("move_down", "chevron-down", "Move group down"),
        bslib::popover(
          shiny::tags$button(
            id = ns("settings_btn"),
            type = "button",
            class = "btn btn-sm p-1 rubric-act-btn",
            title = "Group settings",
            `aria-label` = "Group settings",
            fa_icon("sliders")
          ),
          title = "Group settings",
          placement = "bottom",
          id = ns("settings_popover"),
          options = list(customClass = "rubric-wide-popover"),
          shiny::div(
            class = "rubric-group-settings small",
            style = "min-width: 270px;",
            `data-settings-for` = ns("container"),
            !!!routing,
            shiny::tags$label(
              class = "form-label d-block mb-2",
              "Label",
              shiny::tags$input(
                type = "text", class = "form-control form-control-sm mt-1",
                `data-field` = "label", value = values$label
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::tags$label(
                class = "form-label d-block",
                "Floor",
                shiny::tags$input(
                  type = "number", class = "form-control form-control-sm mt-1",
                  `data-field` = "floor", max = 0, step = 0.5, value = values$floor
                )
              ),
              shiny::tags$label(
                class = "form-label d-block",
                "Ceiling",
                shiny::tags$input(
                  type = "number", class = "form-control form-control-sm mt-1",
                  `data-field` = "ceiling", min = 0, step = 0.5, value = values$ceiling
                )
              )
            ),
            shiny::div(
              class = "text-muted mb-2",
              style = "font-size: 11px;",
              "Bounds the sum of this group's selected items. Floor \u2264 0 \u2264 ceiling; blank means no bound."
            ),
            shiny::div(id = ns("settings_feedback"), class = "rubric-group-feedback text-danger mb-2", style = "font-size: 11px;"),
            shiny::div(
              class = "form-check form-switch mb-2",
              shiny::tags$label(
                class = "form-check-label",
                shiny::tags$input(
                  type = "checkbox", class = "form-check-input", role = "switch",
                  `data-field` = "exclusive", checked = if (values$exclusive) NA
                ),
                "Only one item can be selected"
              )
            ),
            shiny::div(
              class = "d-flex justify-content-between align-items-center gap-2 mt-1",
              shiny::span(
                class = "text-muted",
                style = "font-size: 11px;",
                "Applies to all repositories. Labels may appear in feedback when group headings are exported."
              ),
              shiny::tags$button(
                id = ns("ungroup_btn"),
                type = "button",
                class = "btn btn-outline-danger btn-sm flex-shrink-0",
                `data-group-action` = "ungroup",
                "Ungroup"
              )
            )
          )
        )
      )
    ),
    shiny::div(id = ns("items"), item_uis)
  )
}

# The page-head script behind every group, included once by mark(). It
# reports a group's interactions to the group_event input named by the
# nearest data-group-events ancestor as {group, action, value}: a click on a
# button carrying data-group-action ("move_up", "move_down", "ungroup"), and
# a committed settings change (on change, i.e. blur or Enter for the text and
# number fields): "label" (text), "floor" / "ceiling" (a number, or null for
# blank) and "exclusive" (logical). It also handles the server's in-place
# patches: the subtotals and capped styling (markermd_update_group_subtotals),
# a group's header, settings controls and the items' menu entries after a
# change or a refusal (markermd_update_group), the settings feedback line
# (markermd_group_feedback), and opening or closing a settings popover
# (markermd_show_popover; the bslib popover element takes the same message
# its Shiny input binding does).

rubric_groups_js = function() {
  shiny::tags$script(shiny::HTML("
    (function() {
      var nonce = 0;
      function send(el, action, value) {
        var box = el.closest('[data-group-events]');
        if (!box || !box.dataset.groupId) return;
        Shiny.setInputValue(box.dataset.groupEvents, {
          group: box.dataset.groupId, action: action,
          value: value === undefined ? null : value, nonce: ++nonce
        }, {priority: 'event'});
      }
      document.addEventListener('click', function(e) {
        if (!e.target.closest) return;
        var btn = e.target.closest('button[data-group-action]');
        if (btn) send(btn, btn.dataset.groupAction);
      });
      document.addEventListener('change', function(e) {
        var el = e.target.closest && e.target.closest('.rubric-group-settings [data-field]');
        if (!el) return;
        var field = el.dataset.field;
        var value;
        if (field === 'exclusive') {
          value = el.checked;
        } else if (field === 'label') {
          value = el.value;
        } else {
          value = el.value === '' ? null : parseFloat(el.value);
          if (value !== null && isNaN(value)) value = null;
        }
        send(el, field, value);
      });
      document.addEventListener('keydown', function(e) {
        var el = e.target.closest && e.target.closest('.rubric-group-settings input[data-field=label]');
        if (el && e.key === 'Enter') {
          e.preventDefault();
          el.blur();
        }
      });
      function settingsFor(id) {
        return document.querySelectorAll('[data-settings-for=\"' + id + '\"]');
      }
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
          msg.ids.forEach(function(id) {
            var el = document.getElementById(id);
            if (el) {
              var name = el.querySelector('.rubric-group-name');
              if (name) name.textContent = msg.label;
              var chips = el.querySelector('.rubric-group-chips');
              if (chips) chips.innerHTML = msg.chips;
            }
            settingsFor(id).forEach(function(box) {
              box.querySelectorAll('[data-field]').forEach(function(input) {
                var v = msg.values[input.dataset.field];
                if (input.dataset.field === 'exclusive') input.checked = !!v;
                else input.value = v;
              });
            });
          });
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

        Shiny.addCustomMessageHandler('markermd_group_feedback', function(msg) {
          msg.ids.forEach(function(id) {
            var el = document.getElementById(id);
            if (el) el.textContent = msg.text || '';
          });
        });

        Shiny.addCustomMessageHandler('markermd_show_popover', function(msg) {
          var el = document.getElementById(msg.id);
          if (!el) return;
          if (msg.value !== 'hide') el.scrollIntoView({block: 'nearest'});
          if (typeof el.receiveMessage === 'function') {
            el.receiveMessage(el, {method: 'toggle', value: msg.value || 'show'});
          }
        });
      });
    })();
  "))
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
