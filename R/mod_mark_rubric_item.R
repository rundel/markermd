# Mark Rubric Items
#
# The markup and client behavior of the rubric's item rows. An item has no
# module server of its own: its state lives in a rubric_item_handle() held by
# the rubric module, and every interaction with a row (the toggle, the move,
# group and delete actions, the points and description edits) is reported by
# one delegated handler (rubric_item_editing_head()) to the rubric module's
# single item_event input, so a rubric of hundreds of items binds no
# per-item inputs, outputs or observers.

# Colors used to indicate positive vs negative point values
POINTS_POSITIVE_COLOR = "#28a745"
POINTS_NEGATIVE_COLOR = "#dc3545"

# The live state of one rubric item: a reactive holder in place of a module
# server, read with item() and replaced with update_item()
#
# id: The item id
# initial_item: markermd_rubric_item S7 object

rubric_item_handle = function(id, initial_item) {
  state = shiny::reactiveVal(initial_item)
  list(
    id = id,
    item = function() state(),
    update_item = function(new_item) state(new_item)
  )
}

# The toggle button's label and look for an item: the first ten items show
# their hotkey (1-9, then 0) and items past the tenth have none, so their
# button is blank with a dashed outline (rubric-hk-none) and is toggled by
# clicking. The number-key handler (rubric_hotkeys_js) matches buttons by
# this label, so a blank button is never reached from the keyboard. In an
# exclusive group the button is round and announced as a radio button,
# since selecting it deselects the group's other items. Shared by the
# render and the in-place patch (markermd_item_buttons).
#
# rubric_item: markermd_rubric_item S7 object
# exclusive: Logical. Whether the item's group allows one selected item

rubric_item_button_state = function(rubric_item, exclusive = FALSE) {
  has_hotkey = !is.na(rubric_item@hotkey)
  list(
    label = if (has_hotkey) as.character(rubric_item@hotkey %% 10) else "",
    selected = isTRUE(rubric_item@selected),
    exclusive = isTRUE(exclusive),
    hotkey = has_hotkey
  )
}

# The toggle button for a rubric item (the Grade tab only). A plain button:
# a click is reported as a "toggle" item event, and selection changes are
# patched onto it in place rather than re-rendered.
#
# id: Character. The item's namespaced id
# rubric_item: markermd_rubric_item S7 object
# exclusive: Logical. Whether the item's group allows one selected item

mark_rubric_item_hotkey_btn = function(id, rubric_item, exclusive = FALSE) {
  state = rubric_item_button_state(rubric_item, exclusive)
  shiny::tags$button(
    id = paste0(id, "-hotkey_btn"),
    type = "button",
    class = paste(
      "btn btn-sm rubric-hk-btn flex-shrink-0",
      if (state$selected) "btn-success" else "btn-outline-secondary",
      if (state$exclusive) "rubric-hk-radio",
      if (!state$hotkey) "rubric-hk-none"
    ),
    `data-action` = "toggle",
    title = if (state$hotkey) NULL else "No hotkey: click to toggle",
    `aria-label` = if (state$hotkey) paste("Toggle item", state$label) else "Toggle item",
    role = if (state$exclusive) "radio" else NULL,
    `aria-checked` = if (state$exclusive) tolower(state$selected) else NULL,
    `aria-pressed` = if (state$exclusive) NULL else tolower(state$selected),
    state$label
  )
}

# The attributes of an item container (the Grade tab's item list, or a
# question's card in the Rubric tab) that route its rows' events: the
# rubric module's item_event input, the view's id prefix stripped from a
# row's id to get the item id, and the question's groups for the rows'
# group menus (built when a menu opens, from this one list)
#
# ns: The rubric module's namespace function
# view: The view the rows belong to ("grade" or "editor"); it prefixes their
#   ids, so both tabs' rows can be on the page at once
# groups: Named list of the question's markermd_rubric_group objects

rubric_item_container_attrs = function(ns, view, groups) {
  list(
    `data-item-events` = ns("item_event"),
    `data-item-prefix` = ns(paste0(view, "-")),
    `data-groups` = as.character(jsonlite::toJSON(
      lapply(names(groups), function(group_id) list(id = group_id, label = groups[[group_id]]@label)),
      auto_unbox = TRUE
    ))
  )
}

# Creates UI for a single rubric item row
#
# id: Character. The item's namespaced id for its view (the rubric module's
#   ns("<view>-<item id>"))
# rubric_item: markermd_rubric_item S7 object containing item properties
# exclusive: Whether the item's group is exclusive (the toggle's look)
# selected_count: NULL in the Grade tab (the item shows its toggle button);
#   in the Rubric tab's editor, how many repositories have the item
#   selected, shown as "<count>/<repo_count>" beside the action buttons
#   instead of the toggle
# repo_count: With selected_count, the number of repositories
# fresh: Whether the item was just added with a placeholder description,
#   which the row selects on focus so typing replaces it

mark_rubric_item_ui = function(id, rubric_item, exclusive = FALSE, selected_count = NULL, repo_count = NULL,
                               fresh = FALSE) {
  editor = !is.null(selected_count)

  shiny::div(
    id = paste0(id, "-container"),
    class = "rubric-item-row px-2 py-1 m-0 rounded position-relative",
    `data-group` = if (is.na(rubric_item@group)) "" else rubric_item@group,
    # Action buttons row: always present at reduced emphasis, full opacity on
    # hover or keyboard focus (styled via .rubric-action-btns in mark-app.R).
    # Every item has the full set, grouped or not: move within its run,
    # move to another group (or out of its group), delete. In the Rubric
    # tab's editor the item's selection count sits just left of them.
    shiny::div(
      class = "rubric-item-actions position-absolute d-flex align-items-center gap-2",
      if (editor) {
        shiny::span(
          class = paste(
            "badge rounded-pill rubric-selected-count",
            if (selected_count > 0) "text-bg-primary" else "text-bg-light border text-muted"
          ),
          title = paste0("Selected for ", selected_count, " of ", repo_count, " repositor", if (repo_count == 1) "y" else "ies"),
          paste0(selected_count, "/", repo_count)
        )
      },
      shiny::div(
        class = "rubric-action-btns d-flex",
        rubric_item_action_btn(id, "move_up", "chevron-up", "Move item up"),
        rubric_item_action_btn(id, "move_down", "chevron-down", "Move item down"),
        # Group menu: a Bootstrap dropdown listing "No group" and the
        # question's groups, filled in when it opens. Groups are created
        # with Add Group; this menu only moves the item between them.
        shiny::div(
          class = "dropdown d-inline-block",
          shiny::tags$button(
            type = "button",
            class = "btn btn-sm p-1 rubric-act-btn rubric-group-menu-btn",
            `data-bs-toggle` = "dropdown",
            `aria-expanded` = "false",
            title = "Move item to a group",
            cached_icon("layer-group")
          ),
          shiny::tags$ul(class = "dropdown-menu dropdown-menu-end shadow-sm rubric-group-menu")
        ),
        rubric_item_action_btn(id, "delete", "times", "Delete rubric item", class = "rubric-act-del")
      )
    ),
    # Rubric item content: the toggle beside a tight points/description stack
    shiny::div(
      class = "d-flex align-items-start gap-2",
      if (!editor) mark_rubric_item_hotkey_btn(id, rubric_item, exclusive),
      shiny::div(
        class = "flex-grow-1",
        style = "min-width: 0;",
        shiny::div(
          id = paste0(id, "-points_text"),
          class = "rubric-points-edit",
          contenteditable = "true",
          role = "textbox",
          `aria-label` = "Points",
          `data-points` = rubric_item@points,
          style = paste0("color: ", if (rubric_item@points >= 0) POINTS_POSITIVE_COLOR else POINTS_NEGATIVE_COLOR, ";"),
          paste0(
            if (rubric_item@points >= 0) "+" else "",
            rubric_item@points, if (identical(rubric_item@points, 1)) " pt" else " pts"
          )
        ),
        # Marks an item a markermd call (an agent's import) selected and
        # nobody has reviewed in the app since; filled in place
        # (markermd_agent_badges), and it concerns the pair on screen, so the
        # editor has none
        if (!editor) shiny::span(id = paste0(id, "-agent_badge"), class = "rubric-agent-badge-slot"),
        shiny::div(
          id = paste0(id, "-description_text"),
          contenteditable = "true",
          role = "textbox",
          `aria-label` = "Description",
          `aria-multiline` = "true",
          class = paste("form-control-like rubric-desc-edit", if (fresh) "rubric-desc-fresh"),
          `data-placeholder` = "Enter description...",
          rubric_item@description
        )
      )
    )
  )
}

# One of an item's action buttons, reported as that item event by the
# delegated click handler
#
# id: The item's namespaced id
# action: The item event's action ("move_up", "move_down", "delete")
# icon: Font Awesome icon name
# title: Tooltip text
# class: Extra classes, or NULL

rubric_item_action_btn = function(id, action, icon, title, class = NULL) {
  shiny::tags$button(
    id = paste0(id, "-", action, "_btn"),
    type = "button",
    class = paste(c("btn btn-sm p-1 rubric-act-btn", class), collapse = " "),
    `data-action` = action,
    title = title,
    `aria-label` = title,
    cached_icon(icon)
  )
}

# The page-head styles and script behind every item row, included once by
# mark(). The script reports a row's interactions to its container's
# item_event input as {item, action, value}: a click on a button carrying
# data-action, a group menu choice ("group", the group id or "" for none),
# a description edit ("description", on blur and after a pause in typing,
# not per keystroke) and a committed points edit ("points", on blur or
# Enter; the points show the bare number while focused). Pastes are reduced
# to plain text. It also fills a group menu when it opens, selects a fresh
# item's placeholder description on focus, and handles the server's in-place
# patches of the toggle buttons (markermd_item_buttons), agent badges
# (markermd_agent_badges) and a refused description (markermd_item_text).

rubric_item_editing_head = function() {
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .rubric-item-actions { top: 0; right: 8px; }
      .rubric-action-btns { gap: 2px; }
      .rubric-act-btn {
        border: none; background: transparent; color: #6c757d; font-size: 12px; line-height: 1;
      }
      .rubric-act-btn.rubric-act-del { font-size: 16px; }
      /* Sized explicitly in every dimension, min and max included: a long
         description would otherwise squeeze the flex item down to its label */
      .rubric-hk-btn {
        display: inline-block; box-sizing: border-box;
        width: 28px; min-width: 28px; max-width: 28px;
        height: 28px; min-height: 28px; max-height: 28px;
        padding: 0; margin: 0; line-height: 26px; text-align: center;
        font-size: 10px; vertical-align: top;
      }
      .rubric-hk-btn.rubric-hk-none { border-style: dashed; }
      .rubric-points-edit, .rubric-desc-edit {
        border: 1px solid transparent; background: transparent; padding: 2px 4px;
        border-radius: 4px; outline: none; transition: all 0.2s ease;
      }
      .rubric-points-edit {
        font-size: 14px; font-weight: bold; display: inline-block; width: fit-content;
      }
      .rubric-desc-edit {
        margin: 0; font-size: 12px; line-height: 1.3; min-height: 20px;
        white-space: pre-wrap; word-wrap: break-word; overflow-wrap: break-word;
      }
      .rubric-points-edit:hover, .rubric-desc-edit:hover {
        border: 1px solid #80bdff !important; background: #f8f9fa !important;
      }
      .rubric-points-edit:focus, .rubric-desc-edit:focus {
        border: 1px solid #80bdff !important; background: white !important;
      }
      .rubric-points-edit:focus { color: #495057 !important; }
      .rubric-desc-edit:empty:before { content: attr(data-placeholder); color: #adb5bd; }
      .rubric-desc-edit:focus:before { display: none; }
    ")),
    shiny::tags$script(shiny::HTML(glue::glue("
      (function() {
        var nonce = 0;
        function send(el, action, value) {
          var row = el.closest('.rubric-item-row');
          var box = el.closest('[data-item-events]');
          if (!row || !box) return false;
          var prefix = box.dataset.itemPrefix;
          var item = row.id.slice(prefix.length, row.id.length - '-container'.length);
          Shiny.setInputValue(box.dataset.itemEvents, {
            item: item, action: action, value: value === undefined ? null : value, nonce: ++nonce
          }, {priority: 'event'});
          return true;
        }
        function points(el) {
          var v = parseFloat(el.getAttribute('data-points'));
          return isNaN(v) ? 0 : v;
        }
        function esc(text) {
          var d = document.createElement('div');
          d.textContent = text;
          return d.innerHTML;
        }
        function menuEntry(group, label, active) {
          return '<li><a class=\"dropdown-item rubric-group-menu-item d-flex align-items-center gap-2\" href=\"#\" data-group=\"' +
            esc(group) + '\"><i class=\"fas fa-check fa-fw' + (active ? '' : ' invisible') +
            '\" role=\"presentation\" aria-label=\"check icon\"></i><span class=\"rubric-group-menu-label\">' +
            esc(label) + '</span></a></li>';
        }
        document.addEventListener('show.bs.dropdown', function(e) {
          var menu = e.target.parentElement && e.target.parentElement.querySelector('.rubric-group-menu');
          if (!menu) return;
          var row = menu.closest('.rubric-item-row');
          var box = menu.closest('[data-groups]');
          var groups = box ? JSON.parse(box.dataset.groups || '[]') : [];
          var current = row ? row.dataset.group : '';
          var html = '<li><h6 class=\"dropdown-header\">Move item to group</h6></li>' + menuEntry('', 'No group', current === '');
          groups.forEach(function(g) { html += menuEntry(g.id, g.label, current === g.id); });
          menu.innerHTML = html;
        });
        document.addEventListener('click', function(e) {
          if (!e.target.closest) return;
          var link = e.target.closest('a.rubric-group-menu-item');
          if (link) {
            e.preventDefault();
            send(link, 'group', link.dataset.group);
            return;
          }
          var btn = e.target.closest('.rubric-item-row button[data-action]');
          if (btn) send(btn, btn.dataset.action);
        });
        var descTimer = null;
        function sendDescription(el) {
          send(el, 'description', el.innerText || el.textContent || '');
        }
        document.addEventListener('input', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-desc-edit');
          if (!el) return;
          el.classList.remove('rubric-desc-fresh');
          if (descTimer) clearTimeout(descTimer);
          descTimer = setTimeout(function() { descTimer = null; sendDescription(el); }, 750);
        });
        document.addEventListener('focusout', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-desc-edit');
          if (!el) return;
          if (descTimer) { clearTimeout(descTimer); descTimer = null; }
          sendDescription(el);
        });
        document.addEventListener('paste', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-desc-edit, .rubric-points-edit');
          if (!el) return;
          e.preventDefault();
          document.execCommand('insertText', false, e.clipboardData.getData('text/plain'));
        });
        function selectContents(el) {
          setTimeout(function() {
            var range = document.createRange();
            range.selectNodeContents(el);
            var selection = window.getSelection();
            selection.removeAllRanges();
            selection.addRange(range);
          }, 10);
        }
        document.addEventListener('focusin', function(e) {
          var fresh = e.target.closest && e.target.closest('.rubric-desc-fresh');
          if (fresh) { selectContents(fresh); return; }
          var el = e.target.closest && e.target.closest('.rubric-points-edit');
          if (!el) return;
          el.innerHTML = points(el).toString();
          selectContents(el);
        });
        document.addEventListener('keydown', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-points-edit');
          if (el && e.key === 'Enter') {
            e.preventDefault();
            el.blur();
          }
        });
        document.addEventListener('focusout', function(e) {
          var el = e.target.closest && e.target.closest('.rubric-points-edit');
          if (!el) return;
          var value = parseFloat(el.innerText || el.textContent || '0');
          if (isNaN(value)) value = points(el);
          el.setAttribute('data-points', value);
          // Singular only for exactly 1, matching the R render
          el.innerHTML = (value >= 0 ? '+' : '') + value + ' ' + (value === 1 ? 'pt' : 'pts');
          el.style.color = value >= 0 ? '<<POINTS_POSITIVE_COLOR>>' : '<<POINTS_NEGATIVE_COLOR>>';
          send(el, 'points', value);
        });
        $(document).ready(function() {
          Shiny.addCustomMessageHandler('markermd_item_buttons', function(msg) {
            msg.forEach(function(b) {
              var el = document.getElementById(b.id);
              if (!el) return;
              el.textContent = b.label;
              el.classList.toggle('btn-success', b.selected);
              el.classList.toggle('btn-outline-secondary', !b.selected);
              el.classList.toggle('rubric-hk-radio', b.exclusive);
              el.classList.toggle('rubric-hk-none', !b.hotkey);
              if (b.hotkey) el.removeAttribute('title'); else el.title = 'No hotkey: click to toggle';
              el.setAttribute('aria-label', b.hotkey ? 'Toggle item ' + b.label : 'Toggle item');
              if (b.exclusive) {
                el.setAttribute('role', 'radio');
                el.setAttribute('aria-checked', b.selected ? 'true' : 'false');
                el.removeAttribute('aria-pressed');
              } else {
                el.removeAttribute('role');
                el.removeAttribute('aria-checked');
                el.setAttribute('aria-pressed', b.selected ? 'true' : 'false');
              }
            });
          });
          Shiny.addCustomMessageHandler('markermd_item_text', function(msg) {
            msg.ids.forEach(function(id) {
              var el = document.getElementById(id);
              if (el) el.textContent = msg.text;
            });
          });
          Shiny.addCustomMessageHandler('markermd_agent_badges', function(msg) {
            var box = document.getElementById(msg.container);
            if (!box) return;
            box.querySelectorAll('.rubric-agent-badge-slot').forEach(function(slot) {
              var title = msg.badges[slot.id];
              slot.innerHTML = title ? '<span class=\"agent-badge text-info ms-1\" title=\"' + esc(title) + '\">' + msg.icon + '</span>' : '';
            });
          });
        });
      })();
    ", .open = "<<", .close = ">>")))
  )
}
