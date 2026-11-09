# UI helpers shared by the template and marking apps

# Font Awesome icon tag, the same markup as shiny::icon() but much cheaper:
# shiny::icon() looks the name up in fontawesome's 2000-row icon table and
# attaches the dependency to every tag, which adds up when a render needs
# dozens of icons. The name is resolved to its classes (solid or regular,
# aliases mapped to the canonical name) by shiny::icon() once per name and
# cached; the tag carries no dependency, so each app's page head includes
# fa_dependency() once. Icons are decorative (aria-hidden): every control
# that carries one also has a text label or a title, and a role of
# "presentation" would make assistive technology ignore an aria-label anyway.
#
# name: Font Awesome icon name, as for shiny::icon()
# class: Extra classes, or NULL
# ...: Further attributes for the <i> tag (style, title, ...)

.fa_class_cache = new.env(parent = emptyenv())

fa_icon = function(name, class = NULL, ...) {
  base = .fa_class_cache[[name]]
  if (is.null(base)) {
    base = shiny::icon(name)$attribs$class
    .fa_class_cache[[name]] = base
  }
  htmltools::tags$i(
    class = paste(c(base, class), collapse = " "),
    `aria-hidden` = "true",
    ...
  )
}

# The Font Awesome stylesheet dependency that fa_icon() tags rely on

fa_dependency = function() {
  htmltools::htmlDependencies(shiny::icon("check"))
}

# Compact modal-header styling shared by both apps (shiny::modalDialog()'s
# default header is taller than either app's dialogs want)

markermd_modal_css = function() {
  shiny::tags$style(shiny::HTML("
    .modal-header { padding: 8px 15px !important; }
    .modal-title { margin: 0 !important; padding: 0 !important; line-height: 1.2 !important; }
  "))
}

# Import/Export popover: an exchange-arrows trigger button opening a popover
# with one or more download buttons and an Import button that forwards its
# click to a hidden fileInput. Shared by the template editor's save controls
# and the rubric pane header. Must be placed in static UI, not inside a
# renderUI: an open popover holds the moved content element, so re-rendering
# the surrounding UI would leave a duplicate download-button binding behind
# with a dead download link.
#
# id: Element id for the trigger button
# title: Popover title
# export_buttons: List of shiny::downloadButton()s
# import_input_id: Id of the hidden fileInput the Import button forwards to
# button_title: Tooltip text for the trigger button

io_menu_ui = function(id, title, export_buttons, import_input_id, button_title = "Import / Export") {
  bslib::popover(
    shiny::tags$button(
      fa_icon("right-left"),
      id = id,
      type = "button",
      class = "btn btn-outline-secondary btn-sm",
      title = button_title
    ),
    shiny::div(
      class = "d-grid gap-2",
      export_buttons,
      shiny::tags$button(
        "Import",
        type = "button",
        class = "btn btn-outline-secondary btn-sm",
        onclick = glue::glue(
          "document.getElementById('<<import_input_id>>').click();",
          .open = "<<", .close = ">>"
        )
      )
    ),
    title = title
  )
}

# A full-page loading overlay shown from the first paint until the server's
# first flush has finished (the first shiny:idle), so the seconds an app
# spends building its initial outputs show a spinner rather than an empty
# page. Removed once and never shown again.
#
# message: Text beside the spinner

app_loading_overlay = function(message = "Loading\u2026") {
  htmltools::tagList(
    htmltools::div(
      id = "markermd-loading",
      class = "position-fixed top-0 start-0 w-100 h-100 d-flex flex-column align-items-center justify-content-center gap-3 bg-body",
      style = "z-index: 2000; transition: opacity 0.2s ease;",
      htmltools::div(class = "spinner-border text-primary", role = "status"),
      htmltools::div(class = "text-muted", message)
    ),
    htmltools::tags$script(htmltools::HTML("
      $(document).one('shiny:idle', function() {
        var el = document.getElementById('markermd-loading');
        if (!el) return;
        el.style.opacity = '0';
        setTimeout(function() { el.remove(); }, 200);
      });
    "))
  )
}

# A DOM-safe id for a question, unique per name: the name with everything
# outside [A-Za-z0-9_-] replaced by "_", plus a short hash of the full name
# whenever that replacement lost information, so "Q1 (a)" and "Q1 [a]" get
# different ids while a plain "Q2" stays "Q2". Shared by the grade servers,
# the validation cards and the table's score columns.
#
# question_name: Character vector of question names

question_dom_id = function(question_name) {
  safe = gsub("[^A-Za-z0-9_-]", "_", question_name)
  lossy = safe != question_name
  safe[lossy] = paste0(safe[lossy], "_", substr(vapply(question_name[lossy], rlang::hash, character(1)), 1, 6))
  safe
}

# One-line text: whitespace collapsed and cut to width with an ellipsis. Used
# by the History tab and the snapshot list's print method.
#
# text: Character vector (NA becomes "")
# width: Maximum number of characters

one_line_text = function(text, width = 80) {
  text = ifelse(is.na(text), "", gsub("\\s+", " ", trimws(text)))
  ifelse(nchar(text) > width, paste0(substr(text, 1, width - 3), "…"), text)
}

# A blank (NULL, empty or whitespace-only) text input as NULL, else the text;
# the snapshot dialogs pass their optional label and summary through it
#
# x: Character scalar or NULL

blank_to_null = function(x) {
  if (is.null(x) || !nzchar(trimws(x))) NULL else x
}

# One labeled row of a Bootstrap description list (a dt / dd pair), or NULL
# when the value is missing (NULL, empty, NA or ""), so detail panes skip the
# fields an entry does not have
#
# name: Character scalar. The label
# value: The value: text or a tag

detail_field = function(name, value) {
  missing_text = length(value) == 1 && is.atomic(value) && (is.na(value) || identical(value, ""))
  if (is.null(value) || length(value) == 0 || missing_text) {
    return(NULL)
  }
  htmltools::tagList(
    shiny::tags$dt(class = "col-sm-3 fw-normal text-muted", name),
    shiny::tags$dd(class = "col-sm-9 mb-1", value)
  )
}

# The Take snapshot dialog shared by the History tab and the template editor:
# an intro line, a label and a summary (passed through blank_to_null() on
# confirm), any extra inputs, and the confirm button. The inputs are
# "snapshot_label", "snapshot_summary" and "confirm_snapshot" under ns.
#
# intro: Tag list shown in the small muted intro paragraph
# label_placeholder: Placeholder text for the label input
# ns: Namespace function for the input ids (identity outside a module)
# ...: Further inputs shown after the summary

snapshot_take_modal = function(intro, label_placeholder, ns = shiny::NS(NULL), ...) {
  shiny::modalDialog(
    title = "Take snapshot",
    shiny::p(class = "small text-muted", intro),
    shiny::textInput(ns("snapshot_label"), "Label", placeholder = label_placeholder, width = "100%"),
    shiny::textAreaInput(ns("snapshot_summary"), "Summary", placeholder = "What is about to change, or why this state is worth keeping", width = "100%", rows = 3),
    ...,
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("confirm_snapshot"), "Take snapshot", class = "btn-primary")
    )
  )
}

# The app wording of a question rename report (see
# reconcile_question_names_conn()): one message per renamed question whose
# grading moved, and one for removed questions still holding grading. The
# console form is report_question_name_changes().
#
# changes: The list reconcile_question_names_conn() returns, or NULL

question_name_change_messages = function(changes) {
  renamed = changes$renamed
  moved = if (!is.null(renamed) && nrow(renamed) > 0) {
    as.character(glue::glue("Carried the grading of \"{renamed$from}\" over to its new name \"{renamed$to}\"."))
  } else {
    character(0)
  }
  orphaned = changes$orphaned
  left = if (length(orphaned) > 0) {
    one = length(orphaned) == 1
    paste0(
      "Removed question", if (one) "" else "s", " ", paste0("\"", orphaned, "\"", collapse = ", "),
      " still hold", if (one) "s" else "", " grading, which mark() and the exports no longer show; ",
      "restore the template from a snapshot to bring it back (a new question with the same name starts empty)."
    )
  } else {
    character(0)
  }
  c(moved, left)
}

# Show question_name_change_messages() as persistent warning notifications:
# the template editor's save and the History tab's restore both report
# grading that followed a question id to a new name
#
# changes: The list reconcile_question_names_conn() returns, or NULL

notify_question_name_changes = function(changes) {
  for (message in question_name_change_messages(changes)) {
    shiny::showNotification(message, type = "warning", duration = NULL)
  }
  invisible(NULL)
}
