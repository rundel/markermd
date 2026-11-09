# UI helpers shared by the template and marking apps

# Font Awesome icon tag, the same markup as shiny::icon() but much cheaper:
# shiny::icon() looks the name up in fontawesome's 2000-row icon table and
# attaches the dependency to every tag, which adds up when a render needs
# dozens of icons. The name is resolved to its classes (solid or regular,
# aliases mapped to the canonical name) by shiny::icon() once per name and
# cached; the tag carries no dependency, so each app's page head includes
# fa_dependency() once.
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
    role = "presentation",
    `aria-label` = paste(sub("^fa[a-z]* fa-", "", base), "icon"),
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
