# UI helpers shared by the template and marking apps

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
      shiny::icon("right-left"),
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
