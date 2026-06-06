# A modified version of Shiny's modalDialog with a properly positioned close
# button in the header.
#
# ...: UI elements to include within the modal
# title: Character string. Modal title
# footer: UI elements for the modal footer (default: modalButton("Dismiss"))
# size: Character. Modal size: "m", "s", "l", or "xl"
# easyClose: Logical. If TRUE, also allows closing by clicking outside or pressing Escape
# fade: Logical. If TRUE, modal fades in/out

markermd_modal = function(..., title = NULL, footer = shiny::modalButton("Dismiss"),
                        size = c("m", "s", "l", "xl"), easyClose = TRUE, fade = TRUE) {

  size = match.arg(size)
  backdrop = if (!easyClose) "static"
  keyboard = if (!easyClose) "false"

  # Always provide a header close button so the modal can be dismissed (the
  # data-dismiss pair covers both Bootstrap 4 and 5).
  modal_header = if (!is.null(title)) {
    shiny::div(
      class = "modal-header",
      shiny::tags$h5(class = "modal-title", title),
      shiny::tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "modal",
        `data-dismiss` = "modal",
        `aria-label` = "Close"
      )
    )
  }
  
  # Build complete modal structure
  shiny::div(
    id = "shiny-modal",
    class = "modal",
    class = if (fade) "fade",
    tabindex = "-1",
    `data-backdrop` = backdrop,
    `data-bs-backdrop` = backdrop,
    `data-keyboard` = keyboard,
    `data-bs-keyboard` = keyboard,
    shiny::div(
      class = "modal-dialog",
      class = switch(size,
        s = "modal-sm",
        m = NULL,
        l = "modal-lg", 
        xl = "modal-xl"
      ),
      shiny::div(
        class = "modal-content",
        modal_header,
        shiny::div(class = "modal-body", ...),
        if (!is.null(footer)) shiny::div(class = "modal-footer", footer)
      )
    ),
    # Under bslib (Bootstrap 5) shiny::showModal() inserts this modal but does
    # not display it, so trigger the show explicitly. getOrCreateInstance reuses
    # any existing instance, so the dismiss buttons (data-bs-dismiss) and Escape
    # / backdrop close act on the same instance.
    shiny::tags$script(shiny::HTML(
      "(function() {
         var el = document.getElementById('shiny-modal');
         if (window.bootstrap && bootstrap.Modal.VERSION.charAt(0) !== '4') {
           bootstrap.Modal.getOrCreateInstance(el).show();
         } else {
           $(el).modal().focus();
         }
       })();"
    ))
  )
}