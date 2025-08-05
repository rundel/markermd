#' MarkerMD Modal Dialog with Bootstrap 5 Close Button
#'
#' A modified version of Shiny's modalDialog that adds a properly positioned
#' close button in the header when easyClose is TRUE.
#'
#' @param ... UI elements to include within the modal
#' @param title Character string. Modal title
#' @param footer UI elements for the modal footer (default: modalButton("Dismiss"))  
#' @param size Character. Modal size: "m", "s", "l", or "xl"
#' @param easyClose Logical. If TRUE, adds close button to header and allows closing by clicking outside
#' @param fade Logical. If TRUE, modal fades in/out
#'
#' @return Modal dialog HTML structure
#'
markermd_modal = function(..., title = NULL, footer = shiny::modalButton("Dismiss"), 
                        size = c("m", "s", "l", "xl"), easyClose = FALSE, fade = TRUE) {
  
  size = match.arg(size)
  backdrop = if (!easyClose) "static"
  keyboard = if (!easyClose) "false"
  
  # Create modal header with optional close button
  modal_header = if (!is.null(title)) {
    header_content = list(shiny::tags$h5(class = "modal-title", title))
    
    # Add close button if easyClose is TRUE
    if (easyClose) {
      header_content = append(header_content, list(
        shiny::tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "modal",
          `aria-label` = "Close"
        )
      ))
    }
    
    shiny::div(class = "modal-header", header_content)
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
    shiny::tags$script(shiny::HTML("
      if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\\./)) {
        var modal = new bootstrap.Modal(document.getElementById('shiny-modal'));
        modal.show();
      } else {
        $('#shiny-modal').modal().focus();
      }
    "))
  )
}