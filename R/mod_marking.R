# Shiny module for grading assignments

# Marking UI
#
# id: Character. Module namespace ID

marking_ui = function(id) {
  ns = shiny::NS(id)
  
  bslib::layout_columns(
    col_widths = c(4, 8),
    shiny::div(
      shiny::h3("Questions"),
      shiny::div(
        class = "border border-secondary p-2 bg-light overflow-auto",
        style = "height: 500px;",
        shiny::div(
          id = ns("questions_list"),
          shiny::p("No template loaded. Please create a template first.")
        )
      )
    ),
    shiny::div(
      shiny::h3("Assignment Content"),
      shiny::tabsetPanel(
        id = ns("content_tabs"),
        shiny::tabPanel(
          "Document View",
          shiny::div(
            class = "border border-secondary p-2 bg-light overflow-auto",
            style = "height: 450px;",
            shiny::verbatimTextOutput(ns("document_content"))
          )
        ),
        shiny::tabPanel(
          "Grading",
          shiny::div(
            class = "p-2",
            shiny::h4("Assignment Grading"),
            shiny::br(),
            shiny::numericInput(
              ns("score"),
              "Score:",
              value = 0,
              min = 0,
              max = 100,
              step = 1
            ),
            shiny::textAreaInput(
              ns("feedback"),
              "Feedback:",
              rows = 6,
              placeholder = "Enter feedback for this question..."
            ),
            shiny::br(),
            shiny::actionButton(ns("save_grade"), "Save Grade", class = "btn-success")
          )
        )
      )
    )
  )
}

# Marking server
#
# id: Character. Module namespace ID
# ast: Reactive. The parsed AST object
# template_obj: Reactive. markermd_template S7 object with questions and rules
# validation_results: Reactive. Optional validation results for current repository

marking_server = function(id, ast, template_obj, validation_results = shiny::reactiveVal(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive value for managing saved grades
    grades = shiny::reactiveVal(list())

    # Display document content
    output$document_content = shiny::renderText({
      if (is.null(ast())) {
        return("No document loaded")
      }
      
      # This is a placeholder - in a real implementation we would
      # display the actual document content, possibly with highlighting
      # for the currently selected question nodes
      
      summary = get_document_summary(ast())
      
      paste(
        "Document Summary:",
        paste("Total nodes:", summary$total_nodes),
        paste("Code chunks:", summary$code_chunks),
        paste("Markdown sections:", summary$markdown_sections),
        paste("Has YAML header:", summary$has_yaml),
        "",
        "Full document content would be displayed here...",
        sep = "\n"
      )
    })
    
    # Update questions list based on template
    shiny::observe({
      if (is.null(template_obj()) || length(template_obj()@questions) == 0) {
        content = shiny::p("No template loaded. Please create a template first.")
      } else {
        # Extract question content for tooltips
        question_contents = if (!is.null(ast())) {
          extract_question_content(ast(), template_obj())
        } else {
          list()
        }

        content = shiny::div(
          style = "padding: 10px;",
          shiny::h5("Questions:", style = "margin-bottom: 10px;"),

          shiny::tags$ul(
            style = "list-style-type: none; padding-left: 0; margin: 0;",
            lapply(seq_along(template_obj()@questions), function(i) {
              question_name = template_obj()@questions[[i]]@name
              # Get tooltip content
              tooltip_content = question_contents[[question_name]] %||% "No content available for this question."

              # Get validation status if available
              validation_status = if (!is.null(validation_results()) && !is.null(validation_results()[[question_name]])) {
                status = validation_results()[[question_name]]$status
                if (status == "pass") {
                  as.character(shiny::span(class = "text-success ms-2", shiny::icon("check-circle")))
                } else if (status == "fail") {
                  as.character(shiny::span(class = "text-danger ms-2", shiny::icon("times-circle")))
                } else if (status == "error") {
                  as.character(shiny::span(class = "text-warning ms-2", shiny::icon("exclamation-triangle")))
                } else {
                  ""
                }
              } else {
                ""
              }
              
              # Create a unique ID for this question item
              item_id = session$ns(paste0("question_item_", i))
              
              # Escape content for JavaScript (simple approach)
              escaped_content = gsub("'", "\\\\'", tooltip_content)
              escaped_content = gsub("\"", "\\\\\"", escaped_content)
              escaped_content = gsub("\n", "\\\\n", escaped_content)
              escaped_content = substr(escaped_content, 1, 800)  # Limit length
              
              # Create question item with both hover tooltip and click modal
              shiny::tags$li(
                id = item_id,
                style = "padding: 8px 0; border-bottom: 1px solid #eee; cursor: help;",
                
                # Hover tooltip (simple version)
                title = paste(substr(tooltip_content, 1, 200), if(nchar(tooltip_content) > 200) "..." else ""),
                
                # Click handler for detailed modal
                onclick = paste0("Shiny.setInputValue('", session$ns("show_question_content"), "', {question: '", question_name, "', content: ", jsonlite::toJSON(tooltip_content, auto_unbox = TRUE), "});"),
                
                # Also add hover effect with CSS
                onmouseover = "this.style.backgroundColor = '#f0f0f0';",
                onmouseout = "this.style.backgroundColor = 'transparent';",
                
                shiny::HTML(paste0(question_name, validation_status))
              )
            })
          )
        )
      }
      
      shiny::removeUI(selector = paste0("#", session$ns("questions_list"), " > *"))
      shiny::insertUI(
        selector = paste0("#", session$ns("questions_list")),
        ui = content
      )
    })
    
    # Handle question content modal display
    shiny::observe({
      if (!is.null(input$show_question_content)) {
        question_name = input$show_question_content$question
        content = input$show_question_content$content

        # Monaco Editor handles indentation properly, so we keep original content

        # Determine Monaco Editor language based on content type
        # Try to detect the content type from the content itself
        monaco_language = if (grepl("^---\\s*$", content, perl = TRUE)) {
          "yaml"  # YAML front matter
        } else if (grepl("^```\\{r", content, perl = TRUE)) {
          "r"  # R chunk
        } else if (grepl("^```\\{python", content, perl = TRUE)) {
          "python"  # Python chunk
        } else if (grepl("^```\\{sql", content, perl = TRUE)) {
          "sql"  # SQL chunk
        } else if (grepl("^```\\{bash", content, perl = TRUE)) {
          "shell"  # Bash chunk
        } else if (grepl("^```", content, perl = TRUE)) {
          "markdown"  # Generic code block - use markdown
        } else {
          "markdown"  # Default to markdown
        }

        # Generate unique editor ID for this question content
        editor_id = paste0("monaco-editor-marking-", gsub("[^A-Za-z0-9]", "", question_name))

        shiny::showModal(
          shiny::modalDialog(
            title = shiny::span(paste("Content for", question_name), style = "font-size: 16px; font-weight: bold;"),
            size = "l",
            shiny::div(
              style = "height: 400px;",
              shiny::div(
                id = editor_id,
                style = "height: 100%; width: 100%; border: 1px solid #e1e5e9;"
              )
            ),
            footer = NULL,
            easyClose = TRUE
          )
        )

        render_monaco_editor(editor_id, content, monaco_language)
      }
    }) |>
      shiny::bindEvent(input$show_question_content)

    # Save grade for the current assignment
    shiny::observe({
      current_grades = grades()

      current_grades[["assignment"]] = list(
        score = input$score,
        feedback = input$feedback,
        timestamp = Sys.time()
      )

      grades(current_grades)
    }) |>
      shiny::bindEvent(input$save_grade)

    # Return reactive values
    return(list(
      grades = grades
    ))
  })
}