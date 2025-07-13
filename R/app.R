#' Create Shiny App for markermd
#'
#' Internal function to create the Shiny application
#'
#' @return Shiny app object
#'
create_markermd_app = function() {
  
  # Get configuration from global environment
  config = get(".markermd_config", envir = .GlobalEnv)
  
  # Define UI
  ui = bslib::page_navbar(
    title = "markermd - Assignment Grading",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    # Add FontAwesome dependency
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
    ),
    bslib::nav_spacer(),
    
    # Template tab (right aligned)
    bslib::nav_panel(
      title = "Template Creation",
      value = "template",
      template_ui("template_module")
    ),
    
    # Marking tab (right aligned)
    bslib::nav_panel(
      title = "Assignment Grading", 
      value = "marking",
      marking_ui("marking_module")
    ),
    
    sidebar = bslib::sidebar(
      shiny::h4("Assignment Info"),
      shiny::p(shiny::strong("Path:"), config$assignment_path),
      shiny::p(shiny::strong("File:"), config$filename),
      if (config$is_github_repo) shiny::p(shiny::strong("Local dir:"), config$local_dir),
      shiny::hr(),
      
      shiny::h4("Document Status"),
      shiny::verbatimTextOutput("document_status")
    )
  )
  
  # Define server logic
  server = function(input, output, session) {
    
    # Reactive values
    document_ast = shiny::reactiveVal(NULL)
    document_path = shiny::reactiveVal(NULL)
    template_data = shiny::reactiveVal(NULL)
    
    # Initialize the app
    shiny::observe({
      tryCatch({
        # Setup repository
        repo_path = setup_assignment_repo(
          config$assignment_path,
          config$local_dir,
          config$is_github_repo
        )
        
        # Validate and get assignment file path
        file_path = validate_assignment_file(repo_path, config$filename)
        document_path(file_path)
        
        # Parse the document
        ast = parse_assignment_document(file_path)
        document_ast(ast)
        
        # Assignment loaded successfully
        
      }, error = function(e) {
        # Error loading assignment
      })
    })
    
    # Document status display
    output$document_status = shiny::renderText({
      if (is.null(document_ast())) {
        return("No document loaded")
      }
      
      summary = get_document_summary(document_ast())
      paste(
        paste("Nodes:", summary$total_nodes),
        paste("Code chunks:", summary$code_chunks),
        paste("Markdown sections:", summary$markdown_sections),
        sep = "\n"
      )
    })
    
    # Template module
    template_result = template_server("template_module", document_ast)
    
    # Update template data when questions change
    shiny::observe({
      current_state = template_result$state()
      template_data(current_state$questions)
    })
    
    # Marking module
    marking_result = marking_server("marking_module", document_ast, template_data)
    
    # Handle navbar tab switching
    shiny::observeEvent(input$navbar, {
      if (!is.null(input$navbar)) {
        # Switched tabs
      }
    })
  }
  
  # Return the app
  shiny::shinyApp(ui = ui, server = server)
}