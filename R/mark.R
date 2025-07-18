#' Launch the markermd Shiny Application
#'
#' @param collection_path Character string. Path to directory containing subdirectories with assignment repositories
#' @param template Optional template for validation. Can be:
#'   - Character path to .rds file containing template data
#'   - List with raw template data (from readRDS)
#'   - List with transformed templates (from create_question_templates)
#'   - NULL (no template validation)
#' @param use_qmd Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE). Default is TRUE.
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @return Launches Shiny application
#' @export
#'
#' @examples
#' \dontrun{
#' # Parse qmd files from collection of repositories
#' mark("/path/to/assignments/")
#' 
#' # Parse Rmd files from collection of repositories
#' mark("/path/to/assignments/", use_qmd = FALSE)
#' 
#' # Parse with template validation
#' mark("/path/to/assignments/", template = "template.rds")
#' }
mark = function(collection_path, template = NULL, use_qmd = TRUE, ...) {
  
  # Validate inputs
  if (missing(collection_path)) {
    stop("collection_path is required")
  }
  
  # Validate collection directory exists
  if (!dir.exists(collection_path)) {
    stop("Collection directory does not exist: ", collection_path)
  }
  
  # Get subdirectories (each representing a repository)
  repo_dirs = list.dirs(collection_path, recursive = FALSE, full.names = TRUE)
  
  if (length(repo_dirs) == 0) {
    stop("No subdirectories found in collection path: ", collection_path)
  }
  
  # Process template if provided and store both raw and processed versions
  raw_template_data = NULL
  processed_template = NULL
  
  if (!is.null(template)) {
    # Store raw template data for content extraction
    if (is.character(template) && length(template) == 1) {
      raw_template_data = readRDS(template)
    } else if (is.list(template) && all(c("original_ast", "questions") %in% names(template))) {
      raw_template_data = template
    }
    
    # Process template for validation
    processed_template = process_mark_templates(template)
  }
  
  # Create and launch the Shiny app
  app = create_markermd_app(collection_path, processed_template, raw_template_data, use_qmd)
  shiny::runApp(app, ...)
}

#' Launch the markermd Template Creation Application
#'
#' @param assignment_path Character string. Path to local directory or GitHub repo in format "owner/repo"
#' @param local_dir Character string. Local directory for cloning (required for remote repos)
#' @param filename Character string. Glob pattern to match Rmd/qmd file to grade. Default glob matches any .Rmd or .qmd file.
#' @param template Character string. Optional path to a template RDS file to load on startup. Default is NULL.
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @return Launches Shiny application for template creation
#' @export
#'
#' @examples
#' \dontrun{
#' # Local assignment with default pattern
#' template("/path/to/assignment")
#' 
#' # Local assignment with specific filename
#' template("/path/to/assignment", filename = "homework.Rmd")
#' 
#' # Remote GitHub repo
#' template("username/repo-name", local_dir = "/tmp/grading", filename = "assignment.qmd")
#' 
#' # Load assignment with existing template
#' template("/path/to/assignment", template = "/path/to/saved_template.rds")
#' }
template = function(assignment_path, local_dir = NULL, filename = "*.[Rq]md", template = NULL, ...) {
  
  # Validate inputs
  if (missing(assignment_path)) {
    stop("assignment_path is required")
  }
  
  # Validate template file if provided
  if (!is.null(template)) {
    if (!file.exists(template)) {
      stop("Template file does not exist: ", template)
    }
    if (!grepl("\\.rds$", template, ignore.case = TRUE)) {
      stop("Template file must have .rds extension: ", template)
    }
  }
  
  # Check if assignment_path is a GitHub repo (contains "/")
  is_github_repo = grepl("/", assignment_path) && !file.exists(assignment_path)
  
  if (is_github_repo && is.null(local_dir)) {
    stop("local_dir is required for GitHub repositories")
  }
  
  # Validate local directory exists for local assignments
  if (!is_github_repo && !dir.exists(assignment_path)) {
    stop("Local assignment directory does not exist: ", assignment_path)
  }
  
  # For local assignments, resolve the filename pattern immediately
  resolved_filename = filename
  if (!is_github_repo) {
    
    matched_files = Sys.glob(file.path(assignment_path, filename) )
    
    if (length(matched_files) == 0) {
      stop("No files found matching pattern '", filename, "' in directory: ", assignment_path)
    }
    
    if (length(matched_files) > 1) {
      stop("Multiple files found matching pattern '", filename, "':\n  ", 
           paste(fs::path_file(matched_files), collapse = "\n  "), 
           "\nPlease specify a more specific pattern that matches exactly one file.")
    }
    
    # Use just the filename, not the full path
    resolved_filename = fs::path_file(matched_files[1])
  }
  
  # Create and launch the template creation Shiny app
  app = create_template_app(assignment_path, local_dir, resolved_filename, is_github_repo, template)
  shiny::runApp(app, ...)
}

#' Create Shiny App for markermd
#'
#' Internal function to create the Shiny application
#'
#' @param collection_path Character string. Path to directory containing assignment repositories
#' @param template Named list of parsermd templates for validation, or NULL
#' @param raw_template_data Raw template data with node selections, or NULL
#' @param use_qmd Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE)
#'
#' @return Shiny app object
#'
create_markermd_app = function(collection_path, template, raw_template_data, use_qmd) {
  
  # Define UI
  ui = bslib::page_navbar(
    title = "markermd - Assignment Grading",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    
    # Right-align the navigation tabs
    bslib::nav_spacer(),
    
    # Explore tab (right aligned)
    bslib::nav_panel(
      title = "Explore",
      value = "explore",
      # Add FontAwesome dependency
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
      ),
      explore_ui("explore_module")
    ),
    
    # Marking tab (right aligned)
    bslib::nav_panel(
      title = "Assignment Grading", 
      value = "marking",
      marking_ui("marking_module")
    ),
    
    sidebar = bslib::sidebar(
      shiny::h4("Collection Info"),
      shiny::p(shiny::strong("Path:"), collection_path),
      shiny::p(shiny::strong("File type:"), if(use_qmd) ".qmd" else ".Rmd"),
      if (!is.null(template)) shiny::p(shiny::strong("Template:"), "Validation enabled"),
      shiny::hr(),
      
      shiny::h4(if (!is.null(template)) "Repositories (Pass/Total)" else "Repositories"),
      gt::gt_output("repo_table")
    )
  )
  
  # Define server logic
  server = function(input, output, session) {
    
    # Reactive values
    collection_ast = shiny::reactiveVal(NULL)
    repo_names = shiny::reactiveVal(NULL)
    current_repo_ast = shiny::reactiveVal(NULL)
    current_repo_name = shiny::reactiveVal(NULL)
    all_repo_validation = shiny::reactiveVal(list())
    
    # Initialize the app
    shiny::observe({
      tryCatch({
        # Parse the collection using parsermd
        if (use_qmd) {
          collection = parsermd::parse_qmd_collection(collection_path)
        } else {
          collection = parsermd::parse_rmd_collection(collection_path)
        }
        
        collection_ast(collection)
        
        # Get repository names from collection tibble
        if (!is.null(collection) && nrow(collection) > 0) {
          # Extract repo names from path column
          repo_list = collection$path |> dirname() |> basename() |> unique()
          repo_names(repo_list)
          
          # Validate all repositories if template is available
          if (!is.null(template) && length(template) > 0) {
            validation_results = list()
            for (repo in repo_list) {
              repo_rows = collection$path |> dirname() |> basename() == repo
              if (any(repo_rows)) {
                repo_ast = collection$ast[repo_rows][[1]]
                repo_validation = validate_repo_against_templates(repo_ast, template)
                validation_results[[repo]] = repo_validation
              }
            }
            all_repo_validation(validation_results)
          }
          
          # Set initial current repo AST (first repository)
          if (length(repo_list) > 0) {
            first_repo_rows = collection$path |> dirname() |> basename() == repo_list[1]
            if (any(first_repo_rows)) {
              current_repo_ast(collection$ast[first_repo_rows][[1]])
              current_repo_name(repo_list[1])
            }
          }
        }
        
      }, error = function(e) {
        # Error loading collection
        message("Error loading collection: ", e$message)
      })
    })
    
    # Track selected repository for highlighting
    selected_repo_index = shiny::reactiveVal(1)
    
    # Create repository table with gt
    output$repo_table = gt::render_gt({
      if (is.null(repo_names()) || length(repo_names()) == 0) {
        return(gt::gt(data.frame(Repository = character(0))))
      }
      
      # Create table data with action buttons and validation summary
      repo_df = data.frame(
        Repository = repo_names(),
        stringsAsFactors = FALSE
      )
      
      # Add validation summary column
      validation_data = all_repo_validation()
      repo_df$Validation = sapply(repo_names(), function(repo) {
        if (is.null(template) || length(template) == 0) {
          return("")  # No validation without template
        }
        
        repo_validation = validation_data[[repo]]
        if (is.null(repo_validation) || length(repo_validation) == 0) {
          return("")  # No validation data
        }
        
        # Count validation results
        pass_count = sum(sapply(repo_validation, function(v) v$status == "pass"))
        fail_count = sum(sapply(repo_validation, function(v) v$status == "fail"))
        error_count = sum(sapply(repo_validation, function(v) v$status == "error"))
        total_count = length(repo_validation)
        
        # Create summary with icons
        if (fail_count == 0 && error_count == 0) {
          # All passed
          paste0('<span style="color: #28a745;"><i class="fas fa-check-circle"></i> ', pass_count, '/', total_count, '</span>')
        } else if (error_count > 0) {
          # Has errors
          paste0('<span style="color: #ffc107;"><i class="fas fa-exclamation-circle"></i> ', pass_count, '/', total_count, '</span>')
        } else {
          # Has failures
          paste0('<span style="color: #dc3545;"><i class="fas fa-times-circle"></i> ', pass_count, '/', total_count, '</span>')
        }
      })
      
      # Add row numbers for button IDs
      repo_df$row_id = seq_len(nrow(repo_df))
      
      # Create clickable repository names with action buttons
      repo_df$Repository = purrr::map_chr(seq_len(nrow(repo_df)), function(i) {
        button_style = if (i == selected_repo_index()) {
          "background-color: #007bff; color: white; border: 1px solid #007bff; padding: 8px 12px; border-radius: 4px; text-align: left; width: 100%; cursor: pointer; font-size: 14px;"
        } else {
          "background-color: white; color: #333; border: 1px solid #ddd; padding: 8px 12px; border-radius: 4px; text-align: left; width: 100%; cursor: pointer; font-size: 14px;"
        }
        
        paste0(
          '<button onclick="Shiny.setInputValue(\'repo_select_', i, '\', Math.random())" style="', 
          button_style, 
          '">',
          repo_df$Repository[i],
          '</button>'
        )
      })
      
      # Create gt table with validation column if template is available
      table_data = if (is.null(template) || length(template) == 0) {
        repo_df[, c("Repository"), drop = FALSE]
      } else {
        repo_df[, c("Repository", "Validation"), drop = FALSE]
      }
      
      gt_table = gt::gt(table_data) |>
        gt::fmt_markdown(columns = Repository)
      
      # Add validation column formatting if it exists
      if ("Validation" %in% names(table_data)) {
        gt_table = gt_table |>
          gt::fmt_markdown(columns = Validation) |>
          gt::cols_label(Repository = "", Validation = "") |>
          gt::cols_width(
            Repository ~ px(150),
            Validation ~ px(80)
          )
      } else {
        gt_table = gt_table |>
          gt::cols_label(Repository = "")
      }
      
      gt_table |>
        gt::tab_options(
          table.font.size = "14px",
          data_row.padding = "2px",
          column_labels.hidden = TRUE,
          table.border.top.style = "none",
          table.border.bottom.style = "none",
          table.border.left.style = "none",
          table.border.right.style = "none"
        ) |>
        gt::opt_css(
          css = "
          .gt_table {
            border: none !important;
          }
          .gt_col_heading {
            display: none !important;
          }
          "
        )
    })
    
    # Handle repository button clicks
    shiny::observe({
      if (!is.null(repo_names())) {
        for (i in seq_along(repo_names())) {
          local({
            row_index = i
            button_id = paste0("repo_select_", row_index)
            
            shiny::observeEvent(input[[button_id]], {
              if (!is.null(collection_ast()) && !is.null(repo_names())) {
                selected_repo = repo_names()[row_index]
                collection = collection_ast()
                
                # Update selected index for highlighting
                selected_repo_index(row_index)
                
                # Find rows for the selected repository
                repo_rows = collection$path |> dirname() |> basename() == selected_repo
                if (any(repo_rows)) {
                  # Get the first document's AST for this repository
                  current_repo_ast(collection$ast[repo_rows][[1]])
                  current_repo_name(selected_repo)
                }
              }
            })
          })
        }
      }
    })
    
    # Explore module - pass current repository AST, name, and template
    explore_result = explore_server("explore_module", current_repo_ast, current_repo_name, template_data)
    
    # Marking module with processed template and raw template data for content extraction
    template_data = shiny::reactiveVal(template)
    raw_template_reactive = shiny::reactiveVal(raw_template_data)
    
    marking_result = marking_server("marking_module", current_repo_ast, template_data, raw_template_reactive)
    
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

#' Create Template Creation Shiny App
#'
#' Internal function to create the template creation Shiny application
#'
#' @param assignment_path Character string. Path to local directory or GitHub repo
#' @param local_dir Character string. Local directory for cloning (for remote repos)
#' @param filename Character string. Name of the Rmd/qmd file to grade
#' @param is_github_repo Logical. Whether assignment_path is a GitHub repo
#' @param template_path Character string. Optional path to template RDS file to load
#'
#' @return Shiny app object
#'
create_template_app = function(assignment_path, local_dir, filename, is_github_repo, template_path = NULL) {
  
  # Define UI
  ui = bslib::page_navbar(
    title = "markermd - Template Creation",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    
    # Right-align the navigation tab
    bslib::nav_spacer(),
    
    # Template tab (right aligned)
    bslib::nav_panel(
      title = "Template Creation",
      value = "template",
      # Add FontAwesome dependency
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
      ),
      template_ui("template_module")
    ),
    
    sidebar = bslib::sidebar(
      shiny::h4("Assignment Info"),
      shiny::p(shiny::strong("Path:"), assignment_path),
      shiny::p(shiny::strong("File:"), filename),
      if (is_github_repo) shiny::p(shiny::strong("Local dir:"), local_dir),
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
    
    # Initialize the app
    shiny::observe({
      tryCatch({
        # Setup repository
        repo_path = setup_assignment_repo(
          assignment_path,
          local_dir,
          is_github_repo
        )
        
        # Validate and get assignment file path
        file_path = validate_assignment_file(repo_path, filename)
        document_path(file_path)
        
        # Parse the document
        ast = parse_assignment_document(file_path)
        document_ast(ast)
        
      }, error = function(e) {
        # Error loading assignment - log the error for debugging
        message("Error loading assignment: ", e$message)
        cat("Error loading assignment: ", e$message, "\n")
        # In a test environment, it's helpful to see the error
        if (interactive() || identical(Sys.getenv("TESTTHAT"), "true")) {
          warning("Failed to load assignment: ", e$message)
        }
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
    template_result = template_server("template_module", document_ast, template_path)
  }
  
  # Return the app
  shiny::shinyApp(ui = ui, server = server)
}