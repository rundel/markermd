#' Launch the markermd Shiny Application
#'
#' @param collection_path Character string. Path to directory containing subdirectories with assignment repositories
#' @param template Optional template for validation. Can be:
#'   - Character path to .rds file containing template data
#'   - List with raw template data (from readRDS)
#'   - List with transformed templates (from create_question_templates)
#'   - NULL (no template validation)
#' @param use_qmd Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE). Default is TRUE.
#' @param download_archives Logical. Whether to download all archives at app launch (TRUE) or on-demand (FALSE). Default is TRUE.
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
#' 
#' # Disable upfront archive downloading
#' mark("/path/to/assignments/", download_archives = FALSE)
#' }
mark = function(collection_path, template = NULL, use_qmd = TRUE, download_archives = TRUE, ...) {
  app = mark_app(collection_path, template = template, use_qmd = use_qmd, download_archives = download_archives)
  shiny::runApp(app, ...)
}

# Build the Shiny application object for the marking interface
#
# Performs all setup (template load, collection parse, validation, database
# init) and returns the app object without running it, so it can be tested.
#
# collection_path: Path to the directory of assignment repositories
# template: markermd_template object, path to a template .rds, or NULL
# use_qmd: Whether to match .qmd files (TRUE) or .Rmd files (FALSE)
# download_archives: Whether to download all archives at launch

mark_app = function(collection_path, template = NULL, use_qmd = TRUE, download_archives = TRUE) {

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
  
  template_obj = NULL
  
  if (!is.null(template)) {
    if (is.character(template) && length(template) == 1) {
      if (!file.exists(template)) {
        stop("Template file does not exist: ", template, call. = FALSE)
      }
      template_obj = readRDS(template)
      if (!S7::S7_inherits(template_obj, markermd_template)) {
        stop("Template file must contain a markermd_template S7 object")
      }
    } else if (S7::S7_inherits(template, markermd_template)) {
      template_obj = template
    } else {
      stop("Template must be a file path or markermd_template S7 object")
    }

    assert_template_compatible(template_obj)
  }

  # Initialize database for persistent storage
  database_state = NULL
  if (!is.null(template_obj)) {
    # Initialize database and load existing state
    tryCatch({
      database_state = initialize_database_state(collection_path, template_obj)
    }, error = function(e) {
      warning("Database initialization failed: ", e$message)
      database_state = NULL
    })
  }
  
  # Parse the collection (knitr-style chunk headers are normalised per file)
  collection = parse_assignment_collection(collection_path, use_qmd)

  # Get repository names from collection tibble
  repo_list = character(0)
  validation_results = list()
  initial_repo_ast = NULL
  initial_repo_name = NULL
  
  if (is.null(collection) || nrow(collection) == 0) {
    stop("No valid documents found in collection path: ", collection_path)
  }
  
  # Extract repo names from path column
  repo_list = collection$path |> dirname() |> basename() |> unique()
  
  if (length(repo_list) == 0) {
    stop("No repositories found in collection")
  }
  
  # Validate all repositories if template is available
  if (!is.null(template_obj)) {
    for (repo in repo_list) {
      repo_rows = collection$path |> dirname() |> basename() == repo
      if (any(repo_rows)) {
        repo_ast = collection$ast[repo_rows][[1]]
        repo_validation = validate_repo_against_rules(repo_ast, template_obj)
        validation_results[[repo]] = repo_validation
      }
    }
  }
  
  # Collect GitHub repository information
  artifact_status = list()
  github_repos = character(0)
  repo_to_github = list()
  
  # First pass: collect all GitHub repo names
  for (repo in repo_list) {
    repo_path = file.path(collection_path, repo)
    
    tryCatch({
      git_root = gert::git_find(repo_path)
      if (!is.null(git_root)) {
        remotes = gert::git_remote_list(repo = repo_path)
        if (nrow(remotes) > 0 && any(grepl("github\\.com", remotes$url, ignore.case = TRUE))) {
          # Extract repo name from GitHub URL
          github_url = remotes$url[grepl("github\\.com", remotes$url)][1]
          if (grepl("github\\.com[:/]([^/]+)/([^/\\.]+)", github_url)) {
            repo_match = regmatches(github_url, regexec("github\\.com[:/]([^/]+)/([^/\\.]+)", github_url))[[1]]
            if (length(repo_match) >= 3) {
              github_repo = paste0(repo_match[2], "/", repo_match[3])
              github_repos = c(github_repos, github_repo)
              repo_to_github[[repo]] = github_repo
            }
          }
        }
      }
    }, error = function(e) {
      # Skip this repo
    })
  }
  
  # Initialize artifact status - checking what's locally available
  # Archive downloading will happen asynchronously in the app if needed
  for (repo in repo_list) {
    if (repo %in% names(repo_to_github)) {
      # Check if archive file exists locally
      cached_path = get_cached_artifact_path(collection_path, repo)
      artifact_status[[repo]] = file.exists(cached_path)
    } else {
      artifact_status[[repo]] = NA  # Not a GitHub repo
    }
  }
  
  # Set initial current repo AST (first repository)
  first_repo_rows = collection$path |> dirname() |> basename() == repo_list[1]
  if (any(first_repo_rows)) {
    initial_repo_ast = collection$ast[first_repo_rows][[1]]
    initial_repo_name = repo_list[1]
  } else {
    stop("Could not load initial repository data")
  }
  
  app = create_markermd_app(
    collection_path, 
    template_obj, 
    use_qmd,
    collection,
    repo_list,
    validation_results,
    initial_repo_ast,
    initial_repo_name,
    artifact_status,
    repo_to_github,
    template_path = if(is.character(template)) template else NULL,
    download_archives = download_archives,
    database_state = database_state
  )
}


# Create the Shiny application object for the marking interface
#
# collection_path: Path to directory containing assignment repositories
# template_obj: markermd_template S7 object with node selections, or NULL
# use_qmd: Whether to parse .qmd files (TRUE) or .Rmd files (FALSE)
# collection: Parsed collection data (data frame with path and ast columns)
# repo_list: Character vector of repository names
# validation_results: List of validation results for each repository
# initial_repo_ast: Initial repository AST to display
# initial_repo_name: Name of initial repository
# artifact_status: List of artifact availability status for each repository
# repo_to_github: Named list mapping repository names to GitHub repos
# template_path: Path to template file (optional)
# download_archives: Whether archives were downloaded at launch
# database_state: Database state loaded from SQLite (optional)

create_markermd_app = function(collection_path, template_obj, use_qmd, collection, repo_list, validation_results, initial_repo_ast, initial_repo_name, artifact_status, repo_to_github, template_path = NULL, download_archives = TRUE, database_state = NULL) {
  
  # Define UI  
  ui = bslib::page_navbar(
    title = "markermd - Marking",
    theme = bslib::bs_theme(version = 5),
    selected = "validation",  # Set validation tab as default
    id = "main_navbar",  # Add id to enable navigation tracking
    
    # Right-align the navigation tabs
    bslib::nav_spacer(),
    
    # Validation tab (right aligned)
    bslib::nav_panel(
      title = "Validation",
      value = "validation",
      # Add Font Awesome for GitHub icons and other dependencies
      shiny::tags$head(
        shinyjs::useShinyjs(),
        shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
        # CSS to fix modal content formatting (copied from working template app)
        shiny::tags$style(shiny::HTML('
          /* Modal styling */
          .modal-header { padding: 8px 15px !important; }
          .modal-title { margin: 0 !important; padding: 0 !important; line-height: 1.2 !important; }

          /* Content repo select styling - selectize */
          .selectize-control.single .selectize-input {
            font-size: 12px !important;
            font-weight: normal !important;
            height: 32px !important;
            padding: 6px 8px !important;
            line-height: 20px !important;
          }
          
          .selectize-dropdown .selectize-dropdown-content .option {
            font-size: 12px !important;
            font-weight: normal !important;
            padding: 4px 8px !important;
          }
          
          
          /* Section highlighting for scrolling - single wrapper container approach */
          .section-highlight-wrapper {
            background-color: rgba(255, 235, 59, 0.15) !important;
            border-left: 3px solid rgba(255, 193, 7, 0.8) !important;
            padding: 8px !important;
            padding-left: 12px !important;
            margin: 8px 0 !important;
            transition: background-color 0.3s ease, border-left 0.3s ease !important;
          }
          
          /* Ensure content inside wrapper maintains natural spacing */
          .section-highlight-wrapper > * {
            margin-top: 0 !important;
          }
          
          .section-highlight-wrapper > *:not(:last-child) {
            margin-bottom: 1rem !important;
          }
          
          .section-highlight-wrapper > *:last-child {
            margin-bottom: 0 !important;
          }
          
          /* Override Quarto complex grid layout for simpler body-content focus */
          
          body .page-columns,
          #html-content-container .page-columns,
          .page-columns {
            display: block !important;
            gap: 0 !important;
            max-width: none !important;
            width: 100% !important;
            margin: 0 !important;
            padding: 0 !important;
          }

          body .page-columns main,
          #html-content-container .page-columns main,
          .page-columns main,
          body main,
          #html-content-container main,
          main {
            width: calc(100% - 1.5rem) !important;
            max-width: calc(100% - 1.5rem) !important;
            font-size: 0.9em !important;
            margin-left: auto !important;
            margin-right: auto !important;
          }
          
          /* Make switch more visible when in false state */
          .form-switch .form-check-input:not(:checked) {
            border-color: #212529 !important;
          }
        
        '))
      ),
      # Main content area 
      shiny::div(
        id = "main-app-content",
        bslib::layout_columns(
          col_widths = c(6, 6),
          # Repository table card (left)
          bslib::card(
            bslib::card_header("Assignments", class = "bg-light"),
            bslib::card_body(
              shiny::uiOutput("repo_table"),
              shiny::div(
                class = "mt-3 text-center",
                shiny::uiOutput("sync_button_ui")
              )
            )
          ),
          # Validation results card (right)  
          bslib::card(
            bslib::card_header("Validation", class = "bg-light"),
            bslib::card_body(
              mark_validate_ui("explore_module")
            )
          )
        )
      )
    ),
    
    # Rubric tab (right aligned)
    bslib::nav_panel(
      title = "Rubric", 
      value = "rubric",
      mark_rubric_ui("rubric_module")
    ),
    
    
    # Footer with collection and template info
    footer = shiny::div(
      class = "bg-light border-top text-center text-muted p-2 mt-3 fs-6",
      shiny::span(shiny::strong("Collection path:"), " ", shiny::code(collection_path, class = "bg-light px-1 py-1 rounded small"), class = "me-3"),
      if (!is.null(template_path)) {
        shiny::span(shiny::strong("Template:"), " ", shiny::code(template_path, class = "bg-light px-1 py-1 rounded small"), class = "me-3")
      },
      shiny::span(shiny::strong("File type:"), " ", shiny::code(if(use_qmd) ".qmd" else ".Rmd", class = "bg-light px-1 py-1 rounded small"))
    )
  )
  
  # Define server logic
  server = function(input, output, session) {

    # Export values for testing
    shiny::exportTestValues(
      repo_names = repo_list,
      n_questions = if (is.null(template_obj)) 0L else length(template_obj@questions),
      validation_status = lapply(validation_results, function(repo_res) {
        vapply(repo_res, function(q) q$status, character(1))
      })
    )

    # Reactive values for user selections
    current_repo_ast = shiny::reactiveVal(initial_repo_ast)
    current_repo_name = shiny::reactiveVal(initial_repo_name)
    
    # Track selected repository for highlighting
    selected_repo_index = shiny::reactiveVal(1)
    
    # Make artifact status reactive so table updates when it changes
    artifact_status_reactive = shiny::reactiveVal(artifact_status)
    
    # Reactive trigger for auto-sync on app launch
    auto_sync_trigger = shiny::reactiveVal(0)

    # Create reactive trigger for progress updates (initialized here to ensure it exists)
    progress_update_trigger = shiny::reactiveVal(0)
    
    # Create repository table with gt. Rendered as static HTML (rather than
    # gt::render_gt) so the gt_shiny input binding is not registered on the same
    # id as the output, which would trigger a shared input/output id warning.
    output$repo_table = shiny::renderUI({
      # Include the progress update trigger as a dependency to force refresh when needed
      trigger_value = progress_update_trigger()
      
      # Create table data with action buttons and validation summary
      repo_df = data.frame(
        Repository = repo_list,
        OriginalName = repo_list,  # Store original names for button creation
        stringsAsFactors = FALSE
      )
      
      # Add GitHub detection for each repository
      repo_df$IsGitHub = sapply(repo_list, function(repo) {
        repo_path = file.path(collection_path, repo)
        
        tryCatch({
          # Check if directory is a git repository
          git_root = gert::git_find(repo_path)
          if (!is.null(git_root)) {
            remotes = gert::git_remote_list(repo = repo_path)
            if (nrow(remotes) > 0 && any(grepl("github\\.com", remotes$url, ignore.case = TRUE))) {
              return(TRUE)
            }
          }
          return(FALSE)
        }, error = function(e) {
          return(FALSE)
        })
      })
      
      # Add Folder column
      repo_df$Folder = sapply(seq_along(repo_list), function(i) {
        folder_button_id = paste0("folder_", i)
        return(paste0('<button onclick="Shiny.setInputValue(\'', folder_button_id, '\', Math.random())" class="btn btn-link p-0 border-0 text-reset" title="Open folder"><i class="far fa-folder-open fs-6"></i></button>'))
      })
      
      # Add GitHub column
      repo_df$GitHub = sapply(repo_list, function(repo) {
        if (repo %in% names(repo_to_github)) {
          github_repo = repo_to_github[[repo]]
          github_url = paste0("https://github.com/", github_repo)
          return(paste0('<a href="', github_url, '" target="_blank" class="text-reset text-decoration-none"><i class="fab fa-github fs-6" title="Open on GitHub"></i></a>'))
        } else {
          return("")
        }
      })
      
      # Add artifact status column with clickable icons
      repo_df$Artifacts = sapply(seq_along(repo_list), function(i) {
        repo = repo_list[i]
        artifact_status_val = artifact_status_reactive()[[repo]]
        
        if (is.na(artifact_status_val)) {
          # Not a GitHub repo - no icon
          return("")
        } else if (artifact_status_val) {
          # Has artifacts - clickable archive icon
          button_id = paste0("artifact_", i)
          return(paste0('<button onclick="Shiny.setInputValue(\'', button_id, '\', Math.random())" class="btn btn-link p-0 border-0 text-reset" title="View artifact"><i class="far fa-file fs-6"></i></button>'))
        } else {
          # GitHub repo but no artifacts - show greyed out unclickable icon
          return('<i class="far fa-file fs-6" style="opacity: 0.3;" title="No artifact available"></i>')
        }
      })
      
      # Add source code column with clickable file-code icons
      repo_df$Source = sapply(seq_along(repo_list), function(i) {
        repo = repo_list[i]
        button_id = paste0("source_", i)
        return(paste0('<button onclick="Shiny.setInputValue(\'', button_id, '\', Math.random())" class="btn btn-link p-0 border-0 text-reset" title="View source code"><i class="far fa-file-code fs-6"></i></button>'))
      })
      
      # Add validation summary column
      repo_df$Validation = sapply(
        repo_list, validation_status_cell,
        validation_results = validation_results, template_obj = template_obj
      )
      
      # Calculate grading progress for all repos at once (for efficiency)
      all_progress = NULL
      question_names = NULL
      if (!is.null(template_obj) && length(template_obj@questions) > 0) {
        question_names = sapply(template_obj@questions, function(q) q@name)
        all_progress = calculate_grading_progress(collection_path, question_names, repo_list)
      }
      
      # Add grading progress column with sparkline bars
      repo_df$Grading = sapply(
        repo_list, grading_progress_cell,
        template_obj = template_obj, all_progress = all_progress,
        collection_path = collection_path, question_names = question_names
      )
      
      # Add row numbers for button IDs
      repo_df$row_id = seq_len(nrow(repo_df))
      
      # Create clickable repository names with action buttons and GitHub icons
      repo_df$Repository = purrr::map_chr(seq_len(nrow(repo_df)), function(i) {
        button_style = if (i == selected_repo_index()) {
          "background-color: #007bff; color: white; border: 1px solid #007bff; padding: 8px 12px; border-radius: 4px; text-align: left; width: 100%; cursor: pointer; font-size: 12px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
        } else {
          "background-color: white; color: #333; border: 1px solid #ddd; padding: 8px 12px; border-radius: 4px; text-align: left; width: 100%; cursor: pointer; font-size: 12px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
        }
        
        # Just show repo name - no GitHub icon here
        content = repo_df$OriginalName[i]
        
        paste0(
          '<button onclick="Shiny.setInputValue(\'repo_select_', i, '\', Math.random())" style="', 
          button_style, 
          '">',
          content,
          '</button>'
        )
      })
      
      # Create gt table with all columns
      table_data = repo_df[, c("Repository", "Folder", "GitHub", "Artifacts", "Source", "Validation", "Grading"), drop = FALSE]
      
      gt_table = gt::gt(table_data) |>
        gt::fmt_markdown(columns = .data$Repository) |>
        gt::fmt_markdown(columns = .data$Folder) |>
        gt::fmt_markdown(columns = .data$GitHub) |>
        gt::fmt_markdown(columns = .data$Artifacts) |>
        gt::fmt_markdown(columns = .data$Source) |>
        gt::fmt_markdown(columns = .data$Grading) |>
        gt::fmt_markdown(columns = .data$Validation) |>
        gt::cols_label(
          Repository = "Repository", 
          Folder = "", GitHub = "", Artifacts = "", Source = "",
          Validation = "Validation", Grading = "Progress"
        ) |>
        gt::cols_width(
          Repository ~ pct(33),
          Folder ~ pct(6),
          GitHub ~ pct(6),
          Artifacts ~ pct(6),
          Source ~ pct(6),
          Validation ~ pct(17),
          Grading ~ pct(26)
        ) |>
        gt::cols_align(align = "center", columns = .data$Validation)
      
      styled_table = gt_table |>
        gt::tab_options(
          table.font.size = "12px",  # Smaller font size
          data_row.padding = "2px",
          column_labels.hidden = FALSE,  # Show column headers
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
            font-size: 11px !important;
            font-weight: bold !important;
            padding: 4px 2px !important;
          }
          "
        )

      shiny::HTML(gt::as_raw_html(styled_table, inline_css = FALSE))
    })
    
    # Handle repository button clicks
    shiny::observe({
      for (i in seq_along(repo_list)) {
          local({
            row_index = i
            button_id = paste0("repo_select_", row_index)
            
            shiny::observe({
              selected_repo = repo_list[row_index]

              # Update selected index for highlighting
              selected_repo_index(row_index)

              # Find rows for the selected repository
              repo_rows = collection$path |> dirname() |> basename() == selected_repo
              if (any(repo_rows)) {
                # Get the first document's AST for this repository
                current_repo_ast(collection$ast[repo_rows][[1]])
                current_repo_name(selected_repo)
              }
            }) |> bindEvent(input[[button_id]])
          })
        }
    })
    
    # Handle artifact button clicks
    shiny::observe({
      for (i in seq_along(repo_list)) {
        local({
          row_index = i
          artifact_button_id = paste0("artifact_", row_index)
          
          shiny::observe({
            repo = repo_list[row_index]

            # Only process if this repo has artifacts
            if (!is.na(artifact_status[[repo]]) && artifact_status[[repo]]) {
              # Get cached path and check if file exists
              cached_path = get_cached_artifact_path(collection_path, repo)
              
              if (file.exists(cached_path)) {
                # Read the HTML content and display it directly
                html_content = tryCatch({
                  readLines(cached_path, warn = FALSE) |> paste(collapse = "\n")
                }, error = function(e) {
                  paste("Error reading file:", e$message)
                })
                
                # Show artifact in modal with HTML content
                shiny::showModal(
                  markermd_modal(
                    title = repo,
                    size = "xl",
                    easyClose = TRUE,
                    footer = NULL,
                    shiny::div(
                      style = "height: 70vh; width: 100%; overflow: auto; border: 1px solid #dee2e6; background: white; font-size: 12px; padding: 8px;",
                      shiny::HTML(html_content)
                    )
                  )
                )
              } else {
                # Show error modal - file should exist if download_archives was TRUE
                shiny::showModal(
                  markermd_modal(
                    title = "Archive Not Available",
                    easyClose = TRUE,
                    footer = NULL,
                    shiny::div(
                      class = "p-4 text-center",
                      shiny::tags$i(class = "fas fa-exclamation-triangle fs-3 text-warning me-2"),
                      "Archive file not found. Try using 'Sync Archives' to download it."
                    )
                  )
                )
              }
            }
          }) |> bindEvent(input[[artifact_button_id]])
        })
      }
    })
    
    # Handle source button clicks
    shiny::observe({
      for (i in seq_along(repo_list)) {
        local({
          row_index = i
          source_button_id = paste0("source_", row_index)
          
          shiny::observe({
            repo = repo_list[row_index]

            # Find the source file path from collection
            repo_rows = collection$path |> dirname() |> basename() == repo
            if (any(repo_rows)) {
              file_path = collection$path[repo_rows][1]
              
              if (file.exists(file_path)) {
                # Read the raw source content
                raw_content = tryCatch({
                  readLines(file_path, warn = FALSE) |> paste(collapse = "\n")
                }, error = function(e) {
                  paste("Error reading file:", e$message)
                })
                
                # Determine file extension for title
                file_ext = tools::file_ext(file_path)
                file_name = basename(file_path)
                
                # Create unique editor ID
                editor_id = paste0("monaco-editor-source-", gsub("[^A-Za-z0-9]", "", repo))
                
                # Show source in modal with Monaco Editor
                shiny::showModal(
                  markermd_modal(
                    title = paste("Source Code:", file_name),
                    size = "xl",
                    easyClose = TRUE,
                    footer = NULL,
                    shiny::div(
                      style = "height: 70vh;",
                      shiny::div(
                        id = editor_id,
                        style = "height: 100%; width: 100%; border: 1px solid #e1e5e9;"
                      )
                    )
                  )
                )
                
                # Initialize Monaco Editor
                shinyjs::runjs(paste0("
                  (function() {
                    // Load Monaco Editor if not already loaded
                    if (typeof monaco === 'undefined') {
                      var script = document.createElement('script');
                      script.src = 'https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs/loader.js';
                      script.onload = function() {
                        require.config({ paths: { vs: 'https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs' } });
                        require(['vs/editor/editor.main'], function() {
                          createEditor();
                        });
                      };
                      document.head.appendChild(script);
                    } else {
                      createEditor();
                    }
                    
                    function createEditor() {
                      // Clean up any existing editor
                      var existingContainer = document.getElementById('", editor_id, "');
                      if (existingContainer && existingContainer.editor) {
                        existingContainer.editor.dispose();
                      }
                      
                      // Create the editor
                      var editor = monaco.editor.create(document.getElementById('", editor_id, "'), {
                        value: ", jsonlite::toJSON(raw_content, auto_unbox = TRUE), ",
                        language: 'markdown',
                        theme: 'vs',
                        readOnly: true,
                        wordWrap: 'on',
                        wrappingIndent: 'indent',
                        fontSize: 12,
                        lineNumbers: 'on',
                        minimap: { enabled: false },
                        scrollBeyondLastLine: false,
                        automaticLayout: true,
                        contextmenu: false,
                        selectOnLineNumbers: false
                      });
                      
                      // Store reference for cleanup
                      document.getElementById('", editor_id, "').editor = editor;
                    }
                  })();
                "))
              } else {
                # Show error modal - file not found
                shiny::showModal(
                  markermd_modal(
                    title = "Source File Not Available",
                    footer = NULL,
                    shiny::div(
                      class = "p-4 text-center",
                      shiny::tags$i(class = "fas fa-exclamation-triangle fs-3 text-warning me-2"),
                      "Source file not found."
                    )
                  )
                )
              }
            }
          }) |> bindEvent(input[[source_button_id]])
        })
      }
    })

    # Handle folder button clicks
    shiny::observe({
      for (i in seq_along(repo_list)) {
        local({
          row_index = i
          folder_button_id = paste0("folder_", row_index)
          
          shiny::observe({
            repo = repo_list[row_index]
            # Expand tilde in collection path and normalize the full path
            expanded_collection_path = path.expand(collection_path)
            repo_path = file.path(expanded_collection_path, repo)
            repo_path = normalizePath(repo_path, mustWork = FALSE)

            # Attempt to open the folder
            success = open_folder(repo_path)

            if (!success) {
              # Show error modal if folder couldn't be opened
              shiny::showModal(
                markermd_modal(
                  title = "Error Opening Folder",
                  easyClose = TRUE,
                  footer = NULL,
                  shiny::div(
                    class = "p-4 text-center",
                    shiny::tags$i(class = "fas fa-exclamation-triangle fs-3 text-danger me-2"),
                    paste("Could not open folder:", repo_path)
                  )
                )
              )
            }
          }) |> bindEvent(input[[folder_button_id]])
        })
      }
    })
    
    # Create reactive for current repository validation results
    current_repo_validation = shiny::reactive({
      repo_name = current_repo_name()
      if (!is.null(repo_name) && !is.null(validation_results) && repo_name %in% names(validation_results)) {
        validation_results[[repo_name]]
      } else {
        NULL
      }
    })
    
    # Mark validation module - pass validation data
    explore_result = mark_validate_server("explore_module", current_repo_ast, current_repo_name, current_repo_validation, shiny::reactiveVal(NULL), shiny::reactiveVal(template_obj))
    
    
    # Initialize rubric module with callback
    rubric_result = mark_rubric_server("rubric_module", template_obj, artifact_status_reactive, collection_path, use_qmd, collection, database_state)
    
    
    
    # Render sync button (only show if there are GitHub repos)
    output$sync_button_ui = shiny::renderUI({
      if (length(repo_to_github) > 0) {
        shiny::actionButton(
          "sync_archives",
          "Sync Artifacts",
          icon = shiny::icon("sync-alt"),
          class = "btn-outline-primary btn-sm"
        )
      }
    })
    
    # Shared sync logic function
    perform_sync = function() {
      # First check if any archives need downloading
      github_repos_vec = unique(unlist(repo_to_github))
      
      # If no GitHub repos are configured, don't show any modal
      if (length(github_repos_vec) == 0) {
        return()
      }
      
      # Quick check to see if any downloads are needed
      archives_to_download = 0
      archives_available = 0  # Track how many repos have archives available
      
      # Get metadata to determine what's available
      metadata = get_archive_metadata(github_repos_vec)
      use_metadata = is.data.frame(metadata) && nrow(metadata) > 0
      
      for (github_repo in github_repos_vec) {
        local_repo = names(repo_to_github)[repo_to_github == github_repo][1]
        if (is.na(local_repo)) next
        
        # Check if archive exists in metadata
        if (use_metadata) {
          repo_metadata = metadata[metadata$repo == github_repo, ]
          if (nrow(repo_metadata) == 0) next  # No archive available
        }
        
        archives_available = archives_available + 1
        cached_path = get_cached_artifact_path(collection_path, local_repo)
        
        # Check if download is needed
        needs_download = if (use_metadata) {
          !check_archive_freshness(cached_path, github_repo, metadata)
        } else {
          !file.exists(cached_path)
        }
        
        if (needs_download) {
          archives_to_download = archives_to_download + 1
        }
      }
      
      # Only proceed with progress if there are archives to download
      if (archives_to_download == 0) {
        # Show notification that everything is up to date
        shiny::showNotification(
          "Sync complete! 0 archives updated",
          type = "default",
          duration = 5
        )
        return()
      }
      
      # Use Shiny's built-in Progress class for actual downloads
      progress = shiny::Progress$new()
      progress$set(message = "Syncing archives", value = 0)
      
      # Ensure progress is closed when done
      on.exit(progress$close())
      
      # Perform sync
      github_repos_vec = unique(unlist(repo_to_github))
      
      # Track total across callback calls
      total_archives = NULL
      
      # Create progress callback using Shiny's Progress  
      progress_callback = function(message, completed = NULL, total = NULL) {
        # Store total when provided
        if (!is.null(total) && total > 0) {
          total_archives <<- total
        }
        
        # Use stored total for progress calculations
        if (!is.null(total_archives) && total_archives > 0 && !is.null(completed)) {
          progress$set(
            #message = "Downloading archives\n",
            value = completed / total_archives,
            message = paste("Downloaded", completed, "of", total_archives, "archives")
          )
        } else {
          progress$set(message = message)
        }
        
        # Add small delay to make progress visible
        Sys.sleep(0.1)
      }
      
      sync_result = sync_archives(github_repos_vec, repo_to_github, collection_path, progress_callback)
      downloaded_count = sync_result$downloaded_count
      
      # Update artifact status for any newly downloaded files
      updated_status = artifact_status_reactive()
      for (repo in repo_list) {
        if (repo %in% names(repo_to_github)) {
          cached_path = get_cached_artifact_path(collection_path, repo)
          updated_status[[repo]] = file.exists(cached_path)
        }
      }
      artifact_status_reactive(updated_status)
      
      # Show completion notification regardless of download count
      shiny::showNotification(
        paste("Sync complete!", downloaded_count, "archives updated"),
        type = "default",
        duration = 5
      )
    }
    
    # Handle sync archives button click
    shiny::observe({
      perform_sync()
    }) |> bindEvent(input$sync_archives)

    # Handle auto-sync trigger
    shiny::observe({
      if (auto_sync_trigger() > 0) {
        perform_sync()
      }
    }) |> bindEvent(auto_sync_trigger())
    
    # Handle navbar tab switching to update grading progress
    shiny::observe({
      if (!is.null(input$main_navbar) && input$main_navbar == "validation") {
        # When validation pane becomes active, refresh the grading progress
        # This ensures the progress column reflects current grading status
        if (length(repo_list) > 0 && !is.null(template_obj)) {
          # Increment trigger to force table recalculation
          progress_update_trigger(progress_update_trigger() + 1)
        }
      }
    }) |> bindEvent(input$main_navbar, ignoreInit = TRUE)
  }
  
  # Return the app
  shiny::shinyApp(ui = ui, server = server)
}

