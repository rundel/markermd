#' Launch the markermd Marking Application
#'
#' Points the marking app at an initialized markermd project (a directory
#' containing a `.markermd/` folder; see [init_project()]). The project's
#' configuration determines where the student repositories, grading template,
#' grading database, and rendered HTML reports ("artifacts") are located.
#'
#' @param path Character string. Path to a markermd project directory.
#' @param template Optional template override for validation, taking precedence
#'   over the project's configured template. Can be:
#'   - Character path to a saved template (`.yaml`/`.yml`)
#'   - A `markermd_template` S7 object
#'   - NULL (use the project's configured template)
#' @param use_qmd Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE). Default is TRUE.
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @return Launches Shiny application
#' @export
#'
#' @examples
#' \dontrun{
#' # Mark an initialized project
#' mark("/path/to/project/")
#'
#' # Mark .Rmd assignments
#' mark("/path/to/project/", use_qmd = FALSE)
#'
#' # Override the project's configured template
#' mark("/path/to/project/", template = "template.yaml")
#' }
mark = function(path, template = NULL, use_qmd = TRUE, ...) {
  app = mark_app(path, template = template, use_qmd = use_qmd)
  shiny::runApp(app, ...)
}

# Build the Shiny application object for the marking interface
#
# Loads the project configuration, then performs all setup (template load,
# collection parse, validation, database init, artifact resolution) and returns
# the app object without running it, so it can be tested.
#
# path: Path to a markermd project directory (containing .markermd/)
# template: markermd_template object or path overriding the project's template
# use_qmd: Whether to match .qmd files (TRUE) or .Rmd files (FALSE)

mark_app = function(path, template = NULL, use_qmd = TRUE) {

  if (missing(path)) {
    stop("path is required")
  }

  # Load and resolve the project configuration (errors if not a project)
  project = project_config(path)
  root = project@root

  if (is.na(project@repos)) {
    cli::cli_abort(c(
      "No repos directory is configured for this project.",
      "i" = "Record one with {.code markermd::project_set(\"{root}\", repos = \"repos\")}."
    ))
  }
  repos_dir = fs::path(root, project@repos)
  if (!fs::dir_exists(repos_dir)) {
    cli::cli_abort("Configured repos directory does not exist: {.path {repos_dir}}")
  }

  template_obj = resolve_mark_template(project, template)

  # Initialize database for persistent storage (lives at <root>/.markermd/)
  database_state = NULL
  if (!is.null(template_obj)) {
    tryCatch({
      database_state = initialize_database_state(root, template_obj)
    }, error = function(e) {
      warning("Database initialization failed: ", e$message)
      database_state = NULL
    })
  }

  # Parse the collection (knitr-style chunk headers are normalised per file)
  collection = parse_assignment_collection(repos_dir, use_qmd)

  validation_results = list()
  initial_repo_ast = NULL
  initial_repo_name = NULL

  if (is.null(collection) || nrow(collection) == 0) {
    stop("No valid documents found in repos directory: ", repos_dir)
  }

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

  # Detect GitHub remotes so the table can link each repo to its GitHub page
  repo_to_github = list()
  for (repo in repo_list) {
    repo_path = file.path(repos_dir, repo)

    tryCatch({
      git_root = gert::git_find(repo_path)
      if (!is.null(git_root)) {
        remotes = gert::git_remote_list(repo = repo_path)
        if (nrow(remotes) > 0 && any(grepl("github\\.com", remotes$url, ignore.case = TRUE))) {
          github_url = remotes$url[grepl("github\\.com", remotes$url)][1]
          if (grepl("github\\.com[:/]([^/]+)/([^/\\.]+)", github_url)) {
            repo_match = regmatches(github_url, regexec("github\\.com[:/]([^/]+)/([^/\\.]+)", github_url))[[1]]
            if (length(repo_match) >= 3) {
              repo_to_github[[repo]] = paste0(repo_match[2], "/", repo_match[3])
            }
          }
        }
      }
    }, error = function(e) {
      # Skip this repo
    })
  }

  # Resolve each repo's rendered HTML report from the project's artifacts dirs
  artifact_paths = resolve_repo_artifacts(project, repo_list)

  # Set initial current repo AST (first repository)
  first_repo_rows = collection$path |> dirname() |> basename() == repo_list[1]
  if (any(first_repo_rows)) {
    initial_repo_ast = collection$ast[first_repo_rows][[1]]
    initial_repo_name = repo_list[1]
  } else {
    stop("Could not load initial repository data")
  }

  app = create_markermd_app(
    root,
    repos_dir,
    template_obj,
    use_qmd,
    collection,
    repo_list,
    validation_results,
    initial_repo_ast,
    initial_repo_name,
    artifact_paths,
    repo_to_github,
    template_path = if (is.character(template)) template else project@template,
    database_state = database_state
  )
}

# Resolve the template to use for marking.
#
# An explicit `template` override (path or markermd_template object) takes
# precedence over the project's configured template. Returns a validated
# markermd_template object, or errors if neither is available.
#
# project: markermd_project object
# template: override template (path, markermd_template, or NULL)

resolve_mark_template = function(project, template) {
  if (S7::S7_inherits(template, markermd_template)) {
    assert_template_compatible(template)
    return(template)
  }

  template_path = if (is.character(template) && length(template) == 1) {
    template
  } else if (!is.null(template)) {
    stop("Template must be a file path or markermd_template S7 object")
  } else if (!is.na(project@template)) {
    if (fs::is_absolute_path(project@template)) project@template else fs::path(project@root, project@template)
  } else {
    cli::cli_abort(c(
      "No grading template is configured for this project.",
      "i" = "Record one with {.code markermd::project_set(\"{project@root}\", template = \"template.yaml\")} or pass {.arg template}."
    ))
  }

  if (!file.exists(template_path)) {
    stop("Template file does not exist: ", template_path, call. = FALSE)
  }
  template_obj = read_template_yaml(template_path, require_ast = FALSE)
  if (!S7::S7_inherits(template_obj, markermd_template)) {
    stop("Template file must contain a markermd_template S7 object")
  }
  assert_template_compatible(template_obj)
  template_obj
}


# Create the Shiny application object for the marking interface
#
# root: Project root directory (base for the grading database, under .markermd/)
# repos_dir: Directory containing the student repository subdirectories
# template_obj: markermd_template S7 object with node selections, or NULL
# use_qmd: Whether to parse .qmd files (TRUE) or .Rmd files (FALSE)
# collection: Parsed collection data (data frame with path and ast columns)
# repo_list: Character vector of repository names
# validation_results: List of validation results for each repository
# initial_repo_ast: Initial repository AST to display
# initial_repo_name: Name of initial repository
# artifact_paths: Named character vector mapping each repo to its local HTML
#   report path, or NA when none was found
# repo_to_github: Named list mapping repository names to GitHub repos
# template_path: Path to template file (optional)
# database_state: Database state loaded from SQLite (optional)

create_markermd_app = function(root, repos_dir, template_obj, use_qmd, collection, repo_list, validation_results, initial_repo_ast, initial_repo_name, artifact_paths, repo_to_github, template_path = NULL, database_state = NULL) {
  
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
              shiny::uiOutput("repo_table")
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
    
    
    # Footer with project and template info
    footer = shiny::div(
      class = "bg-light border-top text-center text-muted p-2 mt-3 fs-6",
      shiny::span(shiny::strong("Project:"), " ", shiny::code(root, class = "bg-light px-1 py-1 rounded small"), class = "me-3"),
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
      
      # Add artifact column with clickable icons for repos with a local report
      repo_df$Artifacts = sapply(seq_along(repo_list), function(i) {
        repo = repo_list[i]
        if (!is.na(artifact_paths[[repo]])) {
          # Has a resolved local report - clickable file icon
          button_id = paste0("artifact_", i)
          return(paste0('<button onclick="Shiny.setInputValue(\'', button_id, '\', Math.random())" class="btn btn-link p-0 border-0 text-reset" title="View artifact"><i class="far fa-file fs-6"></i></button>'))
        } else {
          # No report found - greyed out unclickable icon
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
        all_progress = calculate_grading_progress(root, question_names, repo_list)
      }

      # Add grading progress column with sparkline bars
      repo_df$Grading = sapply(
        repo_list, grading_progress_cell,
        template_obj = template_obj, all_progress = all_progress,
        collection_path = root, question_names = question_names
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
            cached_path = artifact_paths[[repo]]

            # Only process if this repo has a resolved local report
            if (!is.na(cached_path)) {
              if (file.exists(cached_path)) {
                # Read the HTML content and display it directly
                html_content = tryCatch({
                  readLines(cached_path, warn = FALSE) |> paste(collapse = "\n")
                }, error = function(e) {
                  paste("Error reading file:", e$message)
                })

                # Show artifact in modal with HTML content
                shiny::showModal(
                  shiny::modalDialog(
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
                # Show error modal - resolved file went missing
                shiny::showModal(
                  shiny::modalDialog(
                    title = "Artifact Not Available",
                    easyClose = TRUE,
                    shiny::div(
                      class = "p-4 text-center",
                      shiny::tags$i(class = "fas fa-exclamation-triangle fs-3 text-warning me-2"),
                      "Artifact file not found."
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
                  shiny::modalDialog(
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
                  shiny::modalDialog(
                    title = "Source File Not Available",
                    easyClose = TRUE,
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
            repo_path = normalizePath(file.path(repos_dir, repo), mustWork = FALSE)

            # Attempt to open the folder
            success = open_folder(repo_path)

            if (!success) {
              # Show error modal if folder couldn't be opened
              shiny::showModal(
                shiny::modalDialog(
                  title = "Error Opening Folder",
                  easyClose = TRUE,
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
    rubric_result = mark_rubric_server("rubric_module", template_obj, artifact_paths, root, use_qmd, collection, database_state)

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

