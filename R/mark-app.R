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

  # Parse the collection (knitr-style chunk headers are normalised per file).
  # Parse failures are captured per document, not raised, so one bad student
  # qmd does not abort the app; failing repos stay visible with their error.
  collection = parse_assignment_collection(repos_dir, use_qmd)

  # Attribute each document to the top-level repo directory it lives under:
  # the search is recursive, so a document may sit in a subdirectory of its
  # repo and basename(dirname(path)) would misattribute it
  collection$repo = if (nrow(collection) > 0) {
    vapply(
      fs::path_split(fs::path_rel(collection$path, repos_dir)),
      function(parts) parts[[1]],
      character(1)
    )
  } else {
    character(0)
  }

  validation_results = list()
  initial_repo_ast = NULL
  initial_repo_name = NULL

  # Every top-level directory under repos/ is a repository, so students with a
  # missing or unparseable document still get a row in the table instead of
  # silently vanishing.
  repo_list = fs::dir_ls(repos_dir, type = "directory") |>
    fs::path_file() |>
    sort()

  if (length(repo_list) == 0) {
    stop("No repositories found in repos directory: ", repos_dir)
  }

  # Per-repo problem messages: a missing document or the parse error verbatim
  doc_ext = if (use_qmd) ".qmd" else ".Rmd"
  repo_errors = list()
  for (repo in repo_list) {
    repo_rows = which(collection$repo == repo)
    if (length(repo_rows) == 0) {
      repo_errors[[repo]] = paste0("No ", doc_ext, " document found in this repository.")
    } else if (is.null(collection$ast[repo_rows][[1]])) {
      repo_errors[[repo]] = collection$error[repo_rows[1]]
    }
  }

  # Validate all repositories that parsed if a template is available
  if (!is.null(template_obj)) {
    for (repo in setdiff(repo_list, names(repo_errors))) {
      repo_rows = collection$repo == repo
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
          # Restrict owner/repo to GitHub-legal characters so a hostile remote
          # URL cannot smuggle markup into the table's href; keep dots in repo
          # names (only a trailing .git is dropped)
          github_re = "github\\.com[:/]([A-Za-z0-9-]+)/([A-Za-z0-9_.-]+)"
          if (grepl(github_re, github_url)) {
            repo_match = regmatches(github_url, regexec(github_re, github_url))[[1]]
            if (length(repo_match) >= 3) {
              repo_to_github[[repo]] = paste0(repo_match[2], "/", sub("\\.git$", "", repo_match[3]))
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

  # Set initial current repo AST: the first repository that parsed. When every
  # repo failed, the app still launches with the first repo selected so its
  # error is shown in the Validation card.
  ok_repos = setdiff(repo_list, names(repo_errors))
  if (length(ok_repos) > 0) {
    first_repo_rows = collection$repo == ok_repos[1]
    initial_repo_ast = collection$ast[first_repo_rows][[1]]
    initial_repo_name = ok_repos[1]
  } else {
    initial_repo_ast = NULL
    initial_repo_name = repo_list[1]
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
    template_path = if (is.character(template)) template else "project database",
    database_state = database_state,
    repo_errors = repo_errors
  )
}

# Resolve the template to use for marking.
#
# An explicit `template` override (path or markermd_template object) takes
# precedence over the template stored in the project database. Returns a
# validated markermd_template object, or errors if neither is available.
#
# project: markermd_project object
# template: override template (path, markermd_template, or NULL)

resolve_mark_template = function(project, template) {
  if (S7::S7_inherits(template, markermd_template)) {
    assert_template_compatible(template)
    return(template)
  }

  if (is.character(template) && length(template) == 1) {
    if (!file.exists(template)) {
      stop("Template file does not exist: ", template, call. = FALSE)
    }
    template_obj = read_template_yaml(template, require_ast = FALSE)
  } else if (!is.null(template)) {
    stop("Template must be a file path or markermd_template S7 object")
  } else {
    template_obj = load_template_from_db(project@root, base_dir = project@root)
    if (is.null(template_obj)) {
      cli::cli_abort(c(
        "No grading template is configured for this project.",
        "i" = "Author one with {.code markermd::template(\"{project@root}\")}, import one with {.code markermd::template_import()}, or pass {.arg template}."
      ))
    }
  }

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
# repo_errors: Named list mapping repos to their missing-document or parse
#   error message; repos absent from it parsed cleanly

create_markermd_app = function(root, repos_dir, template_obj, use_qmd, collection, repo_list, validation_results, initial_repo_ast, initial_repo_name, artifact_paths, repo_to_github, template_path = NULL, database_state = NULL, repo_errors = list()) {

  # Serve each repo's artifact via addResourcePath so the report's sibling
  # resources resolve; both viewers embed these URLs in same-origin iframes
  artifact_urls = register_artifact_resources(artifact_paths)

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
      # Font Awesome ships with shiny::icon()'s html dependency (the rubric
      # tab uses static icons), so no CDN stylesheet is needed
      shiny::tags$head(
        shinyjs::useShinyjs(),
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

          /* Repository select buttons. Selection highlight is toggled via the
             .active class (shinyjs) so selecting a repo does not re-render the
             whole table. Uses the theme primary color rather than a
             hard-coded Bootstrap 4 blue. */
          .repo-select-btn {
            background-color: white; color: #333; border: 1px solid #ddd;
            padding: 8px 12px; border-radius: 4px; text-align: left; width: 100%;
            cursor: pointer; font-size: 12px; white-space: nowrap;
            overflow: hidden; text-overflow: ellipsis;
          }
          .repo-select-btn.active {
            background-color: var(--bs-primary); color: white; border-color: var(--bs-primary);
          }

          /* Compact filter inputs in the Assignments card header */
          .repo-table-controls .shiny-input-container { margin-bottom: 0; }
          .repo-table-controls .form-control,
          .repo-table-controls .form-select {
            font-size: 12px; padding-top: 2px; padding-bottom: 2px;
          }


          /* Make switch more visible when in false state */
          .form-switch .form-check-input:not(:checked) {
            border-color: #212529 !important;
          }

          /* Monaco line decorations for the question highlight in the raw
             Source view (the overview-ruler marker comes from the decoration
             options themselves) */
          .highlight-line {
            background-color: rgba(255, 235, 59, 0.15) !important;
          }
          .highlight-margin {
            background-color: rgba(255, 235, 59, 0.15) !important;
            border-left: 3px solid rgba(255, 193, 7, 0.8) !important;
            width: 100% !important;
          }

          /* Rubric item rows: action buttons stay visible at reduced emphasis
             and regain full opacity on hover or keyboard focus, so they are
             reachable by keyboard and on touch devices */
          .rubric-item-row { transition: background-color 0.2s ease; }
          .rubric-item-row:hover { background-color: #e9ecef; }
          .rubric-action-btns { opacity: 0.35; transition: opacity 0.15s ease-in-out; }
          .rubric-item-row:hover .rubric-action-btns,
          .rubric-action-btns:hover,
          .rubric-action-btns:focus-within { opacity: 1; }
        
        '))
      ),
      # Main content area: layout_columns sits directly in the fillable nav
      # panel so both cards take the viewport height and scroll internally
      bslib::layout_columns(
        col_widths = c(6, 6),
        # Repository table card (left)
        bslib::card(
          bslib::card_header(
            class = "bg-light repo-table-controls",
            shiny::div(
              class = "d-flex justify-content-between align-items-center gap-2",
              shiny::span("Assignments"),
              shiny::div(
                class = "d-flex align-items-center gap-2",
                shiny::div(
                  style = "width: 150px;",
                  shiny::textInput("repo_filter", NULL, placeholder = "Filter name", width = "100%")
                ),
                shiny::div(
                  style = "width: 170px;",
                  shiny::selectInput(
                    "repo_status_filter", NULL,
                    choices = c(
                      "All repositories" = "all",
                      "Failed validation" = "failed",
                      "Not fully graded" = "ungraded"
                    ),
                    width = "100%",
                    selectize = FALSE
                  )
                )
              )
            )
          ),
          bslib::card_body(
            class = "overflow-auto",
            shiny::uiOutput("repo_table")
          )
        ),
        # Validation results card (right)
        bslib::card(
          bslib::card_header("Validation", class = "bg-light"),
          bslib::card_body(
            class = "overflow-auto",
            shiny::uiOutput("repo_parse_error"),
            mark_validate_ui("explore_module")
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
      class = "bg-light border-top text-center text-muted p-2 fs-6 text-truncate",
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
    selected_repo_index = shiny::reactiveVal(match(initial_repo_name, repo_list))

    # Surface a missing-document or parse error for the selected repo in place
    # of its validation results, quoting the parser's message verbatim
    output$repo_parse_error = shiny::renderUI({
      err = repo_errors[[current_repo_name()]]
      if (is.null(err)) return(NULL)
      shiny::div(
        class = "alert alert-danger d-flex align-items-start gap-2 small mb-2",
        shiny::icon("triangle-exclamation", class = "mt-1"),
        shiny::div(
          shiny::strong("This repository could not be validated."),
          shiny::tags$pre(
            err,
            class = "mb-0 mt-1 small bg-transparent border-0 p-0",
            style = "white-space: pre-wrap;"
          )
        )
      )
    })

    # Create reactive trigger for progress updates (initialized here to ensure it exists)
    progress_update_trigger = shiny::reactiveVal(0)

    # Debounced name filter so the table does not rebuild per keystroke
    repo_name_filter = shiny::debounce(shiny::reactive(input$repo_filter), 300)

    # Icon HTML for the raw gt cells; the Font Awesome dependency is already
    # on the page via the static shiny::icon() uses in the rubric tab
    cell_icon = function(name, ...) {
      as.character(shiny::icon(name, class = "fa-fw fs-6", ...))
    }

    # Create repository table with gt. Rendered as static HTML (rather than
    # gt::render_gt) so the gt_shiny input binding is not registered on the same
    # id as the output, which would trigger a shared input/output id warning.
    output$repo_table = shiny::renderUI({
      # Include the progress update trigger as a dependency to force refresh when needed
      trigger_value = progress_update_trigger()

      # Calculate grading progress for all repos at once: one set of queries
      # feeds the counts, the per-repo ungraded tooltips, and the status filter
      all_progress = NULL
      question_names = NULL
      graded_pairs = NULL
      if (!is.null(template_obj) && length(template_obj@questions) > 0) {
        question_names = sapply(template_obj@questions, function(q) q@name)
        graded_pairs = graded_question_pairs(root)
        all_progress = calculate_grading_progress(root, question_names, repo_list, graded_pairs = graded_pairs)
      }

      # Apply the name and status filters from the card header. Button ids
      # stay keyed to each repo's position in the full repo_list so the
      # observers registered over seq_along(repo_list) keep working.
      visible = repo_list
      flt = repo_name_filter()
      if (!is.null(flt) && nzchar(trimws(flt))) {
        visible = visible[grepl(tolower(trimws(flt)), tolower(visible), fixed = TRUE)]
      }
      if (identical(input$repo_status_filter, "failed")) {
        failed = vapply(repo_list, function(repo) {
          if (!is.null(repo_errors[[repo]])) return(TRUE)
          res = validation_results[[repo]]
          !is.null(res) && any(vapply(res, function(q) q$status %in% c("fail", "error"), logical(1)))
        }, logical(1))
        visible = intersect(visible, repo_list[failed])
      }
      if (identical(input$repo_status_filter, "ungraded") && !is.null(all_progress)) {
        visible = intersect(visible, repo_list[all_progress[repo_list] < length(question_names)])
      }

      if (length(visible) == 0) {
        return(shiny::p("No repositories match the current filters.", class = "text-muted fst-italic m-2"))
      }

      visible_idx = match(visible, repo_list)

      # Create table data with action buttons and validation summary
      repo_df = data.frame(
        Repository = visible,
        OriginalName = visible,  # Store original names for button creation
        stringsAsFactors = FALSE
      )

      # Add Folder column
      repo_df$Folder = sapply(visible_idx, function(i) {
        folder_button_id = paste0("folder_", i)
        paste0(
          '<button onclick="Shiny.setInputValue(\'', folder_button_id, '\', Math.random())" class="btn btn-link p-0 border-0 text-reset" title="Open folder">',
          cell_icon("folder-open"),
          '</button>'
        )
      })

      # Add GitHub column
      repo_df$GitHub = sapply(visible, function(repo) {
        if (repo %in% names(repo_to_github)) {
          github_repo = repo_to_github[[repo]]
          github_url = paste0("https://github.com/", github_repo)
          paste0(
            '<a href="', htmltools::htmlEscape(github_url, attribute = TRUE),
            '" target="_blank" class="text-reset text-decoration-none" title="Open on GitHub">',
            cell_icon("github"),
            '</a>'
          )
        } else {
          ""
        }
      })

      # Add artifact column with clickable icons for repos with a local report
      repo_df$Artifacts = sapply(visible_idx, function(i) {
        repo = repo_list[i]
        if (!is.na(artifact_paths[[repo]])) {
          # Has a resolved local report - clickable file icon
          button_id = paste0("artifact_", i)
          paste0(
            '<button onclick="Shiny.setInputValue(\'', button_id, '\', Math.random())" class="btn btn-link p-0 border-0 text-reset" title="View artifact">',
            cell_icon("file"),
            '</button>'
          )
        } else {
          # No report found - greyed out unclickable icon
          paste0('<span class="opacity-25" title="No artifact available">', cell_icon("file"), '</span>')
        }
      })

      # Add source code column with clickable file-code icons; repos without a
      # matching document get a greyed-out marker instead of a dead button
      repos_with_doc = collection$repo
      repo_df$Source = sapply(visible_idx, function(i) {
        repo = repo_list[i]
        if (!repo %in% repos_with_doc) {
          return(paste0('<span class="opacity-25" title="No document found">', cell_icon("file-code"), '</span>'))
        }
        button_id = paste0("source_", i)
        paste0(
          '<button onclick="Shiny.setInputValue(\'', button_id, '\', Math.random())" class="btn btn-link p-0 border-0 text-reset" title="View source code">',
          cell_icon("file-code"),
          '</button>'
        )
      })

      # Add validation summary column
      repo_df$Validation = sapply(
        visible, validation_status_cell,
        validation_results = validation_results, template_obj = template_obj,
        repo_errors = repo_errors
      )

      # Add grading progress column with progress bars
      repo_df$Grading = sapply(
        visible, grading_progress_cell,
        template_obj = template_obj, all_progress = all_progress,
        graded_pairs = graded_pairs, question_names = question_names
      )

      # Create clickable repository names. The active-selection highlight is read
      # once via isolate() (so the table is not re-rendered on every repo click)
      # and baked into the class; subsequent selections toggle .active via shinyjs.
      active_row = shiny::isolate(selected_repo_index())
      repo_df$Repository = purrr::map_chr(seq_along(visible), function(k) {
        i = visible_idx[k]
        active_class = if (i == active_row) " active" else ""

        # Just show repo name - no GitHub icon here. Escaped so a directory
        # name cannot inject markup into the raw-HTML table.
        content = htmltools::htmlEscape(visible[k])

        paste0(
          '<button onclick="Shiny.setInputValue(\'repo_select_', i, '\', Math.random())"',
          ' class="repo-select-btn', active_class, '" data-row="', i, '">',
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

              # Find rows for the selected repository; repos whose document is
              # missing or failed to parse get a NULL AST and show their error
              current_repo_name(selected_repo)
              repo_rows = collection$repo == selected_repo
              if (any(repo_rows) && !is.null(collection$ast[repo_rows][[1]])) {
                current_repo_ast(collection$ast[repo_rows][[1]])
              } else {
                current_repo_ast(NULL)
              }
            }) |> bindEvent(input[[button_id]])
          })
        }
    })

    # Move the selected-repo highlight in place instead of re-rendering the table.
    # Fires on init too, so the initial selection is highlighted once the table
    # is present.
    shiny::observe({
      idx = selected_repo_index()
      shinyjs::removeClass(selector = ".repo-select-btn", class = "active")
      shinyjs::addClass(
        selector = paste0(".repo-select-btn[data-row='", idx, "']"),
        class = "active"
      )
    }) |>
      shiny::bindEvent(selected_repo_index())

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
                # Show the served report in an iframe so its figures and
                # styles load without touching the app document
                shiny::showModal(
                  shiny::modalDialog(
                    title = repo,
                    size = "xl",
                    easyClose = TRUE,
                    footer = shiny::tags$a(
                      href = artifact_urls[[repo]],
                      target = "_blank",
                      class = "btn btn-outline-secondary btn-sm",
                      shiny::icon("up-right-from-square"),
                      " Open in browser"
                    ),
                    shiny::div(
                      style = "height: 70vh;",
                      shiny::tags$iframe(
                        src = artifact_urls[[repo]],
                        class = "w-100 h-100 border rounded bg-white"
                      )
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
                      shiny::icon("triangle-exclamation", class = "fa-fw fs-3 text-warning me-2"),
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
            repo_rows = collection$repo == repo
            if (any(repo_rows)) {
              file_path = collection$path[repo_rows][1]
              
              if (file.exists(file_path)) {
                # Read the raw source content
                raw_content = readLines(file_path, warn = FALSE) |> paste(collapse = "\n")

                file_name = basename(file_path)

                # One fixed editor id: the shared init disposes the previous
                # modal's editor, so repeated viewing does not leak instances
                editor_id = "markermd-source-modal-editor"

                # Show source in modal with Monaco Editor
                shiny::showModal(
                  shiny::modalDialog(
                    title = paste("Source Code:", file_name),
                    size = "xl",
                    easyClose = TRUE,
                    footer = shiny::modalButton("Close"),
                    shiny::div(
                      style = "height: 70vh;",
                      shiny::div(
                        id = editor_id,
                        class = "h-100 w-100 border rounded"
                      )
                    )
                  )
                )

                render_monaco_editor(editor_id, raw_content, "markdown")
              } else {
                # Show error modal - file not found
                shiny::showModal(
                  shiny::modalDialog(
                    title = "Source File Not Available",
                    easyClose = TRUE,
                    shiny::div(
                      class = "p-4 text-center",
                      shiny::icon("triangle-exclamation", class = "fa-fw fs-3 text-warning me-2"),
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
                    shiny::icon("triangle-exclamation", class = "fa-fw fs-3 text-danger me-2"),
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
    
    
    # Initialize rubric module, sharing the table's repo selection so both
    # tabs always point at the same repository
    rubric_result = mark_rubric_server(
      "rubric_module", template_obj, artifact_paths, artifact_urls, root,
      use_qmd, collection, database_state,
      selected_repo = current_repo_name
    )

    # Reverse sync: z/x navigation (or the dropdown) in the Rubric tab moves
    # the Assignments-table selection too, so the Validation card and the
    # table highlight stay honest when tabbing back
    shiny::observe({
      repo = rubric_result$selected_content_repo()
      shiny::req(repo)
      idx = match(repo, repo_list)
      if (!is.na(idx) && idx != shiny::isolate(selected_repo_index())) {
        selected_repo_index(idx)
        current_repo_name(repo)
        repo_rows = collection$repo == repo
        if (any(repo_rows) && !is.null(collection$ast[repo_rows][[1]])) {
          current_repo_ast(collection$ast[repo_rows][[1]])
        } else {
          current_repo_ast(NULL)
        }
      }
    }) |>
      bindEvent(rubric_result$selected_content_repo())

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

