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
#' @return The value returned by [shiny::runApp()] when the app exits; called
#'   for its side effect of running the marking application.
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
    cli::cli_abort("path is required")
  }

  # Load and resolve the project configuration (errors if not a project)
  project = project_config(path)
  root = project@root

  repos_dir = project_repos_dir(project)

  template_obj = resolve_mark_template(project, template)

  # Initialize database for persistent storage (lives at <root>/.markermd/). A
  # failure is downgraded to a warning so the app still opens in a read-only-ish
  # degraded mode; database_state then stays NULL (set above).
  database_state = NULL
  if (!is.null(template_obj)) {
    tryCatch({
      database_state = initialize_database_state(root, template_obj)
    }, error = function(e) {
      warning("Database initialization failed: ", e$message)
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
    cli::cli_abort("No repositories found in repos directory: {repos_dir}")
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
      cli::cli_abort("Template file does not exist: {template}")
    }
    template_obj = read_template_yaml(template, require_ast = FALSE)
  } else if (!is.null(template)) {
    cli::cli_abort("Template must be a file path or markermd_template S7 object")
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
    cli::cli_abort("Template file must contain a markermd_template S7 object")
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
# database_state: Database state loaded from SQLite (optional)
# repo_errors: Named list mapping repos to their missing-document or parse
#   error message; repos absent from it parsed cleanly

create_markermd_app = function(root, repos_dir, template_obj, use_qmd, collection, repo_list, validation_results, initial_repo_ast, initial_repo_name, artifact_paths, repo_to_github, database_state = NULL, repo_errors = list()) {

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
        markermd_modal_css(),
        shiny::tags$style(shiny::HTML('
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


          /* Hide the parse-error slot when empty so the card body flex gap
             does not push the validation cards down */
          #repo_parse_error:empty { display: none; }

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
              class = "d-flex justify-content-between align-items-center gap-2 w-100",
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
                      "Incomplete" = "ungraded",
                      "Complete" = "graded"
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
    
    # Rubric tab (right aligned): the content pane and rubric are sibling
    # modules; the hotkey script drives ids in both, so it gets both namespaces
    bslib::nav_panel(
      title = "Rubric",
      value = "rubric",
      bslib::layout_columns(
        col_widths = c(7, 5),
        class = "h-100",
        mark_content_ui("content_module"),
        mark_rubric_ui("rubric_module"),
        rubric_hotkeys_js(shiny::NS("rubric_module"), shiny::NS("content_module"))
      )
    ),
    
    
    # Footer with project info
    footer = shiny::div(
      class = "bg-light border-top text-center text-muted p-2 fs-6 text-truncate",
      shiny::span(shiny::strong("Project:"), " ", shiny::code(root, class = "bg-light px-1 py-1 rounded small"))
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

    # Debounced name filter so the table does not rebuild per keystroke
    repo_name_filter = shiny::debounce(shiny::reactive(input$repo_filter), 300)

    # Create repository table with gt (the data/gt builders live in
    # utils_mark_table.R). Rendered as static HTML (rather than gt::render_gt)
    # so the gt_shiny input binding is not registered on the same id as the
    # output, which would trigger a shared input/output id warning.
    output$repo_table = shiny::renderUI({
      # Rebuild only after grading data is actually written (the rubric module
      # bumps its version on selection, comment, delete, and import writes);
      # the output is suspended while the Validation tab is hidden, so tabbing
      # back recomputes at most once
      rubric_result$grading_data_version()

      # Calculate grading progress for all repos at once: one set of queries
      # feeds the counts, the per-repo ungraded tooltips, and the status filter
      all_progress = NULL
      question_names = NULL
      graded_pairs = NULL
      if (!is.null(template_obj) && length(template_obj@questions) > 0) {
        question_names = template_question_names(template_obj)
        graded_pairs = graded_question_pairs(root)
        all_progress = calculate_grading_progress(root, question_names, repo_list, graded_pairs = graded_pairs)
      }

      visible = filter_repo_table_rows(
        repo_list,
        name_filter = repo_name_filter(),
        status_filter = input$repo_status_filter,
        validation_results = validation_results,
        repo_errors = repo_errors,
        all_progress = all_progress,
        question_names = question_names
      )

      if (length(visible) == 0) {
        return(shiny::p("No repositories match the current filters.", class = "text-muted fst-italic m-2"))
      }

      # The active-selection highlight is read once via isolate() (so the
      # table is not re-rendered on every repo click) and baked into the
      # class; subsequent selections toggle .active via shinyjs.
      table_data = build_repo_table_data(
        visible = visible,
        visible_idx = match(visible, repo_list),
        repo_list = repo_list,
        collection = collection,
        artifact_paths = artifact_paths,
        repo_to_github = repo_to_github,
        validation_results = validation_results,
        template_obj = template_obj,
        repo_errors = repo_errors,
        all_progress = all_progress,
        graded_pairs = graded_pairs,
        question_names = question_names,
        active_row = shiny::isolate(selected_repo_index())
      )

      shiny::HTML(gt::as_raw_html(repo_table_gt(table_data), inline_css = FALSE))
    })
    
    # Handle repository button clicks (the clicked row index is the input value)
    shiny::observe({
      row_index = input$repo_select_clicked
      selected_repo = repo_list[row_index]

      # Update selected index for highlighting
      selected_repo_index(row_index)

      # Repos whose document is missing or failed to parse get a NULL AST and
      # show their error
      current_repo_name(selected_repo)
      current_repo_ast(collection_ast_for(collection, selected_repo))
    }) |> shiny::bindEvent(input$repo_select_clicked)

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
      repo = repo_list[input$artifact_clicked]
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
    }) |> shiny::bindEvent(input$artifact_clicked)
    
    # Handle source button clicks
    shiny::observe({
      repo = repo_list[input$source_clicked]

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
    }) |> shiny::bindEvent(input$source_clicked)

    # Handle folder button clicks
    shiny::observe({
      repo = repo_list[input$folder_clicked]
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
    }) |> shiny::bindEvent(input$folder_clicked)
    
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
    mark_validate_server("explore_module", current_repo_ast, current_repo_validation, shiny::reactiveVal(template_obj))
    
    
    # Initialize the content module, sharing the table's repo selection so
    # both tabs always point at the same repository. selected_question is a
    # lazy forward reference: reactives only evaluate at flush time, after
    # rubric_result is assigned below.
    content_result = mark_content_server(
      "content_module", template_obj, collection, artifact_paths, artifact_urls, use_qmd,
      selected_question = shiny::reactive(rubric_result$selected_question()),
      external_repo = current_repo_name
    )

    # Initialize the rubric module, keyed on the content pane's repo selection
    rubric_result = mark_rubric_server(
      "rubric_module", template_obj, repo_list, root, database_state,
      selected_repo = content_result$selected_repo,
      set_repo_labels = content_result$set_repo_labels
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
        current_repo_ast(collection_ast_for(collection, repo))
      }
    }) |>
      shiny::bindEvent(rubric_result$selected_content_repo())

  }
  
  # Return the app
  shiny::shinyApp(ui = ui, server = server)
}

