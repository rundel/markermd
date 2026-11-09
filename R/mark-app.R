#' Launch the markermd Marking Application
#'
#' Points the marking app at an initialized markermd project (a directory
#' containing a `.markermd/` folder; see [init_project()]). The project's
#' configuration determines where the student repositories, grading template,
#' grading database, and rendered HTML reports ("artifacts") are located.
#'
#' @param project Character string. Path to a markermd project directory.
#' @param template Optional template override for validation, taking precedence
#'   over the project's configured template. Can be:
#'   - Character path to a saved template (`.yaml`/`.yml`)
#'   - A `markermd_template` S7 object
#'   - NULL (use the project's configured template)
#' @param use_qmd Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE). Default is TRUE.
#' @param terminal Whether to add a Terminal tab that runs a shell or an agent
#'   CLI (`claude`, `codex`) in the project root, using the optional
#'   [shinyterm](https://github.com/rundel/shinyterm) package. One of:
#'   - `FALSE` (the default): no Terminal tab
#'   - `TRUE`: offer the user's shell plus `claude` and `codex` when they are
#'     on the `PATH`, each agent with its own sandbox confined to the project
#'     (see [shinyterm::default_commands()])
#'   - A named list of commands, as accepted by
#'     [shinyterm::terminal_tabs_server()], e.g.
#'     `list(Claude = shinyterm::claude_command(wd = project, permission_mode = "acceptEdits"))`
#'
#'   A terminal runs as you, with your files and credentials, so only use it
#'   on your own machine; shinyterm refuses to run on a hosted server. With
#'   the tab on, the app is protected by a random access token: the browser
#'   opens at a URL ending in `?shinyterm_token=...` (also printed when the
#'   app starts), and a page opened without it gets no terminal process.
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
#'
#' # Add a Terminal tab for a shell, Claude Code or Codex
#' mark("/path/to/project/", terminal = TRUE)
#' }
mark = function(project, template = NULL, use_qmd = TRUE, terminal = FALSE, ...) {
  app = mark_app(project, template = template, use_qmd = use_qmd, terminal = terminal)
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
# terminal: FALSE, TRUE or a named list of commands (see mark())
# terminal_token: The Terminal tab's access token, as for
#   shinyterm::terminal_token(); FALSE turns it off (the browser tests do,
#   since AppDriver cannot add the token's query string)

mark_app = function(project, template = NULL, use_qmd = TRUE, terminal = FALSE, terminal_token = TRUE) {

  if (missing(project)) {
    cli::cli_abort("project is required")
  }

  # Load and resolve the project configuration (errors if not a project)
  config = project_config(project)
  root = config@root
  terminal_commands = resolve_mark_terminal(terminal, root)

  repos_dir = project_repos_dir(config)

  template_obj = resolve_mark_template(config, template)

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

  # Every top-level directory under repos/ is a repository, so students with a
  # missing or unparseable document still get a row in the table instead of
  # silently vanishing.
  repo_list = fs::dir_ls(repos_dir, type = "directory") |>
    fs::path_file() |>
    sort()

  if (length(repo_list) == 0) {
    cli::cli_abort("No repositories found in repos directory: {repos_dir}")
  }

  # Parse one document per repository (knitr-style chunk headers are
  # normalized per file): the document validate_project() and parse_project()
  # pick, so the app and the headless checks agree. Parse failures are
  # captured per repository, not raised, so one bad student qmd does not
  # abort the app; failing repos stay visible with their error.
  assignment_file = basename_or_null(attr(template_obj, "markermd_source_raw"))
  collection = parse_repo_documents(repos_dir, repo_list, assignment_file, use_qmd)

  checked = check_repo_documents(collection, repo_list, template_obj, repos_dir, use_qmd)
  repo_errors = checked$errors
  validation_results = checked$validation

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
  artifact_paths = resolve_repo_artifacts(config, repo_list)

  # The initially selected repository: the first that parsed. When every repo
  # failed, the app still launches with the first repo selected.
  ok_repos = setdiff(repo_list, names(repo_errors))
  initial_repo_name = if (length(ok_repos) > 0) ok_repos[1] else repo_list[1]

  app = create_markermd_app(
    root,
    repos_dir,
    template_obj,
    use_qmd,
    collection,
    repo_list,
    validation_results,
    initial_repo_name,
    artifact_paths,
    repo_to_github,
    database_state = database_state,
    repo_errors = repo_errors,
    terminal_commands = terminal_commands,
    terminal_token = if (!is.null(terminal_commands)) shinyterm::terminal_token(terminal_token)
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


# The per-repository problem messages (a missing document or the parse error
# verbatim) and validation results for a parsed collection. Shared by the
# app's startup and its per-repository re-check.
#
# collection: Data frame from parse_repo_documents()
# repo_list: The repository names to report on
# template_obj: markermd_template object, or NULL (no validation)
# repos_dir: The directory holding the repositories
# use_qmd: Whether the documents are .qmd (TRUE) or .Rmd (FALSE)
# Returns: List with errors (named list by repo) and validation (named list
#   by repo of validate_repo_against_rules() results)

check_repo_documents = function(collection, repo_list, template_obj, repos_dir, use_qmd) {
  doc_ext = if (use_qmd) ".qmd" else ".Rmd"
  errors = list()
  validation = list()
  for (repo in repo_list) {
    repo_rows = which(collection$repo == repo)
    if (length(repo_rows) == 0) {
      errors[[repo]] = paste0("No ", doc_ext, " document found in this repository.")
    } else if (is.null(collection$ast[repo_rows][[1]])) {
      errors[[repo]] = collection$error[repo_rows[1]]
    } else if (!is.null(template_obj)) {
      validation[[repo]] = validate_repo_against_rules(
        collection$ast[repo_rows][[1]], template_obj, repo_dir = file.path(repos_dir, repo)
      )
    }
  }
  list(errors = errors, validation = validation)
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
# initial_repo_name: Name of initial repository
# artifact_paths: Named character vector mapping each repo to its local HTML
#   report path, or NA when none was found
# repo_to_github: Named list mapping repository names to GitHub repos
# database_state: Database state loaded from SQLite (optional)
# repo_errors: Named list mapping repos to their missing-document or parse
#   error message; repos absent from it parsed cleanly
# terminal_commands: Named list of shinyterm commands for the Terminal tab,
#   or NULL for no tab (see resolve_mark_terminal())
# terminal_token: The Terminal tab's access token, or NULL for none

create_markermd_app = function(root, repos_dir, template_obj, use_qmd, collection, repo_list, validation_results, initial_repo_name, artifact_paths, repo_to_github, database_state = NULL, repo_errors = list(), terminal_commands = NULL, terminal_token = NULL) {

  # Serve each repo's artifact via addResourcePath so the report's sibling
  # resources resolve; the viewers embed these URLs in sandboxed iframes
  artifact_urls = register_artifact_resources(artifact_paths)

  # Define UI
  ui = bslib::page_navbar(
    title = "markermd - Marking",
    theme = bslib::bs_theme(version = 5),
    selected = "assignments",
    id = "main_navbar",  # Add id to enable navigation tracking
    
    # Right-align the navigation tabs
    bslib::nav_spacer(),
    
    # Assignments tab (right aligned): the landing page, one card with the
    # per-repository table (links, validation status, grading progress and
    # scores, with the class summary pinned beneath)
    bslib::nav_panel(
      title = "Assignments",
      value = "assignments",
      # Font Awesome comes from fa_dependency() in the head below (the
      # fa_icon() tags carry none), so no CDN stylesheet is needed
      app_loading_overlay("Loading the grading data\u2026"),
      shiny::tags$head(
        shinyjs::useShinyjs(),
        markermd_modal_css(),
        fa_dependency(),
        mark_validate_script("explore_module"),
        rubric_item_editing_head(),
        # A focused score cell in the Assignments table opens on Enter or
        # Space like a click (the cells are plain spans; see score_cell())
        shiny::tags$script(shiny::HTML("
          document.addEventListener('keydown', function(e) {
            if ((e.key === 'Enter' || e.key === ' ') && e.target.matches && e.target.matches('#repo_table .score-link')) {
              e.preventDefault();
              e.target.click();
            }
          });
        ")),
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

          /* Compact filter inputs in the Assignments card header */
          .repo-table-controls .shiny-input-container { margin-bottom: 0; }
          .repo-table-controls .form-control,
          .repo-table-controls .form-select {
            font-size: 12px; padding-top: 2px; padding-bottom: 2px;
          }

          /* Score cells in the Assignments table read as links on hover;
             the click is handled by the table onClick. The summary footer
             stays pinned while the rows scroll (reactable pins the header
             by itself). */
          .score-link { cursor: pointer; }
          .score-link:hover { text-decoration: underline; }
          #repo_table .rt-tfoot { position: sticky; bottom: 0; z-index: 1; }


          /* Hide the parse-error slot when empty so the card body flex gap
             does not push the validation cards down */
          #repo_parse_error:empty { display: none; }

          /* The navbar snapshot status: a quiet link that turns amber once
             many selection events have piled up since the latest snapshot */
          #snapshot_status .snapshot-status { font-size: 12px; text-decoration: none; }
          #snapshot_status .snapshot-status-warn { color: #b35c00 !important; }

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
          .rubric-group-head:hover .rubric-action-btns,
          .rubric-action-btns:hover,
          .rubric-action-btns:focus-within { opacity: 1; }
          .rubric-group-menu { font-size: 12px; min-width: 200px; }
          .rubric-group-menu .dropdown-item { padding: 4px 12px; }
          .rubric-group-menu .dropdown-header { padding: 4px 12px; font-size: 11px; }

          /* Rubric item groups: a left rule (amber while a bound is hit)
             around a header row and the member rows, with the subtotal in
             the header keeping clear of its hover actions */
          .rubric-group { border-left: 3px solid #ced4da; margin: 8px 0 8px 2px; }
          .rubric-group-capped { border-left-color: #f0a04b; }
          .rubric-group-head {
            padding: 4px 8px 4px 10px; background: #f8f9fa; border-radius: 0 4px 4px 0;
          }
          .rubric-group-name { font-weight: 600; font-size: 12.5px; }
          .rubric-group-chip {
            font-size: 11px; border: 1px solid #ced4da; border-radius: 10px; padding: 0 7px;
            color: #495057; background: #fff; white-space: nowrap;
          }
          .rubric-group-chip-excl { border-color: #9ec5fe; color: #0a58ca; background: #f0f6ff; }
          .rubric-group-subtotal {
            margin-left: auto; font-size: 12px; font-weight: 600; white-space: nowrap;
            padding-right: 64px;
          }
          .rubric-group-subtotal .raw {
            text-decoration: line-through; color: #6c757d; font-weight: 400; margin-right: 4px;
          }
          .rubric-group-capped .rubric-group-subtotal .eff { color: #b35c00; }
          .rubric-group .rubric-item-row { margin-left: 6px !important; }
          .rubric-hk-radio { border-radius: 50% !important; }
          .rubric-group-settings .form-group { margin-bottom: 8px; }
          .rubric-group-settings .control-label { font-size: 12px; font-weight: 600; margin-bottom: 2px; }
          .rubric-group-settings .form-control { font-size: 12.5px; padding: 3px 8px; }

          /* The cap badge beside the score and the breakdown it opens; the
             breakdown and group settings popovers are wider than the
             Bootstrap default so their columns fit */
          .rubric-wide-popover { --bs-popover-max-width: 400px; }
          .rubric-cap-badge {
            background: #fff3e0; color: #b35c00; border: 1px solid #f3c58f; border-radius: 10px;
            font-size: 11px; font-weight: 600; padding: 1px 8px; cursor: pointer; white-space: nowrap;
          }
          .rubric-score-breakdown td { padding: 2px 6px; font-size: 12px; }

        
        '))
      ),
      # The table card sits directly in the fillable nav panel so it takes
      # the viewport height and the full width (a long template has many
      # score columns); the table fills the card body and scrolls inside it
      # with its header and summary footer pinned
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
          class = "p-2",
          reactable::reactableOutput("repo_table", height = "100%")
        )
      )
    ),

    # Grade tab (right aligned): the content pane and rubric are sibling
    # modules; the hotkey script drives ids in both, so it gets both namespaces
    bslib::nav_panel(
      title = "Grade",
      value = "grade",
      bslib::layout_columns(
        col_widths = c(7, 5),
        class = "h-100",
        mark_content_ui("content_module"),
        mark_rubric_ui("rubric_module"),
        rubric_hotkeys_js(shiny::NS("rubric_module"), shiny::NS("content_module"))
      )
    ),

    # Rubric tab (right aligned): every question's rubric edited in one
    # place, without the documents; drawn by the same rubric module as the
    # Grade tab's pane, so the two never disagree
    bslib::nav_panel(
      title = "Rubric",
      value = "rubric",
      mark_rubric_editor_ui("rubric_module")
    ),

    # History tab (right aligned): one timeline of the project's snapshots
    # and its activity log (markermd calls by agents and scripts, the app's
    # own grading), with the detail, compare, restore and export actions
    # (see mod_mark_history.R)
    bslib::nav_panel(
      title = "History",
      value = "history",
      mark_history_ui("history_module")
    ),

    # Terminal tab (only when mark(terminal = ...) opts in): a shinyterm
    # tabset for a shell or an agent CLI in the project root
    if (!is.null(terminal_commands)) {
      bslib::nav_panel(
        title = "Terminal",
        value = "terminal",
        mark_terminal_ui("terminal_module")
      )
    },

    # The snapshot status: when the latest snapshot was taken and how far the
    # grading has moved since; opens the History tab
    bslib::nav_item(shiny::uiOutput("snapshot_status", inline = TRUE)),

    # Notices about changes made outside the app that it cannot show in place
    header = shiny::uiOutput("external_change_banner"),

    # Footer with project info
    footer = shiny::div(
      class = "bg-light border-top text-center text-muted p-2 fs-6 text-truncate",
      shiny::span(shiny::strong("Project:"), " ", shiny::code(root, class = "bg-light px-1 py-1 rounded small"))
    )
  )
  
  # Define server logic
  server = function(input, output, session) {

    # The rubric module's result is assigned after the table and content
    # modules that read it (reactives only evaluate at flush time); the
    # placeholder makes it a local for static code inspection
    rubric_result = NULL

    # One database connection for the session's reactive work, instead of
    # one (and its PRAGMAs) per call; see with_database()
    open_session_database(root, session)

    # Export values for testing
    shiny::exportTestValues(
      repo_names = repo_list,
      n_questions = if (is.null(template_obj)) 0L else length(template_obj@questions),
      validation_status = lapply(validation_results, function(repo_res) {
        vapply(repo_res, function(q) q$status, character(1))
      })
    )

    # Changes made outside this session (an agent's import, a script, another
    # browser tab): the watcher bumps db_changed on every commit by another
    # connection, and the observer below reads the action log for recorded
    # calls and bumps external_version when there are new ones, which every
    # database-backed display depends on
    db_changed = shiny::reactiveVal(0L)
    external_version = shiny::reactiveVal(0L)
    external_notice = shiny::reactiveVal(NULL)
    last_action_id = with_database(root, latest_action_row_id)
    template_stamp = function() {
      with_database(root, function(conn) {
        DBI::dbGetQuery(conn, "SELECT updated_at FROM metadata WHERE key = 'template'")$updated_at
      })
    }
    initial_template_stamp = template_stamp()
    db_watch = watch_project_db(root, session, function() db_changed(shiny::isolate(db_changed()) + 1L))
    # Called right after each of this session's own writes, so the change it
    # makes does not wake the external-change observer below
    note_own_commit = db_watch$absorb

    # The parsed documents and their validation, re-read for one repository
    # on request (the validation modal's Re-check, after a submission was
    # repaired in place); everything that shows them reads these
    collection_rv = shiny::reactiveVal(collection)
    validation_rv = shiny::reactiveVal(validation_results)
    errors_rv = shiny::reactiveVal(repo_errors)
    assignment_file = basename_or_null(attr(template_obj, "markermd_source_raw"))

    recheck_repo = function(repo) {
      fresh = parse_repo_documents(repos_dir, repo, assignment_file, use_qmd)
      rows = collection_rv()
      rows = rbind(rows[rows$repo != repo, , drop = FALSE], fresh)
      rows = rows[order(match(rows$repo, repo_list)), , drop = FALSE]
      rownames(rows) = NULL
      collection_rv(rows)
      checked = check_repo_documents(fresh, repo, template_obj, repos_dir, use_qmd)
      errors = errors_rv()
      errors[[repo]] = checked$errors[[repo]]
      errors_rv(errors)
      validation = validation_rv()
      validation[[repo]] = checked$validation[[repo]]
      validation_rv(validation)
      invisible(NULL)
    }

    # The actions this session recorded itself (rubric edits and imports,
    # restores; see with_action()), so the external-change watcher below does
    # not announce them as changes made outside the app
    own_action_ids = character(0)
    note_own_action = function() {
      id = last_recorded_action_id()
      if (!is.null(id)) {
        own_action_ids <<- c(own_action_ids, id)
      }
      note_own_commit()
      invisible(id)
    }

    # Run a bulk operation of the app as a recorded action with the actor
    # "app" (see with_action()); the rubric module's imports, edits and
    # deletions go through it
    app_action = function(fn, code, call = fn, summary = NULL, parts = character(0)) {
      result = withVisible(with_action(root, fn, code, call = call, summary = summary, parts = parts, actor = "app"))
      note_own_action()
      if (result$visible) result$value else invisible(result$value)
    }

    # Reactive values for user selections
    current_repo_name = shiny::reactiveVal(initial_repo_name)

    # The one way to change the app-wide repository selection. Used by the
    # table's score click-through and the rubric's navigation sync.
    select_repo = function(repo) {
      if (length(repo) != 1 || !repo %in% repo_list) {
        return(invisible(FALSE))
      }
      current_repo_name(repo)
      invisible(TRUE)
    }

    # The validation modal: one repository's per-question rule results,
    # opened from that repository's validation marker in the table, without
    # moving the app-wide selection. validation_shown is bumped on every
    # opening, so the results render afresh into the modal's new DOM even
    # when the same repository is opened twice.
    validation_repo = shiny::reactiveVal(initial_repo_name)
    validation_shown = shiny::reactiveVal(0L)

    show_validation_modal = function() {
      repo = shiny::isolate(validation_repo())
      validation_shown(shiny::isolate(validation_shown()) + 1L)
      shiny::showModal(shiny::modalDialog(
        title = shiny::div(
          class = "d-flex justify-content-between align-items-center gap-3",
          shiny::span("Validation: ", shiny::tags$code(repo)),
          shiny::div(
            class = "fs-6 fw-normal",
            style = "width: 220px;",
            mark_validate_question_select_ui(
              "explore_module",
              validation_question_choices(
                if (is.null(template_obj)) character(0) else template_question_names(template_obj),
                shiny::isolate(validation_rv())[[repo]]
              )
            )
          )
        ),
        size = "xl",
        easyClose = TRUE,
        footer = shiny::tagList(
          # Re-parse and re-validate this repository, for a submission
          # repaired while the app is open (the Terminal tab, an editor)
          bslib::tooltip(
            shiny::actionButton("revalidate_repo", "Re-check", icon = fa_icon("rotate"), class = "btn-outline-secondary"),
            "Parse and validate this repository's document again"
          ),
          shiny::modalButton("Close")
        ),
        shiny::uiOutput("repo_parse_error"),
        shiny::uiOutput("repo_parse_warnings"),
        mark_validate_ui("explore_module")
      ))
    }

    # The modal repo's parser warnings: the document parsed, but the parser
    # flagged something (q2r keeps them on the AST rather than printing them)
    output$repo_parse_warnings = shiny::renderUI({
      validation_shown()
      parse_warnings_ui(
        document_parse_warnings(collection_ast_for(collection_rv(), validation_repo())),
        what = "this repository's document"
      )
    })

    shiny::observe({
      repo = validation_repo()
      shiny::req(repo)
      recheck_repo(repo)
      show_validation_modal()
      shiny::showNotification(paste0("Re-checked ", repo, "."), type = "message")
    }) |> shiny::bindEvent(input$revalidate_repo)

    # Surface a missing-document or parse error for the modal's repo in place
    # of its validation results, quoting the parser's message verbatim
    output$repo_parse_error = shiny::renderUI({
      validation_shown()
      err = errors_rv()[[validation_repo()]]
      if (is.null(err)) return(NULL)
      shiny::div(
        class = "alert alert-danger d-flex align-items-start gap-2 small mb-2",
        fa_icon("triangle-exclamation", class = "mt-1"),
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

    # Everything the table reads from the database, computed once per grading
    # write or visit to the tab: the graded pairs and per-repo progress (which
    # also drive the status filter), the per-question totals, and the wide
    # score table with its class summary. The rubric module bumps its version
    # on selection, comment, delete and import writes; a return to the tab
    # (assignments_visit) recomputes as well, which picks up a question total
    # edited in the rubric pane (that edit does not bump the version). Only
    # arrivals count: reading input$main_navbar itself re-rendered the
    # leaving table on every switch away, before the client reported it
    # hidden.
    assignments_visit = shiny::reactiveVal(0L)
    shiny::observe({
      if (identical(input$main_navbar, "assignments")) {
        assignments_visit(shiny::isolate(assignments_visit()) + 1L)
      }
    }) |>
      shiny::bindEvent(input$main_navbar)

    table_state = shiny::reactive({
      rubric_result$grading_data_version()
      external_version()
      assignments_visit()

      if (is.null(template_obj) || length(template_obj@questions) == 0) {
        return(list(
          question_names = NULL, graded_pairs = NULL, all_progress = NULL,
          totals = numeric(0), optional = character(0), scores = NULL, summary = NULL
        ))
      }
      question_names = template_question_names(template_obj)
      graded_pairs = graded_question_pairs(root, repo_list)
      score_data = collect_score_data(root, question_names, repo_list)
      scores = build_scores_table_data(score_data, question_names, repo_list)
      list(
        question_names = question_names,
        graded_pairs = graded_pairs,
        all_progress = calculate_grading_progress(root, question_names, repo_list, graded_pairs = graded_pairs),
        totals = question_total_scores(root, question_names),
        optional = optional_question_names(root, question_names),
        scores = scores,
        summary = build_scores_summary_rows(scores, question_names)
      )
    })

    # For shinytest2 assertions on the score columns and their summary
    shiny::exportTestValues(
      scores_table = table_state()$scores,
      scores_summary = table_state()$summary
    )

    # The Assignments table (the data and reactable builders live in
    # utils_mark_table.R); the filters narrow the rows server-side, and the
    # summary footer always covers the whole class
    output$repo_table = reactable::renderReactable({
      state = table_state()

      visible = filter_repo_table_rows(
        repo_list,
        name_filter = repo_name_filter(),
        status_filter = input$repo_status_filter,
        validation_results = validation_rv(),
        repo_errors = errors_rv(),
        all_progress = state$all_progress,
        question_names = state$question_names
      )

      table_data = build_repo_table_data(
        visible = visible,
        visible_idx = match(visible, repo_list),
        repo_list = repo_list,
        collection = collection_rv(),
        artifact_paths = artifact_paths,
        repo_to_github = repo_to_github,
        validation_results = validation_rv(),
        template_obj = template_obj,
        repo_errors = errors_rv(),
        all_progress = state$all_progress,
        graded_pairs = state$graded_pairs,
        question_names = state$question_names,
        scores_table = state$scores
      )

      repo_table_reactable(
        table_data, state$summary, state$totals,
        click_input = "score_clicked", optional = state$optional
      )
    })

    # A click on a score cell in the table opens that repository and question
    # in the Grade tab. The content module follows current_repo_name and the
    # rubric module follows the content module.
    shiny::observe({
      target = input$score_clicked
      shiny::req(select_repo(target$repo))
      rubric_result$select_question(target$question)
      bslib::nav_select("main_navbar", "grade")
    }) |> shiny::bindEvent(input$score_clicked)

    # A click on a repo's validation marker in the table opens its
    # validation results in the modal
    shiny::observe({
      repo = repo_list[input$validation_clicked]
      shiny::req(length(repo) == 1, !is.na(repo))
      validation_repo(repo)
      show_validation_modal()
    }) |> shiny::bindEvent(input$validation_clicked)

    # Handle artifact button clicks
    shiny::observe({
      repo = repo_list[input$artifact_clicked]
      cached_path = artifact_paths[[repo]]

      # Only process if this repo has a resolved local report
      if (!is.na(cached_path)) {
        if (file.exists(cached_path)) {
          # Show the served report in a sandboxed iframe so its figures
          # and styles load without its scripts reaching the app
          shiny::showModal(
            shiny::modalDialog(
              title = repo,
              size = "xl",
              easyClose = TRUE,
              footer = shiny::tags$a(
                href = artifact_urls[[repo]],
                target = "_blank",
                class = "btn btn-outline-secondary btn-sm",
                fa_icon("up-right-from-square"),
                " Open in browser"
              ),
              # Names the report shown when several HTML files were found
              # for the repo (NULL otherwise)
              artifact_ambiguity_note(artifact_paths, repo),
              shiny::div(
                style = "height: 70vh;",
                artifact_iframe(
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
                fa_icon("triangle-exclamation", class = "fa-fw fs-3 text-warning me-2"),
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
      rows = collection_rv()
      repo_rows = rows$repo == repo
      if (any(repo_rows)) {
        file_path = rows$path[repo_rows][1]

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
                fa_icon("triangle-exclamation", class = "fa-fw fs-3 text-warning me-2"),
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
              fa_icon("triangle-exclamation", class = "fa-fw fs-3 text-danger me-2"),
              paste("Could not open folder:", repo_path)
            )
          )
        )
      }
    }) |> shiny::bindEvent(input$folder_clicked)
    
    # The validation modal's content, for the modal's repository. A node
    # preview replaces the modal (only one modal shows at a time), so the
    # preview's Back button re-opens it.
    mark_validate_server(
      "explore_module",
      ast = shiny::reactive({
        validation_shown()
        collection_ast_for(collection_rv(), validation_repo())
      }),
      validation_results = shiny::reactive({
        validation_shown()
        validation_rv()[[validation_repo()]]
      }),
      template = shiny::reactiveVal(template_obj),
      on_back = show_validation_modal
    )

    
    # Initialize the content module, sharing the table's repo selection so
    # both tabs always point at the same repository. selected_question is a
    # lazy forward reference: reactives only evaluate at flush time, after
    # rubric_result is assigned below.
    content_result = mark_content_server(
      "content_module", template_obj, collection_rv, artifact_paths, artifact_urls, use_qmd,
      selected_question = shiny::reactive(rubric_result$selected_question()),
      external_repo = current_repo_name,
      repos_dir = repos_dir
    )

    # Initialize the rubric module, keyed on the content pane's repo
    # selection. The rubric is read afresh for each session (database_state
    # only records that the database opened), so reloading the page shows
    # changes made outside the app since it started.
    session_database_state = if (is.null(database_state)) NULL else initialize_database_state(root, template_obj)
    rubric_result = mark_rubric_server(
      "rubric_module", template_obj, repo_list, root, session_database_state,
      selected_repo = content_result$selected_repo,
      set_repo_labels = content_result$set_repo_labels,
      external_version = shiny::reactive(external_version()),
      active_tab = shiny::reactive(input$main_navbar),
      app_action = app_action,
      on_commit = note_own_commit
    )

    # A commit by another connection: list the markermd calls recorded since
    # the last look, announce each, and refresh what can be refreshed in
    # place (the table, History, Activity and the badges follow
    # external_version; the pair on screen is re-read). A changed rubric or
    # template cannot be swapped into a running session, so those raise a
    # banner instead.
    shiny::observe({
      rows = with_database(root, function(conn) action_rows(conn, "id > ?", list(last_action_id)))
      if (nrow(rows) > 0) {
        last_action_id <<- max(rows$id)
        # This session's own actions already show in place
        rows = rows[!rows$action_id %in% own_action_ids, , drop = FALSE]
      }
      changed_template = !identical(template_stamp(), initial_template_stamp)
      if (nrow(rows) == 0 && !changed_template) {
        return()
      }
      if (nrow(rows) > 0) {
        external_version(shiny::isolate(external_version()) + 1L)
        lines = vapply(seq_len(nrow(rows)), function(i) action_notice_text(rows[i, , drop = FALSE]), character(1))
        shiny::showNotification(
          htmltools::tagList(
            shiny::div(class = "fw-semibold", if (nrow(rows) == 1) "Changed outside the app" else paste(nrow(rows), "changes made outside the app")),
            lapply(utils::head(lines, 5), shiny::div),
            if (length(lines) > 5) shiny::div(class = "text-muted", paste0("and ", length(lines) - 5, " more")),
            shiny::actionLink("activity_notice_link", "Show in History")
          ),
          id = "external_change_notice", duration = 10, type = "message"
        )
        kept = rubric_result$refresh_pair()
        if (isTRUE(kept)) {
          shiny::showNotification(
            "Your unsaved comment was kept; it will be saved over the outside change.",
            duration = 8, type = "warning"
          )
        }
      }
      parts = unique(unlist(rows$parts))
      notice = shiny::isolate(external_notice())
      if ("rubric" %in% parts || "template" %in% parts || changed_template) {
        who = rows[vapply(rows$parts, function(p) length(p) > 0, logical(1)), , drop = FALSE]
        external_notice(list(
          rubric = isTRUE(notice$rubric) || "rubric" %in% parts,
          template = isTRUE(notice$template) || "template" %in% parts || changed_template,
          by = unique(c(notice$by, paste0(who$fn, " by ", who$actor)))
        ))
      }
    }) |> shiny::bindEvent(db_changed(), ignoreInit = TRUE)

    output$external_change_banner = shiny::renderUI({
      notice = external_notice()
      if (is.null(notice)) {
        return(NULL)
      }
      by = if (length(notice$by) > 0) paste0(" (", paste(notice$by, collapse = ", "), ")") else ""
      shiny::div(
        class = "alert alert-warning d-flex align-items-center gap-2 small mb-0 py-2 px-3 rounded-0",
        fa_icon("triangle-exclamation"),
        if (notice$template) {
          shiny::span(paste0("The grading template was changed outside the app", by, ". Restart mark() to apply it."))
        } else {
          shiny::span(paste0("The rubric was changed outside the app", by, ". Reload the page to show it; saved grading is kept."))
        },
        if (!notice$template) shiny::actionButton("external_reload", "Reload", class = "btn-sm btn-warning ms-auto")
      )
    })

    shiny::observe({
      session$reload()
    }) |> shiny::bindEvent(input$external_reload)

    shiny::observe({
      bslib::nav_select("main_navbar", "history")
    }) |> shiny::bindEvent(input$activity_notice_link)

    # Reverse sync: z/x navigation (or the dropdown) in the Grade tab moves
    # the app-wide selection too, so a later score click on the repository
    # the rubric already shows still counts as a change
    shiny::observe({
      repo = rubric_result$selected_content_repo()
      shiny::req(repo)
      if (!identical(repo, shiny::isolate(current_repo_name()))) {
        select_repo(repo)
      }
    }) |>
      shiny::bindEvent(rubric_result$selected_content_repo())

    # The History tab: snapshots of the grading state. Its listing and the
    # status indicator re-read the database on every grading write and on
    # every tab change; a restore rebuilds the rubric module from the
    # database.
    history_result = mark_history_server(
      "history_module", root,
      question_names = if (is.null(template_obj)) character(0) else template_question_names(template_obj),
      refresh = shiny::reactive(list(rubric_result$grading_data_version(), external_version(), input$main_navbar)),
      reload_rubric = rubric_result$reload_from_db,
      on_action = note_own_action
    )

    output$snapshot_status = shiny::renderUI({
      status = history_result$status()
      warn = status$events_since > snapshot_warn_events()
      if (is.null(status$latest) && !status$changed) {
        return(NULL)
      }
      n = status$events_since
      changes = paste0(n, " change", if (n == 1) "" else "s")
      when = if (is.null(status$latest)) "No snapshot yet" else paste("Last snapshot", format_age(status$latest$created_at))
      # Two lines beside a larger camera: the changes recorded since the
      # latest snapshot, and when it was taken
      text = htmltools::span(
        class = "d-inline-flex align-items-center gap-2",
        fa_icon("camera", class = "fs-5"),
        htmltools::span(
          class = "d-inline-flex flex-column lh-sm text-start",
          htmltools::span(changes),
          htmltools::span(class = "small", when)
        )
      )
      shiny::actionLink(
        "snapshot_status_link", text,
        class = paste("nav-link snapshot-status", if (warn) "snapshot-status-warn text-warning-emphasis" else "text-muted"),
        title = if (warn) {
          "Many selections have been recorded since the latest snapshot; press s to take a named checkpoint"
        } else {
          "Open the History tab"
        }
      )
    })

    shiny::observe({
      bslib::nav_select("main_navbar", "history")
    }) |> shiny::bindEvent(input$snapshot_status_link)

    if (!is.null(terminal_commands)) {
      mark_terminal_server("terminal_module", terminal_commands, root, token = terminal_token)
    }

    # The Grade tab's camera button opens the same Take snapshot modal
    shiny::observe({
      history_result$open_take_snapshot()
    }) |> shiny::bindEvent(input$`rubric_module-take_snapshot`)

  }
  
  app = shiny::shinyApp(ui = ui, server = server)
  if (!is.null(terminal_commands)) {
    app = shinyterm::token_app(app, terminal_token)
  }
  app
}

