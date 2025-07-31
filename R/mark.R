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
#' Create cache directory for artifacts
#'
#' @param collection_path Character string. Path to collection directory
#' @return Character string. Path to cache directory
#'
create_cache_dir = function(collection_path) {
  # Expand tilde in path
  expanded_path = path.expand(collection_path)
  cache_dir = file.path(expanded_path, ".markermd")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  return(cache_dir)
}

#' Get cached artifact path
#'
#' @param collection_path Character string. Path to collection directory
#' @param repo_name Character string. Repository name
#' @return Character string. Path to cached artifact file
#'
get_cached_artifact_path = function(collection_path, repo_name) {
  cache_dir = create_cache_dir(collection_path)
  normalizePath(file.path(cache_dir, paste0(repo_name, ".html")), mustWork = FALSE)
}

#' Download artifact if not cached
#'
#' @param github_repo Character string. GitHub repository in format "owner/repo"
#' @param repo_name Character string. Local repository name
#' @param collection_path Character string. Path to collection directory
#' @return List with success status and file path or error message
#'
download_artifact_if_needed = function(github_repo, repo_name, collection_path) {
  cached_path = get_cached_artifact_path(collection_path, repo_name)
  
  # Return cached version if it exists
  if (file.exists(cached_path)) {
    cat("DEBUG: Using cached artifact at", cached_path, "\n")
    return(list(success = TRUE, path = cached_path, from_cache = TRUE))
  }
  
  # Download artifact
  tryCatch({
    cache_dir = create_cache_dir(collection_path)
    cat("DEBUG: Created cache dir:", cache_dir, "\n")
    cat("DEBUG: Downloading artifact for repo:", github_repo, "\n")
    
    # Save current working directory and change to cache directory
    old_wd = getwd()
    setwd(cache_dir)
    
    tryCatch({
      # Use ghclass to download the artifact - specify the cache directory
      ghclass::action_artifact_download(github_repo, dir = cache_dir)
      cat("DEBUG: Download command completed\n")
    }, finally = {
      # Always restore working directory
      setwd(old_wd)
    })
    
    # List all files in cache dir to see what was downloaded
    all_files = list.files(cache_dir, full.names = TRUE)
    cat("DEBUG: All files in cache dir:", paste(all_files, collapse = ", "), "\n")
    
    # Also check for files that might have been downloaded with tilde paths
    if (length(all_files) == 0) {
      # Try looking in the unexpanded path as well
      unexpanded_cache = file.path(collection_path, ".markermd") 
      if (dir.exists(unexpanded_cache)) {
        all_files = list.files(unexpanded_cache, full.names = TRUE)
        cat("DEBUG: Files in unexpanded cache dir:", paste(all_files, collapse = ", "), "\n")
        if (length(all_files) > 0) {
          # Move files from unexpanded to expanded location
          for (f in all_files) {
            new_path = file.path(cache_dir, basename(f))
            file.copy(f, new_path)
            file.remove(f)
          }
          all_files = list.files(cache_dir, full.names = TRUE)
        }
      }
    }
    
    if (length(all_files) > 0) {
      # Look for HTML files first
      html_files = all_files[grepl("\\.html$", all_files)]
      
      if (length(html_files) > 0) {
        # Use the first HTML file found
        source_file = html_files[1]
      } else {
        # No HTML files, use the first file
        source_file = all_files[1]
      }
      
      cat("DEBUG: Using source file:", source_file, "\n")
      cat("DEBUG: Target path:", cached_path, "\n")
      
      # Copy/move the file to our standard location if it's not already there
      if (normalizePath(source_file, mustWork = FALSE) != normalizePath(cached_path, mustWork = FALSE)) {
        file.copy(source_file, cached_path, overwrite = TRUE)
        if (file.exists(cached_path)) {
          file.remove(source_file)
          cat("DEBUG: Successfully moved file to:", cached_path, "\n")
        }
      }
      
      # Verify the final file exists
      if (file.exists(cached_path)) {
        return(list(success = TRUE, path = cached_path, from_cache = FALSE))
      } else {
        return(list(success = FALSE, error = "File was not saved to expected location"))
      }
    } else {
      cat("DEBUG: No files downloaded\n")
      return(list(success = FALSE, error = "No artifacts downloaded"))
    }
    
  }, error = function(e) {
    cat("DEBUG: Error during download:", e$message, "\n")
    return(list(success = FALSE, error = paste("Download failed:", e$message)))
  })
}

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
  
  template_obj = NULL
  
  if (!is.null(template)) {
    if (is.character(template) && length(template) == 1) {
      template_obj = readRDS(template)
      if (!S7::S7_inherits(template_obj, markermd_template)) {
        stop("Template file must contain a markermd_template S7 object")
      }
    } else if (S7::S7_inherits(template, markermd_template)) {
      template_obj = template
    } else {
      stop("Template must be a file path or markermd_template S7 object")
    }
  }
  
  # Load parsermd package explicitly for internal functions
  if (!requireNamespace("parsermd", quietly = TRUE)) {
    stop("parsermd package is required but not available")
  }
  library(parsermd, quietly = TRUE)
  
  # Parse the collection using parsermd
  if (use_qmd) {
    collection = parsermd::parse_qmd_collection(collection_path)
  } else {
    collection = parsermd::parse_rmd_collection(collection_path)
  }
  
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
  
  # Check for GitHub artifacts once before app starts - batch all requests
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
  
  # Batch request for all GitHub repos at once
  if (length(github_repos) > 0) {
    tryCatch({
      all_artifacts = ghclass::action_artifacts(github_repos)
      
      # Process results
      for (repo in repo_list) {
        if (repo %in% names(repo_to_github)) {
          github_repo = repo_to_github[[repo]]
          repo_artifacts = all_artifacts[all_artifacts$repo == github_repo, ]
          artifact_status[[repo]] = nrow(repo_artifacts) > 0
        } else {
          artifact_status[[repo]] = NA  # Not a GitHub repo
        }
      }
    }, error = function(e) {
      # Set all to FALSE on error
      for (repo in repo_list) {
        artifact_status[[repo]] = if (repo %in% names(repo_to_github)) FALSE else NA
      }
    })
  } else {
    # No GitHub repos found
    for (repo in repo_list) {
      artifact_status[[repo]] = NA
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
    template_path = if(is.character(template)) template else NULL
  )
  shiny::runApp(app, ...)
}

#' Launch the markermd Template Creation Application
#'
#' @param assignment_path Assignment source or existing template. Can be:
#'   - Character path to local directory containing assignment
#'   - Character GitHub repo in format "owner/repo"  
#'   - Character path to .rds file containing markermd_template
#'   - markermd_template S7 object
#' @param local_dir Character string. Local directory for cloning (required for remote GitHub repos, ignored for templates)
#' @param filename Character string. Glob pattern to match Rmd/qmd file to grade (ignored for templates). Default glob matches any .Rmd or .qmd file.
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
#' # Load existing template from file
#' template("/path/to/saved_template.rds")
#' 
#' # Load existing template from S7 object
#' my_template = readRDS("/path/to/template.rds")
#' template(my_template)
#' }
template = function(assignment_path, local_dir = NULL, filename = "*.[Rq]md", ...) {
  
  # Validate inputs
  if (missing(assignment_path)) {
    stop("assignment_path is required")
  }
  
  # Determine what type of input we have
  template_obj = NULL
  is_template_mode = FALSE
  
  if (S7::S7_inherits(assignment_path, markermd_template)) {
    # Input is an S7 template object
    template_obj = assignment_path
    is_template_mode = TRUE
    
  } else if (is.character(assignment_path) && length(assignment_path) == 1) {
    # Input is a character string - could be assignment path or template file
    
    if (grepl("\\.rds$", assignment_path, ignore.case = TRUE) && file.exists(assignment_path)) {
      # Input appears to be a template RDS file
      template_obj = tryCatch({
        readRDS(assignment_path)
      }, error = function(e) {
        stop("Failed to load template file: ", e$message)
      })
      
      # Validate it's a template object
      if (!S7::S7_inherits(template_obj, markermd_template)) {
        stop("RDS file must contain a markermd_template S7 object")
      }
      
      is_template_mode = TRUE
      
    } else {
      # Input is an assignment path (local directory or GitHub repo)
      is_template_mode = FALSE
    }
    
  } else {
    stop("assignment_path must be a character string (assignment path or template file) or markermd_template S7 object")
  }
  
  # Handle template mode vs assignment mode
  if (is_template_mode) {
    # Template mode: use AST from template, ignore filename/local_dir
    ast = template_obj@original_ast
    
    # Create simplified app info for footer
    assignment_path_display = if (S7::S7_inherits(assignment_path, markermd_template)) {
      "Template Object"
    } else {
      basename(assignment_path)
    }
    
    # Create app with template data
    app = create_template_app(
      assignment_path = assignment_path_display,
      local_dir = NULL,
      filename = "N/A",
      is_github_repo = FALSE,
      template_obj = template_obj,
      file_path = NULL,
      ast = ast
    )
    
  } else {
    # Assignment mode: parse document and create template
    
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
    
    # Setup repository and parse document before creating app
    repo_path = setup_assignment_repo(assignment_path, local_dir, is_github_repo)
    
    # Validate and get assignment file path
    file_path = validate_assignment_file(repo_path, resolved_filename)
    
    # Parse the document
    ast = parse_assignment_document(file_path)
    
    # Create app without template
    app = create_template_app(
      assignment_path = assignment_path,
      local_dir = local_dir,
      filename = resolved_filename,
      is_github_repo = is_github_repo,
      template_obj = NULL,
      file_path = file_path,
      ast = ast
    )
  }
  
  # Launch the app
  shiny::runApp(app, ...)
}

#' Create Shiny App for markermd
#'
#' Internal function to create the Shiny application
#'
#' @param collection_path Character string. Path to directory containing assignment repositories
#' @param template_obj markermd_template S7 object with node selections, or NULL
#' @param use_qmd Logical. Whether to parse .qmd files (TRUE) or .Rmd files (FALSE)
#' @param collection Parsed collection data from parsermd
#' @param repo_list Character vector of repository names
#' @param validation_results List of validation results for each repository
#' @param initial_repo_ast Initial repository AST to display
#' @param initial_repo_name Name of initial repository
#'
#' @return Shiny app object
#'
create_markermd_app = function(collection_path, template_obj, use_qmd, collection, repo_list, validation_results, initial_repo_ast, initial_repo_name, artifact_status, repo_to_github, template_path = NULL) {
  
  # Define UI  
  ui = bslib::page_navbar(
    title = "markermd - Assignment Grading",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    
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
        shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism.min.css"),
        shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-core.min.js"),
        shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/plugins/autoloader/prism-autoloader.min.js"),
        # CSS to fix modal content formatting (copied from working template app)
        shiny::tags$style(shiny::HTML("
          /* Modal styling */
          .modal-header { padding: 8px 15px !important; }
          .modal-title { margin: 0 !important; padding: 0 !important; line-height: 1.2 !important; }

          /* Code syntax highlighting */
          .modal-content pre[class*='language-'],
          .modal-content pre#syntax-content {
            margin: 0 !important;
            padding: 15px !important;
            text-indent: 0 !important;
            background: #f5f2f0 !important;
            white-space: pre-wrap !important;
            word-wrap: break-word !important;
            overflow-wrap: break-word !important;
          }

          .modal-content code[class*='language-'],
          .modal-content .token {
            text-indent: 0 !important;
            padding: 0 !important;
            margin: 0 !important;
            white-space: pre-wrap !important;
            word-wrap: break-word !important;
            overflow-wrap: break-word !important;
          }

          .modal-content code[class*='language-'] {
            display: block !important;
            padding-left: 0 !important;
            margin-left: 0 !important;
          }

          .modal-content pre[class*='language-']:before,
          .modal-content pre[class*='language-']:after,
          .modal-content code[class*='language-']:before,
          .modal-content code[class*='language-']:after {
            content: none !important;
          }
        "))
      ),
      # Layout with repository table (1/3) and validation results (2/3)
      bslib::layout_columns(
        col_widths = c(4, 8),
        # Repository table card (1/3)
        bslib::card(
          bslib::card_header("Assignments"),
          bslib::card_body(
            gt::gt_output("repo_table")
          )
        ),
        # Validation results card (2/3)  
        bslib::card(
          bslib::card_header("Validation"),
          bslib::card_body(
            explore_ui("explore_module")
          )
        )
      )
    ),
    
    # Marking tab (right aligned)
    bslib::nav_panel(
      title = "Assignment Grading", 
      value = "marking",
      marking_ui("marking_module")
    ),
    
    # Footer with collection and template info
    footer = shiny::div(
      style = "background-color: #e9ecef; border-top: 1px solid #dee2e6; text-align: center; font-size: 12px; color: #495057; padding: 8px; margin-top: 20px;",
      shiny::span(shiny::strong("Collection path:"), " ", shiny::code(collection_path, style = "background-color: #f8f9fa; padding: 2px 4px; border-radius: 4px; font-size: 11px;"), style = "margin-right: 15px;"),
      if (!is.null(template_path)) {
        shiny::span(shiny::strong("Template:"), " ", shiny::code(template_path, style = "background-color: #f8f9fa; padding: 2px 4px; border-radius: 4px; font-size: 11px;"), style = "margin-right: 15px;")
      },
      shiny::span(shiny::strong("File type:"), " ", shiny::code(if(use_qmd) ".qmd" else ".Rmd", style = "background-color: #f8f9fa; padding: 2px 4px; border-radius: 4px; font-size: 11px;"))
    )
  )
  
  # Define server logic
  server = function(input, output, session) {
    
    # Reactive values for user selections
    current_repo_ast = shiny::reactiveVal(initial_repo_ast)
    current_repo_name = shiny::reactiveVal(initial_repo_name)
    
    # Track selected repository for highlighting
    selected_repo_index = shiny::reactiveVal(1)
    
    # Create repository table with gt
    output$repo_table = gt::render_gt({
      
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
      
      # Add GitHub column
      repo_df$GitHub = sapply(repo_list, function(repo) {
        if (repo %in% names(repo_to_github)) {
          github_repo = repo_to_github[[repo]]
          github_url = paste0("https://github.com/", github_repo)
          return(paste0('<a href="', github_url, '" target="_blank" style="color: inherit; text-decoration: none;"><i class="fab fa-github" style="color: inherit; font-size: 16px;" title="Open on GitHub"></i></a>'))
        } else {
          return("")
        }
      })
      
      # Add artifact status column with clickable icons
      repo_df$Artifacts = sapply(seq_along(repo_list), function(i) {
        repo = repo_list[i]
        artifact_status_val = artifact_status[[repo]]
        
        if (is.na(artifact_status_val)) {
          # Not a GitHub repo - no icon
          return("")
        } else if (artifact_status_val) {
          # Has artifacts - clickable archive icon
          button_id = paste0("artifact_", i)
          return(paste0('<button onclick="Shiny.setInputValue(\'', button_id, '\', Math.random())" style="background: none; border: none; cursor: pointer; padding: 0;" title="View artifact"><i class="fas fa-file-archive" style="color: inherit; font-size: 14px;"></i></button>'))
        } else {
          # GitHub repo but no artifacts - red archive icon (not clickable)
          return('<i class="fas fa-file-archive" style="color: #dc3545; font-size: 14px;" title="No artifacts"></i>')
        }
      })
      
      # Add validation summary column
      repo_df$Validation = sapply(repo_list, function(repo) {
        if (is.null(template_obj)) {
          return("")  # No validation without template
        }
        
        repo_validation = validation_results[[repo]]
        if (is.null(repo_validation) || length(repo_validation) == 0) {
          return("")  # No validation data
        }
        
        # Count validation results
        pass_count = sum(sapply(repo_validation, function(v) v$status == "pass"))
        fail_count = sum(sapply(repo_validation, function(v) v$status == "fail"))
        error_count = sum(sapply(repo_validation, function(v) v$status == "error"))
        total_count = length(repo_validation)
        
        # Create detailed summary with tooltips
        tooltip_text = paste(
          "Validation rules:", total_count,
          "| Passed:", pass_count,
          "| Failed:", fail_count,
          if (error_count > 0) paste("| Errors:", error_count) else ""
        )
        
        if (fail_count == 0 && error_count == 0) {
          # All passed
          paste0('<span style="color: #28a745;" title="', tooltip_text, '"><i class="fas fa-check-circle"></i> ', pass_count, '/', total_count, '</span>')
        } else if (error_count > 0) {
          # Has errors
          paste0('<span style="color: #ffc107;" title="', tooltip_text, '"><i class="fas fa-exclamation-circle"></i> ', pass_count, '/', total_count, '</span>')
        } else {
          # Has failures
          paste0('<span style="color: #dc3545;" title="', tooltip_text, '"><i class="fas fa-times-circle"></i> ', pass_count, '/', total_count, '</span>')
        }
      })
      
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
      
      # Create gt table with validation column if template is available
      table_data = if (is.null(template_obj)) {
        repo_df[, c("Repository", "GitHub", "Artifacts"), drop = FALSE]
      } else {
        repo_df[, c("Repository", "GitHub", "Artifacts", "Validation"), drop = FALSE]
      }
      
      gt_table = gt::gt(table_data) |>
        gt::fmt_markdown(columns = Repository) |>
        gt::fmt_markdown(columns = GitHub) |>
        gt::fmt_markdown(columns = Artifacts) |>
        gt::cols_label(GitHub = "", Artifacts = "")
      
      # Add validation column formatting if it exists
      if ("Validation" %in% names(table_data)) {
        gt_table = gt_table |>
          gt::fmt_markdown(columns = Validation) |>
          gt::cols_label(Repository = "Repo", Validation = "Rules") |>
          gt::cols_width(
            Repository ~ pct(50),
            GitHub ~ pct(10),
            Artifacts ~ pct(10),
            Validation ~ pct(30)
          )
      } else {
        gt_table = gt_table |>
          gt::cols_label(Repository = "Repo") |>
          gt::cols_width(
            Repository ~ pct(70),
            GitHub ~ pct(15),
            Artifacts ~ pct(15)
          )
      }
      
      gt_table |>
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
    })
    
    # Handle repository button clicks
    shiny::observe({
      for (i in seq_along(repo_list)) {
          local({
            row_index = i
            button_id = paste0("repo_select_", row_index)
            
            shiny::observeEvent(input[[button_id]], {
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
            })
          })
        }
    })
    
    # Handle artifact button clicks
    shiny::observe({
      for (i in seq_along(repo_list)) {
        local({
          row_index = i
          artifact_button_id = paste0("artifact_", row_index)
          
          shiny::observeEvent(input[[artifact_button_id]], {
            repo = repo_list[row_index]
            
            # Only process if this repo has artifacts
            if (!is.na(artifact_status[[repo]]) && artifact_status[[repo]] && repo %in% names(repo_to_github)) {
              github_repo = repo_to_github[[repo]]
              
              # Download artifact and show result
              tryCatch({
                download_result = download_artifact_if_needed(github_repo, repo, collection_path)
                
                if (download_result$success) {
                  # Read the HTML content and display it directly
                  html_content = tryCatch({
                    readLines(download_result$path, warn = FALSE) |> paste(collapse = "\n")
                  }, error = function(e) {
                    paste("Error reading file:", e$message)
                  })
                  
                  # Show artifact in modal with HTML content
                  shiny::showModal(
                    customModalDialog(
                      title = repo,
                      size = "xl",
                      easyClose = TRUE,
                      footer = shiny::div(
                        if (download_result$from_cache) {
                          shiny::span("Loaded from cache", style = "color: #6c757d; font-size: 12px;")
                        } else {
                          shiny::span("Downloaded and cached", style = "color: #28a745; font-size: 12px;")
                        }
                      ),
                      shiny::div(
                        style = "height: 70vh; width: 100%; overflow: auto; border: 1px solid #dee2e6; background: white; font-size: 12px; padding: 8px;",
                        shiny::HTML(html_content)
                      )
                    )
                  )
                } else {
                  # Show error modal
                  shiny::showModal(
                    customModalDialog(
                      title = "Download Error",
                      easyClose = TRUE,
                      footer = NULL,
                      shiny::div(
                        style = "padding: 20px; text-align: center;",
                        shiny::tags$i(class = "fas fa-exclamation-triangle", style = "font-size: 24px; color: #dc3545; margin-right: 10px;"),
                        paste("Failed to download artifact:", download_result$error)
                      )
                    )
                  )
                }
              }, error = function(e) {
                # Show error modal for any unexpected errors
                shiny::showModal(
                  customModalDialog(
                    title = "Unexpected Error",
                    easyClose = TRUE,
                    footer = NULL,
                    shiny::div(
                      style = "padding: 20px; text-align: center;",
                      shiny::tags$i(class = "fas fa-exclamation-triangle", style = "font-size: 24px; color: #dc3545; margin-right: 10px;"),
                      paste("An unexpected error occurred:", e$message)
                    )
                  )
                )
              })
            }
          })
        })
      }
    })
    
    # Explore module - pass validation data
    explore_result = explore_server("explore_module", current_repo_ast, current_repo_name, current_repo_validation, shiny::reactiveVal(NULL), shiny::reactiveVal(template_obj))
    
    
    # Marking module with template object for content extraction
    template_reactive = shiny::reactiveVal(template_obj)
    
    # Create reactive for current repository validation results
    current_repo_validation = shiny::reactive({
      repo_name = current_repo_name()
      if (!is.null(repo_name) && !is.null(validation_results) && repo_name %in% names(validation_results)) {
        validation_results[[repo_name]]
      } else {
        NULL
      }
    })
    
    marking_result = marking_server("marking_module", current_repo_ast, template_reactive, current_repo_validation)
    
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
#' @param template_obj markermd_template S7 object. Optional template to load on startup
#' @param file_path Character string. Path to the assignment file
#' @param ast Parsed AST object from the assignment document
#'
#' @return Shiny app object
#'
create_template_app = function(assignment_path, local_dir, filename, is_github_repo, template_obj = NULL, file_path, ast) {
  
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
      # Template app UI will be rendered here
      shiny::uiOutput("template_app_content")
    ),
    
    # Footer with assignment info
    footer = shiny::div(
      style = "background-color: #e9ecef; border-top: 1px solid #dee2e6; text-align: center; font-size: 14px; color: #495057; padding: 12px;",
      shiny::span(shiny::strong("Path"), " ", shiny::code(assignment_path, style = "background-color: #f8f9fa; padding: 2px 4px; border-radius: 6px; font-size: 13px;"), style = "margin-right: 20px;"),
      shiny::span(shiny::strong("File"), " ", shiny::code(filename, style = "background-color: #f8f9fa; padding: 2px 4px; border-radius: 6px; font-size: 13px;")),
      if (is_github_repo) shiny::span(shiny::strong("Local dir"), " ", shiny::code(local_dir, style = "background-color: #f8f9fa; padding: 2px 4px; border-radius: 6px; font-size: 13px;"), style = "margin-left: 20px;")
    )
  )
  
  # Define server logic
  server = function(input, output, session) {
    
    # Template app - get the app components and render UI dynamically
    template_app_components = template_app(shiny::reactiveVal(ast), template_obj)
    
    # Render template app UI into the placeholder div
    output$template_app_content = shiny::renderUI({
      template_app_components$ui
    })
    
    # Run template app server
    template_app_components$server(input, output, session)
  }
  
  # Return the app
  shiny::shinyApp(ui = ui, server = server)
}