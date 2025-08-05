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

#' Download artifact if not cached or forced
#'
#' @param github_repo Character string. GitHub repository in format "owner/repo"
#' @param repo_name Character string. Local repository name
#' @param collection_path Character string. Path to collection directory
#' @param force Logical. Whether to force download even if file exists
#' @return List with success status and file path or error message
#'
download_artifact_if_needed = function(github_repo, repo_name, collection_path, force = FALSE) {
  cached_path = get_cached_artifact_path(collection_path, repo_name)
  
  # Return cached version if it exists and not forcing re-download
  if (file.exists(cached_path) && !force) {
    return(list(success = TRUE, path = cached_path, from_cache = TRUE))
  }
  
  # Download artifact
  tryCatch({
    cache_dir = create_cache_dir(collection_path)
    
    # Save current working directory and change to cache directory
    old_wd = getwd()
    setwd(cache_dir)
    
    # Create a temporary subdirectory for this specific download to avoid conflicts
    temp_download_dir = file.path(cache_dir, paste0("temp_", gsub("/", "_", github_repo), "_", Sys.time() |> as.numeric() |> round()))
    dir.create(temp_download_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Set up cleanup to happen when function exits (regardless of success/failure)
    on.exit({
      # Always restore working directory
      setwd(old_wd)
      # Clean up temp directory
      if (dir.exists(temp_download_dir)) {
        unlink(temp_download_dir, recursive = TRUE)
      }
    })
    
    tryCatch({
      # Use ghclass to download the artifact - specify the temporary directory
      suppressMessages({
        capture.output({
          ghclass::action_artifact_download(github_repo, dir = temp_download_dir)
        }, type = "output")
      })
      
      # List files in the temporary directory
      all_files = list.files(temp_download_dir, full.names = TRUE, recursive = TRUE)
      
    }, error = function(e) {
      # Return error result
      return(list(success = FALSE, message = paste("Download failed:", e$message)))
    })
    
    # Also check for files that might have been downloaded with tilde paths (legacy)
    if (length(all_files) == 0) {
      # Try looking in the unexpanded path as well
      unexpanded_cache = file.path(collection_path, ".markermd") 
      if (dir.exists(unexpanded_cache)) {
        all_files = list.files(unexpanded_cache, full.names = TRUE)
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
      
      
      # Copy/move the file to our standard location if it's not already there
      if (normalizePath(source_file, mustWork = FALSE) != normalizePath(cached_path, mustWork = FALSE)) {
        # Only overwrite if we're forcing a re-download or if the target doesn't exist
        if (force || !file.exists(cached_path)) {
          file.copy(source_file, cached_path, overwrite = force)
          if (file.exists(cached_path)) {
            file.remove(source_file)
          }
        } else {
          # Target exists and we're not forcing - clean up the downloaded file
          file.remove(source_file)
          return(list(success = TRUE, path = cached_path, from_cache = TRUE))
        }
      }
      
      # Verify the final file exists
      if (file.exists(cached_path)) {
        return(list(success = TRUE, path = cached_path, from_cache = FALSE))
      } else {
        return(list(success = FALSE, error = "File was not saved to expected location"))
      }
    } else {
      return(list(success = FALSE, error = "No artifacts downloaded"))
    }
    
  }, error = function(e) {
    return(list(success = FALSE, error = paste("Download failed:", e$message)))
  })
}

#' Get archive metadata from GitHub
#'
#' @param github_repos Character vector. GitHub repositories in format "owner/repo"
#' @return Data frame with repository and artifact metadata
#'
get_archive_metadata = function(github_repos) {
  if (length(github_repos) == 0) {
    return(data.frame())
  }
  
  tryCatch({
    # Suppress all output and messages from ghclass
    all_artifacts = suppressMessages(suppressWarnings({
      capture.output({
        result = tryCatch({
          ghclass::action_artifacts(github_repos)
        }, error = function(e) {
          # If ghclass fails, return empty data frame
          data.frame()
        })
        result
      }, type = "output")
      result
    }))
    
    # Validate the result structure
    if (is.data.frame(all_artifacts) && nrow(all_artifacts) > 0) {
      # Check if required columns exist
      required_cols = c("repo", "name", "created")
      missing_cols = setdiff(required_cols, names(all_artifacts))
      if (length(missing_cols) > 0) {
        warning("Archive metadata missing columns: ", paste(missing_cols, collapse = ", "), 
                ". Available columns: ", paste(names(all_artifacts), collapse = ", "))
        return(data.frame())
      }
    }
    
    return(all_artifacts)
  }, error = function(e) {
    warning("Failed to get archive metadata: ", e$message)
    return(data.frame())
  })
}

#' Check if local archive is up to date
#'
#' @param cached_path Character string. Path to cached archive file
#' @param github_repo Character string. GitHub repository in format "owner/repo"
#' @param metadata Data frame. Archive metadata from get_archive_metadata()
#' @return Logical. TRUE if local file is up to date, FALSE otherwise
#'
check_archive_freshness = function(cached_path, github_repo, metadata) {
  # File doesn't exist - needs download
  if (!file.exists(cached_path)) {
    return(FALSE)
  }
  
  # No metadata available - assume file is fresh
  if (is.null(metadata) || nrow(metadata) == 0) {
    return(TRUE)
  }
  
  # Find metadata for this repo
  repo_metadata = metadata[metadata$repo == github_repo, ]
  if (nrow(repo_metadata) == 0) {
    return(TRUE)  # No artifacts available, keep existing file
  }
  
  # Get local file birth time using fs::dir_info()
  file_info = fs::dir_info(dirname(cached_path))
  file_info = file_info[file_info$path == cached_path, ]
  
  if (nrow(file_info) == 0) {
    return(FALSE)  # File doesn't exist
  }
  
  local_birth_time = file_info$birth_time[1]
  
  # Compare with remote artifact creation time
  if ("created" %in% names(repo_metadata)) {
    remote_time = as.POSIXct(repo_metadata$created[1])
    # Local file is fresh if it was created after or at the same time as remote artifact
    return(!is.na(local_birth_time) && local_birth_time >= remote_time)
  }
  
  # Fallback to modification time if birth_time is not available
  local_mtime = file.mtime(cached_path)
  if ("created" %in% names(repo_metadata)) {
    remote_time = as.POSIXct(repo_metadata$created[1])
    return(local_mtime >= remote_time)
  }
  
  # Default to keeping existing file if no timestamp info
  return(TRUE)
}

#' Download all archives with progress tracking
#'
#' @param github_repos Character vector. GitHub repositories to download
#' @param repo_to_github Named list. Mapping from local repo names to GitHub repos
#' @param collection_path Character string. Path to collection directory
#' @param progress_callback Function. Optional callback for progress updates
#' @return List with success status and results
#'
download_all_archives = function(github_repos, repo_to_github, collection_path, progress_callback = NULL) {
  if (length(github_repos) == 0) {
    return(list(success = TRUE, results = list()))
  }
  
  # Get metadata for all repos
  if (!is.null(progress_callback)) {
    progress_callback("Getting archive metadata...")
  }
  
  metadata = get_archive_metadata(github_repos)
  
  # If metadata retrieval failed, we can still proceed with downloads
  # but without timestamp-based cache validation
  use_metadata = is.data.frame(metadata) && nrow(metadata) > 0
  
  # First pass: determine which archives need downloading
  repos_needing_download = c()
  for (github_repo in github_repos) {
    # Find local repo name
    local_repo = names(repo_to_github)[repo_to_github == github_repo][1]
    if (is.na(local_repo)) {
      next
    }
    
    # Check if archive exists in metadata (if we have metadata)
    if (use_metadata) {
      repo_metadata = metadata[metadata$repo == github_repo, ]
      if (nrow(repo_metadata) == 0) {
        # No archive available for this repo - skip it entirely
        next
      }
    }
    
    cached_path = get_cached_artifact_path(collection_path, local_repo)
    
    # Check if download is needed
    needs_download = if (use_metadata) {
      !check_archive_freshness(cached_path, github_repo, metadata)
    } else {
      # Without metadata, only download if file doesn't exist
      !file.exists(cached_path)
    }
    
    if (needs_download) {
      repos_needing_download = c(repos_needing_download, github_repo)
    }
  }
  
  results = list()
  downloaded_count = 0  # Count of successful downloads
  processed_count = 0   # Count of archives processed (for progress tracking)
  total_count = length(repos_needing_download)  # Only count repos that need downloading
  
  # If no archives need downloading, return immediately
  if (total_count == 0) {
    if (!is.null(progress_callback)) {
      progress_callback("All archives are up to date", 0, 0)  # Set total to 0
    }
    return(list(success = TRUE, results = list(), downloaded_count = 0, total_needing_download = 0))
  }
  
  # Notify progress callback of the actual total that needs downloading
  if (!is.null(progress_callback)) {
    # Use a special call to set the total - we'll modify progress_callback to handle this
    progress_callback(paste("Found", total_count, "archives to download..."), 0, total_count)
  }
  
  for (github_repo in repos_needing_download) {
    # Find local repo name
    local_repo = names(repo_to_github)[repo_to_github == github_repo][1]
    if (is.na(local_repo)) {
      next
    }
    
    if (!is.null(progress_callback)) {
      progress_callback(paste("Downloading", local_repo, "..."), processed_count)
    }
    
    # Force download since this repo is in the needs_download list
    cached_path = get_cached_artifact_path(collection_path, local_repo)
    force_download = !file.exists(cached_path) || TRUE  # Always force since it's in the needs_download list
    result = download_artifact_if_needed(github_repo, local_repo, collection_path, force = force_download)
    results[[local_repo]] = result
    
    # Always increment processed count
    processed_count = processed_count + 1
    
    if (result$success) {
      downloaded_count = downloaded_count + 1
      # Update progress after successful download
      if (!is.null(progress_callback)) {
        progress_callback(paste("Downloaded", local_repo), processed_count)
      }
    } else {
      # Don't increment downloaded_count for failed downloads
      if (!is.null(progress_callback)) {
        progress_callback(paste("Failed to download", local_repo), processed_count)
      }
    }
  }
  
  # Add results for repositories that are up to date (not in repos_needing_download)
  for (github_repo in github_repos) {
    if (!github_repo %in% repos_needing_download) {
      local_repo = names(repo_to_github)[repo_to_github == github_repo][1]
      if (!is.na(local_repo)) {
        results[[local_repo]] = list(success = TRUE, from_cache = TRUE, message = "Up to date")
      }
    }
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(paste("Download complete:", downloaded_count, "of", total_count, "archives updated"), downloaded_count)
  }
  
  return(list(success = TRUE, results = results, downloaded_count = downloaded_count, total_needing_download = total_count))
}

#' Sync archives (update only out-of-date files)
#'
#' @param github_repos Character vector. GitHub repositories to sync
#' @param repo_to_github Named list. Mapping from local repo names to GitHub repos
#' @param collection_path Character string. Path to collection directory
#' @param progress_callback Function. Optional callback for progress updates
#' @return List with success status and results
#'
sync_archives = function(github_repos, repo_to_github, collection_path, progress_callback = NULL) {
  if (!is.null(progress_callback)) {
    progress_callback("Checking for archive updates...")
  }
  
  result = download_all_archives(github_repos, repo_to_github, collection_path, progress_callback)
  return(result)
}

#' Open folder in system file manager (cross-platform)
#'
#' @param folder_path Character string. Path to folder to open
#'
open_folder = function(folder_path) {
  if (!dir.exists(folder_path)) {
    return(FALSE)
  }
  
  tryCatch({
    if (Sys.info()[["sysname"]] == "Darwin") {
      # macOS
      system(paste("open", shQuote(folder_path)))
    } else if (Sys.info()[["sysname"]] == "Windows") {
      # Windows
      system(paste("explorer", shQuote(folder_path)))
    } else {
      # Linux and other Unix-like systems
      system(paste("xdg-open", shQuote(folder_path)))
    }
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}


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
    download_archives = download_archives
  )
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
#' @param artifact_status List of artifact availability status for each repository
#' @param repo_to_github Named list mapping repository names to GitHub repos
#' @param template_path Character string. Path to template file (optional)
#' @param download_archives Logical. Whether archives were downloaded at launch
#'
#' @return Shiny app object
#'
create_markermd_app = function(collection_path, template_obj, use_qmd, collection, repo_list, validation_results, initial_repo_ast, initial_repo_name, artifact_status, repo_to_github, template_path = NULL, download_archives = TRUE) {
  
  # Define UI  
  ui = bslib::page_navbar(
    title = "markermd - Assignment Grading",
    theme = bslib::bs_theme(version = 5),
    selected = "validation",  # Set validation tab as default
    
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
      # Main content area 
      shiny::div(
        id = "main-app-content",
        bslib::layout_columns(
          col_widths = c(5, 7),
          # Repository table card (left)
          bslib::card(
            bslib::card_header("Assignments", class = "bg-light"),
            bslib::card_body(
              gt::gt_output("repo_table"),
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
              explore_ui("explore_module")
            )
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
    
    # Reactive values for user selections
    current_repo_ast = shiny::reactiveVal(initial_repo_ast)
    current_repo_name = shiny::reactiveVal(initial_repo_name)
    
    # Track selected repository for highlighting
    selected_repo_index = shiny::reactiveVal(1)
    
    # Make artifact status reactive so table updates when it changes
    artifact_status_reactive = shiny::reactiveVal(artifact_status)
    
    # Reactive trigger for auto-sync on app launch
    auto_sync_trigger = shiny::reactiveVal(0)
    
    # Automatically trigger sync archives once on app launch if downloads are enabled and repos exist
    if (download_archives && length(repo_to_github) > 0) {
      # Use session$onFlushed to ensure app is fully loaded, then trigger sync
      session$onFlushed(function() {
        # Simple trigger without invalidateLater
        auto_sync_trigger(1)
      }, once = TRUE)
    }
    
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
          # GitHub repo but no artifacts - show blank space
          return("")
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
        
        # Count validation results (treat errors as failures)
        pass_count = sum(sapply(repo_validation, function(v) v$status == "pass"))
        fail_count = sum(sapply(repo_validation, function(v) v$status %in% c("fail", "error")))
        total_count = length(repo_validation)
        
        # Create detailed tooltip with failing questions in template order
        validation_names = names(repo_validation)
        failed_questions = character(0)
        
        # Get question names from template in order
        template_question_names = sapply(template_obj@questions, function(q) q@name)
        
        # Collect failed questions (including errors) in template order
        for (question_name in template_question_names) {
          if (question_name %in% validation_names) {
            validation = repo_validation[[question_name]]
            if (validation$status %in% c("fail", "error")) {
              failed_questions = c(failed_questions, question_name)
            }
          }
        }
        
        # Build tooltip text
        tooltip_parts = c()
        if (fail_count > 0) {
          if (length(failed_questions) > 0) {
            tooltip_parts = c(tooltip_parts, "Failed validation:")
            tooltip_parts = c(tooltip_parts, paste("•", failed_questions))
          }
        } else {
          tooltip_parts = paste("All", total_count, "validation rules passed")
        }
        
        tooltip_text = paste(tooltip_parts, collapse = "&#10;")
        
        if (fail_count == 0) {
          # All passed
          paste0('<span class="text-success" title="', tooltip_text, '"><i class="fas fa-check-circle"></i> ', pass_count, '/', total_count, '</span>')
        } else {
          # Has failures (including errors)
          paste0('<span class="text-danger" title="', tooltip_text, '"><i class="fas fa-times-circle"></i> ', pass_count, '/', total_count, '</span>')
        }
      })
      
      # Add grading progress column with sparkline bars
      repo_df$Grading = sapply(repo_list, function(repo) {
        if (is.null(template_obj)) {
          # No template - show empty or placeholder
          return("")
        }
        
        # Get number of questions from template
        total_questions = length(template_obj@questions)
        if (total_questions == 0) {
          return("")
        }
        
        # Get question names from template
        question_names = sapply(template_obj@questions, function(q) q@name)
        
        # Generate random graded questions for testing (in real implementation, this would come from actual grading data)
        graded_questions = sample(0:total_questions, 1)
        percentage = round((graded_questions / total_questions) * 100)
        
        # Determine ungraded questions for tooltip in template order
        if (graded_questions < total_questions) {
          # Randomly select which questions are ungraded for testing
          ungraded_indices = sample(seq_len(total_questions), total_questions - graded_questions)
          # Keep the ungraded questions in template order (not random order)
          ungraded_questions = question_names[sort(ungraded_indices)]
        } else {
          ungraded_questions = character(0)
        }
        
        # Create tooltip text
        tooltip_parts = c()
        if (graded_questions == total_questions) {
          tooltip_parts = paste("All", total_questions, "questions graded")
        } else {
          if (length(ungraded_questions) > 0) {
            tooltip_parts = c(tooltip_parts, "Ungraded questions:")
            tooltip_parts = c(tooltip_parts, paste("•", ungraded_questions))
          }
        }
        
        tooltip_text = paste(tooltip_parts, collapse = "&#10;")
        
        # Create sparkline bar with percentage
        bar_width = percentage  # Width as percentage
        bar_color = if (percentage == 100) {
          "#28a745"  # Green for complete
        } else if (percentage >= 50) {
          "#ffc107"  # Yellow for partial
        } else {
          "#dc3545"  # Red for minimal progress
        }
        
        # HTML for sparkline bar with text inside and tooltip
        paste0(
          '<div title="', tooltip_text, '" style="width: 100%; height: 16px; background-color: #e9ecef; border-radius: 8px; position: relative; overflow: hidden;">',
          '<div style="height: 100%; background-color: ', bar_color, '; width: ', bar_width, '%; border-radius: 8px; transition: width 0.3s ease;"></div>',
          '<span style="position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); font-size: 10px; font-weight: 600; color: #333; white-space: nowrap; pointer-events: none;">', graded_questions, '/', total_questions, '</span>',
          '</div>'
        )
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
      
      # Create gt table with all columns
      table_data = repo_df[, c("Repository", "Folder", "GitHub", "Artifacts", "Validation", "Grading"), drop = FALSE]
      
      gt_table = gt::gt(table_data) |>
        gt::fmt_markdown(columns = Repository) |>
        gt::fmt_markdown(columns = Folder) |>
        gt::fmt_markdown(columns = GitHub) |>
        gt::fmt_markdown(columns = Artifacts) |>
        gt::fmt_markdown(columns = Grading) |>
        gt::fmt_markdown(columns = Validation) |>
        gt::cols_label(
          Repository = "Repository", 
          Folder = "", GitHub = "", Artifacts = "",
          Validation = "Validation", Grading = "Progress"
        ) |>
        gt::cols_width(
          Repository ~ pct(39),
          Folder ~ pct(6),
          GitHub ~ pct(6),
          Artifacts ~ pct(6),
          Validation ~ pct(17),
          Grading ~ pct(26)
        ) |>
        gt::cols_align(align = "center", columns = Validation)
      
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
                  customModalDialog(
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
                  customModalDialog(
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
          })
        })
      }
    })
    
    # Handle folder button clicks
    shiny::observe({
      for (i in seq_along(repo_list)) {
        local({
          row_index = i
          folder_button_id = paste0("folder_", row_index)
          
          shiny::observeEvent(input[[folder_button_id]], {
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
                customModalDialog(
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
    
    # Render sync button (only show if there are GitHub repos)
    output$sync_button_ui = shiny::renderUI({
      if (length(repo_to_github) > 0) {
        shiny::actionButton(
          "sync_archives",
          "Sync Archives",
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
    shiny::observeEvent(input$sync_archives, {
      perform_sync()
    })
    
    # Handle auto-sync trigger
    shiny::observeEvent(auto_sync_trigger(), {
      if (auto_sync_trigger() > 0) {
        perform_sync()
      }
    })
    
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

