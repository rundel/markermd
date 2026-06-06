# Artifact caching and synchronisation helpers for the marking app
#
# Download, cache, freshness-check, and sync the rendered HTML artifacts that
# the rubric viewer displays for GitHub-backed repositories.

# Create cache directory for artifacts and return its path
#
# collection_path: Path to collection directory

create_cache_dir = function(collection_path) {
  # Expand tilde in path
  expanded_path = path.expand(collection_path)
  cache_dir = file.path(expanded_path, ".markermd")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  return(cache_dir)
}

# Get the path to a repository's cached artifact file
#
# collection_path: Path to collection directory
# repo_name: Repository name

get_cached_artifact_path = function(collection_path, repo_name) {
  cache_dir = create_cache_dir(collection_path)
  normalizePath(file.path(cache_dir, paste0(repo_name, ".html")), mustWork = FALSE)
}

# Download a repository's artifact, returning a cached copy when available
#
# github_repo: GitHub repository in format "owner/repo"
# repo_name: Local repository name
# collection_path: Path to collection directory
# force: Whether to force download even if file exists

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
    
    # Download the artifact; treat a download failure as "no files found"
    all_files = tryCatch({
      suppressMessages({
        utils::capture.output({
          ghclass::action_artifact_download(github_repo, dir = temp_download_dir)
        }, type = "output")
      })
      list.files(temp_download_dir, full.names = TRUE, recursive = TRUE)
    }, error = function(e) {
      character(0)
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

# Get archive metadata for a set of GitHub repositories
#
# github_repos: GitHub repositories in format "owner/repo"

get_archive_metadata = function(github_repos) {
  if (length(github_repos) == 0) {
    return(data.frame())
  }
  
  tryCatch({
    # Suppress all output and messages from ghclass
    all_artifacts = suppressMessages(suppressWarnings({
      utils::capture.output({
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

# Check whether a local archive is up to date relative to remote metadata
#
# cached_path: Path to cached archive file
# github_repo: GitHub repository in format "owner/repo"
# metadata: Archive metadata from get_archive_metadata()

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

# Download all out-of-date archives with progress tracking
#
# github_repos: GitHub repositories to download
# repo_to_github: Named mapping from local repo names to GitHub repos
# collection_path: Path to collection directory
# progress_callback: Optional callback for progress updates

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
    
    # Always force re-download: this repo is in the needs_download list because
    # its cached file is missing or stale, so an existing file must be overwritten.
    result = download_artifact_if_needed(github_repo, local_repo, collection_path, force = TRUE)
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

# Sync archives, updating only out-of-date files
#
# github_repos: GitHub repositories to sync
# repo_to_github: Named mapping from local repo names to GitHub repos
# collection_path: Path to collection directory
# progress_callback: Optional callback for progress updates

sync_archives = function(github_repos, repo_to_github, collection_path, progress_callback = NULL) {
  if (!is.null(progress_callback)) {
    progress_callback("Checking for archive updates...")
  }
  
  result = download_all_archives(github_repos, repo_to_github, collection_path, progress_callback)
  return(result)
}

# Open a folder in the system file manager (cross-platform)
#
# folder_path: Path to folder to open

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

