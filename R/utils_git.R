# Set up an assignment repository, handling both local directories and remote
# GitHub repositories
#
# assignment_path: Character. Local path or "owner/repo" format
# local_dir: Character. Local directory for cloning (required for remote)
# is_github_repo: Logical. Whether this is a GitHub repo

setup_assignment_repo = function(assignment_path, local_dir = NULL, is_github_repo = FALSE) {
  
  if (!is_github_repo) {
    # Local directory - just return the path
    return(normalizePath(assignment_path))
  }
  
  
  # GitHub repository handling
  if (is.null(local_dir)) {
    stop("local_dir is required for GitHub repositories")
  }
  
  # Create local directory if it doesn't exist
  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
  }
  
  # Parse GitHub repo
  repo_parts = strsplit(assignment_path, "/")[[1]]
  if (length(repo_parts) != 2) {
    stop("GitHub repository must be in format 'owner/repo'")
  }
  
  owner = repo_parts[1]
  repo = repo_parts[2]
  
  # Construct GitHub URL
  github_url = paste0("https://github.com/", owner, "/", repo, ".git")
  repo_local_path = file.path(local_dir, repo)
  
  # Clone or pull repository
  if (dir.exists(repo_local_path)) {
    # Repository exists, try to pull latest changes
    tryCatch({
      gert::git_pull(repo = repo_local_path)
      message("Updated existing repository: ", repo_local_path)
    }, error = function(e) {
      warning("Could not pull latest changes: ", e$message)
    })
  } else {
    gert::git_clone(url = github_url, path = repo_local_path)
    message("Cloned repository to: ", repo_local_path)
  }
  
  return(normalizePath(repo_local_path))
}

# Find the single assignment file in a directory matching a glob pattern
#
# dir: Character. Directory to search
# pattern: Character. Glob pattern (e.g. "*.[Rq]md")

resolve_assignment_file = function(dir, pattern) {
  matched = Sys.glob(file.path(dir, pattern))

  if (length(matched) == 0) {
    stop(
      "No files matching '", pattern, "' found in directory: ", dir, "\n",
      "Pass the assignment file directly, or a `filename` pattern that matches one file.",
      call. = FALSE
    )
  }

  if (length(matched) > 1) {
    stop(
      "Multiple files match '", pattern, "' in ", dir, ":\n  ",
      paste(fs::path_file(matched), collapse = "\n  "), "\n",
      "Pass the assignment file directly, or a `filename` pattern that matches exactly one file.",
      call. = FALSE
    )
  }

  normalizePath(matched[1])
}