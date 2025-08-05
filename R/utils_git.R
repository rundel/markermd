#' Git Operations Utilities
#'
#' Functions for handling git repositories in markermd

#' Setup Assignment Repository
#'
#' Handles both local directories and remote GitHub repositories
#'
#' @param assignment_path Character. Local path or "owner/repo" format
#' @param local_dir Character. Local directory for cloning (required for remote)
#' @param is_github_repo Logical. Whether this is a GitHub repo
#'
#' @return Character path to the local assignment directory
#'
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
    # Clone the repository
    tryCatch({
      gert::git_clone(url = github_url, path = repo_local_path)
      message("Cloned repository to: ", repo_local_path)
    }, error = function(e) {
      stop("Failed to clone repository: ", e$message)
    })
  }
  
  return(normalizePath(repo_local_path))
}

#' Validate Assignment File
#'
#' Check if the specified assignment file exists and is valid
#'
#' @param repo_path Character. Path to the assignment repository
#' @param filename Character. Name of the assignment file
#'
#' @return Character path to the assignment file
#'
validate_assignment_file = function(repo_path, filename) {
  
  assignment_file = file.path(repo_path, filename)
  
  if (!file.exists(assignment_file)) {
    stop("Assignment file not found: ", assignment_file)
  }
  
  # Check file extension
  file_ext = tools::file_ext(filename)
  if (!file_ext %in% c("Rmd", "qmd", "rmd")) {
    stop("Assignment file must be .Rmd or .qmd, got: .", file_ext)
  }
  
  return(normalizePath(assignment_file))
}