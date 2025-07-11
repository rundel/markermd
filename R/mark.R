#' Launch the markermd Shiny Application
#'
#' @param assignment_path Character string. Path to local directory or GitHub repo in format "owner/repo"
#' @param local_dir Character string. Local directory for cloning (required for remote repos)
#' @param filename Character string. Name of the Rmd/qmd file to grade
#' @param ... Additional arguments passed to shiny::runApp()
#'
#' @return Launches Shiny application
#' @export
#'
#' @examples
#' \dontrun{
#' # Local assignment
#' mark("/path/to/assignment", filename = "homework.Rmd")
#' 
#' # Remote GitHub repo
#' mark("username/repo-name", local_dir = "/tmp/grading", filename = "assignment.qmd")
#' }
mark = function(assignment_path, local_dir = NULL, filename, ...) {
  
  # Validate inputs
  if (missing(assignment_path)) {
    stop("assignment_path is required")
  }
  
  if (missing(filename)) {
    stop("filename is required")
  }
  
  # Check if assignment_path is a GitHub repo (contains "/")
  is_github_repo = grepl("/", assignment_path) && !file.exists(assignment_path)
  
  if (is_github_repo && is.null(local_dir)) {
    stop("local_dir is required for GitHub repositories")
  }
  
  # Validate local directory exists for local assignments
  if (!is_github_repo && !dir.exists(assignment_path)) {
    stop("Local assignment directory does not exist: ", assignment_path)
  }
  
  # Store configuration in global environment for app access
  assign(".markermd_config", list(
    assignment_path = assignment_path,
    local_dir = local_dir,
    filename = filename,
    is_github_repo = is_github_repo
  ), envir = .GlobalEnv)
  
  # Create and launch the Shiny app
  app = create_markermd_app()
  shiny::runApp(app, ...)
}