# Local artifact resolution helpers for the marking app
#
# The marking app displays a rendered HTML report ("artifact") for each repo.
# Reports are read from the project's configured `artifacts:` directories
# rather than downloaded; see resolve_repo_artifacts().

# Resolve each repo to a local rendered HTML report under the project's
# configured artifacts directories.
#
# project: markermd_project object (provides @root and @artifacts)
# repo_list: Character vector of repository names
#
# Returns a named character vector mapping each repo to the absolute path of
# its HTML report, or NA when no matching report is found. The artifacts
# directories are searched in config order and the first match wins, preferring
# "<dir>/<repo>.html", then the first HTML file inside "<dir>/<repo>/", then the
# first "<dir>/<repo>*.html".

resolve_repo_artifacts = function(project, repo_list) {
  artifact_dirs = fs::path(project@root, project@artifacts)
  artifact_dirs = artifact_dirs[fs::dir_exists(artifact_dirs)]

  find_one = function(repo) {
    for (dir in artifact_dirs) {
      exact = fs::path(dir, paste0(repo, ".html"))
      if (fs::file_exists(exact)) {
        return(as.character(fs::path_real(exact)))
      }

      sub = fs::path(dir, repo)
      if (fs::dir_exists(sub)) {
        htmls = fs::dir_ls(sub, recurse = FALSE, type = "file", glob = "*.html")
        if (length(htmls) > 0) {
          return(as.character(fs::path_real(htmls[[1]])))
        }
      }

      htmls = fs::dir_ls(dir, recurse = FALSE, type = "file", glob = "*.html")
      hit = htmls[startsWith(fs::path_file(htmls), repo)]
      if (length(hit) > 0) {
        return(as.character(fs::path_real(hit[[1]])))
      }
    }
    NA_character_
  }

  stats::setNames(vapply(repo_list, find_one, character(1)), repo_list)
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
