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
# first "<dir>/<repo><delimiter>*.html" (a decorated name such as
# "<repo>-report.html"). The trailing-prefix match requires a non-alphanumeric
# boundary after the repo name so repo "hw1" does not capture "hw10.html".

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
      fname = fs::path_file(htmls)
      remainder = substring(fname, nchar(repo) + 1L)
      hit = htmls[startsWith(fname, repo) & grepl("^([^[:alnum:]]|$)", remainder)]
      if (length(hit) > 0) {
        return(as.character(fs::path_real(hit[[1]])))
      }
    }
    NA_character_
  }

  stats::setNames(vapply(repo_list, find_one, character(1)), repo_list)
}

# Register each repo's artifact directory as a Shiny static resource path and
# return the URL each report is served under.
#
# Serving the reports (instead of injecting their text into the page) lets a
# non-self-contained report's sibling resources ("<name>_files/" figures, libs
# CSS/JS) resolve, and displaying them in an iframe keeps the report's own
# CSS/JS out of the grading app document. Same-origin on purpose: the
# question scroll/highlight JS reaches into the report document.
#
# artifact_paths: Named character vector from resolve_repo_artifacts()
#
# Returns a named character vector mapping each repo to its served URL, or NA
# when the repo has no artifact.

register_artifact_resources = function(artifact_paths) {
  urls = stats::setNames(rep(NA_character_, length(artifact_paths)), names(artifact_paths))
  for (i in seq_along(artifact_paths)) {
    path = artifact_paths[[i]]
    if (is.na(path)) next
    # Derive the resource prefix from the serving directory rather than the loop
    # position so a second mark() launch in the same R session re-registers each
    # prefix to the same directory instead of silently rebinding it.
    prefix = paste0("markermd_artifact_", substr(rlang::hash(dirname(path)), 1, 16))
    shiny::addResourcePath(prefix, dirname(path))
    urls[[i]] = paste0(prefix, "/", utils::URLencode(basename(path), reserved = TRUE))
  }
  urls
}

# Open a folder in the system file manager (cross-platform)
#
# folder_path: Path to folder to open

open_folder = function(folder_path) {
  if (!dir.exists(folder_path)) {
    return(FALSE)
  }

  cmd = switch(
    Sys.info()[["sysname"]],
    "Darwin" = "open",
    "Windows" = "explorer",
    "xdg-open"
  )
  status = suppressWarnings(
    system2(cmd, shQuote(folder_path), stdout = FALSE, stderr = FALSE)
  )
  # explorer.exe exits non-zero even when it successfully opens the folder, so
  # its status is not authoritative; every other launcher reports 0 on success.
  cmd == "explorer" || identical(as.integer(status), 0L)
}
