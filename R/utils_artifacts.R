# Local artifact resolution helpers for the marking app
#
# The marking app displays a rendered HTML report ("artifact") for each repo.
# Reports are read from the project's configured `artifacts:` directories
# rather than downloaded; see resolve_repo_artifacts().

# Resolve each repo to a local rendered HTML report under the project's
# configured artifacts directories.
#
# Discovery is permissive: every HTML file that belongs to the repo is a
# candidate, where "belongs" means it sits in an artifacts directory as
# "<repo>.html" or "<repo><delimiter>*.html", or anywhere (recursively) under a
# subdirectory named "<repo>" or "<repo><delimiter>*". The delimiter must be
# non-alphanumeric, so repo "hw1" never claims "hw10.html" or "hw10_report/".
# A single candidate is accepted wherever it sits. Several candidates are
# ranked by artifact_candidates()'s heuristic, the best one is used, and one
# consolidated warning names each affected repo with its choice.
#
# project: markermd_project object (provides @root and @artifacts)
# repo_list: Character vector of repository names
#
# Returns a named character vector mapping each repo to the absolute path of
# its HTML report, or NA when no candidate is found, carrying a "candidates"
# attribute with every candidate per repo in ranked order (the app's
# ambiguity notice, artifact_ambiguity_note(), reads it).

resolve_repo_artifacts = function(project, repo_list) {
  artifact_dirs = fs::path(project@root, project@artifacts)
  artifact_dirs = artifact_dirs[fs::dir_exists(artifact_dirs)]

  ranked = lapply(repo_list, function(repo) {
    found = lapply(seq_along(artifact_dirs), function(i) {
      df = artifact_candidates(artifact_dirs[[i]], repo)
      if (!is.null(df)) df$dir_index = i
      df
    })
    found = do.call(rbind, found)
    if (is.null(found)) {
      return(character(0))
    }
    found = found[
      order(found$dir_index, found$tier, found$resource, found$decorated, found$depth, found$path),
      , drop = FALSE
    ]
    as.character(fs::path_real(found$path))
  })
  names(ranked) = repo_list

  paths = vapply(ranked, function(x) if (length(x) > 0) x[[1]] else NA_character_, character(1))
  attr(paths, "candidates") = ranked

  warn_ambiguous_artifacts(ranked, project@root)
  paths
}

# Keep the paths whose file name is repo, or repo followed by a
# non-alphanumeric delimiter
#
# paths: Character vector of paths
# repo: Repository name

prefixed_by_repo = function(paths, repo) {
  name = fs::path_file(paths)
  remainder = substring(name, nchar(repo) + 1L)
  paths[startsWith(name, repo) & grepl("^([^[:alnum:]]|$)", remainder)]
}

# Every HTML file in one artifacts directory that belongs to repo (see
# resolve_repo_artifacts()), with the keys its candidates are ranked on. The
# ranking is modelled on how reports actually arrive:
#
#   tier 1  "<dir>/<repo>.html", a report named for the repo
#   tier 2  a file directly inside "<dir>/<repo>/"
#   tier 3  "<dir>/<repo><delimiter>*.html", a decorated file name
#   tier 4  anything deeper. ghclass's action_artifact_download() (called by
#           org_grade_assignment()) extracts each GitHub Actions artifact into
#           "<dir>/<repo>_<artifact-name>/" by default, or
#           "<dir>/<repo>/<artifact-name>/" with nest = TRUE, so the report
#           sits one or more levels below a repo-named directory
#
# Within a tier, files under a rendering's resource directories ("*_files/",
# "libs/", "site_libs/") rank last since they are never the report, then an
# exact "<repo>/" directory beats a decorated sibling, shallower beats deeper,
# and the path breaks ties.
#
# dir: One artifacts directory
# repo: Repository name
#
# Returns a data frame (path, tier, resource, decorated, depth) or NULL when
# nothing belongs to the repo

artifact_candidates = function(dir, repo) {
  entries = prefixed_by_repo(fs::dir_ls(dir, recurse = FALSE), repo)
  files = entries[fs::is_file(entries) & fs::path_ext(entries) == "html"]
  dirs = entries[fs::is_dir(entries)]
  nested = if (length(dirs) > 0) {
    fs::dir_ls(dirs, recurse = TRUE, type = "file", glob = "*.html")
  } else {
    character(0)
  }
  paths = as.character(c(files, nested))
  if (length(paths) == 0) {
    return(NULL)
  }

  parts = fs::path_split(fs::path_rel(paths, dir))
  depth = lengths(parts)
  in_exact_dir = vapply(parts, function(p) length(p) > 1 && p[[1]] == repo, logical(1))
  resource = vapply(parts, function(p) {
    inner = p[-length(p)]
    any(grepl("_files$", inner) | inner %in% c("libs", "site_libs"))
  }, logical(1))

  tier = ifelse(
    depth == 1 & fs::path_file(paths) == paste0(repo, ".html"), 1L,
    ifelse(depth == 2 & in_exact_dir, 2L, ifelse(depth == 1, 3L, 4L))
  )

  data.frame(
    path = paths,
    tier = tier,
    resource = resource,
    decorated = depth > 1 & !in_exact_dir,
    depth = depth,
    stringsAsFactors = FALSE
  )
}

# One consolidated warning naming each repo that had more than one candidate
# report, the one chosen and how many others were passed over
#
# ranked: Named list of ranked candidate paths per repo
# root: Project root, so the message can show root-relative paths

warn_ambiguous_artifacts = function(ranked, root) {
  ambiguous = ranked[lengths(ranked) > 1]
  if (length(ambiguous) == 0) {
    return(invisible(NULL))
  }

  root = fs::path_real(root)
  bullets = vapply(names(ambiguous), function(repo) {
    chosen = fs::path_rel(ambiguous[[repo]][[1]], root)
    n_other = length(ambiguous[[repo]]) - 1L
    cli::format_inline("{.val {repo}}: using {.file {chosen}} ({n_other} other HTML file{?s} found)")
  }, character(1))
  # format_inline() output is final text; double any braces so cli_warn() does
  # not re-interpolate a path
  bullets = gsub("([{}])", "\\1\\1", bullets)

  cli::cli_warn(c(
    "Several HTML files were found for {length(ambiguous)} repositor{?y/ies}; the most likely report was chosen for each.",
    stats::setNames(bullets, rep("*", length(bullets))),
    "i" = "If the wrong report is shown, remove or rename the extra files under the project's artifacts directories."
  ))
}

# A small warning banner for the app when a repo had more than one candidate
# report (see resolve_repo_artifacts()), or NULL when there is nothing to say
#
# artifact_paths: Named character vector from resolve_repo_artifacts()
# repo: Repository name

artifact_ambiguity_note = function(artifact_paths, repo) {
  candidates = attr(artifact_paths, "candidates")[[repo]]
  if (length(candidates) <= 1) {
    return(NULL)
  }
  # Show the candidates relative to what they share, so same-directory
  # alternatives read as bare file names and cross-directory ones keep their
  # distinguishing prefix
  shown = as.character(fs::path_rel(candidates, fs::path_common(candidates)))

  shiny::div(
    class = "alert alert-warning py-1 px-2 mb-1 small",
    role = "alert",
    fa_icon("triangle-exclamation", class = "me-1"),
    glue::glue(
      "Several HTML files were found for this repository; showing \"{shown[[1]]}\". ",
      "Also found: {paste0('\"', shown[-1], '\"', collapse = ', ')}. ",
      "If this is the wrong report, remove or rename the extra files."
    )
  )
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
