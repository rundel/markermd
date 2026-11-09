# Set up an assignment repository, handling both local directories and remote
# GitHub repositories. The owner and repository names are checked against
# GitHub's naming rules before anything touches the file system, so "owner/.."
# can never resolve to local_dir's parent. An existing checkout is reused only
# when its origin is the requested repository (anything else aborts rather
# than being pulled); a pull failure on it is a warning and the checkout is
# used as is (best effort). A fresh clone failure aborts with a hint.
#
# assignment_path: Character. Local path or "owner/repo" format
# local_dir: Character. Local directory for cloning (required for remote)
# is_github_repo: Logical. Whether this is a GitHub repo
# remote_url: The URL to clone, or NULL for the repository on GitHub (tests
#   pass a local repository)

setup_assignment_repo = function(assignment_path, local_dir = NULL, is_github_repo = FALSE, remote_url = NULL) {

  if (!is_github_repo) {
    return(normalizePath(assignment_path))
  }

  if (is.null(local_dir)) {
    cli::cli_abort("{.arg local_dir} is required for GitHub repositories.")
  }

  parts = parse_github_repo_name(assignment_path)
  url = if (!is.null(remote_url)) remote_url else paste0("https://github.com/", parts$owner, "/", parts$repo, ".git")

  if (!dir.exists(local_dir)) {
    dir.create(local_dir, recursive = TRUE)
  }
  repo_local_path = file.path(local_dir, parts$repo)

  if (dir.exists(repo_local_path)) {
    origin = existing_origin_url(repo_local_path)
    if (is.na(origin) || !same_remote_url(origin, url)) {
      cli::cli_abort(c(
        "{.path {repo_local_path}} already exists but is not a checkout of {.val {assignment_path}}.",
        "i" = "Its origin is {.val {if (is.na(origin)) 'none' else origin}}.",
        "i" = "Move or remove it, or choose another {.arg local_dir}."
      ))
    }
    pulled = purrr::safely(gert::git_pull)(repo = repo_local_path, verbose = FALSE)
    if (is.null(pulled$error)) {
      cli::cli_inform("Updated existing repository {.path {repo_local_path}}.")
    } else {
      cli::cli_warn(c(
        "Could not pull the latest changes into {.path {repo_local_path}}; using it as is.",
        "x" = conditionMessage(pulled$error)
      ))
    }
  } else {
    cloned = purrr::safely(gert::git_clone)(url = url, path = repo_local_path, verbose = FALSE)
    if (!is.null(cloned$error)) {
      cli::cli_abort(c(
        "Could not clone {.val {assignment_path}} from {.url {url}}.",
        "i" = "Check the {.val owner/repo} spelling, that the repository is visible to you, and your network and git credentials."
      ), parent = cloned$error)
    }
    cli::cli_inform("Cloned {.val {assignment_path}} to {.path {repo_local_path}}.")
  }

  normalizePath(repo_local_path)
}

# Split and check an "owner/repo" GitHub name: an owner of letters, digits
# and single inner hyphens (at most 39 characters), and a repository of
# letters, digits, ".", "_" and "-" that is not "." or ".." (a trailing ".git"
# is dropped). Aborts on anything else.
#
# name: Character scalar

parse_github_repo_name = function(name) {
  parts = strsplit(name, "/", fixed = TRUE)[[1]]
  owner = if (length(parts) == 2) parts[1] else ""
  repo = if (length(parts) == 2) sub("\\.git$", "", parts[2]) else ""
  owner_ok = nchar(owner) <= 39 && grepl("^[A-Za-z0-9](?:[A-Za-z0-9]|-(?=[A-Za-z0-9]))*$", owner, perl = TRUE)
  repo_ok = grepl("^[A-Za-z0-9._-]+$", repo) && !repo %in% c(".", "..")
  if (!owner_ok || !repo_ok) {
    cli::cli_abort(c(
      "{.val {name}} is not a GitHub repository name.",
      "i" = "Use the form {.val owner/repo}: an owner of letters, digits and hyphens and a repository of letters, digits, {.val .}, {.val _} and {.val -}."
    ))
  }
  list(owner = owner, repo = repo)
}

# The origin remote's URL of an existing directory, or NA when it is not a git
# repository or has no origin
#
# path: Directory

existing_origin_url = function(path) {
  if (!file.exists(file.path(path, ".git"))) {
    return(NA_character_)
  }
  remotes = gert::git_remote_list(repo = path)
  url = remotes$url[remotes$name == "origin"]
  if (length(url) == 0) NA_character_ else url[1]
}

# Whether two remote URLs name the same repository, ignoring a trailing ".git"
# or "/" and letter case (GitHub names are case-insensitive)
#
# a, b: Character scalars

same_remote_url = function(a, b) {
  norm = function(u) tolower(sub("(\\.git)?/*$", "", u))
  identical(norm(a), norm(b))
}

# Find the single assignment file in a directory matching a glob pattern
#
# dir: Character. Directory to search
# pattern: Character. Glob pattern (e.g. "*.[Rq]md")

resolve_assignment_file = function(dir, pattern) {
  matched = Sys.glob(file.path(dir, pattern))

  if (length(matched) == 0) {
    cli::cli_abort("No files matching '{pattern}' found in directory: {dir}\nPass the assignment file directly, or a `filename` pattern that matches one file.")
  }

  if (length(matched) > 1) {
    matched_files = paste(fs::path_file(matched), collapse = "\n  ")
    cli::cli_abort("Multiple files match '{pattern}' in {dir}:\n  {matched_files}\nPass the assignment file directly, or a `filename` pattern that matches exactly one file.")
  }

  normalizePath(matched[1])
}