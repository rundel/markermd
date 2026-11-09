# The assignment's other files (template format 3.2): locating a template
# file in a repository, telling text from images and other binaries, and the
# implicit validation of a question on a file (the file must exist).

# Locate one of a template's files in a repository: the repository-relative
# path when it exists, otherwise a file with the same name anywhere in the
# repository (the shallowest one, then alphabetically), skipping .git.
#
# repo_dir: The repository directory
# path: The template file path, relative to the repository root
# Returns: The absolute path of the file found, or NA_character_

find_repo_file = function(repo_dir, path) {
  root = normalizePath(repo_dir, winslash = "/", mustWork = FALSE)
  # A file is only found when it really lives in the repository: a committed
  # symlink to a file elsewhere on the grader's machine would otherwise be
  # shown in the grader's page
  inside_repo = function(p) {
    startsWith(normalizePath(p, winslash = "/", mustWork = FALSE), paste0(root, "/"))
  }

  exact = file.path(repo_dir, path)
  if (file.exists(exact) && !dir.exists(exact) && inside_repo(exact)) {
    return(normalizePath(exact, winslash = "/"))
  }
  candidates = list_repo_files(repo_dir)
  candidates = candidates[basename(candidates) == basename(path)]
  candidates = candidates[vapply(file.path(repo_dir, candidates), inside_repo, logical(1))]
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  depth = lengths(strsplit(candidates, "/", fixed = TRUE))
  best = candidates[order(depth, candidates)][1]
  normalizePath(file.path(repo_dir, best), winslash = "/")
}

# Directory names never entered when looking for a submission's file:
# dependency and render-output trees can hold thousands of files and never
# the submission's own

repo_skip_dirs_regex = "^(\\.git|renv|node_modules|\\.quarto|_freeze|.*_files)$"

# The repository-relative paths of every file in a repository, pruning the
# skipped trees as the walk reaches them rather than listing them first
#
# repo_dir: The repository directory

list_repo_files = function(repo_dir) {
  walk = function(rel) {
    full = if (nzchar(rel)) file.path(repo_dir, rel) else repo_dir
    entries = list.files(full, all.files = TRUE, no.. = TRUE)
    unlist(lapply(entries, function(entry) {
      rel_entry = if (nzchar(rel)) paste0(rel, "/", entry) else entry
      if (dir.exists(file.path(repo_dir, rel_entry))) {
        if (grepl(repo_skip_dirs_regex, entry)) character(0) else walk(rel_entry)
      } else {
        rel_entry
      }
    }), use.names = FALSE)
  }
  as.character(walk(""))
}

# How a file is shown: "image" for the image formats a browser displays,
# "text" for anything that reads as text, "binary" otherwise
#
# path: Path to the file

file_display_kind = function(path) {
  ext = tolower(fs::path_ext(path))
  if (ext %in% c("png", "jpg", "jpeg", "gif", "svg", "webp", "bmp")) {
    return("image")
  }
  if (!file.exists(path)) {
    return("text")
  }
  con = file(path, "rb")
  on.exit(close(con))
  head = readBin(con, "raw", n = 8000)
  if (any(head == as.raw(0))) "binary" else "text"
}

# The Monaco editor language for a file, from its extension or name (the
# languages bundled with Monaco; anything else, a Makefile among them, shows
# as plain text)
#
# path: File path

file_monaco_language = function(path) {
  name = tolower(basename(path))
  if (name == "dockerfile") {
    return("dockerfile")
  }
  switch(
    tolower(fs::path_ext(path)),
    r = "r", py = "python", qmd = , rmd = , md = "markdown",
    sh = , bash = "shell", sql = "sql", json = "json",
    yaml = , yml = "yaml", toml = "ini", ini = "ini", cfg = "ini",
    js = "javascript", ts = "typescript", css = "css", html = , htm = "html",
    xml = "xml", cpp = , cc = , c = , h = , hpp = "cpp", java = "java",
    jl = "julia", rs = "rust", go = "go",
    "plaintext"
  )
}

# The validation result of a question on a file: it passes when the
# repository has the file. Same shape as validate_question_rules()'s result,
# plus the path that was found.
#
# question: markermd_question S7 object on a file
# repo_dir: The repository directory, or NULL when there is none to look in

validate_file_question = function(question, repo_dir) {
  found = if (is.null(repo_dir)) NA_character_ else find_repo_file(repo_dir, question@file)
  relative = if (is.na(found)) NA_character_ else {
    as.character(fs::path_rel(found, normalizePath(repo_dir, winslash = "/")))
  }
  message = if (is.na(found)) {
    paste0(question@file, " not found in the repository")
  } else if (identical(relative, question@file)) {
    paste0("Found ", question@file)
  } else {
    paste0("Found ", question@file, " at ", relative)
  }
  list(
    question_name = question@name,
    status = if (is.na(found)) "fail" else "pass",
    messages = message,
    passed = !is.na(found),
    details = paste0("File: ", question@file, "\n", message),
    file = question@file,
    path = found
  )
}

# The largest text file the file view embeds in the editor (the whole text
# is sent to the browser)

file_view_max_bytes = 2 * 1024^2

# A read-only view of one file: its source in a Monaco editor (language from
# the extension), an image preview, or a note for other binaries (and for a
# text file over file_view_max_bytes), under a header naming the file. Used by the template editor's file view and by the
# mark app's content pane for a question on a file.
#
# path: Absolute path of the file, or NA when it was not found
# label: The file's repository-relative path, shown in the header
# editor_id: DOM id for the Monaco editor (one per view, so the shared editor
#   registry disposes the previous editor on re-render)
# note: Optional extra text for the header (e.g. where the file was found)

file_view_ui = function(path, label, editor_id, note = NULL) {
  header = htmltools::div(
    class = "d-flex align-items-center gap-2 px-2 py-1 border-bottom bg-light small",
    fa_icon("file-lines", class = "text-muted"),
    htmltools::tags$code(label),
    if (!is.null(note)) htmltools::span(class = "text-muted", note)
  )

  if (is.na(path)) {
    return(htmltools::div(
      header,
      htmltools::div(
        class = "alert alert-warning m-2 small",
        fa_icon("triangle-exclamation"),
        paste0(" ", label, " was not found in this repository.")
      )
    ))
  }

  kind = file_display_kind(path)
  body = if (identical(kind, "image")) {
    ext = tolower(fs::path_ext(path))
    mime = if (ext == "svg") "image/svg+xml" else paste0("image/", if (ext == "jpg") "jpeg" else ext)
    htmltools::div(
      class = "p-2 overflow-auto",
      htmltools::tags$img(
        src = paste0("data:", mime, ";base64,", jsonlite::base64_enc(readBin(path, "raw", file.size(path)))),
        class = "img-fluid border rounded",
        alt = label
      )
    )
  } else if (identical(kind, "binary")) {
    htmltools::div(
      class = "text-muted small p-3",
      paste0("Binary file, ", format(structure(file.size(path), class = "object_size"), units = "auto"), ".")
    )
  } else if (file.size(path) > file_view_max_bytes) {
    htmltools::div(
      class = "text-muted small p-3",
      paste0(
        "Text file of ", format(structure(file.size(path), class = "object_size"), units = "auto"),
        ", too large to show here (the limit is ",
        format(structure(file_view_max_bytes, class = "object_size"), units = "auto"), ")."
      )
    )
  } else {
    content = paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # A fixed height: a container sized from its parent's percentage heights
    # has no definite height here, and Monaco's automatic layout then keeps
    # growing it
    htmltools::tagList(
      htmltools::div(
        class = "p-2",
        style = "height: 70vh;",
        htmltools::div(id = editor_id, class = "h-100 w-100 border rounded")
      ),
      htmltools::tags$script(htmltools::HTML(
        monaco_editor_config(editor_id, content, file_monaco_language(path), font_size = 11)
      ))
    )
  }

  htmltools::div(header, body)
}

# The directory an assignment's other files are listed from, and that template
# file paths are relative to: the repository root. That is the given key
# directory when there is one (a project's paths.key, or the directory handed
# to assignment_outline()); otherwise the nearest enclosing git repository
# when the document sits at most max_depth directories below it; otherwise the
# document's own directory. The depth limit keeps a loose document inside some
# unrelated outer repository (an example shipped in a package's source tree)
# from listing that whole repository.
#
# file: Path to the assignment document
# key_dir: The key directory, or NULL
# max_depth: How many directories above the document's to look for .git

assignment_files_root = function(file, key_dir = NULL, max_depth = 2) {
  if (!is.null(key_dir)) {
    return(normalizePath(key_dir, winslash = "/"))
  }
  dir = normalizePath(dirname(file), winslash = "/")
  candidate = dir
  for (i in 0:max_depth) {
    if (file.exists(file.path(candidate, ".git"))) {
      return(candidate)
    }
    parent = dirname(candidate)
    if (identical(parent, candidate)) break
    candidate = parent
  }
  dir
}

# The files an assignment could add to its template: everything under the key
# (solution) directory except .git and the Quarto document itself, each
# marked as filtered (hidden by default in template()'s Add files list) when
# it is a hidden file or under a hidden directory, render output (a _files,
# _freeze, .quarto or _site directory, renv's library), a lock file, a
# project file, or the README.
#
# root: The key directory
# exclude: Repository-relative paths to leave out (the Quarto document)
# limit: The most files to list
# Returns: Data frame with path (repository-relative), filtered and reason

assignment_file_candidates = function(root, exclude = character(0), limit = 2000) {
  paths = list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  paths = paths[!grepl("(^|/)\\.git(/|$)", paths)]
  paths = setdiff(sort(paths), exclude)
  paths = utils::head(paths, limit)

  base = basename(paths)
  lock_files = c(
    "uv.lock", "renv.lock", "package-lock.json", "yarn.lock", "pnpm-lock.yaml",
    "Pipfile.lock", "poetry.lock", "Cargo.lock", "Gemfile.lock", "composer.lock"
  )
  reason = ifelse(
    grepl("(^|/)\\.", paths), "hidden",
    ifelse(
      grepl("(^|/)([^/]*_files|_freeze|_site|\\.quarto)(/|$)|(^|/)renv/(library|staging)(/|$)", paths), "render output",
      ifelse(
        base %in% lock_files, "lock file",
        ifelse(
          grepl("\\.(Rproj|code-workspace)$", base, ignore.case = TRUE), "project file",
          ifelse(tolower(base) == "readme.md", "README", NA_character_)
        )
      )
    )
  )
  data.frame(path = paths, filtered = !is.na(reason), reason = reason, stringsAsFactors = FALSE)
}

# The icon for a file in the template editor's file lists
#
# path: File path

file_icon_name = function(path) {
  ext = tolower(fs::path_ext(path))
  if (ext %in% c("png", "jpg", "jpeg", "gif", "svg", "webp", "bmp")) return("file-image")
  if (ext %in% c("r", "py", "sh", "js", "ts", "jl", "sql", "c", "cpp", "h", "rs", "go", "java")) return("file-code")
  if (ext %in% c("csv", "tsv", "json", "xlsx", "xls", "rds", "parquet")) return("file-csv")
  if (ext %in% c("qmd", "rmd", "md", "txt")) return("file-lines")
  "file"
}
