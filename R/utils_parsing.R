#' Parse an assignment document into a Pandoc AST
#'
#' Reads a qmd document, normalises any knitr-style chunk headers so the
#' Quarto parser accepts them, and parses it into a q2r `pandoc` AST.
#'
#' Parse diagnostics are surfaced rather than stripped: error-kind
#' diagnostics from q2r are raised as R errors. Warning-kind diagnostics
#' are not signalled (they would otherwise print to the console from inside
#' the apps); they stay attached to the returned AST's `@diagnostics` slot,
#' labelled with the document's file name, and [mark()], [template()] and
#' [parse_project()] report them. Rewriting a knitr-style chunk header adds
#' lines, so a diagnostic whose line number moved carries a hint giving the
#' line of the file as written. If parsing yields an empty AST (no blocks)
#' an error is reported.
#'
#' @param file_path Character. Path to the assignment file
#' @return A q2r `pandoc` AST object for the parsed document.
#' @export
parse_assignment_document = function(file_path) {
  if (!file.exists(file_path)) {
    cli::cli_abort("Assignment file does not exist: {file_path}")
  }

  lines = readLines(file_path, warn = FALSE)
  normalized = normalize_knitr_chunks(lines)
  line_map = attr(normalized, "line_map")
  text = paste(normalized, collapse = "\n")
  file_name = basename(file_path)
  # The error is re-raised with the file name and the original line numbers
  # on its diagnostics; the warning handler sits outside so warnings
  # signalled before the error are still muffled
  ast = withCallingHandlers(
    tryCatch(
      q2r::parse_qmd(text, quiet = FALSE),
      q2r_parse_error = function(e) stop(relabel_parse_error(e, line_map, file_name))
    ),
    q2r_parse_warning = function(w) invokeRestart("muffleWarning")
  )
  ast@diagnostics = lapply(ast@diagnostics, function(d) relabel_diagnostic(d, line_map, file_name))

  if (length(ast@blocks@content) == 0) {
    cli::cli_abort("Failed to parse assignment file (empty AST): {file_path}")
  }

  ast
}

# Relabel a q2r diagnostic with the document's file name and, when the
# knitr rewrite moved its line, a hint giving the line of the file as
# written (the diagnostic's own excerpt shows the rewritten text, which is
# what the parser saw)
#
# d: A q2r pampa_diagnostic object
# line_map: Integer vector from normalize_knitr_chunks(): the original line
#   of each parsed line
# file_name: The document's file name

relabel_diagnostic = function(d, line_map, file_name) {
  d@source_filename = file_name
  row = d@location$start_row
  if (!is.null(row) && length(row) == 1 && !is.na(row) && row >= 1 && row <= length(line_map)) {
    original = line_map[row]
    if (!is.na(original) && original != row) {
      d@hints = c(
        d@hints,
        paste0(
          "Line ", row, " here is line ", original, " of ", file_name,
          " (knitr-style chunk headers are expanded before parsing)."
        )
      )
    }
  }
  d
}

# A q2r parse error re-raised with its diagnostics relabelled (see
# relabel_diagnostic()) and its message rendered from them without ANSI
# styling, keeping the condition's class and fields
#
# e: A q2r_parse_error condition
# line_map, file_name: See relabel_diagnostic()

relabel_parse_error = function(e, line_map, file_name) {
  diagnostics = lapply(e$diagnostics, relabel_diagnostic, line_map = line_map, file_name = file_name)
  message = paste(vapply(diagnostics, function(d) format(d, color = FALSE), character(1)), collapse = "\n")
  errorCondition(message, class = "q2r_parse_error", diagnostics = diagnostics, result = e$result, call = NULL)
}

# Rewrite knitr-style chunk headers into Quarto #| form
#
# The Quarto parser (q2r/pampa) rejects knitr-style executable chunk headers
# such as ```{r label} or ```{r label, echo=FALSE} with error Q-2-36. This
# rewrites the opening fence of every such chunk to a bare ```{engine} header
# followed by #| label: / #| <option>: lines, leaving the chunk body and all
# other content untouched. Plain fences (```r, ```), pandoc-attribute fences
# ({.r ...}, {#id ...}) and chunks already in #| form are left as-is. The
# result carries a "line_map" attribute: for each output line, the input
# line it came from (the inserted #| lines map to their header's line), so
# positions in the parsed text can be traced back to the file.
#
# lines: Character vector of document lines

normalize_knitr_chunks = function(lines) {
  fence_open = "^(\\s*)(`{3,}|~{3,})\\{([^}]*)\\}\\s*$"
  fence_any = "^\\s*(`{3,}|~{3,})"
  fence_close = "^\\s*(`{3,}|~{3,})\\s*$"

  out = character(0)
  line_map = integer(0)
  in_block = FALSE

  for (i in seq_along(lines)) {
    line = lines[i]
    if (in_block) {
      out = c(out, line)
      line_map = c(line_map, i)
      if (grepl(fence_close, line)) in_block = FALSE
      next
    }

    m = regmatches(line, regexec(fence_open, line))[[1]]
    if (length(m) == 4L) {
      converted = convert_chunk_header(trimws(m[4]))
      if (is.null(converted)) {
        out = c(out, line)
        line_map = c(line_map, i)
      } else {
        added = c(
          paste0(m[2], m[3], "{", converted$engine, "}"),
          paste0(m[2], "#| ", converted$options)
        )
        out = c(out, added)
        line_map = c(line_map, rep(i, length(added)))
      }
      in_block = TRUE
      next
    }

    if (grepl(fence_any, line)) in_block = TRUE
    out = c(out, line)
    line_map = c(line_map, i)
  }

  attr(out, "line_map") = line_map
  out
}

# Translate line ranges of the normalized document (see
# normalize_knitr_chunks()) back to ranges of the file as written
#
# ranges: List of start / end ranges over the normalized lines
# line_map: The "line_map" attribute of the normalized lines
# Returns: List of start / end ranges over the original lines

map_ranges_to_original = function(ranges, line_map) {
  if (length(ranges) == 0 || is.null(line_map)) {
    return(ranges)
  }
  original = sort(unique(unlist(lapply(ranges, function(r) {
    line_map[seq.int(max(1L, r$start), min(length(line_map), r$end))]
  }))))
  group_consecutive_lines(original)
}

# Convert the contents of a knitr chunk header into engine + Quarto options
#
# Returns NULL when the header is not a knitr executable chunk that needs
# rewriting (a bare {r}, a pandoc-attribute form, or non-engine content).
#
# inner: Character. The text between the chunk-header braces

convert_chunk_header = function(inner) {
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*", inner)) {
    return(NULL)
  }

  engine = sub("^([A-Za-z][A-Za-z0-9_]*).*$", "\\1", inner)
  rest = trimws(sub("^[A-Za-z][A-Za-z0-9_]*", "", inner))
  if (!nzchar(rest)) {
    return(NULL)
  }

  rest = trimws(sub("^,", "", rest))
  parts = trimws(split_top_level(rest, ","))
  parts = parts[nzchar(parts)]

  options = character(0)
  have_label = FALSE
  for (part in parts) {
    if (grepl("=", part)) {
      key = gsub("\\.", "-", trimws(sub("=.*$", "", part)))
      value = convert_knitr_value(trimws(sub("^[^=]*=", "", part)))
      options = c(options, paste0(key, ": ", value))
    } else if (!have_label && !grepl("^[.#]", part)) {
      # A leading "." (class) or "#" (id) is a pandoc attribute, not a label.
      options = c(options, paste0("label: ", part))
      have_label = TRUE
    }
  }

  if (length(options) == 0) {
    return(NULL)
  }

  list(engine = engine, options = options)
}

# Convert a knitr R option value into its Quarto YAML equivalent
#
# value: Character. The right-hand side of a knitr key=value option

convert_knitr_value = function(value) {
  if (value %in% c("TRUE", "T")) {
    return("true")
  }
  if (value %in% c("FALSE", "F")) {
    return("false")
  }
  value
}

# Split a string on a separator, ignoring separators inside quotes or brackets
#
# s: Character. The string to split
# sep: Character. A single-character separator

split_top_level = function(s, sep) {
  chars = strsplit(s, "")[[1]]
  depth = 0
  in_quote = ""
  current = ""
  result = character(0)

  for (ch in chars) {
    if (nzchar(in_quote)) {
      current = paste0(current, ch)
      if (ch == in_quote) in_quote = ""
    } else if (ch %in% c("'", "\"")) {
      in_quote = ch
      current = paste0(current, ch)
    } else if (ch %in% c("(", "[", "{")) {
      depth = depth + 1
      current = paste0(current, ch)
    } else if (ch %in% c(")", "]", "}")) {
      depth = depth - 1
      current = paste0(current, ch)
    } else if (ch == sep && depth == 0) {
      result = c(result, current)
      current = ""
    } else {
      current = paste0(current, ch)
    }
  }

  c(result, current)
}

# Parse every assignment document under a collection directory
#
# Finds assignment files recursively and parses each via
# parse_assignment_document (which normalises knitr-style chunk headers),
# returning a data frame with one row per file: a `path` (character) column,
# an `ast` (list of pandoc objects, NULL on failure) column, and an `error`
# column holding the parse error message verbatim (NA on success). Parse
# failures are captured per file rather than aborting the whole collection so
# one malformed student document cannot block grading of the others; the
# error text itself is surfaced, never stripped.
#
# collection_path: Path to the directory of repositories
# use_qmd: Logical. Match .qmd files (TRUE) or .Rmd files (FALSE)

parse_assignment_collection = function(collection_path, use_qmd = TRUE) {
  pattern = if (use_qmd) "\\.qmd$" else "\\.Rmd$"
  files = list.files(
    collection_path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  collection = data.frame(path = files, stringsAsFactors = FALSE)
  parsed = lapply(files, purrr::safely(parse_assignment_document))
  collection$ast = lapply(parsed, function(p) p$result)
  collection$error = vapply(parsed, function(p) {
    if (is.null(p$error)) NA_character_ else conditionMessage(p$error)
  }, character(1))
  collection
}

# Parse one document per student repository: the file find_repo_assignment()
# picks for each repository (the template's document by basename, else a
# root-level document, else the first match), parsed with
# parse_assignment_document(). The same choice validate_project() and
# parse_project() make, so the app grades and validates the document the
# headless checks report on. Parse failures are captured per repository
# rather than raised, so one malformed document cannot block the others.
#
# repos_dir: The directory holding the repositories
# repo_list: The repository names (directories under repos_dir)
# assignment_file: The template document's basename, or NULL
# use_qmd: Logical. Match .qmd files (TRUE) or .Rmd files (FALSE)
# Returns: Data frame with one row per repository that has a document: repo,
#   path, ast (list column, NULL on failure) and error (NA on success)

parse_repo_documents = function(repos_dir, repo_list, assignment_file = NULL, use_qmd = TRUE) {
  ext = if (use_qmd) "\\.qmd$" else "\\.Rmd$"
  paths = vapply(repo_list, function(repo) {
    find_repo_assignment(file.path(repos_dir, repo), assignment_file, ext)
  }, character(1))
  found = !is.na(paths)
  rows = parse_repo_document_rows(repo_list[found], unname(paths[found]))
  rows
}

# The collection rows for the given repositories and document paths (see
# parse_repo_documents()); the app re-parses one repository through it
#
# repos: Repository names
# paths: Their documents' paths

parse_repo_document_rows = function(repos, paths) {
  parsed = lapply(paths, purrr::safely(parse_assignment_document))
  collection = data.frame(repo = repos, path = paths, stringsAsFactors = FALSE)
  collection$ast = lapply(parsed, function(p) p$result)
  collection$error = vapply(parsed, function(p) {
    if (is.null(p$error)) NA_character_ else conditionMessage(p$error)
  }, character(1))
  collection
}

# The parsed AST for one repo of a collection, or NULL when the repo has no
# document or its document failed to parse
#
# collection: Collection data frame (see parse_assignment_collection(), with
#   the repo column attributed by the caller)
# repo: Character. Repository name

collection_ast_for = function(collection, repo) {
  repo_rows = collection$repo == repo
  if (any(repo_rows) && !is.null(collection$ast[repo_rows][[1]])) {
    collection$ast[repo_rows][[1]]
  } else {
    NULL
  }
}


# The warning-kind parse diagnostics of a parsed document (see
# parse_assignment_document(), which keeps them instead of signalling them)
#
# ast: A q2r pandoc AST, or NULL

document_parse_warnings = function(ast) {
  if (is.null(ast)) {
    return(list())
  }
  Filter(function(d) identical(d@kind, "warning"), ast@diagnostics)
}

# Parse diagnostics as plain text, one string each: q2r's rendering of the
# message, the source excerpt and the hints, without ANSI styling
#
# diagnostics: List of q2r pampa_diagnostic objects

format_parse_diagnostics = function(diagnostics) {
  vapply(diagnostics, function(d) cli::ansi_strip(format(d, color = FALSE)), character(1))
}

# Parse warnings for the apps: an amber notice whose summary line counts
# them and which expands to each warning's text. NULL when there are none.
#
# diagnostics: List of q2r pampa_diagnostic objects (warnings)
# what: Short description of the document, for the summary line

parse_warnings_ui = function(diagnostics, what = "the document") {
  if (length(diagnostics) == 0) {
    return(NULL)
  }
  n = length(diagnostics)
  htmltools::div(
    class = "alert alert-warning small py-2 mb-2",
    htmltools::tags$details(
      htmltools::tags$summary(
        fa_icon("triangle-exclamation", class = "me-1"),
        paste0(n, " parser warning", if (n == 1) "" else "s", " in ", what),
        htmltools::span(class = "text-muted ms-1", "(the document still parsed; click for details)")
      ),
      lapply(format_parse_diagnostics(diagnostics), function(text) {
        htmltools::tags$pre(class = "small bg-body border rounded p-2 mt-2 mb-0", style = "white-space: pre-wrap;", text)
      })
    )
  )
}
