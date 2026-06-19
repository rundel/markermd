#' Parse an assignment document into a Pandoc AST
#'
#' Reads a qmd document, normalises any knitr-style chunk headers so the
#' Quarto parser accepts them, and parses it into a q2r `pandoc` AST.
#'
#' Parse diagnostics are surfaced rather than stripped: error-kind
#' diagnostics from q2r are raised as R errors and warnings as R warnings.
#' If parsing yields an empty AST (no blocks) an error is reported.
#'
#' @param file_path Character. Path to the assignment file
#' @export
parse_assignment_document = function(file_path) {
  if (!file.exists(file_path)) {
    stop("Assignment file does not exist: ", file_path)
  }

  lines = readLines(file_path, warn = FALSE)
  text = paste(normalize_knitr_chunks(lines), collapse = "\n")
  ast = q2r::parse_qmd(text, quiet = FALSE)

  if (length(ast@blocks@content) == 0) {
    stop("Failed to parse assignment file (empty AST): ", file_path)
  }

  ast
}

# Rewrite knitr-style chunk headers into Quarto #| form
#
# The Quarto parser (q2r/pampa) rejects knitr-style executable chunk headers
# such as ```{r label} or ```{r label, echo=FALSE} with error Q-2-36. This
# rewrites the opening fence of every such chunk to a bare ```{engine} header
# followed by #| label: / #| <option>: lines, leaving the chunk body and all
# other content untouched. Plain fences (```r, ```), pandoc-attribute fences
# ({.r ...}, {#id ...}) and chunks already in #| form are left as-is.
#
# lines: Character vector of document lines

normalize_knitr_chunks = function(lines) {
  fence_open = "^(\\s*)(`{3,}|~{3,})\\{([^}]*)\\}\\s*$"
  fence_any = "^\\s*(`{3,}|~{3,})"
  fence_close = "^\\s*(`{3,}|~{3,})\\s*$"

  out = character(0)
  in_block = FALSE

  for (line in lines) {
    if (in_block) {
      out = c(out, line)
      if (grepl(fence_close, line)) in_block = FALSE
      next
    }

    m = regmatches(line, regexec(fence_open, line))[[1]]
    if (length(m) == 4L) {
      converted = convert_chunk_header(trimws(m[4]))
      if (is.null(converted)) {
        out = c(out, line)
      } else {
        out = c(
          out,
          paste0(m[2], m[3], "{", converted$engine, "}"),
          paste0(m[2], "#| ", converted$options)
        )
      }
      in_block = TRUE
      next
    }

    if (grepl(fence_any, line)) in_block = TRUE
    out = c(out, line)
  }

  out
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

