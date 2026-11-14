# markermd's read-only code view, built on Prism (vendored in inst/www/prism;
# see its README). A view is plain markup, a <pre><code> holding the text,
# that markermd-code-view.js highlights in the browser as it enters the
# page, splitting the result into one element per line so line numbers and
# question highlights sit exactly on their lines. Without the dependency the
# same markup shows the text unhighlighted.

prism_version = function() {
  "1.30.0"
}

# The Prism language components markermd loads, in dependency order (a
# component after every component it extends)

prism_components = function() {
  c(
    "markup", "css", "clike", "javascript", "typescript", "c", "cpp", "java",
    "go", "rust", "json", "yaml", "markdown", "r", "python", "bash", "sql",
    "latex", "ini", "toml", "julia", "docker", "makefile"
  )
}

# The htmlDependency both apps attach for the code views

code_view_dependency = function() {
  htmltools::htmlDependency(
    name = "markermd-code-view",
    version = prism_version(),
    src = c(file = "www/prism"),
    package = "markermd",
    script = c(
      "prism-config.js",
      "prism-core.min.js",
      file.path("components", paste0("prism-", prism_components(), ".min.js")),
      "markermd-qmd.js",
      "markermd-code-view.js"
    ),
    stylesheet = "markermd-code-view.css"
  )
}

# Line ranges as the JSON the code view reads from data-highlight
#
# highlight_ranges: List of list(start =, end =) line ranges (or NULL)

code_view_ranges_json = function(highlight_ranges) {
  ranges = code_view_ranges(highlight_ranges)
  if (length(ranges) == 0) {
    return("[]")
  }
  as.character(jsonlite::toJSON(ranges, auto_unbox = TRUE))
}

# Line ranges as a list of list(start =, end =) integer pairs
#
# highlight_ranges: List of list(start =, end =) line ranges (or NULL)

code_view_ranges = function(highlight_ranges) {
  lapply(highlight_ranges, function(range) {
    list(start = as.integer(range$start), end = as.integer(range$end))
  })
}

# A read-only code view of `content`
#
# The <pre><code> is written as one HTML string: htmltools would indent the
# <code> tag inside the <pre>, and that whitespace would render as a blank
# first line.
#
# id: DOM id of the view (used to update its highlight in place)
# content: The text to show
# language: Prism language name (see file_code_language() and
#   code_language_for_engine()); an unknown name shows plain text
# highlight_ranges: Optional list of list(start =, end =) line ranges to
#   highlight; the first is scrolled into view
# font_size: Font size in px
# class: Extra classes for the view's container (its height comes from them
#   or from the parent)

code_view_ui = function(id, content, language, highlight_ranges = NULL, font_size = 12, class = NULL) {
  htmltools::div(
    id = id,
    class = paste(c("markermd-code-view border rounded", class), collapse = " "),
    style = paste0("--mm-code-size: ", font_size, "px;"),
    `data-language` = language,
    `data-highlight` = code_view_ranges_json(highlight_ranges),
    htmltools::HTML(paste0(
      "<pre><code>",
      htmltools::htmlEscape(paste(content, collapse = "\n")),
      "</code></pre>"
    ))
  )
}

# Replace a rendered code view's highlighted lines in place, scrolling the
# first one into view
#
# session: Shiny session
# id: DOM id of the code view
# highlight_ranges: List of list(start =, end =) line ranges (or NULL)

update_code_view_highlight = function(session, id, highlight_ranges) {
  session$sendCustomMessage("markermd_code_view_highlight", list(
    id = id,
    ranges = code_view_ranges(highlight_ranges)
  ))
}

# The Prism language for a file, from its extension or name (anything not
# covered shows as plain text)
#
# path: File path

file_code_language = function(path) {
  name = tolower(basename(path))
  if (name == "dockerfile") {
    return("docker")
  }
  if (name %in% c("makefile", "gnumakefile")) {
    return("makefile")
  }
  switch(
    tolower(fs::path_ext(path)),
    r = "r", py = "python", qmd = , rmd = "qmd", md = , markdown = "markdown",
    sh = , bash = , zsh = "bash", sql = "sql", json = "json",
    yaml = , yml = "yaml", toml = "toml", ini = , cfg = "ini",
    js = , mjs = "javascript", ts = "typescript", css = "css",
    html = , htm = , xml = , svg = "markup",
    c = , h = "c", cpp = , cc = , hpp = "cpp", java = "java",
    jl = "julia", rs = "rust", go = "go", tex = "latex",
    "none"
  )
}

# The Prism language for a code-cell engine (unknown engines show the cell as
# a qmd document)
#
# engine: Character. A code-cell engine name (e.g. "r", "python")

code_language_for_engine = function(engine) {
  switch(
    engine,
    "r" = "r",
    "python" = "python",
    "sql" = "sql",
    "bash" = , "sh" = "bash",
    "javascript" = , "js" = , "ojs" = "javascript",
    "css" = "css",
    "html" = "markup",
    "yaml" = "yaml",
    "json" = "json",
    "julia" = "julia",
    "qmd"
  )
}

# The Prism language for a Pandoc node: the engine's for a code cell, else
# qmd (so a node shown with its fence is highlighted as a document)
#
# node: A q2r pandoc node

code_language_for_node = function(node) {
  if (S7::S7_inherits(node, q2r::pandoc_code_block) && q2r::is_code_cell(node)) {
    return(code_language_for_engine(q2r::cell_engine(node)))
  }
  "qmd"
}
