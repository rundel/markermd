# Helpers bridging markermd to the q2r Pandoc AST
#
# These centralise the q2r-specific operations markermd relies on: rendering a
# single node back to qmd, producing a human-readable tree label, and flattening
# the nested Pandoc AST into an ordered, indexable node list that mirrors the
# flat-list model the rest of the package is built around.

# Render a single Pandoc node (or whole document) back to qmd source
#
# q2r's to_qmd() only dispatches on a document root, so a single block is
# wrapped in a minimal pandoc document before rendering.
#
# node: A q2r pandoc node or pandoc document

node_to_qmd = function(node) {
  if (S7::S7_inherits(node, q2r::pandoc)) {
    return(q2r::to_qmd(node))
  }
  q2r::to_qmd(q2r::pandoc(blocks = q2r::pandoc_blocks(list(node))))
}

# Classes attached to a node's pandoc attributes, or character(0)
#
# node: A q2r pandoc node

node_classes = function(node) {
  if (!("attr" %in% names(S7::props(node)))) {
    return(character(0))
  }
  classes = node@attr@classes
  if (is.null(classes)) character(0) else classes
}

# Collapse whitespace and truncate a string for display
#
# x: Character
# n: Integer. Maximum length

truncate_text = function(x, n) {
  x = gsub("\\s+", " ", trimws(x))
  if (nchar(x) > n) paste0(substr(x, 1, n - 1), "…") else x
}

# Human-readable label describing a Pandoc node for the tree view
#
# node: A q2r pandoc node

q2r_node_label = function(node) {
  if (S7::S7_inherits(node, q2r::pandoc_header)) {
    return(sprintf("Heading [h%d] - %s", node@level, q2r::ast_text(node)))
  }

  if (S7::S7_inherits(node, q2r::pandoc_code_block)) {
    if (q2r::is_code_cell(node)) {
      label = q2r::cell_label(node)
      n_lines = length(unlist(strsplit(q2r::cell_code(node), "\n", fixed = TRUE)))
      label_txt = if (is.na(label)) "" else paste0(" ", label)
      return(sprintf(
        "Chunk [%s%s, %d line%s]",
        q2r::cell_engine(node), label_txt, n_lines, if (n_lines == 1) "" else "s"
      ))
    }
    return("Code block")
  }

  if (S7::S7_inherits(node, q2r::pandoc_div)) {
    classes = node_classes(node)
    if (length(classes) > 0) {
      return(paste0("Div (.", paste(classes, collapse = " ."), ")"))
    }
    return("Div")
  }

  if (S7::S7_inherits(node, q2r::pandoc_paragraph) || S7::S7_inherits(node, q2r::pandoc_plain)) {
    return(paste0("Markdown: ", truncate_text(q2r::ast_text(node), 60)))
  }

  gsub("_", " ", sub("^q2r::pandoc_", "", class(node)[1]))
}

# Heading-section chain for each line of a markdown document
#
# Scans lines tracking ATX headings (ignoring any inside fenced code blocks so
# R comments are not mistaken for headings) and assigns every line the chain of
# enclosing heading titles as a length-6 NA-filled vector (h1..h6). A heading
# line belongs to the section it introduces. Used to map selected sections onto
# line ranges in the displayed document.
#
# lines: Character vector of document lines

line_section_chains = function(lines) {
  fence_open = "^\\s*(`{3,}|~{3,})"
  fence_close = "^\\s*(`{3,}|~{3,})\\s*$"
  heading_re = "^(#{1,6})\\s+(.*?)\\s*#*\\s*$"

  section = rep(NA_character_, 6)
  in_block = FALSE
  chains = vector("list", length(lines))

  for (i in seq_along(lines)) {
    line = lines[i]

    if (in_block) {
      if (grepl(fence_close, line)) in_block = FALSE
      chains[[i]] = section
      next
    }

    if (grepl(fence_open, line)) {
      in_block = TRUE
      chains[[i]] = section
      next
    }

    m = regmatches(line, regexec(heading_re, line))[[1]]
    if (length(m) == 3L) {
      level = nchar(m[2])
      section[level] = trimws(m[3])
      if (level < 6) section[(level + 1):6] = NA_character_
    }

    chains[[i]] = section
  }

  chains
}

# Monaco editor language for a code-cell engine
#
# engine: Character. A code-cell engine name (e.g. "r", "python")

monaco_language_for_engine = function(engine) {
  switch(
    engine,
    "r" = "r",
    "python" = "python",
    "sql" = "sql",
    "bash" = "shell",
    "sh" = "shell",
    "javascript" = "javascript",
    "js" = "javascript",
    "css" = "css",
    "html" = "html",
    "yaml" = "yaml",
    "json" = "json",
    "markdown"
  )
}

# Monaco editor language for a Pandoc node (engine for code cells, else markdown)
#
# node: A q2r pandoc node

monaco_language_for_node = function(node) {
  if (S7::S7_inherits(node, q2r::pandoc_code_block) && q2r::is_code_cell(node)) {
    return(monaco_language_for_engine(q2r::cell_engine(node)))
  }
  "markdown"
}

# Flatten a q2r document into an ordered list of indexable node records
#
# Walks the Pandoc AST in document order, descending into fenced divs so that
# every node receives a stable 1-based index (the model the tree, selection,
# and validation code keys on). Each record carries the live node, its nesting
# depth (top-level = 1), the index of its parent (0 = document root), the
# enclosing heading-section chain (h1..h6), and a display label.
#
# doc: A q2r pandoc document

q2r_flatten = function(doc) {
  records = list()
  heading_stack = list()
  idx = 0

  section_vector = function() {
    section = stats::setNames(rep(NA_character_, 6), paste0("h", 1:6))
    for (heading in heading_stack) section[heading$level] = heading$title
    section
  }

  # Record a node (and recurse into fenced-div children) under a fixed parent
  add_node = function(node, parent, depth, section) {
    idx <<- idx + 1
    this = idx
    records[[this]] <<- list(
      index = this,
      node = node,
      type = sub("^q2r::", "", class(node)[1]),
      depth = depth,
      parent = parent,
      section = section,
      label = q2r_node_label(node)
    )

    if (S7::S7_inherits(node, q2r::pandoc_div)) {
      for (child in node@content@content) {
        add_node(child, this, depth + 1, section)
      }
    }

    this
  }

  for (node in doc@blocks@content) {
    if (S7::S7_inherits(node, q2r::pandoc_header)) {
      level = node@level
      while (length(heading_stack) > 0 && heading_stack[[length(heading_stack)]]$level >= level) {
        heading_stack[[length(heading_stack)]] = NULL
      }
      parent = if (length(heading_stack) > 0) heading_stack[[length(heading_stack)]]$index else 0
      depth = length(heading_stack) + 1
      heading_stack[[length(heading_stack) + 1]] = list(index = idx + 1, level = level, title = q2r::ast_text(node))
      add_node(node, parent, depth, section_vector())
    } else {
      parent = if (length(heading_stack) > 0) heading_stack[[length(heading_stack)]]$index else 0
      depth = length(heading_stack) + 1
      add_node(node, parent, depth, section_vector())
    }
  }

  records
}
