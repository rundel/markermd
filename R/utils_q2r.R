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

# Canonical friendly category for a Pandoc node
#
# Single source of truth shared by the tree labels (q2r_node_label) and the
# validation-rule node-type vocabulary (get_allowed_node_types), so the two
# always use the same words. Returns one short category string.
#
# node: A q2r pandoc node

q2r_node_kind = function(node) {
  if (S7::S7_inherits(node, q2r::pandoc_header)) return("Heading")
  if (S7::S7_inherits(node, q2r::pandoc_code_block)) {
    return(if (isTRUE(q2r::is_code_cell(node))) "Chunk" else "Code block")
  }
  if (S7::S7_inherits(node, q2r::pandoc_raw_block)) return("Raw Block")
  if (S7::S7_inherits(node, q2r::pandoc_div)) return("Div")
  if (S7::S7_inherits(node, q2r::pandoc_paragraph) || S7::S7_inherits(node, q2r::pandoc_plain)) return("Markdown")
  if (S7::S7_inherits(node, q2r::pandoc_bullet_list)) return("Bullet list")
  if (S7::S7_inherits(node, q2r::pandoc_ordered_list)) return("Ordered list")
  if (S7::S7_inherits(node, q2r::pandoc_block_quote)) return("Block quote")
  if (S7::S7_inherits(node, q2r::pandoc_table)) return("Table")
  if (S7::S7_inherits(node, q2r::pandoc_figure)) return("Figure")
  if (S7::S7_inherits(node, q2r::pandoc_horizontal_rule)) return("Horizontal rule")
  if (S7::S7_inherits(node, q2r::pandoc_definition_list)) return("Definition list")
  if (S7::S7_inherits(node, q2r::pandoc_line_block)) return("Line block")

  # Fallback for any other block type: friendly-case the class name
  raw = gsub("_", " ", sub("^q2r::pandoc_", "", class(node)[1]))
  paste0(toupper(substr(raw, 1, 1)), substr(raw, 2, nchar(raw)))
}

# Whether a node can be selected as a question target in the tree
#
# Headings are always selectable; divs only when they carry an explicit id (so
# the selection can be matched by id in each student's document). The single
# source of truth shared by the tree (build_ast_tree_structure / utils_tree) and
# the click-observer wiring (mod_ast).
#
# node: A q2r pandoc node

node_is_selectable = function(node) {
  if (S7::S7_inherits(node, q2r::pandoc_header)) {
    return(TRUE)
  }
  S7::S7_inherits(node, q2r::pandoc_div) && nzchar(node@attr@id)
}

# A node's standard attr id, or "" when the node has no standard pandoc_attr
# (e.g. lists carry pandoc_list_attributes, which has no id)
#
# node: A q2r pandoc node

node_attr_id = function(node) {
  a = attr(node, "attr", exact = TRUE)
  if (S7::S7_inherits(a, q2r::pandoc_attr)) a@id else ""
}

# Human-readable label describing a Pandoc node for the tree view
#
# Builds on q2r_node_kind() so the tree's leading word always matches the rule
# node-type vocabulary, appending node-specific detail. Any node carrying a
# standard attr id shows it as a trailing "(#id)".
#
# node: A q2r pandoc node

q2r_node_label = function(node) {
  kind = q2r_node_kind(node)

  label = if (kind == "Heading") {
    level = node@level
    title = q2r::ast_text(node)
    as.character(glue::glue("Heading [h{level}] - {title}"))
  } else if (kind == "Chunk") {
    engine = q2r::cell_engine(node)
    chunk_label = q2r::cell_label(node)
    n_lines = length(unlist(strsplit(q2r::cell_code(node), "\n", fixed = TRUE)))
    lines_txt = glue::glue("({n_lines} line{if (n_lines == 1) '' else 's'})")
    if (is.na(chunk_label)) {
      as.character(glue::glue("Chunk [{engine}] {lines_txt}"))
    } else {
      as.character(glue::glue("Chunk [{engine}] - {chunk_label} {lines_txt}"))
    }
  } else if (kind == "Raw Block") {
    if (nzchar(node@format)) as.character(glue::glue("Raw Block [{node@format}]")) else "Raw Block"
  } else if (kind == "Div") {
    classes = node_classes(node)
    if (length(classes) > 0) paste0("Div (", paste0(".", classes, collapse = " "), ")") else "Div"
  } else {
    kind
  }

  id = node_attr_id(node)
  if (nzchar(id)) {
    label = paste0(label, " (#", id, ")")
  }

  label
}

# A node's content preview, shown as a smaller second line under its label in
# the tree, or "" when the node has no useful text preview
#
# node: A q2r pandoc node

q2r_node_detail = function(node) {
  if (S7::S7_inherits(node, q2r::pandoc_paragraph) || S7::S7_inherits(node, q2r::pandoc_plain)) {
    return(truncate_text(q2r::ast_text(node), 120))
  }
  ""
}

# Enclosing-node id chain for each line of a markdown document
#
# Scans lines tracking ATX headings and fenced divs (ignoring any inside fenced
# code blocks so R comments are not mistaken for headings or div fences). Each
# heading line, in document order, is zipped to the next parsed header of
# repo_ast so it inherits that header's authoritative q2r id (including dedup
# suffixes and explicit {#id}), avoiding any re-implementation of the Pandoc
# slug algorithm. Fenced-div ids are read directly from the opening fence's
# {#id} attribute (class-only divs push ""). Every line is assigned the vector
# of enclosing heading and div ids; a heading or div fence line includes its
# own. Used to map a question's selected nodes onto line ranges in the displayed
# document.
#
# lines: Character vector of document lines
# repo_ast: q2r pandoc AST parsed from the same document

line_node_id_chains = function(lines, repo_ast) {
  header_records = Filter(
    function(record) S7::S7_inherits(record$node, q2r::pandoc_header),
    q2r_flatten(repo_ast)
  )
  header_ids = vapply(header_records, function(record) record$node@attr@id, character(1))

  fence_open = "^\\s*(`{3,}|~{3,})"
  fence_close = "^\\s*(`{3,}|~{3,})\\s*$"
  heading_re = "^(#{1,6})\\s+(.*?)\\s*#*\\s*$"
  div_close_re = "^\\s*:::+\\s*$"
  div_open_re = "^\\s*:::+\\s*\\S.*$"
  div_id_re = "#([^[:space:].}]+)"

  stack = list()
  div_stack = list()
  next_header = 1L
  in_block = FALSE
  chains = vector("list", length(lines))

  combined_ids = function() {
    c(
      vapply(stack, function(s) s$id, character(1)),
      vapply(div_stack, function(d) d, character(1))
    )
  }

  for (i in seq_along(lines)) {
    line = lines[i]

    if (in_block) {
      if (grepl(fence_close, line)) in_block = FALSE
      chains[[i]] = combined_ids()
      next
    }

    if (grepl(fence_open, line)) {
      in_block = TRUE
      chains[[i]] = combined_ids()
      next
    }

    # Fenced-div close: the line still belongs to the div, then pop it.
    if (length(div_stack) > 0 && grepl(div_close_re, line)) {
      chains[[i]] = combined_ids()
      div_stack[[length(div_stack)]] = NULL
      next
    }

    # Fenced-div open: push its id (or "" when class-only); line includes it.
    if (grepl(div_open_re, line)) {
      m = regmatches(line, regexec(div_id_re, line))[[1]]
      div_stack[[length(div_stack) + 1]] = if (length(m) == 2L) m[2] else ""
      chains[[i]] = combined_ids()
      next
    }

    m = regmatches(line, regexec(heading_re, line))[[1]]
    if (length(m) == 3L) {
      level = nchar(m[2])
      id = if (next_header <= length(header_ids)) header_ids[next_header] else ""
      next_header = next_header + 1L
      while (length(stack) > 0 && stack[[length(stack)]]$level >= level) {
        stack[[length(stack)]] = NULL
      }
      stack[[length(stack) + 1]] = list(level = level, id = id)
    }

    chains[[i]] = combined_ids()
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
# depth (top-level = 1), the enclosing heading-section chain (h1..h6), a
# display label, and three structural fields that keep the two kinds of
# nesting distinct:
#
# - parent: the display-tree parent index (0 = document root). Combines
#   synthetic heading-section nesting (a section block's parent is its
#   heading) with real AST nesting (a div child's parent is the div).
# - container: the real AST parent only - the index of the immediately
#   enclosing div, or 0 for the document's top-level blocks. Headings never
#   contain other nodes here, so container == 0 identifies exactly the
#   records corresponding to doc@blocks@content.
# - block_pos: for top-level records (container == 0), the node's position in
#   doc@blocks@content; NA for nested records. This is the explicit bridge
#   between flatten-index space and block-list space.
#
# doc: A q2r pandoc document

q2r_flatten = function(doc) {
  records = list()
  heading_stack = list()
  idx = 0L

  section_vector = function() {
    section = stats::setNames(rep(NA_character_, 6), paste0("h", 1:6))
    for (heading in heading_stack) section[heading$level] = heading$title
    section
  }

  # Record a node (and recurse into fenced-div children) under a fixed parent
  add_node = function(node, parent, depth, section, container, block_pos) {
    idx <<- idx + 1L
    this = idx
    records[[this]] <<- list(
      index = this,
      node = node,
      type = sub("^q2r::", "", class(node)[1]),
      depth = depth,
      parent = parent,
      container = container,
      block_pos = block_pos,
      section = section,
      label = q2r_node_label(node),
      detail = q2r_node_detail(node)
    )

    if (S7::S7_inherits(node, q2r::pandoc_div)) {
      for (child in node@content@content) {
        add_node(child, this, depth + 1, section, container = this, block_pos = NA_integer_)
      }
    }

    this
  }

  blocks = doc@blocks@content
  for (pos in seq_along(blocks)) {
    node = blocks[[pos]]
    if (S7::S7_inherits(node, q2r::pandoc_header)) {
      level = node@level
      while (length(heading_stack) > 0 && heading_stack[[length(heading_stack)]]$level >= level) {
        heading_stack[[length(heading_stack)]] = NULL
      }
      parent = if (length(heading_stack) > 0) heading_stack[[length(heading_stack)]]$index else 0
      depth = length(heading_stack) + 1
      heading_stack[[length(heading_stack) + 1]] = list(index = idx + 1L, level = level, title = q2r::ast_text(node))
      add_node(node, parent, depth, section_vector(), container = 0L, block_pos = pos)
    } else {
      parent = if (length(heading_stack) > 0) heading_stack[[length(heading_stack)]]$index else 0
      depth = length(heading_stack) + 1
      add_node(node, parent, depth, section_vector(), container = 0L, block_pos = pos)
    }
  }

  records
}
