# Helpers backing the bundled template-scaffolding skill (inst/skills).

#' Outline the gradable sections of an assignment
#'
#' Parses an assignment document and returns one row per anchorable section: a
#' heading or an id'd `div`. Each row records the section's Pandoc anchor (the
#' `id` used as a `node_id` in a template), its level and title, and counts of
#' the content it contains. The counts are cumulative for the section exactly as
#' a validation rule scoped to that anchor would see them (via the same
#' section-selection logic used during grading), so they can drive starter-rule
#' heuristics when scaffolding a template.
#'
#' @param assignment_path Path to an assignment file (`.qmd`/`.Rmd`) or a
#'   directory containing one.
#' @param filename Glob used to find the assignment when `assignment_path` is a
#'   directory. Defaults to any `.Rmd`/`.qmd` file.
#'
#' @return A data frame with one row per anchorable section and columns `type`
#'   (`"heading"` or `"div"`), `id`, `level`, `title`, `n_subsections`, and the
#'   content counts `n_chunk`, `n_code_block`, `n_markdown`, `n_list`,
#'   `n_table`, `n_div`, `n_other` and `n_content` (all non-heading blocks). The
#'   resolved source file is attached as the `"source_file"` attribute.
#' @export
#'
#' @examples
#' key = system.file("examples/test_assignment2/hw3-key", package = "markermd")
#' assignment_outline(key)
assignment_outline = function(assignment_path, filename = "*.[Rq]md") {
  file = if (dir.exists(assignment_path)) {
    resolve_assignment_file(assignment_path, filename)
  } else if (file.exists(assignment_path)) {
    normalizePath(assignment_path)
  } else {
    cli::cli_abort("Assignment path not found: {assignment_path}")
  }

  ast = parse_assignment_document(file)

  content_kinds = c("Chunk", "Code block", "Markdown", "Bullet list", "Ordered list", "Table", "Div")

  rows = list()
  for (record in q2r_flatten(ast)) {
    node = record$node
    is_heading = S7::S7_inherits(node, q2r::pandoc_header)
    is_div = S7::S7_inherits(node, q2r::pandoc_div)
    if (!is_heading && !is_div) next

    id = node@attr@id
    if (!nzchar(id)) next

    question = markermd_question(
      id = 1L,
      name = "outline",
      selected_nodes = markermd_node_selection(node_ids = id)
    )
    blocks = get_question_ast(ast, question)@blocks@content
    kinds = if (length(blocks) > 0) vapply(blocks, q2r_node_kind, character(1)) else character(0)

    rows[[length(rows) + 1]] = data.frame(
      type = if (is_heading) "heading" else "div",
      id = id,
      level = if (is_heading) as.integer(node@level) else NA_integer_,
      title = if (is_heading) q2r::ast_text(node) else q2r_node_label(node),
      n_subsections = max(0L, sum(kinds == "Heading") - as.integer(is_heading)),
      n_chunk = sum(kinds == "Chunk"),
      n_code_block = sum(kinds == "Code block"),
      n_markdown = sum(kinds == "Markdown"),
      n_list = sum(kinds %in% c("Bullet list", "Ordered list")),
      n_table = sum(kinds == "Table"),
      n_div = sum(kinds == "Div"),
      n_other = sum(!kinds %in% c("Heading", content_kinds)),
      n_content = sum(kinds != "Heading"),
      stringsAsFactors = FALSE
    )
  }

  out = if (length(rows) > 0) {
    do.call(rbind, rows)
  } else {
    data.frame(
      type = character(0), id = character(0), level = integer(0), title = character(0),
      n_subsections = integer(0), n_chunk = integer(0), n_code_block = integer(0),
      n_markdown = integer(0), n_list = integer(0), n_table = integer(0),
      n_div = integer(0), n_other = integer(0), n_content = integer(0),
      stringsAsFactors = FALSE
    )
  }

  attr(out, "source_file") = file
  out
}

#' Path to the bundled markermd Claude Code skills
#'
#' Returns the directory holding the Claude Code skills distributed with the
#' package (e.g. the assignment template scaffolding skill). Copy a skill's
#' sub-directory into a `.claude/skills/` directory (per-user `~/.claude/skills`
#' or per-project) to make it available to Claude Code. See the README in that
#' directory for details.
#'
#' @return Character path to the installed `skills` directory.
#' @export
markermd_skills_path = function() {
  system.file("skills", package = "markermd")
}
