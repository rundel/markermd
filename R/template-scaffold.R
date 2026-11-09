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
#'   `n_table`, `n_div`, `n_other` and `n_content` (all non-heading blocks;
#'   HTML comments, which never render, are not counted anywhere). The
#'   resolved source file is attached as the `"source_file"` attribute, and
#'   the assignment's other files (everything else in the repository, which a
#'   template question can cover with `file:`) as the `"files"` attribute: a
#'   data frame with `path` (relative to the repository root: `assignment_path`
#'   when it is a directory, else the enclosing git repository when the
#'   document sits at most two directories below it, else the document's
#'   directory), `size` (bytes), `filtered` (`TRUE` for hidden files, render
#'   output, lock files, project files and the README, which are seldom
#'   graded) and `reason`.
#' @export
#'
#' @examples
#' key = system.file("examples/test_assignment/hw3-key", package = "markermd")
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
      n_other = sum(!kinds %in% c("Heading", "HTML comment", content_kinds)),
      n_content = sum(!kinds %in% c("Heading", "HTML comment")),
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
  root = assignment_files_root(file, key_dir = if (dir.exists(assignment_path)) assignment_path)
  doc_rel = as.character(fs::path_rel(normalizePath(file, winslash = "/"), root))
  files = assignment_file_candidates(root, exclude = doc_rel)
  files$size = file.size(file.path(root, files$path))
  attr(out, "files") = files[c("path", "size", "filtered", "reason")]
  out
}

#' Path to the bundled markermd agent skills
#'
#' Returns the directory holding one agent's set of the skills distributed with
#' the package: the Claude Code set, which [init_project()] installs into
#' `<project>/.claude/skills/`, or the Codex set, installed into
#' `<project>/.agents/skills/`. Both follow the Agent Skills format (a
#' `SKILL.md` per skill) and share skill names, differing only in
#' agent-specific wording and metadata. The root of each set also holds that
#' agent's project instructions file (`CLAUDE.md` or `AGENTS.md`), which
#' [init_project()] copies to the project root when none exists there. Copy a
#' skill's sub-directory into the per-user directory (`~/.claude/skills/` or
#' `~/.agents/skills/`) to make it available outside a project. See the README
#' in the `skills` directory for details.
#'
#' @param agent Which set: `"claude"` (the default) or `"codex"`.
#' @return Character path to the installed skills directory for that agent.
#' @export
markermd_skills_path = function(agent = c("claude", "codex")) {
  agent = match.arg(agent)
  system.file("skills", agent, package = "markermd")
}
