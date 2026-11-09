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
#' @param source Anything [template()] accepts: an initialized markermd
#'   project directory (outlines the key repository's document), a path to an
#'   assignment file (`.qmd`/`.Rmd`), a directory containing one, a saved
#'   template (`.yaml`/`.yml`), a `markermd_template` object, or a GitHub
#'   `<owner>/<repo>` (cloned into `local_dir`).
#' @param filename Glob used to find the assignment when `source` is a
#'   directory, a project or a repository. Defaults to any `.Rmd`/`.qmd` file.
#' @param template Optional `markermd_template`. When given, a heading's counts
#'   stop at the first heading one of the template's questions selects (other
#'   than the row's own), exactly as grading ends that question's section, so
#'   an outline taken against a template shows what each question would be
#'   graded on. When `NULL` (the default) the source's own template is used if
#'   it has one (a template object or file, or a project's stored template);
#'   otherwise a section runs to the next heading of the same or higher level.
#' @param local_dir Directory to clone into when `source` is a GitHub
#'   repository.
#' @param assignment Optional assignment document for a template `source`
#'   whose `source.path` cannot be located.
#'
#' @return A data frame with one row per anchorable section and columns `type`
#'   (`"heading"` or `"div"`), `id`, `level`, `title`, `n_subsections`, and the
#'   content counts `n_chunk`, `n_code_block`, `n_markdown`, `n_list`,
#'   `n_table`, `n_div`, `n_other` and `n_content` (all non-heading blocks;
#'   HTML comments, which never render, are not counted anywhere). The
#'   resolved source file is attached as the `"source_file"` attribute (`NULL`
#'   for a template object whose document is unknown), and
#'   the assignment's other files (everything else in the repository, which a
#'   template question can cover with `file:`) as the `"files"` attribute: a
#'   data frame with `path` (relative to the repository root: the project's
#'   key repository, `source` when it is a directory, the cloned repository,
#'   else the enclosing git repository when the document sits at most two
#'   directories below it, else the document's directory; empty when the
#'   document is unknown), `size` (bytes), `filtered` (`TRUE` for hidden files, render
#'   output, lock files, project files and the README, which are seldom
#'   graded) and `reason`.
#' @export
#'
#' @examples
#' key = system.file("examples/test_assignment/hw3-key", package = "markermd")
#' assignment_outline(key)
assignment_outline = function(source, filename = "*.[Rq]md", template = NULL, local_dir = NULL, assignment = NULL) {
  if (!is.null(template) && !S7::S7_inherits(template, markermd_template)) {
    cli::cli_abort("{.arg template} must be a {.cls markermd_template} or {.code NULL}.")
  }
  resolved = resolve_assignment_source(source, local_dir = local_dir, filename = filename, assignment = assignment)
  if (is.null(template)) template = resolved$template
  template_ids = if (is.null(template)) {
    character(0)
  } else {
    unique(unlist(lapply(template@questions, function(q) {
      if (is_document_question(q) || is_file_question(q)) character(0) else q@selected_nodes@node_ids
    })))
  }

  file = resolved$source_path
  ast = resolved$ast

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
    blocks = get_question_ast(ast, question, setdiff(template_ids, id))@blocks@content
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
  files = if (is.null(file)) {
    data.frame(path = character(0), size = numeric(0), filtered = logical(0), reason = character(0), stringsAsFactors = FALSE)
  } else {
    root = assignment_files_root(file, key_dir = resolved$key_dir)
    doc_rel = as.character(fs::path_rel(normalizePath(file, winslash = "/"), root))
    listed = assignment_file_candidates(root, exclude = doc_rel)
    listed$size = file.size(file.path(root, listed$path))
    listed
  }
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
