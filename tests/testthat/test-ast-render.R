# Unit tests for the shared AST render core (utils_tree.R). These exercise
# render_ast_tree() directly with ns = identity, so no browser is needed.

render_html = function(qmd, opts) {
  ast = q2r::parse_qmd(qmd, quiet = TRUE)
  tree_items = build_ast_tree_structure(ast)
  as.character(render_ast_tree(tree_items, identity, opts))
}

sample_qmd = "# Section\n\nsome paragraph text\n\n```{r}\n1 + 1\n```\n\n## Sub\n\nmore text\n"

test_that("interactive mode draws selection controls for selectable nodes only", {
  html = render_html(sample_qmd, ast_render_opts(mode = "interactive"))

  expect_match(html, "class=\"ast-tree\"", fixed = TRUE)

  # Headings are selectable: both the circle and the text button are emitted.
  expect_match(html, "select_children_1", fixed = TRUE)
  expect_match(html, "select_1\"", fixed = TRUE)

  # The paragraph (index 2) is not selectable, so it has no select control.
  expect_false(grepl("select_children_2", html, fixed = TRUE))
  expect_false(grepl("\"select_2\"", html, fixed = TRUE))
})

test_that("preview buttons are drawn for content nodes but not headings", {
  html = render_html(sample_qmd, ast_render_opts(mode = "interactive"))

  # Paragraph (2) and chunk (3) get preview buttons; headings (1, 4) do not.
  expect_match(html, "preview_2", fixed = TRUE)
  expect_match(html, "preview_3", fixed = TRUE)
  expect_false(grepl("preview_1", html, fixed = TRUE))
  expect_false(grepl("preview_4", html, fixed = TRUE))
})

test_that("id_prefix namespaces the preview button ids", {
  plain = render_html(sample_qmd, ast_render_opts(mode = "readonly"))
  expect_match(plain, "preview_2", fixed = TRUE)
  expect_false(grepl("preview_q_Q1_2", plain, fixed = TRUE))

  prefixed = render_html(sample_qmd, ast_render_opts(mode = "readonly", id_prefix = "q_Q1"))
  expect_match(prefixed, "preview_q_Q1_2", fixed = TRUE)
})

test_that("drop_root with start_depth = 1 drops the document root (mark side)", {
  with_root = render_html(sample_qmd, ast_render_opts(mode = "readonly"))
  expect_match(with_root, ">Document<", fixed = TRUE)
  expect_match(with_root, "class=\"ast-tree-readonly\"", fixed = TRUE)

  dropped = render_html(
    sample_qmd,
    ast_render_opts(mode = "readonly", id_prefix = "q_Q1", start_depth = 1, drop_root = TRUE)
  )
  expect_false(grepl(">Document<", dropped, fixed = TRUE))
  expect_match(dropped, "class=\"ast-tree-readonly-nested\"", fixed = TRUE)
  # Content is still rendered, with prefixed preview ids.
  expect_match(dropped, "Heading", fixed = TRUE)
  expect_match(dropped, "preview_q_Q1_2", fixed = TRUE)
})

test_that("unselected node labels carry no trailing class whitespace", {
  html = render_html(sample_qmd, ast_render_opts(mode = "interactive"))
  expect_false(grepl("tree-node-description \"", html, fixed = TRUE))
  expect_false(grepl("tree-node-description-btn \"", html, fixed = TRUE))
})

test_that("ast_render_opts validates its arguments", {
  expect_error(ast_render_opts(mode = "interactive", drop_root = TRUE))
  expect_error(ast_render_opts(mode = "interactive", start_depth = 1))
  expect_error(ast_render_opts(mode = "readonly", start_depth = -1))
  expect_silent(ast_render_opts(mode = "readonly", start_depth = 1, drop_root = TRUE))
})

test_that("css class names are preserved across modes", {
  expect_match(render_html(sample_qmd, ast_render_opts(mode = "interactive")), "ast-tree", fixed = TRUE)
  expect_match(render_html(sample_qmd, ast_render_opts(mode = "readonly")), "ast-tree-readonly", fixed = TRUE)
  expect_match(
    render_html(sample_qmd, ast_render_opts(mode = "readonly", start_depth = 1, drop_root = TRUE)),
    "ast-tree-readonly-nested",
    fixed = TRUE
  )
})

test_that("the interactive tree reports clicks through one delegated input", {
  html = render_html(sample_qmd, ast_render_opts(mode = "interactive"))

  expect_match(html, "data-tree-input=\"tree_event\"", fixed = TRUE)
  expect_match(html, "data-tree-index=\"1\" data-tree-selectable=\"1\"", fixed = TRUE)
  expect_match(html, "data-tree-index=\"2\" data-tree-selectable=\"0\"", fixed = TRUE)
  expect_match(html, "data-tree-action=\"toggle\"", fixed = TRUE)
  expect_match(html, "data-tree-action=\"preview\"", fixed = TRUE)
  # nothing in the interactive tree binds an input of its own
  expect_false(grepl("action-button", html, fixed = TRUE))

  readonly = render_html(sample_qmd, ast_render_opts(mode = "readonly"))
  expect_false(grepl("data-tree-input", readonly, fixed = TRUE))
  expect_match(readonly, "action-button", fixed = TRUE)
})

test_that("tree_selection_state locks selectable nodes under a direct selection", {
  ast = q2r::parse_qmd(sample_qmd, quiet = TRUE)
  items = build_ast_tree_structure(ast)

  state = tree_selection_state(items, 1L, integer(0))
  expect_equal(state$direct, 1L)
  expect_equal(state$selected, 1:5)
  # the sub-heading (4) sits under the selected section heading
  expect_equal(state$locked, 4L)
  expect_equal(state$filtered, integer(0))

  # the document root (a whole-document question) never locks headings
  expect_equal(tree_selection_state(items, 0L, integer(0))$locked, integer(0))

  # filtered nodes are kept only among the selected ones
  expect_equal(tree_selection_state(items, 4L, c(2L, 5L))$filtered, 5L)
})

test_that("a locked heading renders as an indicator with the selection highlight", {
  html = render_html(sample_qmd, ast_render_opts(mode = "interactive", selected = 1L))

  expect_match(html, "tree-node-content tree-locked\" data-tree-index=\"4\"", fixed = TRUE)
  expect_match(html, "btn btn-default tree-toggle-btn tree-marker-square selected", fixed = TRUE)
  expect_match(html, "btn btn-default tree-toggle-btn selected\" title=\"Toggle this node", fixed = TRUE)
  expect_match(html, "Selected via parent selection", fixed = TRUE)
})

test_that("tree_node_view matches the class order the in-place patch builds", {
  view = tree_node_view(TRUE, TRUE, FALSE, TRUE, TRUE, "btn tree-toggle-btn", "label")
  expect_false(view$open)
  expect_equal(view$marker_class, "btn tree-toggle-btn tree-marker-square selected filtered")
  expect_equal(view$label_class, "label selected filtered")
  expect_equal(view$icon, "ellipsis")
  expect_equal(view$title, "Excluded by question filters")

  open = tree_node_view(TRUE, TRUE, TRUE, FALSE, FALSE, "m", "l")
  expect_true(open$open)
  expect_equal(open$marker_class, "m selected")
  expect_equal(open$icon, "check")
  expect_equal(open$title, "Toggle this node and its children")
})
