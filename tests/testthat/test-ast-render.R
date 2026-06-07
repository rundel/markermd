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
