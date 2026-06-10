test_that("q2r_node_kind classifies blocks into the tree vocabulary", {
  ast = q2r::parse_qmd("# H\n\npara\n\n```{r}\n1\n```\n\n```\nplain\n```\n\n- a\n- b\n\n> quote\n")
  kinds = vapply(ast@blocks@content, q2r_node_kind, character(1))

  expect_equal(
    kinds,
    c("Heading", "Markdown", "Chunk", "Code block", "Bullet list", "Block quote")
  )
})

test_that("q2r_node_label leading word matches q2r_node_kind", {
  ast = q2r::parse_qmd("# H\n\npara\n\n```{r}\n1\n```\n\n- a\n\n> q\n")

  for (node in ast@blocks@content) {
    kind = q2r_node_kind(node)
    label = q2r_node_label(node)
    expect_true(
      startsWith(label, kind),
      info = paste0("label '", label, "' should start with kind '", kind, "'")
    )
  }
})

test_that("q2r_node_label surfaces a div's id and classes", {
  ast = q2r::parse_qmd("::: {#q1-answer .answer}\nbody\n:::\n\n::: {.note}\nx\n:::\n")
  divs = Filter(function(n) S7::S7_inherits(n, q2r::pandoc_div), ast@blocks@content)

  expect_equal(q2r_node_label(divs[[1]]), "Div (.answer) (#q1-answer)")
  expect_equal(q2r_node_label(divs[[2]]), "Div (.note)")
})

test_that("q2r_node_label appends (#id) to any node with a standard attr id", {
  ast = q2r::parse_qmd("# Question 1 {#q1}\n\nplain text\n\n## Auto Heading\n")
  nodes = ast@blocks@content

  expect_equal(q2r_node_label(nodes[[1]]), "Heading [h1] - Question 1 (#q1)")
  expect_equal(q2r_node_label(nodes[[2]]), "Markdown")
  expect_equal(q2r_node_label(nodes[[3]]), "Heading [h2] - Auto Heading (#auto-heading)")
})

test_that("rule node types are the friendly kinds, not raw q2r classes", {
  types = get_allowed_node_types()

  expect_true("Any node" %in% types)
  expect_true(all(c("Heading", "Markdown", "Chunk", "Code block") %in% types))
  expect_false(any(grepl("pandoc_", types, fixed = TRUE)))
})

test_that("assert_template_compatible rejects an older format version", {
  ast = q2r::parse_qmd("# H\n\npara\n")
  tmpl = markermd_template(original_ast = ast, questions = list())

  expect_silent(assert_template_compatible(tmpl))

  old = tmpl
  old@metadata = markermd_metadata(version = "2.0")
  expect_error(assert_template_compatible(old), "recreate the template")
})
