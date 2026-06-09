test_that("normalize_knitr_chunks rewrites knitr-style headers to #| form", {
  lines = c(
    "```{r my-label, echo=FALSE}",
    "plot(1)",
    "```"
  )

  expect_equal(
    markermd:::normalize_knitr_chunks(lines),
    c(
      "```{r}",
      "#| label: my-label",
      "#| echo: false",
      "plot(1)",
      "```"
    )
  )
})


test_that("normalize_knitr_chunks leaves non-knitr fences alone", {
  bare = c("```{r}", "1 + 1", "```")
  expect_equal(markermd:::normalize_knitr_chunks(bare), bare)

  pandoc_attr = c("```{.r .numberLines}", "x", "```")
  expect_equal(markermd:::normalize_knitr_chunks(pandoc_attr), pandoc_attr)

  already_quarto = c("```{r}", "#| label: foo", "1", "```")
  expect_equal(markermd:::normalize_knitr_chunks(already_quarto), already_quarto)

  plain = c("```", "not a chunk, just text", "```")
  expect_equal(markermd:::normalize_knitr_chunks(plain), plain)
})


test_that("normalize_knitr_chunks does not rewrite fence-like lines inside a block", {
  lines = c(
    "````",
    "```{r inner, echo=TRUE}",
    "````",
    "outside"
  )
  expect_equal(markermd:::normalize_knitr_chunks(lines), lines)
})


test_that("normalize_knitr_chunks preserves indentation and tilde fences", {
  indented = c(
    "  ```{r lab, eval=FALSE}",
    "  x",
    "  ```"
  )
  expect_equal(
    markermd:::normalize_knitr_chunks(indented),
    c(
      "  ```{r}",
      "  #| label: lab",
      "  #| eval: false",
      "  x",
      "  ```"
    )
  )

  tildes = c("~~~{r lab, eval=FALSE}", "x", "~~~")
  expect_equal(
    markermd:::normalize_knitr_chunks(tildes),
    c("~~~{r}", "#| label: lab", "#| eval: false", "x", "~~~")
  )
})


test_that("convert_chunk_header maps knitr options to Quarto form", {
  expect_equal(
    markermd:::convert_chunk_header("r my-label, echo=FALSE"),
    list(engine = "r", options = c("label: my-label", "echo: false"))
  )

  # first bare token is the label; key=value pairs convert, dots become dashes
  expect_equal(
    markermd:::convert_chunk_header("r lab, fig.width=5, eval=T"),
    list(engine = "r", options = c("label: lab", "fig-width: 5", "eval: true"))
  )

  # quoted values keep embedded commas intact
  expect_equal(
    markermd:::convert_chunk_header('r lab, fig.cap="a, b"'),
    list(engine = "r", options = c("label: lab", 'fig-cap: "a, b"'))
  )

  # other engines work; options without a label too
  expect_equal(
    markermd:::convert_chunk_header("python setup"),
    list(engine = "python", options = "label: setup")
  )
  expect_equal(
    markermd:::convert_chunk_header("r echo=TRUE"),
    list(engine = "r", options = "echo: true")
  )

  # nothing to rewrite
  expect_null(markermd:::convert_chunk_header("r"))
  expect_null(markermd:::convert_chunk_header(".r .numberLines"))
})


test_that("split_top_level ignores separators inside quotes and brackets", {
  expect_equal(markermd:::split_top_level("a, b", ","), c("a", " b"))
  expect_equal(markermd:::split_top_level('x="a,b", y=2', ","), c('x="a,b"', " y=2"))
  expect_equal(markermd:::split_top_level("x=c(1,2), y=3", ","), c("x=c(1,2)", " y=3"))
  expect_equal(markermd:::split_top_level("x=l[1,2], y={a,b}", ","), c("x=l[1,2]", " y={a,b}"))
  expect_equal(markermd:::split_top_level("single", ","), "single")
  expect_equal(markermd:::split_top_level("a,", ","), c("a", ""))
})


test_that("parse_assignment_document normalizes knitr headers and surfaces missing files", {
  expect_error(
    markermd::parse_assignment_document(tempfile(fileext = ".qmd")),
    "does not exist"
  )

  qmd = tempfile(fileext = ".qmd")
  writeLines(c(
    "# Title",
    "",
    "```{r labelled-chunk, echo=FALSE}",
    "1 + 1",
    "```"
  ), qmd)

  ast = markermd::parse_assignment_document(qmd)
  expect_gt(length(ast@blocks@content), 0)

  kinds = vapply(ast@blocks@content, markermd:::q2r_node_kind, character(1))
  chunk = ast@blocks@content[kinds == "Chunk"][[1]]
  expect_equal(q2r::cell_label(chunk), "labelled-chunk")
})
