# The read-only code view (R/utils_code_view.R, inst/www/prism): the R
# helpers, the qmd grammar (run under V8, no browser) and the rendered view's
# line numbers, highlight band and in-place updates (in a browser).

cv_prism_dir = function() {
  dep = markermd:::code_view_dependency()
  system.file(dep$src$file, package = dep$package)
}

# A V8 context with Prism, every vendored component and the qmd grammar
# loaded, in the dependency's own order (the view script needs a DOM and is
# left out)
cv_prism_context = function() {
  dep = markermd:::code_view_dependency()
  ctx = V8::v8()
  ctx$eval("var window = this;")
  for (script in setdiff(dep$script, "markermd-code-view.js")) {
    ctx$source(file.path(cv_prism_dir(), script))
  }
  ctx
}

cv_highlight = function(ctx, text, language = "qmd") {
  ctx$assign("cv_src", text)
  ctx$get(paste0("Prism.highlight(cv_src, Prism.languages['", language, "'], '", language, "')"))
}

test_that("the code view is vendored and served without a CDN", {
  dep = markermd:::code_view_dependency()
  root = cv_prism_dir()
  expect_true(all(file.exists(file.path(root, dep$script))))
  expect_true(file.exists(file.path(root, dep$stylesheet)))
  expect_true(file.exists(file.path(root, "LICENSE")))
  expect_false(any(grepl("^https?:", c(dep$script, dep$stylesheet))))
  expect_false(dir.exists(system.file("www", "monaco", package = "markermd")))
})

test_that("file and engine languages map to loaded Prism components", {
  files = c(
    "a.r", "a.R", "a.py", "a.qmd", "a.Rmd", "a.md", "a.sh", "a.sql", "a.json", "a.yml",
    "a.toml", "a.ini", "a.js", "a.ts", "a.css", "a.html", "a.xml", "a.c", "a.cpp",
    "a.java", "a.jl", "a.rs", "a.go", "a.tex", "Dockerfile", "Makefile"
  )
  langs = vapply(files, markermd:::file_code_language, character(1))
  expect_equal(unname(langs[c("a.qmd", "a.Rmd")]), c("qmd", "qmd"))
  expect_equal(markermd:::file_code_language("notes.txt"), "none")

  engines = vapply(
    c("r", "python", "sql", "bash", "sh", "js", "ojs", "css", "html", "yaml", "json", "julia"),
    markermd:::code_language_for_engine, character(1)
  )
  expect_equal(markermd:::code_language_for_engine("stan"), "qmd")

  known = c(markermd:::prism_components(), "qmd")
  expect_true(all(c(langs, engines) %in% known))
})

test_that("code_view_ui escapes the text and carries the language and ranges", {
  html = as.character(markermd:::code_view_ui(
    "v", c("x <- 1", "<b>&</b>"), "r",
    highlight_ranges = list(list(start = 2, end = 3)), font_size = 11, class = "h-100"
  ))
  expect_match(html, "id=\"v\"", fixed = TRUE)
  expect_match(html, "markermd-code-view border rounded h-100", fixed = TRUE)
  expect_match(html, "data-language=\"r\"", fixed = TRUE)
  expect_match(html, "--mm-code-size: 11px;", fixed = TRUE)
  expect_match(html, "<pre><code>x &lt;- 1\n&lt;b&gt;&amp;&lt;/b&gt;</code></pre>", fixed = TRUE)
  expect_equal(
    jsonlite::fromJSON(markermd:::code_view_ranges_json(list(list(start = 2, end = 3))), simplifyVector = FALSE),
    list(list(start = 2L, end = 3L))
  )
  expect_equal(markermd:::code_view_ranges_json(NULL), "[]")
  # No whitespace between <pre> and <code>, which would render as a blank
  # first line and push every highlight down
  expect_false(grepl("<pre>\\s+<code>", html))
})

test_that("the qmd grammar highlights chunks, options, inline code and Quarto syntax", {
  testthat::skip_if_not_installed("V8")
  ctx = cv_prism_context()

  doc = paste(c(
    "---",
    "title: \"HW\"",
    "---",
    "",
    "```{r setup, include=FALSE, fig.cap=\"A, b\"}",
    "library(dplyr)",
    "```",
    "",
    "## Question 1 {#q1 .unnumbered}",
    "",
    "<!-- placeholder -->",
    "",
    "Rows: `r nrow(df)`, cols: `{r} ncol(df)`, py: `{python} 1 + 1`.",
    "",
    "::: {#answer .callout-note}",
    "Text.",
    ":::",
    "",
    "```{python}",
    "#| label: py",
    "#| echo: true",
    "import numpy as np",
    "```",
    "",
    "```{stan}",
    "model { }",
    "```",
    "",
    "```r",
    "plain(1)",
    "```"
  ), collapse = "\n")
  html = cv_highlight(ctx, doc)

  has = function(pattern) grepl(pattern, html, fixed = TRUE)
  # YAML front matter and HTML comments, from markdown and markup
  expect_true(has("front-matter-block"))
  expect_true(has("<span class=\"token comment\">&lt;!-- placeholder --></span>"))
  # A knitr-style header: engine, option names, logicals and strings
  expect_true(has("token chunk-r chunk"))
  expect_true(has("<span class=\"token keyword\">r</span> setup"))
  expect_true(has("<span class=\"token attr-name\">include</span>"))
  expect_true(has("<span class=\"token boolean\">FALSE</span>"))
  expect_true(has("<span class=\"token string\">\"A, b\"</span>"))
  # The body is highlighted as R
  expect_true(has("token chunk-code language-r"))
  # A Python chunk, its #| options as YAML and its body as Python
  expect_true(has("token chunk-python chunk"))
  expect_true(has("<span class=\"token option-marker comment\">#|</span>"))
  expect_true(has("<span class=\"token key atrule\">label</span>"))
  expect_true(has("<span class=\"token keyword\">import</span>"))
  # Inline R in both forms, and another engine's inline code
  expect_true(has("<span class=\"token inline-engine keyword\">r</span>"))
  expect_true(has("<span class=\"token inline-engine keyword\">{r}</span>"))
  expect_true(has("<span class=\"token inline-engine keyword\">{python}</span>"))
  expect_true(has("token inline-code language-r"))
  # Div fences and heading attribute blocks
  expect_true(has("token div-fence"))
  expect_true(has("<span class=\"token selector\">#answer</span>"))
  expect_true(has("<span class=\"token class-name\">.callout-note</span>"))
  expect_true(has("<span class=\"token selector\">#q1</span>"))
  # An unknown engine is a plain chunk; a plain ```r fence is R too
  expect_true(has("<span class=\"token keyword\">stan</span>"))
  expect_match(html, "plain<span class=\"token punctuation\">\\(</span><span class=\"token number\">1</span>")

  # The rmd alias is the same grammar
  expect_identical(cv_highlight(ctx, doc, "rmd"), html)
})

test_that("the code view numbers lines, lays the highlight exactly on them and updates it in place", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_on_cran()

  lines = c(
    "```{r}", "#| label: setup", "library(dplyr)", "```",
    paste("Line", 5:80, "of the document.")
  )
  # A Bootstrap 5 page like the apps' (the view's height comes from h-100)
  app_obj = shiny::shinyApp(
    ui = bslib::page_fluid(
      markermd:::code_view_dependency(),
      shiny::actionButton("move", "Move"),
      shiny::div(
        style = "height: 300px;",
        markermd:::code_view_ui(
          "cv", lines, "qmd",
          highlight_ranges = list(list(start = 40, end = 44)), class = "h-100"
        )
      )
    ),
    server = function(input, output, session) {
      shiny::observe(
        markermd:::update_code_view_highlight(session, "cv", list(list(start = 5, end = 6)))
      ) |>
        shiny::bindEvent(input$move)
    }
  )
  app = new_app_driver(app_obj, name = "code_view")
  app$wait_for_js("document.querySelector('#cv[data-rendered]') !== null")

  # One numbered line per source line, highlighted with Prism
  expect_equal(app$get_js("document.querySelectorAll('#cv .mm-line').length"), length(lines))
  expect_equal(app$get_js("document.querySelector('#cv .mm-line').getAttribute('data-n')"), "1")
  expect_true(app$get_js("!!document.querySelector('#cv .token.chunk-header')"))

  # The band covers exactly lines 40 to 44 and its top is where line 40
  # starts: the pre's top padding plus 39 line heights
  expect_equal(
    unlist(app$get_js("Array.from(document.querySelectorAll('#cv .mm-hl')).map(e => Number(e.dataset.n))")),
    40:44
  )
  offset = app$get_js("
    (function() {
      var view = document.getElementById('cv');
      var pre = view.querySelector('pre');
      var band = view.querySelector('.mm-hl');
      var first = view.querySelector('.mm-line');
      var lh = first.getBoundingClientRect().height;
      var pad = parseFloat(getComputedStyle(pre).paddingTop);
      var expected = pre.getBoundingClientRect().top + pad + 39 * lh;
      return Math.abs(band.getBoundingClientRect().top - expected);
    })()
  ")
  expect_lte(offset, 1)

  # The first highlighted line is scrolled into view
  expect_gt(app$get_js("document.getElementById('cv').scrollTop"), 0)
  expect_true(app$get_js("
    (function() {
      var view = document.getElementById('cv').getBoundingClientRect();
      var band = document.querySelector('#cv .mm-hl').getBoundingClientRect();
      return band.top >= view.top && band.bottom <= view.bottom;
    })()
  "))

  # A highlight update moves the band without re-rendering the view
  app$click("move")
  app$wait_for_js("document.querySelector('#cv .mm-line[data-n=\"5\"]').classList.contains('mm-hl')")
  expect_equal(
    unlist(app$get_js("Array.from(document.querySelectorAll('#cv .mm-hl')).map(e => Number(e.dataset.n))")),
    5:6
  )
})
