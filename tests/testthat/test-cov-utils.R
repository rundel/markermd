# Coverage for misc internal utilities:
#   - register_artifact_resources()  (R/utils_artifacts.R)
#   - assignment_outline()           (R/template-scaffold.R)
#   - navigate_select()              (R/mod_mark_rubric.R)

# Writes a qmd document into a fresh temp directory and returns the directory.
cu_key_dir = function(lines) {
  d = normalizePath(tempfile("cu_key_"), winslash = "/", mustWork = FALSE)
  dir.create(d)
  writeLines(lines, file.path(d, "assignment.qmd"))
  d
}

# Writes an html artifact (name may contain a space) into a fresh temp dir and
# returns its absolute path.
cu_artifact_file = function(name) {
  d = normalizePath(tempfile("cu_art_"), winslash = "/", mustWork = FALSE)
  dir.create(d)
  path = file.path(d, name)
  writeLines("<html></html>", path)
  path
}


test_that("register_artifact_resources serves a space-named report and passes NA through", {
  html = cu_artifact_file("my report.html")
  paths = c(repoA = html, repoB = NA_character_)

  urls = markermd:::register_artifact_resources(paths)

  expect_named(urls, c("repoA", "repoB"))
  expect_match(
    urls[["repoA"]],
    "^markermd_artifact_[0-9a-f]{16}/.*%20.*\\.html$"
  )
  expect_true(is.na(urls[["repoB"]]))
})


test_that("register_artifact_resources uses a stable per-directory prefix", {
  html = cu_artifact_file("my report.html")
  paths = c(repoA = html, repoB = NA_character_)

  first = markermd:::register_artifact_resources(paths)
  second = markermd:::register_artifact_resources(paths)

  expect_identical(first, second)
})


test_that("assignment_outline counts a nested subsection and a list under a heading", {
  d = cu_key_dir(c(
    "---", "title: t", "---", "",
    "## Q1 {#q1}", "", "Some intro text.", "",
    "### Part {#part}", "", "- item a", "- item b", "", "More text.", ""
  ))

  out = assignment_outline(d)
  q1 = out[out$id == "q1", ]

  expect_equal(nrow(q1), 1L)
  expect_equal(q1$n_subsections, 1L)
  expect_equal(q1$n_list, 1L)
})


test_that("assignment_outline returns a typed empty frame when no anchors exist", {
  # Pandoc auto-assigns ids to every heading, so the only way to have no
  # anchorable sections is a document with no headings and no id'd divs.
  d = cu_key_dir(c(
    "---", "title: t", "---", "",
    "Just some text here.", "", "- a", "- b", "", "More text.", ""
  ))

  out = assignment_outline(d)

  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 13L)
  expect_identical(names(out), c(
    "type", "id", "level", "title", "n_subsections",
    "n_chunk", "n_code_block", "n_markdown", "n_list", "n_table",
    "n_div", "n_other", "n_content"
  ))
  expect_identical(out$type, character(0))
})


test_that("navigate_select wraps forward past the end to the first choice", {
  choices = c("a", "b", "c")
  expect_identical(markermd:::navigate_select("c", choices, 1), "a")
})


test_that("navigate_select wraps backward from the first choice to the last", {
  choices = c("a", "b", "c")
  expect_identical(markermd:::navigate_select("a", choices, -1), "c")
})


test_that("navigate_select returns NULL for degenerate or unmatched inputs", {
  choices = c("a", "b", "c")
  expect_null(markermd:::navigate_select("a", "a", 1))
  expect_null(markermd:::navigate_select(NULL, choices, 1))
  expect_null(markermd:::navigate_select("z", choices, 1))
})
