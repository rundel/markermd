# Coverage for find_repo_assignment()'s fallback when no basename is recorded:
# a root-level document must win over one nested in a subdirectory (recursive
# listing would otherwise sort the subdirectory file first).

fra_repo = function() {
  d = normalizePath(tempfile("repo_"), winslash = "/", mustWork = FALSE)
  dir.create(file.path(d, "data"), recursive = TRUE)
  writeLines("x", file.path(d, "hw1.qmd"))
  writeLines("y", file.path(d, "data", "analysis.qmd"))
  d
}

test_that("a recorded basename is matched even in a subdirectory", {
  d = fra_repo()
  expect_equal(basename(markermd:::find_repo_assignment(d, "analysis.qmd", "\\.qmd$")), "analysis.qmd")
})

test_that("without a basename, a root-level document is preferred over a subdirectory one", {
  d = fra_repo()
  expect_equal(basename(markermd:::find_repo_assignment(d, NULL, "\\.qmd$")), "hw1.qmd")
})

test_that("find_repo_assignment returns NA when no document matches", {
  d = normalizePath(tempfile("repo_"), winslash = "/", mustWork = FALSE)
  dir.create(d, recursive = TRUE)
  expect_true(is.na(markermd:::find_repo_assignment(d, NULL, "\\.qmd$")))
})
