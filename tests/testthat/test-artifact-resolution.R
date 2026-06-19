# Coverage for resolve_repo_artifacts()'s three-tier match precedence and the
# prefix-boundary fix (repo "hw1" must not capture "hw10.html").

art_project = function(artifacts) {
  d = normalizePath(tempfile("art_"), winslash = "/", mustWork = FALSE)
  for (a in artifacts) dir.create(file.path(d, a), recursive = TRUE)
  markermd_project(root = d, artifacts = artifacts)
}

art_write = function(proj, rel) {
  path = file.path(proj@root, rel)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines("<html></html>", path)
  path
}

test_that("an exact <repo>.html is preferred over a <repo>/ subdirectory report", {
  proj = art_project("html")
  art_write(proj, "html/repoA.html")
  art_write(proj, "html/repoA/index.html")

  hit = resolve_repo_artifacts(proj, "repoA")[["repoA"]]
  expect_equal(basename(hit), "repoA.html")
})

test_that("a <repo>/ subdirectory report is used when there is no exact file", {
  proj = art_project("html")
  art_write(proj, "html/repoB/report.html")

  hit = resolve_repo_artifacts(proj, "repoB")[["repoB"]]
  expect_equal(basename(hit), "report.html")
})

test_that("a decorated <repo>-*.html name is matched as a prefix fallback", {
  proj = art_project("html")
  art_write(proj, "html/repoC-report.html")

  hit = resolve_repo_artifacts(proj, "repoC")[["repoC"]]
  expect_equal(basename(hit), "repoC-report.html")
})

test_that("a prefix match does not capture a longer repo name sharing the prefix", {
  proj = art_project("html")
  art_write(proj, "html/hw10.html")

  expect_true(is.na(resolve_repo_artifacts(proj, "hw1")[["hw1"]]))
  expect_equal(basename(resolve_repo_artifacts(proj, "hw10")[["hw10"]]), "hw10.html")
})

test_that("artifact directories are searched in config order, first match wins", {
  proj = art_project(c("html1", "html2"))
  art_write(proj, "html1/repoX.html")
  art_write(proj, "html2/repoX.html")

  hit = resolve_repo_artifacts(proj, "repoX")[["repoX"]]
  expect_match(hit, "html1/repoX.html$")
})

test_that("an unmatched repo resolves to NA", {
  proj = art_project("html")
  expect_true(is.na(resolve_repo_artifacts(proj, "missing")[["missing"]]))
})
