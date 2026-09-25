# Coverage for resolve_repo_artifacts(): permissive discovery (any single HTML
# file belonging to the repo is accepted), the ranking heuristic used when a
# repo has several candidates (with its consolidated warning), the
# prefix-boundary rule (repo "hw1" must not capture "hw10.html"), and the
# ghclass artifact layouts ("<repo>_<artifact>/" and "<repo>/<artifact>/").

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

# resolve_repo_artifacts() for repos that are expected to be unambiguous: any
# warning is a test failure
art_resolve = function(proj, repos) {
  expect_no_warning(hits <- resolve_repo_artifacts(proj, repos))
  hits
}

# --- single candidates -------------------------------------------------------

test_that("a single <repo>.html is accepted", {
  proj = art_project("html")
  art_write(proj, "html/repoA.html")

  hit = art_resolve(proj, "repoA")[["repoA"]]
  expect_equal(basename(hit), "repoA.html")
})

test_that("a single report inside <repo>/ is accepted", {
  proj = art_project("html")
  art_write(proj, "html/repoB/report.html")

  expect_equal(basename(art_resolve(proj, "repoB")[["repoB"]]), "report.html")
})

test_that("a single decorated <repo>-*.html name is accepted", {
  proj = art_project("html")
  art_write(proj, "html/repoC-report.html")

  expect_equal(basename(art_resolve(proj, "repoC")[["repoC"]]), "repoC-report.html")
})

test_that("a single HTML file anywhere under a repo-named directory is accepted", {
  proj = art_project("html")
  art_write(proj, "html/repoH/some/deep/tree/whatever.html")

  hit = art_resolve(proj, "repoH")[["repoH"]]
  expect_match(hit, "html/repoH/some/deep/tree/whatever.html$")
})

test_that("a ghclass '<repo>_<artifact>/' directory (nest = FALSE) is searched", {
  proj = art_project("html")
  art_write(proj, "html/hw01_team01_hw1-html/hw1.html")
  art_write(proj, "html/hw01_team10_hw1-html/hw1.html")

  hits = art_resolve(proj, c("hw01_team01", "hw01_team10"))
  expect_match(hits[["hw01_team01"]], "html/hw01_team01_hw1-html/hw1.html$")
  expect_match(hits[["hw01_team10"]], "html/hw01_team10_hw1-html/hw1.html$")

  # the delimiter boundary applies to directories too
  expect_true(is.na(art_resolve(proj, "hw01_team1")[["hw01_team1"]]))
})

test_that("a ghclass '<repo>/<artifact>/' directory (nest = TRUE) is searched", {
  proj = art_project("html")
  art_write(proj, "html/repoD/hw1-html/hw1.html")

  expect_match(art_resolve(proj, "repoD")[["repoD"]], "html/repoD/hw1-html/hw1.html$")
})

test_that("a prefix match does not capture a longer repo name sharing the prefix", {
  proj = art_project("html")
  art_write(proj, "html/hw10.html")

  expect_true(is.na(art_resolve(proj, "hw1")[["hw1"]]))
  expect_equal(basename(art_resolve(proj, "hw10")[["hw10"]]), "hw10.html")
})

test_that("an unmatched repo resolves to NA with no candidates", {
  proj = art_project("html")
  hits = art_resolve(proj, "missing")
  expect_true(is.na(hits[["missing"]]))
  expect_length(attr(hits, "candidates")[["missing"]], 0)
})

test_that("files that merely share the repo name's letters are not candidates", {
  proj = art_project("html")
  art_write(proj, "html/other.html")
  art_write(proj, "html/repoZ.html")
  art_write(proj, "html/repoZebra/index.html")

  hits = art_resolve(proj, "repoZ")
  expect_equal(basename(hits[["repoZ"]]), "repoZ.html")
  expect_length(attr(hits, "candidates")[["repoZ"]], 1)
})

# --- several candidates: heuristic + warning ---------------------------------

test_that("an exact <repo>.html is preferred over a <repo>/ subdirectory report", {
  proj = art_project("html")
  art_write(proj, "html/repoA.html")
  art_write(proj, "html/repoA/index.html")

  expect_warning(hits <- resolve_repo_artifacts(proj, "repoA"), "1 repository")
  expect_equal(basename(hits[["repoA"]]), "repoA.html")
  expect_equal(basename(attr(hits, "candidates")[["repoA"]]), c("repoA.html", "index.html"))
})

test_that("a report directly inside <repo>/ beats a decorated top-level file", {
  proj = art_project("html")
  art_write(proj, "html/repoB-report.html")
  art_write(proj, "html/repoB/report.html")

  expect_warning(hits <- resolve_repo_artifacts(proj, "repoB"))
  expect_match(hits[["repoB"]], "html/repoB/report.html$")
})

test_that("a top-level decorated file beats a nested directory report", {
  proj = art_project("html")
  art_write(proj, "html/repoG-report.html")
  art_write(proj, "html/repoG_hw1-html/hw1.html")

  expect_warning(hits <- resolve_repo_artifacts(proj, "repoG"))
  expect_equal(basename(hits[["repoG"]]), "repoG-report.html")
})

test_that("an exact '<repo>/' directory beats decorated siblings", {
  proj = art_project("html")
  art_write(proj, "html/repoF/nested/report.html")
  art_write(proj, "html/repoF_html/report.html")

  expect_warning(hits <- resolve_repo_artifacts(proj, "repoF"))
  expect_match(hits[["repoF"]], "html/repoF/nested/report.html$")
})

test_that("the shallowest report under a repo directory wins", {
  proj = art_project("html")
  art_write(proj, "html/repoE_hw1-html/extra/other.html")
  art_write(proj, "html/repoE_hw1-html/hw1.html")

  expect_warning(hits <- resolve_repo_artifacts(proj, "repoE"))
  expect_match(hits[["repoE"]], "html/repoE_hw1-html/hw1.html$")
})

test_that("files under rendering resource directories rank last even when shallower", {
  proj = art_project("html")
  art_write(proj, "html/repoR_hw1-html/hw1_files/libs/widget.html")
  art_write(proj, "html/repoR_hw1-html/libs/a.html")
  art_write(proj, "html/repoR_hw1-html/out/deeper/hw1.html")

  expect_warning(hits <- resolve_repo_artifacts(proj, "repoR"))
  expect_match(hits[["repoR"]], "html/repoR_hw1-html/out/deeper/hw1.html$")
})

test_that("artifact directories are searched in config order, first match wins", {
  proj = art_project(c("html1", "html2"))
  art_write(proj, "html1/repoX.html")
  art_write(proj, "html2/repoX.html")

  expect_warning(hits <- resolve_repo_artifacts(proj, "repoX"))
  expect_match(hits[["repoX"]], "html1/repoX.html$")
})

test_that("one consolidated warning names each ambiguous repo and its choice", {
  proj = art_project("html")
  art_write(proj, "html/teamA_hw1-html/hw1.html")
  art_write(proj, "html/teamA_hw1-html/scratch.html")
  art_write(proj, "html/teamB_hw1-html/hw1.html")
  art_write(proj, "html/teamB_hw1-html/old/hw1.html")
  art_write(proj, "html/teamC_hw1-html/hw1.html")

  w = testthat::capture_warnings(hits <- resolve_repo_artifacts(proj, c("teamA", "teamB", "teamC")))
  expect_length(w, 1)
  expect_match(w, "2 repositories")
  expect_match(w, "teamA")
  expect_match(w, "teamB")
  expect_no_match(w, "teamC")
  expect_match(w, "1 other HTML file found")

  expect_match(hits[["teamA"]], "teamA_hw1-html/hw1.html$")
  expect_match(hits[["teamB"]], "teamB_hw1-html/hw1.html$")
  expect_match(hits[["teamC"]], "teamC_hw1-html/hw1.html$")

  candidates = attr(hits, "candidates")
  expect_equal(lengths(candidates)[c("teamA", "teamB", "teamC")], c(teamA = 2L, teamB = 2L, teamC = 1L))
  expect_equal(basename(candidates[["teamA"]]), c("hw1.html", "scratch.html"))
})

test_that("artifact_ambiguity_note flags only repos with several candidates", {
  proj = art_project("html")
  art_write(proj, "html/teamA_hw1-html/hw1.html")
  art_write(proj, "html/teamA_hw1-html/scratch.html")
  art_write(proj, "html/teamC_hw1-html/hw1.html")

  hits = suppressWarnings(resolve_repo_artifacts(proj, c("teamA", "teamC")))

  expect_null(artifact_ambiguity_note(hits, "teamC"))

  note = as.character(artifact_ambiguity_note(hits, "teamA"))
  expect_match(note, "alert-warning")
  expect_match(note, "showing \"hw1.html\"", fixed = TRUE)
  expect_match(note, "Also found: \"scratch.html\"", fixed = TRUE)
})
