# setup_assignment_repo() (R/utils_git.R): name checks before any file system
# change, refusing an existing checkout of another repository, and the clone
# failure hint. A local repository stands in for GitHub via remote_url.

make_source_repo = function() {
  src = normalizePath(withr::local_tempdir("src_repo_", .local_envir = parent.frame()), winslash = "/")
  gert::git_init(src)
  writeLines(c("# Q1", "", "Answer."), file.path(src, "hw1.qmd"))
  gert::git_add("hw1.qmd", repo = src)
  gert::git_commit("init", author = gert::git_signature("Key", "key@example.com"), repo = src)
  src
}

test_that("malformed owner/repo names abort before local_dir is created", {
  for (name in c("owner/..", "owner/.", "-owner/repo", "own--er/repo", "owner/re po", "a/b/c", "owner/")) {
    local_dir = file.path(tempdir(), paste0("never_", as.integer(stats::runif(1, 1, 1e6))))
    expect_error(
      markermd:::setup_assignment_repo(name, local_dir, is_github_repo = TRUE),
      "not a GitHub repository name"
    )
    expect_false(dir.exists(local_dir))
  }
  expect_equal(markermd:::parse_github_repo_name("Sta-523/hw1.key.git"), list(owner = "Sta-523", repo = "hw1.key"))
})

test_that("a fresh clone lands under local_dir and a matching checkout is reused", {
  src = make_source_repo()
  local_dir = withr::local_tempdir("clones_")
  path = suppressMessages(markermd:::setup_assignment_repo("owner/hw1", local_dir, is_github_repo = TRUE, remote_url = src))
  expect_true(file.exists(file.path(path, "hw1.qmd")))
  expect_equal(basename(path), "hw1")

  again = suppressMessages(markermd:::setup_assignment_repo("owner/hw1", local_dir, is_github_repo = TRUE, remote_url = paste0(src, ".git/")))
  expect_equal(again, path)
})

test_that("an existing directory with another origin is refused, not pulled", {
  src = make_source_repo()
  other = make_source_repo()
  local_dir = withr::local_tempdir("clones_")
  suppressMessages(markermd:::setup_assignment_repo("owner/hw1", local_dir, is_github_repo = TRUE, remote_url = other))
  expect_error(
    markermd:::setup_assignment_repo("owner/hw1", local_dir, is_github_repo = TRUE, remote_url = src),
    "not a checkout of"
  )

  dir.create(file.path(local_dir, "plain"))
  expect_error(
    markermd:::setup_assignment_repo("owner/plain", local_dir, is_github_repo = TRUE, remote_url = src),
    "not a checkout of"
  )
})

test_that("a failed clone aborts with a hint", {
  local_dir = withr::local_tempdir("clones_")
  missing = file.path(tempdir(), "no_such_repo_here")
  expect_error(
    markermd:::setup_assignment_repo("owner/hw1", local_dir, is_github_repo = TRUE, remote_url = missing),
    "Could not clone"
  )
})
