# A two-repo exchange list exercising selections, empty items, and both
# comment channels.
make_marks_exchange = function() {
  list(
    format_version = "1.0",
    repos = list(
      list(
        name = "hw01-team01",
        questions = list(
          list(name = "Q1", items = c("Issue A", "Issue B"), comment = NULL, private_comment = "machine note"),
          list(name = "Q2", items = character(0), comment = "well done", private_comment = NULL)
        )
      ),
      list(
        name = "hw01-team02",
        questions = list(
          list(name = "Q1", items = "Issue A", comment = NULL, private_comment = NULL)
        )
      )
    )
  )
}


test_that("marks YAML round trips faithfully", {
  marks = make_marks_exchange()
  path = tempfile(fileext = ".yaml")

  expect_identical(write_marks_yaml(marks, path), path)
  expect_equal(read_marks_yaml(path), marks)
})


test_that("empty items serialize as items: [] and parse back to character(0)", {
  marks = make_marks_exchange()
  path = tempfile(fileext = ".yaml")
  write_marks_yaml(marks, path)

  expect_true(any(grepl("items: \\[\\]", readLines(path))))
  expect_identical(read_marks_yaml(path)$repos[[1]]$questions[[2]]$items, character(0))
})


test_that("hand-authored marks YAML parses", {
  path = tempfile(fileext = ".yaml")
  writeLines(c(
    "format_version: '1.0'",
    "repos:",
    "- name: hw01-team01",
    "  questions:",
    "  - name: Q1",
    "    items:",
    "    - Only issue",
    "    private_comment: matches the key otherwise",
    "  - name: Q2",
    "    items: []"
  ), path)

  marks = read_marks_yaml(path)
  expect_length(marks$repos, 1)
  expect_equal(marks$repos[[1]]$questions[[1]]$items, "Only issue")
  expect_equal(marks$repos[[1]]$questions[[1]]$private_comment, "matches the key otherwise")
  expect_null(marks$repos[[1]]$questions[[1]]$comment)
  expect_identical(marks$repos[[1]]$questions[[2]]$items, character(0))
})


test_that("marks_from_list validates version, names, items, and duplicates", {
  good = function() yaml::yaml.load(yaml::as.yaml(marks_to_list(make_marks_exchange())))

  x = good()
  x$format_version = NULL
  expect_error(markermd:::marks_from_list(x), "missing the required 'format_version'")

  x = good()
  x$format_version = "99.0"
  expect_error(markermd:::marks_from_list(x), "newer than this version")

  x = good()
  x$repos[[1]]$name = NULL
  expect_error(markermd:::marks_from_list(x), "non-empty 'name'")

  x = good()
  x$repos[[2]]$name = "hw01-team01"
  expect_error(markermd:::marks_from_list(x), "duplicate repository names")

  x = good()
  x$repos[[1]]$questions[[2]]$name = "Q1"
  expect_error(markermd:::marks_from_list(x), "duplicate question names")

  x = good()
  x$repos[[1]]$questions[[1]]$items = NULL
  expect_error(markermd:::marks_from_list(x), "missing the required 'items'")

  x = good()
  x$repos[[1]]$questions[[1]]$items = list("Issue A", "Issue A")
  expect_error(markermd:::marks_from_list(x), "duplicate rubric item descriptions")

  x = good()
  x$repos[[1]]$questions[[1]]$items = list("Issue A", "   ")
  expect_error(markermd:::marks_from_list(x), "blank rubric item description")
})


test_that("validate_marks_file checks against the bundled schema", {
  skip_if_not_installed("jsonvalidate")

  path = tempfile(fileext = ".yaml")
  write_marks_yaml(make_marks_exchange(), path)
  expect_true(validate_marks_file(path))

  writeLines(c(
    "format_version: '1.0'",
    "repos:",
    "- name: hw01-team01",
    "  questions:",
    "  - name: Q1"
  ), path)
  expect_false(validate_marks_file(path))
})


test_that("collect_marks_data reports active pairs with display-order selections", {
  dir = tempfile("marksdata")
  dir.create(dir)

  markermd:::save_rubric_item(dir, "Q1", "item_0", markermd_rubric_item(1L, -2, "Issue A"))
  markermd:::save_rubric_item(dir, "Q1", "item_1", markermd_rubric_item(2L, -1, "Issue B"))
  markermd:::save_rubric_item(dir, "Q2", "item_2", markermd_rubric_item(1L, -3, "Q2 issue"))

  # repoA / Q1: both items selected, in reverse display order
  markermd:::save_grade_selection(dir, "Q1", "repoA", "item_1", TRUE)
  markermd:::save_grade_selection(dir, "Q1", "repoA", "item_0", TRUE)
  # repoA / Q2: selected then deselected -> active pair with no selections
  markermd:::save_grade_selection(dir, "Q2", "repoA", "item_2", TRUE)
  markermd:::save_grade_selection(dir, "Q2", "repoA", "item_2", FALSE)
  # repoB / Q1: private comment only; repoB / Q2 untouched -> omitted
  markermd:::save_private_comment(dir, "Q1", "repoB", "needs a closer look")
  # repoA / Q1 also has a public comment whose latest revision is blank
  markermd:::save_comment(dir, "Q1", "repoA", "draft")
  markermd:::save_comment(dir, "Q1", "repoA", "   ")

  marks = markermd:::collect_marks_data(dir)

  expect_equal(vapply(marks$repos, function(r) r$name, character(1)), c("repoA", "repoB"))

  repoA = marks$repos[[1]]
  expect_equal(vapply(repoA$questions, function(q) q$name, character(1)), c("Q1", "Q2"))
  expect_equal(repoA$questions[[1]]$items, c("Issue A", "Issue B"))
  expect_null(repoA$questions[[1]]$comment)
  expect_identical(repoA$questions[[2]]$items, character(0))

  repoB = marks$repos[[2]]
  expect_equal(vapply(repoB$questions, function(q) q$name, character(1)), "Q1")
  expect_identical(repoB$questions[[1]]$items, character(0))
  expect_equal(repoB$questions[[1]]$private_comment, "needs a closer look")

  # Filters subset and order the output
  filtered = markermd:::collect_marks_data(dir, repo_names = "repoB")
  expect_equal(vapply(filtered$repos, function(r) r$name, character(1)), "repoB")

  filtered = markermd:::collect_marks_data(dir, question_names = "Q2")
  expect_equal(vapply(filtered$repos, function(r) r$name, character(1)), "repoA")
  expect_equal(vapply(filtered$repos[[1]]$questions, function(q) q$name, character(1)), "Q2")
})
