# Direct coverage for the exported add_question / remove_question / get_question
# helpers and their guard branches.

tm_template = function(questions = list()) {
  markermd_template(original_ast = q2r::parse_qmd("# x", quiet = TRUE), questions = questions)
}

tm_q = function(id, name) markermd_question(id = as.integer(id), name = name)

test_that("add_question appends and rejects duplicate ids and names", {
  tmpl = tm_template()
  tmpl = add_question(tmpl, tm_q(1, "Q1"))
  expect_equal(length(tmpl), 1L)

  tmpl = add_question(tmpl, tm_q(2, "Q2"))
  expect_equal(length(tmpl), 2L)

  expect_error(add_question(tmpl, tm_q(1, "Other")), "ID 1 already exists")
  expect_error(add_question(tmpl, tm_q(3, "Q1")), "name 'Q1' already exists")
})

test_that("add_question validates its argument types", {
  tmpl = tm_template()
  expect_error(add_question("nope", tm_q(1, "Q1")), "must be a markermd_template")
  expect_error(add_question(tmpl, "nope"), "must be a markermd_question")
})

test_that("remove_question drops by id and warns when the id is absent", {
  tmpl = add_question(add_question(tm_template(), tm_q(1, "Q1")), tm_q(2, "Q2"))

  tmpl = remove_question(tmpl, 1L)
  expect_equal(length(tmpl), 1L)
  expect_equal(tmpl@questions[[1]]@id, 2L)

  expect_warning(out <- remove_question(tmpl, 99L), "not found")
  expect_equal(length(out), 1L)
})

test_that("get_question returns the matching question or NULL", {
  tmpl = add_question(add_question(tm_template(), tm_q(1, "Q1")), tm_q(2, "Q2"))

  expect_equal(get_question(tmpl, 2L)@name, "Q2")
  expect_null(get_question(tmpl, 99L))
})
