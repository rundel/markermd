# Coverage for the validation modal's select helpers (R/mod_mark_validate.R):
# the question card ids and the pass/fail labelled question choices.

test_that("question_card_id sanitises the question name", {
  expect_equal(markermd:::question_card_id("Q1"), "qcard_Q1")
  expect_equal(markermd:::question_card_id("Question 1 (a)"), "qcard_Question1a")
})

test_that("validation_question_choices labels questions with the repository's result", {
  results = list(
    Q1 = list(status = "pass"),
    Q2 = list(status = "fail"),
    Q3 = list(status = "error")
  )
  choices = markermd:::validation_question_choices(c("Q1", "Q2", "Q3", "Q4"), results)
  expect_equal(unname(choices), c("Q1", "Q2", "Q3", "Q4"))
  expect_equal(names(choices), c("\u2713 Q1", "\u2717 Q2", "\u2717 Q3", "Q4"))

  # no results (a repository whose document failed to parse): plain names
  plain = markermd:::validation_question_choices(c("Q1", "Q2"), NULL)
  expect_equal(names(plain), c("Q1", "Q2"))

  # no questions at all
  expect_length(markermd:::validation_question_choices(character(0), results), 0)
})
