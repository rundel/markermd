library(shinytest2)

file = system.file("examples/test_assignment/student1-excellent/", package = "markermd")

get_n_questions = function(app) {
  app$get_values(export = "n_questions") |> unlist(use.names = FALSE)
}


test_that("Add 1 question", {
  app = shinytest2::AppDriver$new(
    template(file),
    name  = "add_1"
  )


  expect_equal(get_n_questions(app), 0)

  app$click("add_question")

  expect_equal(get_n_questions(app), 1)

  app$expect_values(export = TRUE)
})

test_that("Add 2 questions", {
  app = shinytest2::AppDriver$new(
    template(file),
    name  = "add_2"
  )
  
  expect_equal(get_n_questions(app), 0)

  app$click("add_question")
  app$click("add_question")

  expect_equal(get_n_questions(app), 2)

  app$expect_values(export = TRUE)
})

test_that("Add 2 questions, delete 1", {
  app = shinytest2::AppDriver$new(
    template(file),
    name  = "add_2-delete_1"
  )
  
  expect_equal(get_n_questions(app), 0)

  app$click("add_question")
  app$click("add_question")

  expect_equal(get_n_questions(app), 2)

  Sys.sleep(1)
  app$click("question_2-delete_question")

  expect_equal(get_n_questions(app), 1)

  app$expect_values(export = TRUE)
})


test_that("Add 2 questions, delete 1, add 1", {
  app = shinytest2::AppDriver$new(
    template(file),
    name  = "add_2-delete_1-add_1"
  )

  expect_equal(get_n_questions(app), 0)
  
  app$click("add_question")
  app$click("add_question")

  expect_equal(get_n_questions(app), 2)

  Sys.sleep(1)
  app$click("question_2-delete_question")

  expect_equal(get_n_questions(app), 1)

  Sys.sleep(1)
  app$click("add_question")

  expect_equal(get_n_questions(app), 2)

  app$expect_values(export = TRUE)
})
