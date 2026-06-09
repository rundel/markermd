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
  # Deleting now requires confirming the modal
  app$click("question_2-confirm_delete_question")

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
  # Deleting now requires confirming the modal
  app$click("question_2-confirm_delete_question")

  expect_equal(get_n_questions(app), 1)

  Sys.sleep(1)
  app$click("add_question")

  expect_equal(get_n_questions(app), 2)

  app$expect_values(export = TRUE)
})


test_that("Save Template downloads a readable YAML template", {
  app = shinytest2::AppDriver$new(
    template(file),
    name = "save_yaml"
  )

  app$click("add_question")
  expect_equal(get_n_questions(app), 1)

  path = app$get_download("save_template")
  expect_match(path, "\\.yaml$")

  back = markermd::read_template_yaml(path, require_ast = TRUE)
  expect_s3_class(back, "markermd::markermd_template")
  expect_length(back@questions, 1)
  expect_gt(back@metadata@total_nodes, 0L)
})
