library(shinytest2)

tr_file = system.file("examples/test_assignment/repos/student1-excellent/", package = "markermd")

tr_n_questions = function(app) {
  app$get_values(export = "n_questions") |> unlist(use.names = FALSE)
}

tr_question_ids = function(app) {
  path = app$get_download("save_template")
  back = markermd::read_template_yaml(path, require_ast = FALSE)
  vapply(back@questions, function(q) q@id, integer(1))
}


test_that("moving a question down reorders the saved template, and the top guard no-ops", {
  app = shinytest2::AppDriver$new(template(tr_file), name = "question_reorder")

  app$click("add_question")
  app$click("add_question")
  expect_equal(tr_n_questions(app), 2)
  app$wait_for_idle()

  # Questions are added with ids 1 then 2, in that order
  expect_equal(tr_question_ids(app), c(1L, 2L))

  # Move the first question (id 1) down: the saved order swaps to 2, 1
  app$click("question_1-move_question_down")
  app$wait_for_idle()
  expect_equal(tr_question_ids(app), c(2L, 1L))

  # Moving the now-top question (id 2) up is out of range and must no-op
  app$click("question_2-move_question_up")
  app$wait_for_idle()
  expect_equal(tr_question_ids(app), c(2L, 1L))
})
