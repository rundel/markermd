# Launch a shinytest2 AppDriver and register it to be stopped when the calling
# test_that() block finishes. Without this the shiny and Chrome processes of
# every driver live until garbage collection, and with 30+ app tests in a run
# the accumulation is enough to OOM a CI runner.
#
# ...: passed through to shinytest2::AppDriver$new()
# .env: environment whose exit tears the app down (the calling test block)

new_app_driver = function(..., .env = parent.frame()) {
  app = shinytest2::AppDriver$new(...)
  withr::defer(app$stop(), envir = .env)
  app
}
