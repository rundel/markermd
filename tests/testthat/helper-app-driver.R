# Launch a shinytest2 AppDriver and register it to be stopped when the calling
# test_that() block finishes. Every browser test starts its apps here, so
# setting MARKERMD_SKIP_BROWSER_TESTS=true skips them all (R-CMD-check.yaml
# does; browser-tests.yaml runs them). Without the teardown the shiny and
# Chrome processes of every driver live until garbage collection, and with
# 30+ app tests in a run the accumulation is enough to OOM a CI runner.
#
# ...: passed through to shinytest2::AppDriver$new()
# .env: environment whose exit tears the app down (the calling test block)

new_app_driver = function(..., .env = parent.frame()) {
  testthat::skip_if(
    identical(tolower(Sys.getenv("MARKERMD_SKIP_BROWSER_TESTS")), "true"),
    "browser tests are turned off (MARKERMD_SKIP_BROWSER_TESTS)"
  )
  app = shinytest2::AppDriver$new(...)
  wait_for_app_ready(app)
  withr::defer({
    if (nzchar(Sys.getenv("CI"))) report_app_problems(app)
    app$stop()
  }, envir = .env)
  app
}

# Wait until an app can be driven: mark()'s question select holds a question
# (or has none to hold) and template()'s Add Question button is bound.
# AppDriver$new() returns once Shiny first goes idle, which on a slow CI
# runner can be before either has happened, and a test that reads exports or
# clicks at that point sees an app that is still starting.
#
# app: The shinytest2 AppDriver

wait_for_app_ready = function(app) {
  app$wait_for_js(
    "(function() {
       var q = document.getElementById('rubric_module-question_select');
       var a = document.getElementById('add_question');
       var questionReady = !q || !!q.value || q.options.length === 0;
       var addReady = !a || a.classList.contains('shiny-bound-input');
       return questionReady && addReady;
     })()",
    timeout = 60 * 1000
  )
  app$wait_for_idle()
}

# On CI, print what went wrong inside an app before it is torn down: whether
# its R process is still alive and any error or warning lines from its server
# log. Browser tests there fail on timing that never reproduces on a dev
# machine, and the test failure alone does not say whether the app crashed,
# hung or errored.
#
# app: The shinytest2 AppDriver

report_app_problems = function(app) {
  alive = isTRUE(try(app$.__enclos_env__$private$shiny_process$is_alive(), silent = TRUE))
  logs = try(as.data.frame(app$get_logs()), silent = TRUE)
  problems = if (inherits(logs, "data.frame")) {
    logs[logs$location == "shiny" & grepl("error|warning|halted|killed|segfault|abort", logs$message, ignore.case = TRUE), , drop = FALSE]
  } else {
    data.frame()
  }
  if (!alive || nrow(problems) > 0) {
    message("---- app problems (", app$get_dir(), "): process ", if (alive) "alive" else "NOT ALIVE", " ----")
    if (nrow(problems) > 0) message(paste(problems$timestamp, problems$message, collapse = "\n"))
  }
}

# Download a file from a Shiny download link once the link is live: a
# download link starts disabled with an empty href until its output binds,
# which on a slow runner can be after the test reaches it (for example a link
# inside a popover that was just opened)
#
# app: The shinytest2 AppDriver
# id: The download output id

download_when_ready = function(app, id) {
  app$wait_for_js(
    paste0(
      "(function() { var el = document.getElementById('", id, "');",
      " return !!el && !el.classList.contains('disabled') && !!el.getAttribute('href'); })()"
    ),
    timeout = 30 * 1000
  )
  app$get_download(id)
}
