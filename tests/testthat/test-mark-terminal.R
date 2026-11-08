test_that("resolve_mark_terminal() is off unless opted in", {
  expect_null(markermd:::resolve_mark_terminal(FALSE, tempdir()))
  expect_null(markermd:::resolve_mark_terminal(NULL, tempdir()))
})

test_that("resolve_mark_terminal() starts bare programs in the project root", {
  testthat::skip_if_not_installed("shinyterm")
  root = withr::local_tempdir()

  cmds = markermd:::resolve_mark_terminal(TRUE, root)
  expect_true("Shell" %in% names(cmds))
  for (cmd in cmds) {
    expect_s3_class(cmd, "shinyterm_command")
    expect_identical(cmd$wd, path.expand(root))
  }

  custom = markermd:::resolve_mark_terminal(
    list(Bash = "bash", Other = shinyterm::terminal_command("zsh", wd = tempdir())),
    root
  )
  expect_identical(custom$Bash$wd, path.expand(root))
  expect_identical(custom$Other$wd, path.expand(tempdir()))

  expect_error(markermd:::resolve_mark_terminal(1, root), "terminal")
})

test_that("mark app shows the Terminal tab only when opted in", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("shinyterm")
  testthat::skip_on_cran()
  testthat::skip_on_os("windows")

  fixture = make_mark_fixture()

  plain = new_app_driver(markermd:::mark_app(fixture$project), name = "mark_no_terminal")
  expect_false(plain$get_js("!!document.querySelector('[data-value=\"terminal\"]')"))

  app = new_app_driver(
    markermd:::mark_app(fixture$project, terminal = list(Shell = "/bin/sh"), terminal_token = FALSE),
    name = "mark_terminal"
  )
  expect_true(app$get_js("!!document.querySelector('a[data-value=\"terminal\"]')"))

  app$run_js("document.querySelector('a[data-value=\"terminal\"]').click()")
  app$wait_for_js("document.querySelector('#terminal_module-terms-tabs-tabset').offsetHeight > 0")

  # The tabset fills the panel rather than shrinking to its content
  fill = app$get_js("
    (() => {
      const tabset = document.querySelector('#terminal_module-terms-tabs-tabset');
      const pane = tabset.closest('.tab-pane');
      return tabset.getBoundingClientRect().height / pane.getBoundingClientRect().height;
    })()
  ")
  expect_gt(fill, 0.9)
})

test_that("the Terminal tab protects the app with an access token", {
  testthat::skip_if_not_installed("shinyterm")
  fixture = make_mark_fixture()

  plain = markermd:::mark_app(fixture$project)
  expect_null(plain$options$launch.browser)

  app = markermd:::mark_app(fixture$project, terminal = list(Shell = "/bin/sh"), terminal_token = "abc")
  opened = NULL
  withr::local_options(shiny.launch.browser = function(url) opened <<- url)
  app$options$launch.browser("http://127.0.0.1:1/")
  expect_equal(opened, "http://127.0.0.1:1/?shinyterm_token=abc")
})
