# CI runners are far slower than a dev machine: app startup regularly blows
# shinytest2's 15s load timeout and chromote's 10s command timeout there.

if (nzchar(Sys.getenv("CI"))) {
  options(
    shinytest2.load_timeout = 120 * 1000,
    shinytest2.timeout = 30 * 1000,
    chromote.timeout = 120
  )
}
