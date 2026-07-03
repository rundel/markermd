# Announce a test file's existing snapshot files before its skip guards run.
# shinytest2 only announces snapshot files once an AppDriver is created, so if
# the app tests skip first (shinytest2 missing, or on CRAN), testthat's
# end-of-run cleanup would delete the committed snapshots as unused. Announced
# files are preserved even when every test in the file skips. The flip side is
# that a genuinely stale snapshot in these directories must be deleted by hand.
#
# stem: the test file's snapshot directory name, e.g. "template-app_ast"

announce_app_snapshots = function(stem) {
  for (f in list.files(testthat::test_path("_snaps", stem))) {
    testthat::announce_snapshot_file(name = f)
  }
}
