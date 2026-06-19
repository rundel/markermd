# Coverage for the database schema-version guard.

dbc_conn = function() {
  d = tempfile("dbc_")
  dir.create(d)
  initialize_database(d)
}

test_that("assert_db_compatible accepts the current schema and a legacy db with no version", {
  conn = dbc_conn()
  on.exit(DBI::dbDisconnect(conn))

  expect_silent(assert_db_compatible(conn))

  delete_metadata(conn, "schema_version")
  expect_silent(assert_db_compatible(conn))
})

test_that("assert_db_compatible rejects a database written by a newer markermd", {
  conn = dbc_conn()
  on.exit(DBI::dbDisconnect(conn))

  set_metadata(conn, "schema_version", "99")
  expect_error(assert_db_compatible(conn), "newer version")
})
