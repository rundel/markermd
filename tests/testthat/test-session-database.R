count_connections = function(code) {
  n = 0
  ns = asNamespace("markermd")
  orig = ns$initialize_database
  unlockBinding("initialize_database", ns)
  assign("initialize_database", function(collection_path) {
    n <<- n + 1
    orig(collection_path)
  }, envir = ns)
  on.exit({
    assign("initialize_database", orig, envir = ns)
    lockBinding("initialize_database", ns)
  })
  force(code)
  n
}

test_that("a session reuses one connection across helper calls and closes it on end", {
  d = suppressMessages(sn_graded_project())
  s = shiny::MockShinySession$new()
  conn = shiny::withReactiveDomain(s, open_session_database(d, s))
  expect_true(DBI::dbIsValid(conn))

  n = shiny::withReactiveDomain(s, count_connections({
    with_database(d, function(c) expect_identical(c, conn))
    load_rubric_items(d, "Q1")
    collect_score_data(d, c("Q1", "Q2"), c("hw01-team01", "hw01-team02"))
  }))
  expect_equal(n, 0)

  # Outside the session's reactive domain every call still opens its own
  expect_equal(count_connections(load_rubric_items(d, "Q1")), 1)

  s$close()
  expect_false(DBI::dbIsValid(conn))
  expect_null(shiny::withReactiveDomain(s, session_database(d)))
})

test_that("nested transactions on a shared connection become savepoints", {
  d = suppressMessages(sn_graded_project())
  s = shiny::MockShinySession$new()
  on.exit(s$close())
  shiny::withReactiveDomain(s, {
    open_session_database(d, s)
    with_database(d, function(conn) {
      with_transaction(conn, "outer", {
        set_metadata(conn, "probe_outer", "1")
        expect_error(
          with_transaction(conn, "inner", {
            set_metadata(conn, "probe_inner", "1")
            stop("boom")
          }),
          "inner: boom"
        )
        expect_true(RSQLite::sqliteIsTransacting(conn))
      })
      expect_false(RSQLite::sqliteIsTransacting(conn))
      expect_equal(get_metadata(conn, "probe_outer"), "1")
      expect_null(get_metadata(conn, "probe_inner"))
    })
  })
})

test_that("an error inside an open transaction does not leak it into the session's next call", {
  d = suppressMessages(sn_graded_project())
  s = shiny::MockShinySession$new()
  on.exit(s$close())
  shiny::withReactiveDomain(s, {
    open_session_database(d, s)
    expect_error(with_database(d, function(conn) {
      DBI::dbBegin(conn)
      set_metadata(conn, "probe_leak", "1")
      stop("boom")
    }), "boom")
    with_database(d, function(conn) {
      expect_false(RSQLite::sqliteIsTransacting(conn))
      expect_null(get_metadata(conn, "probe_leak"))
    })
  })
})

test_that("two sessions keep separate connections and see each other's commits", {
  d = suppressMessages(sn_graded_project())
  a = shiny::MockShinySession$new()
  b = shiny::MockShinySession$new()
  on.exit({
    a$close()
    b$close()
  })
  conn_a = shiny::withReactiveDomain(a, open_session_database(d, a))
  conn_b = shiny::withReactiveDomain(b, open_session_database(d, b))
  expect_false(identical(conn_a, conn_b))

  version = function(conn) DBI::dbGetQuery(conn, "PRAGMA data_version")[[1]]
  before_b = version(conn_b)
  shiny::withReactiveDomain(a, save_grade_selections(d, "Q1", "hw01-team02", list(item_1 = TRUE)))
  expect_false(identical(version(conn_b), before_b))

  before_a = version(conn_a)
  shiny::withReactiveDomain(b, save_comment(d, "Q1", "hw01-team02", "from b"))
  expect_false(identical(version(conn_a), before_a))
  expect_equal(shiny::withReactiveDomain(a, load_comment(d, "Q1", "hw01-team02")), "from b")
})
