# Unit coverage for the shared dynamic-row helpers in mod_question_rows.R.
# The reactiveVal-backed helpers work outside a Shiny session because every
# read/write is wrapped in shiny::isolate() internally.

test_that("make_render_trigger bumps only when asked", {
  trig = markermd:::make_render_trigger()
  expect_equal(shiny::isolate(trig$depend()), 0L)
  trig$bump()
  trig$bump()
  expect_equal(shiny::isolate(trig$depend()), 2L)
})

test_that("make_seen_registry notes keys once and answers membership", {
  reg = markermd:::make_seen_registry()
  expect_false(reg$has("1"))
  reg$note("2")
  reg$note("2")
  reg$note("3")
  expect_true(reg$has("2"))
  expect_true(reg$has("3"))
  expect_false(reg$has("1"))
})

test_that("make_seen_registry forgets deleted keys and leaves the rest alone", {
  reg = markermd:::make_seen_registry()
  reg$note("2")
  reg$note("3")
  reg$forget(c("2", "9"))
  expect_false(reg$has("2"))
  expect_true(reg$has("3"))
})

test_that("sync_keyed_observers creates missing and destroys stale entries", {
  created = character(0)
  destroyed = character(0)
  fake_obs = function(key) {
    e = new.env()
    e$destroy = function() destroyed <<- c(destroyed, key)
    e
  }

  obs = markermd:::sync_keyed_observers(list(), c("1", "2"), function(key) {
    created <<- c(created, key)
    fake_obs(key)
  })
  expect_setequal(names(obs), c("1", "2"))
  expect_setequal(created, c("1", "2"))
  expect_length(destroyed, 0)

  # A list entry (the filters machine's per-group observer pair) has every
  # element destroyed when its key goes stale
  obs[["g1"]] = list(fake_obs("g1a"), fake_obs("g1b"))
  obs = markermd:::sync_keyed_observers(obs, "2", function(key) fake_obs(key))
  expect_equal(names(obs), "2")
  expect_setequal(destroyed, c("1", "g1a", "g1b"))
})
