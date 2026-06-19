# Regression for the Add Item crash: once a question held an NA-hotkey item
# (the 11th+), the next-hotkey max() returned NA and the > 10 test errored.

test_that("next_item_hotkey numbers the first ten slots then returns NA", {
  expect_equal(next_item_hotkey(integer(0)), 1L)
  expect_equal(next_item_hotkey(c(1L, 2L, 3L)), 4L)
  expect_equal(next_item_hotkey(1:9), 10L)
  expect_true(is.na(next_item_hotkey(1:10)))
})

test_that("next_item_hotkey ignores NA hotkeys from items past the tenth", {
  expect_equal(next_item_hotkey(c(1L, 2L, NA_integer_)), 3L)
  expect_true(is.na(next_item_hotkey(c(1:10, NA_integer_))))
  expect_true(is.na(next_item_hotkey(c(1:10, NA_integer_, NA_integer_))))
})
