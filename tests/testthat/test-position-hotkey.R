# position_hotkey() maps a rubric item's display position to its hotkey: the
# first ten positions get 1-10 (shown as 1-9 and 0), everything past the tenth
# has none and is toggled by clicking. It replaced next_item_hotkey(), whose
# max()-based numbering crashed Add Item once a question held an NA-hotkey item.

test_that("position_hotkey numbers the first ten positions then returns NA", {
  expect_equal(position_hotkey(1), 1L)
  expect_equal(position_hotkey(4L), 4L)
  expect_equal(position_hotkey(10), 10L)
  expect_true(is.na(position_hotkey(11)))
  expect_true(is.na(position_hotkey(25L)))
})

test_that("position_hotkey tolerates a missing position", {
  expect_true(is.na(position_hotkey(NA)))
  expect_true(is.na(position_hotkey(NULL)))
})
