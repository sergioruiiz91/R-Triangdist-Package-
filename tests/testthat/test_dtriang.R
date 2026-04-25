test_that("dtriang returns 0 outside [min, max]", {
  expect_equal(dtriang(x = -1, min = 0, max = 2, mode = 1), 0)
  expect_equal(dtriang(x = 3, min = 0, max = 2, mode = 1), 0)
  expect_equal(dtriang(x = 0, min = 0, max = 2, mode = 1), 0)
  expect_equal(dtriang(x = 2, min = 0, max = 2, mode = 1), 0)
})

test_that("dtriang left branch is correct", {
  expect_equal(dtriang(x = 0.5, min = 0, max = 2, mode = 1), 0.5)
})

test_that("dtriang right branch is correct", {
  expect_equal(dtriang(x = 1.5, min = 0, max = 2, mode = 1), 0.5)
})

test_that("dtriang peak value is correct", {
  expect_equal(dtriang(x = 1, min = 0, max = 2, mode = 1), 1)
})

test_that("dtriang is vectorized", {
  result <- dtriang(x = c(0.5, 1, 1.5), min = 0, max = 2, mode = 1)
  expect_equal(length(result), 3)
})

test_that("dtriang throws errors on invalid parameters", {
  expect_error(dtriang(x = 1, min = 2, max = 0, mode = 1))
  expect_error(dtriang(x = 1, min = 0, max = 2, mode = -1))
  expect_error(dtriang(x = 1, min = 0, max = 2, mode = 3))
})
