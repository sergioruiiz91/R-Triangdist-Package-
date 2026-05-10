test_that("rtriang returns correct number of samples", {
  result <- rtriang(n = 100, min = 0, max = 2, mode = 1)
  expect_equal(length(result), 100)
})

test_that("rtriang values are within [min, max]", {
  result <- rtriang(n = 1000, min = 0, max = 2, mode = 1)
  expect_true(all(result >= 0))
  expect_true(all(result <= 2))
})

test_that("rtriang throws errors on invalid parameters", {
  expect_error(rtriang(n = 10, min = 2, max = 0, mode = 1))
  expect_error(rtriang(n = 10, min = 0, max = 2, mode = -1))
  expect_error(rtriang(n = 10, min = 0, max = 2, mode = 3))
})
