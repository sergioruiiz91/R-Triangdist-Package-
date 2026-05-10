test_that("ptriang returns 0 below min", {
  expect_equal(ptriang(q = -1, min = 0, max = 2, mode = 1), 0)
  expect_equal(ptriang(q = 0, min = 0, max = 2, mode = 1), 0)
})

test_that("ptriang returns 1 above max", {
  expect_equal(ptriang(q = 3, min = 0, max = 2, mode = 1), 1)
  expect_equal(ptriang(q = 2, min = 0, max = 2, mode = 1), 1)
})

test_that("ptriang left branch is correct", {
  expect_equal(ptriang(q = 0.5, min = 0, max = 2, mode = 1), 0.125)
})

test_that("ptriang right branch is correct", {
  expect_equal(ptriang(q = 1.5, min = 0, max = 2, mode = 1), 0.875)
})

test_that("ptriang at mode is correct", {
  expect_equal(ptriang(q = 1, min = 0, max = 2, mode = 1), 0.5)
})

test_that("ptriang is vectorized", {
  result <- ptriang(q = c(0.5, 1, 1.5), min = 0, max = 2, mode = 1)
  expect_equal(length(result), 3)
})

test_that("ptriang throws errors on invalid parameters", {
  expect_error(ptriang(q = 1, min = 2, max = 0, mode = 1))
  expect_error(ptriang(q = 1, min = 0, max = 2, mode = -1))
  expect_error(ptriang(q = 1, min = 0, max = 2, mode = 3))
})
