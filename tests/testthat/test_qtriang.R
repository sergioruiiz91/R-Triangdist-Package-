test_that("qtriang boundary values are correct", {
  expect_equal(qtriang(p = 0, min = 0, max = 2, mode = 1), 0)
  expect_equal(qtriang(p = 1, min = 0, max = 2, mode = 1), 2)
})

test_that("qtriang at threshold returns mode", {
  expect_equal(qtriang(p = 0.5, min = 0, max = 2, mode = 1), 1)
})

test_that("qtriang left branch is correct", {
  expect_equal(qtriang(p = 0.125, min = 0, max = 2, mode = 1), 0.5)
})

test_that("qtriang right branch is correct", {
  expect_equal(qtriang(p = 0.875, min = 0, max = 2, mode = 1), 1.5)
})

test_that("qtriang is vectorized", {
  result <- qtriang(p = c(0.125, 0.5, 0.875), min = 0, max = 2, mode = 1)
  expect_equal(length(result), 3)
})

test_that("qtriang throws error on invalid p", {
  expect_error(qtriang(p = -0.1, min = 0, max = 2, mode = 1))
  expect_error(qtriang(p = 1.1, min = 0, max = 2, mode = 1))
})

test_that("qtriang throws errors on invalid parameters", {
  expect_error(qtriang(p = 0.5, min = 2, max = 0, mode = 1))
  expect_error(qtriang(p = 0.5, min = 0, max = 2, mode = -1))
  expect_error(qtriang(p = 0.5, min = 0, max = 2, mode = 3))
})
