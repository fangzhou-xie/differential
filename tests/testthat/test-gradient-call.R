
# test gradient-call

test_that("gradient: add", {
  expect_equal(
    gradient(quote(x+y), wrt(x)), 1
  )
})

test_that("gradient: add", {
  expect_equal(
    gradient(quote(x+y), wrt(y)), 1
  )
})

test_that("gradient: multiply", {
  expect_equal(
    gradient(quote(3*x), wrt(x)), 3
  )
})

test_that("gradient: multiply", {
  expect_equal(
    gradient(quote(y*x), wrt(x)), quote(y)
  )
})

test_that("gradient: power", {
  expect_equal(
    gradient(quote(3*x^2), wrt(x)), quote(6*x)
  )
})

test_that("gradient: power", {
  expect_equal(
    gradient(quote(sin(x^2)), wrt(x)), quote(2 * x * cos(x^2))
  )
})
