

# test polynomial class

test_that("as_polynomial: null", {
  expect_equal(
    as_polynomial(quote(sin(x))), NULL
  )
})

test_that("as_polynomial: numeric", {
  expect_equal(
    as_polynomial("1"), polynomial(1, 1, 1)
  )
})

test_that("as_polynomial: name", {
  expect_equal(
    as_polynomial("a"), polynomial(1, quote(a), 1)
  )
})

test_that("as_polynomial: is_poly_x_times_y", {
  expect_equal(
    as_polynomial("a*x"), polynomial(quote(a), quote(x), 1)
  )
})

test_that("as_polynomial: is_poly_x_b", {
  expect_equal(
    as_polynomial("x^y"), polynomial(1, quote(x), quote(y))
  )
})

test_that("as_polynomial: is_poly_a_times_x_b", {
  expect_equal(
    as_polynomial("c*x^y"), polynomial(quote(c), quote(x), quote(y))
  )
})

test_that("as_polynomial: is_poly_x_b_times_a", {
  expect_equal(
    as_polynomial("x^y*c"), polynomial(quote(c), quote(x), quote(y))
  )
})

test_that("as_polynomial: is_poly_a_over_x_b", {
  expect_equal(
    as_polynomial("1/x^2"), polynomial(1, quote(x), -2)
  )
})

test_that("as_polynomial: is_poly_a_over_x_b", {
  expect_equal(
    as_polynomial("c/x^y"), polynomial(quote(c), quote(x), quote(-y))
  )
})

test_that("as_polynomial: is_poly_x_b_over_a", {
  expect_equal(
    as_polynomial("x^y/c"), polynomial(quote(1/c), quote(x), quote(y))
  )
})

# test operators on polinomials

test_that("polynomial operator: +", {
  p1 <- polynomial(quote(a), quote(x), 1)
  p2 <- polynomial(quote(b), quote(x), 1)
  expect_equal(
    p1 + p2,
    polynomial(quote(a+b), quote(x), 1)
  )
}) 

test_that("polynomial operator: +", {
  p1 <- polynomial(quote(a), quote(x), 1)
  p2 <- polynomial(quote(x), quote(a), 1)
  expect_equal(
    p1 + p2,
    polynomial(quote(2*a), quote(x), 1)
  )
}) 

test_that("polynomial operator: +", {
  p1 <- polynomial(quote(a), quote(x), 2)
  p2 <- polynomial(quote(x), quote(a), 3)
  expect_equal(
    p1 + p2,
    polynomial(1, quote(a * x^2 + x * a^3), 1)
  )
}) 

test_that("polynomial operator: -", {
  p1 <- polynomial(quote(a), quote(x), 1)
  p2 <- polynomial(quote(x), quote(a), 1)
  expect_equal(
    p1 - p2,
    polynomial(0, 1, 1)
  )
}) 

test_that("polynomial operator: -", {
  p1 <- polynomial(quote(a), quote(x), 4)
  p2 <- polynomial(quote(b), quote(x), 4)
  expect_equal(
    p1 - p2,
    polynomial(quote(a-b), quote(x), 4)
  )
}) 

test_that("polynomial operator: -", {
  p1 <- polynomial(quote(a), quote(x), 2)
  p2 <- polynomial(quote(x), quote(a), 3)
  expect_equal(
    p1 - p2,
    polynomial(1, quote(a * x^2 - x * a^3), 1)
  )
}) 

test_that("polynomial operator: *", {
  p1 <- polynomial(quote(a), quote(x), 1)
  p2 <- polynomial(quote(x), quote(a), 1)
  expect_equal(
    p1 * p2,
    polynomial(quote(a), quote(x), 2)
  )
}) 

test_that("polynomial operator: *", {
  p1 <- polynomial(quote(a), quote(x), 1)
  p2 <- polynomial(quote(b), quote(x), 2)
  expect_equal(
    p1 * p2,
    polynomial(quote(a*b), quote(x), 3)
  )
}) 

test_that("polynomial operator: *", {
  p1 <- polynomial(quote(a), quote(x), 2)
  p2 <- polynomial(quote(b), quote(y), 3)
  expect_equal(
    p1 * p2,
    polynomial(quote(a*b), quote(x^2 * y^3), 1)
  )
}) 

test_that("polynomial operator: /", {
  p1 <- polynomial(quote(a), quote(x), 2)
  p2 <- polynomial(quote(b), quote(y), 3)
  expect_equal(
    p1 / p2,
    polynomial(quote(a/b), quote(x^2 / y^3), 1)
  )
}) 

test_that("polynomial operator: /", {
  p1 <- polynomial(quote(a), quote(x), 2)
  p2 <- polynomial(quote(b), quote(x), 3)
  expect_equal(
    p1 / p2,
    polynomial(quote(a/b), quote(x), -1)
  )
}) 

test_that("polynomial operator: /", {
  p1 <- polynomial(quote(a), quote(x), 1)
  p2 <- polynomial(quote(x), quote(a), 1)
  expect_equal(
    p1 / p2,
    polynomial(1, 1, 1)
  )
}) 

test_that("polynomial operator: ^", {
  p1 <- polynomial(quote(a), quote(x), 1)
  p2 <- polynomial(2, 1, 1)
  expect_equal(
    p1 ^ p2,
    polynomial(quote(a^2), quote(x), 2)
  )
}) 

test_that("polynomial operator: ^", {
  p1 <- polynomial(quote(a), quote(x), 1)
  p2 <- polynomial(1, quote(x), 1)
  expect_equal(
    p1 ^ p2,
    polynomial(quote(a^x), quote(x), quote(x))
  )
}) 

