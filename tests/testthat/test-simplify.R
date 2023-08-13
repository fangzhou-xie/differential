
# test polynomial simplification

test_that("simplify: character", {
  expect_equal(
    simplify("3*x^2+2*x^2"), quote(5*x^2)
  )
})

test_that("simplify: name", {
  expect_equal(
    simplify(quote(xyz)), quote(xyz)
  )
})

test_that("simplify: numeric", {
  expect_equal(
    simplify(1), 1
  )
})

test_that("simplify: expression", {
  expect_equal(
    simplify(quote(3*2^4*2+3*x)), quote(96+3*x)
  )
})

test_that("constant folding", {
  expect_equal(
    constant_fold(2), 2
  )
})

test_that("constant folding", {
  expect_equal(
    constant_fold(3*2), 6
  )
})

test_that("constant folding", {
  expect_equal(
    constant_fold(quote(3*2^4*2+3)), 3*2^4*2+3
  )
})

test_that("constant folding", {
  expect_equal(
    constant_fold(quote(3*2^4*2+3*x)), quote(96 + 3 * x)
  )
})


# bench::mark(
#   constant_fold(quote(3*2^4*2+3)),
#   codetools::constantFold(quote(3*2^4*2+3))
# )
# 
# bench::mark(
#   constant_fold(quote(3*2^4*2+3*x)),
#   codetools::constantFold(quote(3*2^4*2+3*x))
# )
# differential:::constant_fold_recurse()
# e <- quote(3*2^4*2+3+x)
# differential:::constant_fold(quote(x^2))
# differential:::constant_fold(quote(3*2^4*2+3))
