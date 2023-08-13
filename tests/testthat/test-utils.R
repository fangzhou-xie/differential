
# library(testthat)
# 
# 
# test_that("check assignment works", {
#   expect_equal(is_assign(quote(x+1)[[1]]), FALSE)
# })
# 
# test_that("check assignment works", {
#   expect_equal(is_assign(quote(y <- x+1)[[1]]), TRUE)
# })
# 
# test_that("check unit call", {
#   expect_equal(is_call_unit(quote(x)), FALSE)
# })
# 
# test_that("check unit call", {
#   expect_equal(is_call_unit(quote(x+1)), TRUE)
# })
# 
# test_that("check unit call", {
#   expect_equal(is_call_unit(quote(2*x+1)), FALSE)
# })
# 
# test_that("check get assigned var", {
#   expect_equal(get_assigned_var(quote(y <- x + 1)), quote(y))
# })
# 
# test_that("check get assigned var", {
#   expect_equal(get_assigned_var(quote(x + 1)), NULL)
# })
# 
# 
# fold_constant(quote(2*x+1))
# 
# get_dependent_var(quote(2*x))
# 
# e <- quote(2*x+1)
# 
# shift_var_right(e)
# 
# 
# fold_constant(e)
# 
# fold_constant(quote(2+3+1))
# 
# fold_and_move(quote(2*x+1))
# fold_and_move(quote(x+2))
# fold_and_move(quote(x*2))
# 
# fold_constant(quote(2+3*x*3))
# constantFold(quote(2+3*x*3))
# 
# e <- quote(2+3*x*3+1)
# constantFold(e)
# Deriv::Simplify(e)
# 
# Deriv::Simplify(quote(2+x^2 + 3*x^2))
# 
# RHS2function(quote(y <- x+1), "x", "x")
# 
# # line expression
# e <- quote(b <- sin(a))
# # to function
# f <- RHS2function(e, "a", "a")
# 
# body(f)[[1]]
# gradient(body(f)[[1]])
# 
# f_grad <- gradient(f, wrt = wrt(a)) 
# # function(a) cos(a)
# gradient(function(a) sin(a))
# 
# 
# body(f)[[1]]
# names(formals(f))
# gradient(body(f)[[1]])
# replace_arg_call(
#   body(gradient(body(f)[[1]])), 
#   "x", 
#   "a")
# 
# gradient_unitcall(f)
# RHS2function(quote(x+1))
