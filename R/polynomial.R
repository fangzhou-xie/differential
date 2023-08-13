
# S3 class for polynomials


#' @export
is_polynomial <- function(...) {
  UseMethod("is_polynomial")
}

#' @export
is_polynomial.call <- function(e) {
  # check expression is of form: a * x^b
  e_poly <- as_polynomial(e)
  !is.null(e_poly)
}

#' @export
is_polynomial.character <- function(e) is_polynomial(str2lang(e))

#' @export
is_polynomial.name <- function(e) TRUE

#' @export
is_polynomial.numeric <- function(e) TRUE




#' @export
as_polynomial <- function(...) {
  UseMethod("as_polynomial")
}

#' @export
as_polynomial.character <- function(e) as_polynomial(str2lang(e))

#' @export
as_polynomial.call <- function(e) poly_case(e)

#' @export
as_polynomial.numeric <- function(e) polynomial(e, 1, 1)

#' @export
as_polynomial.name <- function(e) polynomial(1, e, 1)

#' @export
`as_polynomial.(` <- function(e) as_polynomial(e[[2]])


#' @export
polynomial <- function(const, var, order) {
  # stopifnot(!has_controlflow(e))
  
  c <- simplify_expr(const)
  v <- simplify_expr(var)
  o <- simplify_expr(order)
  
  if (var == 1) {
    e <- const
  } else {
    if (const == 1 && order == 1) {
      e <- substitute(var, list(var = var))
    } else if (const == 1) {
      e <- substitute(var^order, list(var = var, order = order))
    } else if (order == 1) {
      e <- substitute(const * var, list(const = const, var = var))
    } else {
      e <- substitute(const * var^order,
                      list(const = const, var = var, order = order))
    }
  }
  
  structure(
    deparse(e, width.cutoff = 500L),
    class = "polynomial",
    const = c, var = v, order = o, expr = e
  )
}

#' @export
print.polynomial <- function(p) {
  cat(p)
}

#' @export
`==.polynomial` <- function(e1, e2) {
  const1 <- attr(e1, "const")
  const2 <- attr(e2, "const")
  var1 <- attr(e1, "var")
  var2 <- attr(e2, "var")
  order1 <- attr(e1, "order")
  order2 <- attr(e2, "order")
  
  (
    const1 == const2 && var1 == var2 && order1 == order2
  ) || (
    const1 == var2 && var1 == const2 && order1 == 1 && order2 == 1
  )
}

#' @export
`+.polynomial` <- function(e1, e2) {
  const1 <- attr(e1, "const")
  const2 <- attr(e2, "const")
  var1 <- attr(e1, "var")
  var2 <- attr(e2, "var")
  order1 <- attr(e1, "order")
  order2 <- attr(e2, "order")
  expr1 <- attr(e1, "expr")
  expr2 <- attr(e2, "expr")
  # browser()
  if (e1 == e2) {
    c <- simplify_call("*", 2, const1)
    o <- order1
    v <- var1
  } else if (var1 == var2 && order1 == order2) {
    c <- simplify_call("+", const1, const2)
    v <- var1
    o <- order1
  } else {
    c <- o <- 1
    if (order1 <= order2) {
      v <- simplify_call("+", expr1, expr2)
    } else {
      v <- simplify_call("+", expr2, expr1)
    }
  }
  polynomial(c, v, o)
}

#' @export
`-.polynomial` <- function(e1, e2) {
  const1 <- attr(e1, "const")
  const2 <- attr(e2, "const")
  var1 <- attr(e1, "var")
  var2 <- attr(e2, "var")
  order1 <- attr(e1, "order")
  order2 <- attr(e2, "order")
  expr1 <- attr(e1, "expr")
  expr2 <- attr(e2, "expr")
  
  if (e1 == e2) {
    c <- 0
    v <- o <- 1
  }else if (var1 == var2 && order1 == order2) {
    c <- simplify_call("-", const1, const2)
    v <- var1
    o <- order1
  } else {
    c <- o <- 1
    if (order1 <= order2) {
      v <- simplify_call("-", expr1, expr2)
    } else {
      v <- simplify_call("-", expr2, expr1)
    }
  }
  polynomial(c, v, o)
}

#' @export
`*.polynomial` <- function(e1, e2) {
  const1 <- attr(e1, "const")
  const2 <- attr(e2, "const")
  var1 <- attr(e1, "var")
  var2 <- attr(e2, "var")
  order1 <- attr(e1, "order")
  order2 <- attr(e2, "order")
  
  c <- simplify_call("*", const1, const2)
  o <- simplify_call("+", order1, order2)
  
  if (const1 == 0 || const2 == 0)
    v <- o <- 1
  else if (e1 == e2) {
    c <- const1
    o <- simplify_call("*", 2, order1)
    v <- var1
  } else if (var1 == var2) {
    v <- var1
  } else {
    p1 <- simplify_call("^", var1, order1)
    p2 <- simplify_call("^", var2, order2)
    o <- 1
    if (order1 <= order2) {
      v <- simplify_call("*", p1, p2)
    } else {
      v <- simplify_call("*", p2, p1)
    }
  }
  polynomial(c, v, o)
}

#' @export
`/.polynomial` <- function(e1, e2) {
  const1 <- attr(e1, "const")
  const2 <- attr(e2, "const")
  var1 <- attr(e1, "var")
  var2 <- attr(e2, "var")
  order1 <- attr(e1, "order")
  order2 <- attr(e2, "order")
  
  c <- simplify_call("/", const1, const2)
  
  if (var1 == var2) {
    o <- simplify_call("-", order1, order2)
    v <- var1
  } else if (var1 == const2 && var2 == const1 && 
             order1 == 1 && order2 == 1) {
    c <- v <- o <- 1
  } else {
    p1 <- simplify_call("^", var1, order1)
    p2 <- simplify_call("^", var2, order2)
    v <- simplify_call("/", p1, p2)
    o <- 1
  }
  polynomial(c, v, o)
}

#' @export
`^.polynomial` <- function(e1, e2) {
  const1 <- attr(e1, "const")
  const2 <- attr(e2, "const")
  var1 <- attr(e1, "var")
  var2 <- attr(e2, "var")
  order1 <- attr(e1, "order")
  order2 <- attr(e2, "order")
  
  if (const2 == 1 && order2 == 1) 
    p <- var2
  else if (var2 == 1 && order2 == 1)
    p <- const2
  else
    stop("only one of const/var can be 1")
  
  c <- simplify_call("^", const1, p)
  o <- simplify_call("*", order1, p)
  v <- var1
  polynomial(c, v, o)
}
