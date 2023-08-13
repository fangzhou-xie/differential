


# simplify by polynomials
simplify_poly <- function(e) {
  # browser()
  if (is.call(e)) {
    # try to simplify by polynomials
    e_opr <- as.character(e[[1]])
    if (depth(e[[1]]) > 1 && e[[1]][[1]] == quote(gradient))
      return(e)
    else if (e_opr %in% c("+", "-", "*", "/", "^") && length(e) == 3) {
      e1 <- as_polynomial(constant_fold(e[[2]]))
      e2 <- as_polynomial(constant_fold(e[[3]]))
      if (!is.null(e1) && !is.null(e2)) 
        e <- attr(eval(call(e_opr, e1, e2)), "expr")
    }
  }
  simplify_expr(e)
}


is_namenum <- function(e) is.numeric(e) || is.name(e)

# is_poly_const <- function(e) is.numeric(e)
# is_poly_var <- function(e) is.name(e)

remove_parenthesis <- function(e) {
  # browser()
  if (is.call(e) && e[[1]] == as.name("("))
    return(e[[2]])
  else
    return(e)
}

poly_type <- function(e) {
  opr <- e[[1]]
  e1 <- remove_parenthesis(e[[2]])
  
  if (length(e) == 2) {
    if (is_poly_unary_plus_x(opr, e1))
      "unary_plus_x"
    else if (is_poly_unary_minus_x(opr, e2))
      "unary_minus_x"
    else 
      "otherwise"
  } else if (length(e) == 3) {
    e2 <- remove_parenthesis(e[[3]])
    # browser()
    if (is_poly_x_times_y(opr, e1, e2)) 
      "x_times_y"
    else if (is_poly_x_over_y(opr, e1, e2))
      "x_over_y"
    else if (is_poly_x_b(opr, e1, e2))
      "x_b"
    else if (is_poly_a_times_x_b(opr, e1, e2))
      "a_times_x_b"
    else if (is_poly_x_b_times_a(opr, e1, e2))
      "x_b_times_a"
    else if (is_poly_a_over_x_b(opr, e1, e2))
      "a_over_x_b"
    else if (is_poly_x_b_over_a(opr, e1, e2))
      "x_b_over_a"
    else
      "otherwise"
  }
}

is_poly_unary_plus_x <- function(opr, e1) {
  # form: -x
  opr == as.name("+") && is_namenum(e1)
}
is_poly_unary_minus_x <- function(opr, e1) {
  # form: -x
  opr == as.name("-") && is_namenum(e1)
}
is_poly_x_times_y <- function(opr, e1, e2) {
  # form: x * y
  opr == as.name("*") && is_namenum(e1) && is_namenum(e2)
}
is_poly_x_over_y <- function(opr, e1, e2) {
  # form: a/x
  opr == as.name("/") && is_namenum(e1) && is_namenum(e2)
}
is_poly_x_b <- function(opr, e1, e2) {
  # form: x^b
  opr == as.name("^") && is_namenum(e1) && is_namenum(e2)
}
is_poly_a_times_x_b <- function(opr, e1, e2) {
  # form: a * x ^ b
  opr == as.name("*") && is_namenum(e1) &&
    is.call(e2) && e2[[1]] == as.name("^")
}
is_poly_x_b_times_a <- function(opr, e1, e2) {
  # form: x^b*a
  opr == as.name("*") && is_namenum(e2) && 
    is.call(e1) && e1[[1]] == as.name("^")
}
is_poly_a_over_x_b <- function(opr, e1, e2) {
  # form: a/x^b
  opr == as.name("/") && is_namenum(e1) &&
    is.call(e2) && e2[[1]] == as.name("^")
}
is_poly_x_b_over_a <- function(opr, e1, e2) {
  # form: x^b/a
  opr == as.name("/") && is_namenum(e2) &&
    is.call(e1) && e1[[1]] == as.name("^")
}


poly_case <- function(e) {
  polinizer <- switch(poly_type(e),
    unary_plus_x = function(e) {
      list(c = 1, v = e, o = 1)
    },
    unary_minus_x = function(e) {
      list(c = -1, v = e, o = 1)
    },
    x_times_y = function(e) {
      e1 <- remove_parenthesis(e[[2]])
      e2 <- remove_parenthesis(e[[3]])
      list(c = e1, v = e2, o = 1)
    },
    x_over_y = function(e) {
      e1 <- remove_parenthesis(e[[2]])
      e2 <- remove_parenthesis(e[[3]])
      
      c <- e1
      v <- e2
      if (is.call(v) && v[[1]] == as.name("^")) {
        if (v[[3]] == -1L)
          o <- 1
        else if (is.call(v[[3]]) && length(v[[3]]) == 2 && 
                 v[[3]][[1]] == as.name("-"))
          o <- v[[3]][[2]]
        else
          o <- as.call(list(as.name("-"), v[[3]]))
      } else {
        o <- -1
      }
      list(c = c, v = v, o = o)
    },
    x_b = function(e) {
      e1 <- remove_parenthesis(e[[2]])
      e2 <- remove_parenthesis(e[[3]])
      list(c = 1, v = e1, o = e2)
    },
    a_times_x_b = function(e) {
      e1 <- remove_parenthesis(e[[2]])
      e2 <- remove_parenthesis(e[[3]])
      
      list(
        c = e1,
        v = remove_parenthesis(e2[[2]]),
        o = remove_parenthesis(e2[[3]])
      )
    },
    x_b_times_a = function(e) {
      e1 <- remove_parenthesis(e[[2]])
      e2 <- remove_parenthesis(e[[3]])

      list(
        c = e2,
        v = remove_parenthesis(e1[[2]]),
        o = remove_parenthesis(e1[[3]])
      )
    },
    a_over_x_b = function(e) {
      e1 <- remove_parenthesis(e[[2]])
      e2 <- remove_parenthesis(e[[3]])
      # browser()
      c <- e1
      v <- remove_parenthesis(e2[[2]])
      o <- remove_parenthesis(e2[[3]])
      if (is.numeric(o))
        o <- -o
      else if (is.call(o) && o[[1]] == as.name("-") && length(o) == 2)
        o <- o[[2]]
      else
        o <- as.call(list(as.name("-"), o))
      list(c = c, v = v, o = o)
    },
    x_b_over_a = function(e) {
      e1 <- remove_parenthesis(e[[2]])
      e2 <- remove_parenthesis(e[[3]])
      
      c <- e2
      v <- remove_parenthesis(e1[[2]])
      o <- remove_parenthesis(e1[[3]])
      if (is.call(c) && c[[1]] == as.name("/")) 
        c <- c[c(1,3,2)]
      else
        c <- as.call(list(as.name("/"), 1, c))
      
      list(c = c, v = v, o = o)
    },
    otherwise = function(e) {
      list(c = NULL, v = NULL, o = NULL)
    }
  )
  # browser()
  p <- polinizer(e)
  if (all(unlist(Map(Negate(is.null), p))))
    do.call(polynomial, p, quote = TRUE) 
  else
    NULL
}




