
# gradient by working on call objects

#' @export
D2 <- function(expr, name) {
  # extend `stats::D` with same interface
  # create a temp var as name, then grad wrt temp var
  name <- if (is.character(name)) str2lang(name) else name
  stopifnot(is.language(name))
  
  # create temp var to be differentiated
  v_new <- "tmp____var"
  while (v_new %in% all.vars(expr)) 
    v_new <- paste0(v_new, "__1")
  v_new <- str2lang(v_new)
  
  # take deriv by `stats::D`
  e <- replace_argument(simplify(expr), name, v_new)
  e_grad <- stats::D(e, as.character(v_new))
  e_grad <- simplify(e_grad)
  
  # convert the expression back, plug in the grad expr
  replace_argument(e_grad, v_new, name)
}


grad <- function(e, w) {
  if (is.name(e) && e == w) 
    return(1)
  else if ((is.name(e) && e != w) || is.numeric(e))
    return(0)
  else if (is.call(e)) {
    if (e[[1]] == as.name("+")) 
      g <- grad_call_plus(e, w)
    else if (e[[1]] == as.name("-"))
      g <- grad_call_minus(e, w)
    else if (e[[1]] == as.name("*"))
      g <- grad_call_multiply(e, w)
    else if (e[[1]] == as.name("/"))
      g <- grad_call_divide(e, w)
    else if (e[[1]] == as.name("^"))
      g <- grad_call_power(e, w)
    else if (as.character(e[[1]]) %in% D_differentiables())
      g <- grad_call_D(e, w)
    else if (as.character(e[[1]]) == "psigamma")
      g <- grad_call_psigamma(e, w)
    else {
      # all other functions not supported by `stats::D()`
      # TODO: add args inside function
      g_list <- append(
        as.call(list(as.name("gradient"), as.name("f"), str2lang(paste0("wrt(", w, ")")))),
        as.list(e[-1])
      )
      g <- as.call(g_list)
    }
    
    return(g)
  }
    
}

grad_call_plus <- function(e, w) {
  # transform unary operations into binary ones (only for + and -)
  if (length(e) == 2)
    e <- as.call(append(as.list(e), 0, 1))
  
  g1 <- grad(e[[2]], w)
  g2 <- grad(e[[3]], w)
  e_grad <- simplify_call("+", g1, g2)
  simplify(e_grad)
}

grad_call_minus <- function(e, w) {
  # transform unary operations into binary ones (only for + and -)
  if (length(e) == 2)
    e <- as.call(append(as.list(e), 0, 1))
  
  g1 <- grad(e[[2]], w)
  g2 <- grad(e[[3]], w)
  e_grad <- simplify_call("-", g1, g2)
  simplify(e_grad)
}

grad_call_multiply <- function(e, w) {
  g1 <- grad(e[[2]], w)
  g2 <- grad(e[[3]], w)
  # browser()
  g1_e2 <- simplify_call("*", g1, e[[3]])
  g2_e1 <- simplify_call("*", g2, e[[2]])
  
  e_grad <- simplify_call("+", g1_e2, g2_e1)
  # browser()
  simplify(e_grad)
}

grad_call_divide <- function(e, w) {
  g1 <- grad(e[[2]], w)
  g2 <- grad(e[[3]], w)
  
  g1_e2 <- simplify_call("*", g1, e[[3]])
  g2_e1 <- simplify_call("*", g2, e[[2]])
  
  g1_e2_g2_e1 <- simplify_call("-", g1_e2, g2_e1)
  e2_2 <- simplify_call("^", e[[3]], 2)
  
  e_grad <- simplify_call("/", g1_e2_g2_e1, e2_2)
  
  simplify(e_grad)
}

grad_call_power <- function(e, w) {
  # form: f(x)^g(x)
  # grad: f(x)^g(x)[grad(g,x)log(f(x)) + g(x)/f(x)]
  f <- e[[2]]
  g <- e[[3]]
  
  grad_g <- grad(g, w)
  
  if (grad_g == 0) {
    g_minus1 <- simplify_call("-", g, 1)
    f_power_g_minus1 <- simplify_call("^", f, g_minus1)
    
    e_grad <- simplify_call("*", g, f_power_g_minus1)
  } else {
    log_f <- call("log", f)
    f_power_g <- simplify_call("^", f, g)
    grad_g_x_log_f <- simplify_call("*", grad(g, w), log_f)
    g_over_f <- simplify_call("/", g, f)
    grad_g_x_log_f_plus_g_over_f <- simplify_call(
      "+",
      grad_g_x_log_f,
      g_over_f
    )
    
    e_grad <- simplify_call("*", f_power_g, grad_g_x_log_f_plus_g_over_f)
  }
  
  simplify(e_grad)
}

grad_call_D <- function(e, w) {
  # if (as.character(e[[1]]) %in% D_differentiables()) {
  #   e_grad <- D2(e, w)
  # } else 
  #   stop(paste0("function ", deparse(e[[1]]), " not supported yet"))
  D2(e, w)
}

grad_call_psigamma <- function(e, w) {
  if (length(e) == 2)
    e[[3]] <- 1L
  else 
    e[[3]] <- e[[3]] + 1L
  e
}

