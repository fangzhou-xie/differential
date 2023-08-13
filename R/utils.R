
# determine list depth
depth <- function(this) {
  # reference: https://stackoverflow.com/questions/13432863
  # ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
  if (is.call(this)) 1L + max(sapply(this, depth)) else 0L
}
  

# generic recurse function
recurse <- function(e, handler) {
  ..recurse <- function(e) {
    e <- recurse(e, handler)
    handler(e)
  }
  
  if (is.call(e)) {
    e <- as.call(Map(..recurse, e))
    e <- handler(e)
  }
  e
}








# check if expression has control flow
has_controlflow <- function(e) {
  nms <- all.names(e)
  cfs <- c("if", "for", "while", "repeat", "break", "next")
  length(intersect(nms, cfs)) > 0
}

call2chr <- function(cl, ...) {
  # wrap around `deparse` function
  paste(deparse(cl, width.cutoff = 500L, ...), collapse = "\n")
}

deparse2 <- function(cl, ...)
  deparse(cl, width.cutoff = 500L, ...)

not_null <- function(...) !is.null(...)

is_literal <- function(e) {
  # check expression is literal
  class(substitute(e)) %in% c("integer", "numeric", "name")
}

D_differentiables <- function(include_oprs = FALSE) {
  D_rules <- c(
    # "+", "-", "*", "/", "^",
    "exp", "log", "sin", "cos", "tan", "sinh", "cosh", "sqrt", 
    "pnorm", "dnorm", "asin", "acos", "atan", 
    "gamma", "lgamma", "digamma", "trigamma"
    # "psigamma" 
    # for one or two arguments (but derivative only with respect to the first)
  )
  
  if (include_oprs) {
    D_rules <- c(D_rules, "+", "-", "*", "/", "^")
  }
  
  if (getRversion() >= "3.4.0") {
    D_rules <- c(
      D_rules,
      "log1p", "expm1", "log2", "log10", 
      "cospi", "sinpi", "tanpi", "factorial", "lfactorial"
    )
  }
  D_rules
}


is_differentiable_by_D <- function(e) {
  # check expression is supported by `stats::D()`

  expr_oprs <- setdiff(all.names(e), c(all.vars(e), "("))
  length(setdiff(expr_oprs, D_differentiables())) == 0
}

# is_singular_func <- function(func) {
#   # first item in function body is "{" -> must be single line function
#   deparse(body(func))[[1]] != "{"
# }




# replace_argument <- function(e, v_old, v_new) {
#   # e: call
#   # v_old, v_new: character
#   if (is.call(e))
#     return(as.call(Map(function(a) replace_argument(a, v_old, v_new), e)))
#   else if (is.symbol(e) && deparse(e) == v_old)
#     return(str2lang(v_new))
#   else 
#     return(e)
# }


replace_argument <- function(e, v_old, v_new) {
  # e, v_old, v_new: call objects
  if (e == v_old) 
    return(v_new)
  else if (is.call(e))
    return(as.call(Map(function(a) replace_argument(a, v_old, v_new), e)))
  else 
    return(e)
}













is_singular_func <- function(func) {
  func_body <- body(func)
  deparse(func_body[[1]]) != "{" && length(func_body) == 2
}

is_call_unit <- function(cl) {
  # is the call object of depth 1?
  # is.call(e) && !any(unlist(Map(is.call, e), FALSE, FALSE))
  is.call(cl) && all(unlist(Map(Negate(is.call), cl), FALSE, FALSE))
}

is_const_var <- function(cl) {
  # is the call of form: f(const, var) or f(var, const)
  is_call_unit(cl) && 
    length(cl) == 3 &&
    (
      (class(cl[[2]]) %in% c("numeric", "integer") && is.name(cl[[3]])) ||
        (class(cl[[3]]) %in% c("numeric", "integer") && is.name(cl[[2]]))
    )
}


call2func <- function(cl, cl_args, wrt_args) {
  
  func_vars <- ifelse(
    is.null(cl_args),
    paste0(paste0(get_symbols(cl)$vars, "="), collapse = ","),
    paste0(paste0(cl_args, "="), collapse = ", ")
  )
  
  func_args <- ifelse(
    is.null(wrt_args),
    paste0(func_vars, ", "),
    paste0(func_vars, ", wrt = wrt(", wrt_args, ")")
  )
  
  args_alist <- str2lang(paste0("alist(", func_args, ")"))
  subs <- list(args = as.pairlist(eval(args_alist)), body = cl)
  # eval(substitute(`function`(args, body), subs), parent.frame())
  eval(substitute(`function`(args, body), subs))
  # substitute(`function`(args, body), subs)
}

has_gradient <- function(func_chr) {
  
  # check if the input function has gradient defined
  gradient_exist <- gsub(
    ".*[.](.*)$",
    "\\1",
    as.character(methods(gradient))
  )
  
  ifelse(func_chr %in% gradient_exist, TRUE, FALSE)
}


plot_pd <- function(pd) {
  # t = c("text", "name")
  gdf <- data.frame(from = pd$parent, to = pd$id)
  
  vdf <- rbind(pd[c("id", "text")], data.frame(id=0,text="0:"))
  
  pg <- igraph::graph_from_data_frame(gdf, vertices = vdf, directed = TRUE)
  
  p1 <- ggraph::ggraph(pg) +
    ggraph::geom_edge_diagonal() +
    ggraph::geom_node_point() +
    ggraph::geom_node_label(ggplot2::aes(label=text)) + # label=text/name
    ggplot2::theme_void()
  
  p2 <- ggraph::ggraph(pg) +
    ggraph::geom_edge_diagonal() +
    ggraph::geom_node_point() +
    ggraph::geom_node_label(ggplot2::aes(label=name)) + # label=text/name
    ggplot2::theme_void()
  
  cowplot::plot_grid(p1, p2)
}























is_operator <- function(sym) {
  sym_chr <- deparse(sym)
  
  # check the symbol is an operator
  ifelse(
    sym_chr %in% c(
      "+", "-", "*", "/", "^", "%%", "%/%",
      "&", "|", "!",
      "==", "!=", "<", "<=", ">=", ">"
    ),
    TRUE, FALSE
  )
}

is_assign <- function(sym) {
  sym_chr <- deparse(sym)
  
  # check the symbol is assignment operator
  # otherwise, `codetools::getAssignedVar` is not reliable
  ifelse(sym_chr %in% c("=", "<-"), TRUE, FALSE)
}








get_assigned_var <- function(e) {
  if (is_assign(e[[1]])) {
    return(e[[2]])
  } else {
    return(NULL)
  }
}

get_symbols <- function(e) {
  vars <- all.vars(e)
  oprs <- setdiff(all.names(e), vars)
  list(oprs = oprs, vars = vars)
}

# get_symbols2 <- function(e) {
#   
#   get_symbol_call <- function(e, w) {
#     oprs <<- c(oprs, deparse(e[[1]]))
#     
#     for (a in as.list(e[-1])) {
#       codetools::walkCode(a, w)
#     }
#   }
#   
#   get_symbol_leaf <- function(e, w) {
#     if (typeof(e) == "symbol")
#       vars <<- c(vars, deparse(e))
#   }
#   
#   oprs <- c()
#   vars <- c()
#   
#   w <- codetools::makeCodeWalker(
#     call = get_symbol_call,
#     leaf = get_symbol_leaf
#   )
#   codetools::walkCode(e, w)
#   
#   list(oprs = unique(oprs), vars = unique(vars))
# }


get_dependent_var <- function(e) {
  
  get_dep_var_call <- function(e, w) {
    # codetools::walkCode(e[[1]], w)
    
    for (a in as.list(e[-1])) {
      codetools::walkCode(a, w)
    }
  }
  
  get_dep_var_leaf <- function(e, w) {
    if (typeof(e) == "symbol") {
      vars <<- c(vars, deparse(e))
    }
  }
  
  vars <- c()
  
  w <- codetools::makeCodeWalker(
    call = get_dep_var_call,
    leaf = get_dep_var_leaf
  )
  
  codetools::walkCode(e, w)
  
  unique(vars)
}







check_gradient <- function(func_chr) {
  
  # check if the input function has gradient defined
  
  gradient_exist <- gsub(
    ".*[.](.*)$",
    "\\1",
    as.character(methods(gradient))
  )
  
  
  if (!(func_chr %in% gradient_exist))
    stop(paste0("function ", func_chr, " has no gradient defined!"))
}










# TODO: single (nested) line into a function with simple calls
# TODO: need to check transformed function equals original expression
# line2func <- function(line) {
#   # line: cl
#   
#   assign_var <- get_assigned_var(line)
#   if (is.null(assign_var)) return(NULL) # return empty function
#   
#   dep_var <- get_dependent_var(line[[3]]) # dep vars from RHS
#   
#   
# }


RHS2function_unitcall <- function(cl, var_old = NULL, var_new = NULL) {
  # convert unit call expression into function
  
}



# TODO: rewrite original function into single operation functions
RHS2function <- function(cl, var_old, var_new = NULL) {
  # single operation call object to a function
  
  if (!is.null(get_assigned_var(cl))) stop("RHS contains assignment")
  
  cl_fold <- codetools::constantFold(cl)
  dep_vars <- get_dependent_var(cl)
  
  if (!is.null(cl_fold)) {
    expr_chr <- deparse(cl_fold)
  } else if (is.null(var_new)) {
    expr_chr <- deparse(cl)
  } else {
    expr_chr <- deparse(replace_arg_call(cl, var_old, var_new))
  }
  
  return(call2func(str2lang(expr_chr)))
}


# isConstantValue()
# codetools::isConstantValue(1+3)
# isConstantValue(deparse(str2lang("2+3")))
# isConstantValue("pi + 1")


# cl <- quote(y <- x + 1)
# RHS2function(cl, "x", "x_1")
# RHS2function(quote(y <- 1+3), "x", "x_1")
# 
# constantFold(quote(y <- x*2+1*5 -5+4))

# shift_var_right <- function(e) {
#   if (is_call_unit(e)) {
#     print(e)
#     if (typeof(e[[2]]) == "symbol" & 
#         codetools::isConstantValue(e[[3]]))
#       return(e[c(1, 3, 2)])
#     else 
#       return(e)
#   } else {
#     if (!is.null(get_dependent_var(e[[2]])) &
#         codetools::isConstantValue(e[[3]])) 
#       return(shift_var_right(e[c(1, 3, 2)])) # shift symbol to the right
#     else
#       return(shift_var_right(e))
#   }
# }
# 
# 
# fold_constant <- function(e) {
#   # given a RHS call object, try to fold constant as much as possible
#   
#   fold_and_move <- function(e) {
#     # try to fold constant or move constant to left of var
#     
#     if (is_call_unit(e)) {
#       e_consfold <- codetools::constantFold(e)
#       if (is.null(e_consfold))
#         return(e)
#       else
#         return(e_consfold)
#     } else {
#       if (!is.null(get_dependent_var(e[[2]])) &
#           codetools::isConstantValue(e[[3]])) 
#         return(e[c(1, 3, 2)]) # shift symbol to the right
#       else
#         return(e)
#     }
#   }
#   
#   fold_call <- function(e, w) {
#     for (i in seq_along(e)) {
#       print(e)
#       e[[i]] <<- fold_and_move(e[[i]])
#       codetools::walkCode(e[[i]], w)
#     }
#   }
#   
#   w <- codetools::makeCodeWalker(
#     call = fold_call, 
#     leaf = function(e, w) e
#   )
#   
#   codetools::walkCode(e, w)
#   e
# }

# fold_constant(quote(x+3*3))
# codetools::constantFold(quote(x+3*3))



# replace_arg_func <- function(func, var_old, var_new) {
#   
# }



line2vareps <- function(e) {
  # take a single assignment line
  # return assgined var and expression as function (in list)
  
  # assuming the line has assignment
  list(
    var = codetools::getAssignedVar(e), # character name
    eps = e[[3]] # expression call
  )
}



fold_constant <- function(e) {
  if (is.call(e)) {
    if (is_call_unit(e)) {
      
      # TODO: function: fold_constant_level1
      # move constants in front of variables?
      
      e_fold <- codetools::constantFold(e)
      if (!is.null(e_fold)) return(e_fold)
      else return(e)
    } else {
      as.call(Map(fold_constant, e))
    }
  } else {
    return(e)
  }
}


# replace_arg_call <- function(e, var_old, var_new) {
#   
#   if (is.call(e)) {
#     if (is_call_unit(e)) {
#       return(replace_arg_level1(e, var_old, var_new))
#     } else {
#       return(
#         as.call(Map(function(e) replace_arg_call(e, var_old, var_new), e))
#       )
#     }
#   } else {
#     return(e)
#   }
# }
# 
# 
# replace_arg_level1 <- function(e, var_old, var_new) {
#   # e: expression
#   # var_old, var_new: character
#   
#   # helper functions
#   replace_arg_call <- function(e, w) {
#     
#     # TODO: need to call recursively
#     
#     for (i in seq_along(e)) {
#       e[[i]] <<- codetools::walkCode(e[[i]], w)
#     }
#   }
#   
#   replace_arg_leaf <- function(e, w) {
#     if (typeof(e) == "symbol") {
#       if (deparse(e) == var_old) {
#         return(str2lang(var_new))
#       } else if (deparse(e) %in% c("+", "-", "*", "/", "^", "%%", "%/%")) {
#         return(as.name(deparse(e)))
#       } else {
#         return(e)
#       }
#     } else {
#       return(e)
#     }
#   }
#   
#   # e_new <- e # make a copy and modify inplace
#   # e_new <- list() # init empty list and write into it (index?)
#   w <- codetools::makeCodeWalker(
#     call = replace_arg_call,
#     leaf = replace_arg_leaf,
#     write = cat
#   )
#   
#   codetools::walkCode(e, w)
#   
#   e
# }





# e <- quote(2*x_1)
# replace_arg_level1(e, "x_1", "x")
# 
# replace_arg(quote(2 + x ^2), "x", "y")
# 
# 
# 
# 
# Position(
#   function(x) x == quote(1),
#   l
# )
# Position(\(x) x == quote(x*2), l)
# 
# 
# 
# e <- quote(x + 1)
# e_new <- e
# var_old <- "x" # var to be replaced
# var_new <- "y" # outer function argument
# 
# modifyvar_call <- function(e, w) {
#   
#   for (i in seq_along(e)) {
#     print(codetools::walkCode(e[[i]], w))
#     print(e_new[[i]])
#     e_new[[i]] <<- str2lang(codetools::walkCode(e[[i]], w))
#   }
# }
# 
# modifyvar_leaf <- function(e, w) {
#   if (typeof(e) == "symbol") {
#     if (deparse(e) == var_old) {
#       return(var_new)
#     } else if (deparse(e) %in% c("+", "-", "*", "/", "^", "%%", "%/%")) {
#       return(paste0("`", deparse(e), "`"))
#     } else {
#       print("something wrong here")
#       return(deparse(e))
#     }
#   } else {
#     return(deparse(e))
#   }
# }
# 
# w <- codetools::makeCodeWalker(
#   call = modifyvar_call,
#   leaf = modifyvar_leaf,
#   write = cat
# )
# 
# codetools::walkCode(e, w)


# e <- quote(3*x^2+sin(a))
# replace_argument(e, "x", "x_1")
