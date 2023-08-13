

#' @export
simplify <- function(...) {
  UseMethod("simplify")
}

#' @export
simplify.call <- function(e) {
  recurse(e, simplify_handler)
}

#' @export
simplify.character <- function(e) simplify(str2lang(e))

#' @export
simplify.name <- function(e) e

#' @export
simplify.numeric <- function(e) e

#' @export
`simplify.(` <- function(e) as.call(list(as.name("("), simplify(e[[2]])))


simplify_handler <- function(e) {
  # different simplification procedure

  # Funcall <- function(f, ...) f(...)
  # Reduce(
  #   Funcall, 
  #   list(simplify_other, simplify_expr, simplify_poly, simplify_sort),
  #   e, right = TRUE
  # )
  
  e <- simplify_poly(e)
  e <- simplify_sort(e)
  e <- simplify_expr(e)
  e <- simplify_other(e)
}



simplify_call <- function(opr, e1, e2) {
  e <- call(opr, e1, e2)
  simplify_expr(e)
}

simplify_expr <- function(e) {
  # fold constant, no longer use `codetools::constantFold()`
  e <- constant_fold(e)
  
  e <- simplify_naive(e)
  e <- simplify_parenthesis(e)
  # e <- simplify_shiftvar(e)
  
  e
}

simplify_naive <- function(e) {
  # naive simplification rules
  if (is.call(e)) {
    if (e[[1]] == as.name("+") && length(e) == 3) {
      if (e[[2]] == 0)
        e <- e[[3]]
      else if (e[[3]] == 0)
        e <- e[[2]]
      
    } else if (e[[1]] == as.name("-") && length(e) == 3) {
      if (e[[2]] == 0) {
        e <- e[[3]]
        if (is.call(e) && e[[1]] == as.name("-") && length(e) == 2)
          e <- e[[2]]
        else if (is.call(e) && e[[1]] == as.name("+") && length(e) == 2)
          e[[1]] <- as.name("-")
        else
          e <- call("-", e)
      } else if (e[[3]] == 0)
        e <- e[[2]]
    } else if (e[[1]] == as.name("*")) {
      if (e[[2]] == 0 || e[[3]] == 0)
        e <- 0
      else if (e[[2]] == 1)
        e <- e[[3]]
      else if (e[[3]] == 1)
        e <- e[[2]]
      else if (is.call(e[[3]]) && e[[3]][[1]] == as.name("/")) {
        if (e[[2]] == e[[3]][[3]])
          e <- e[[3]][[2]]
      }
      else if (is.call(e[[3]]) && is.numeric(e[[2]]) && is.numeric(e[[3]][[2]])) {
        if (e[[3]][[1]] == as.name("*")) 
          e <- call("*", e[[2]]*e[[3]][[2]], e[[3]][[3]])
        else if (e[[3]][[1]] == as.name("/"))
          e <- call("*", e[[2]]/e[[3]][[2]], e[[3]][[3]])
      }
    } else if (e[[1]] == as.name("/")) {
      if (e[[3]] == 1) 
        e <- e[[2]]
      else if (e[[2]] == e[[3]])
        e <- 1
      else if (is.call(e[[3]]) && is.numeric(e[[2]]) && is.numeric(e[[3]][[2]])) {
        if (e[[3]][[1]] == as.name("*"))
          e <- call("/", e[[2]]/e[[3]][[2]], e[[3]][[3]])
        else if (e[[3]][[1]] == as.name("/"))
          e <- call("*", e[[2]]/e[[3]][[2]], e[[3]][[3]])
      }
    } else if (e[[1]] == as.name("^")) {
      if (e[[3]] == 0)
        e <- 1
      else if (e[[2]] == 0)
        e <- 0
      else if (e[[3]] == 1)
        e <- e[[2]]
      else if (e[[2]] == 1)
        e <- 1
    }
  }
  e
}

simplify_shiftvar <- function(e) {
  # shift var to right
  if (is.call(e)) {
    if (deparse(e[[1]]) %in% c("+", "-", "*") && length(e) == 3) {
      # shift const to left
      if (is.numeric(e[[3]]) && (is.name(e[[2]]) || is.call(e[[2]])))
        e <- e[c(1,3,2)]
      else if (depth(e[[2]]) > depth(e[[3]]))
        e <- e[c(1,3,2)]
    }
  }
  e
}


simplify_parenthesis <- function(e) {
  # remove parenthesis
  # browser()
  if (is.call(e)) {
    if ((e[[1]] == as.name("+") || e[[1]] == as.name("-")) && length(e) == 3) {
      e1 <- e[[2]]
      e2 <- e[[3]]
      if (
        is.call(e1) && e1[[1]] == as.name("(") && 
        is.call(e1[[2]]) && 
        (e1[[2]][[1]] == as.name("+") || e1[[2]][[1]] == as.name("-"))
      ) {
        # e <- quote((1+x)+1)
        e1 <- e1[[2]]
      } else if (
        is.call(e2) && e2[[1]] == as.name("(") && 
        is.call(e2[[2]]) && 
        (e2[[2]][[1]] == as.name("+") || e2[[2]][[1]] == as.name("-"))
      ) {
        e2 <- e2[[2]]
      }
      
      # e <- if (e[[1]] == as.name("+")) call("+", e1, e2) else call("-", e1, e2)
      # e <- str2lang(paste0(
      #   deparse(e1, width.cutoff = 500L),
      #   if (e[[1]] == as.name("+")) "+" else "-",
      #   deparse(e2, width.cutoff = 500L)
      # ))
      e <- combine_expr(e[[1]], e1, e2)
      
    } else if (e[[1]] == as.name("*") || e[[1]] == as.name("/")) {
      e1 <- e[[2]]
      e2 <- e[[3]]
      if (
        is.call(e1) && e1[[1]] == as.name("(") && 
        is.call(e1[[2]]) && 
        (e1[[2]][[1]] == as.name("*") || e1[[2]][[1]] == as.name("/"))
      ) {
        e1 <- e1[[2]]
      } else if (
        is.call(e2) && e2[[1]] == as.name("(") && 
        is.call(e2) && 
        (e2[[2]][[1]] == as.name("*") || e2[[2]][[1]] == as.name("/"))
      ) {
        e2 <- e2[[2]]
      }
      
      # e <- str2lang(paste0(
      #   deparse(e1, width.cutoff = 500L),
      #   if (e[[1]] == as.name("*")) "*" else "/",
      #   deparse(e2, width.cutoff = 500L)
      # ))
      e <- combine_expr(e[[1]], e1, e2)
      
    }
  }
  e
}

combine_expr <- function(opr, e1, e2) {
  if (
    is.call(e1) && 
    is_higher_precedence(deparse(opr), deparse(e1[[1]]))
  )
    e1 <- deparse(call("(", e1), width.cutoff = 500L)
  else
    e1 <- deparse(e1, width.cutoff = 500L)
  
  if (
    is.call(e2) && 
    is_higher_precedence(deparse(opr), deparse(e2[[1]]))
  )
    e2 <- deparse(call("(", e2), width.cutoff = 500L) 
  else
    e2 <- deparse(e2, width.cutoff = 500L)

  str2lang(paste0(e1, deparse(opr), e2))
}
