
# other simplification 
# TODO: add other simplification rules here

simplify_other <- function(e) {
  if (is.call(e)) {
    if (e[[1]] == as.name("+") && length(e) == 3) {
      e <- simplify_other_plus(e)
    } else if (e[[1]] == as.name("-") && length(e) == 3) {
      e <- simplify_other_minus(e)
    }
  }
  e
}

simplify_other_plus <- function(e) {
  e1 <- e[[2]]
  e2 <- e[[3]]
  
  if (e1 == e2) {
    e <- call("*", 2, e1) 
  } else if (
    is.call(e2) && e2[[1]] == as.name("*") && 
    is_namenum(e2[[2]]) && e1 == e2[[3]]
  ) {
    e <- simplify_call("*", simplify_call("+", 1, e2[[2]]), e1)
  } else if (
    is.call(e1) && is.call(e2) &&
    e1[[1]] == as.name("*") && e2[[1]] == as.name("*") &&
    is_namenum(e1[[2]]) && is_namenum(e2[[2]]) &&
    e1[[3]] == e2[[3]]
  ) {
    e <- simplify_call("*", simplify_call("+", e1[[2]], e2[[2]]), e1[[3]])
  }
  e
}

simplify_other_minus <- function(e) {
  e1 <- e[[2]]
  e2 <- e[[3]]
  
  if (e1 == e2) {
    e <- 0
  } else if (
    is.call(e2) && e2[[1]] == as.name("*") && 
    is_namenum(e2[[2]]) && e1 == e2[[3]]
  ) {
    e <- simplify_call("*", simplify_call("-", 1, e2[[2]]), e1)
  } else if (
    is.call(e1) && is.call(e2) &&
    e1[[1]] == as.name("*") && e2[[1]] == as.name("*") &&
    is_namenum(e1[[2]]) && is_namenum(e2[[2]]) &&
    e1[[3]] == e2[[3]]
  ) {
    e <- simplify_call("*", simplify_call("-", e1[[2]], e2[[2]]), e1[[3]])
  }
  e
}
