
# sort expr list by call depth
# implement merge sort (for simplification of multiple + and - operations)
# reference: https://stackoverflow.com/questions/26080716


# simplify by +-*/ on non-polynomials
simplify_sort <- function(e) {
  if (depth(e) > 0) {
    # try to sort by + and -
    e_split_plus <- expr2list_plus(e)
    if (length(e_split_plus) > 1)
      e <- Reduce(expr_plus, merge_sort(e_split_plus))
    
    # browser()
    # try to sort by * and /
    e_split_multiply <- expr2list_multiply(e)
    if (length(e_split_multiply) > 1)
      e <- Reduce(expr_multiply, merge_sort(e_split_multiply))
    
    e
  } else
    e
}



compare_depth <- function(e1, e2) {
  # compare depth and number of vars
  (depth(e1) <= depth(e2)) && 
    (length(all.vars(e1)) <= length(all.vars(e2)))
}

merge_sort <- function(e_list) {
  # e_list: list of expr (calls)
  if(length(e_list) > 1) {
    q <- ceiling(length(e_list) / 2)
    a <- merge_sort(e_list[1:q])
    b <- merge_sort(e_list[(q + 1):length(e_list)])
    mmerge(a, b)
  } else {
    e_list
  }
}

mmerge <- function(a, b) {
  r <- vector(mode = "list", length = length(a)+length(b))
  ai <- bi <- j <- 1
  # browser()
  for (j in 1:length(r)) {
    if (bi > length(b) || (ai <= length(a) && compare_depth(a[[ai]], b[[bi]]))) {
      r[[j]] <- a[[ai]]
      ai <- ai+1
    } else {
      # tryCatch(print(b[[bi]]), error=function(e) {print(b); print(bi)})
      r[[j]] <- b[[bi]]
      bi <- bi+1
    }
  }
  r
}



expr2list_plus <- function(e) {
  # split expr into list of exprs
  e_l <- vector(mode = "list", length = depth(e))
  
  for (i in 1:length(e_l)) {
    if (e[[1]] == as.name("+")) {
      e_l[[i]] <- call("+", e[[3]])
      e <- e[[2]]
    } else if (e[[1]] == as.name("-")) {
      e_l[[i]] <- call("-", e[[3]])
      e <- e[[2]]
    } else {
      e_l[[i]] <- call("+", e)
      break
    }
  }
  # print(e_l)
  Filter(Negate(is.null), e_l)
}


expr_plus <- function(e1, e2) {
  # reduce function
  if (is.call(e1) && e1[[1]] == as.name("+") && length(e1) == 2)
    e1 <- e1[[2]]
  
  if (e2[[1]] == as.name("+"))
    call("+", e1, e2[[2]])
  else if (e2[[1]] == as.name("-"))
    call("-", e1, e2[[2]])
}


expr2list_multiply <- function(e) {
  # split expr into list of exprs
  e_l <- vector(mode = "list", length = depth(e))

  for (i in 1:length(e_l)) {
    if (is.call(e) && e[[1]] == as.name("*")) {
      e_l[[i]] <- call("*", e[[3]])
      e <- e[[2]]
    } else if (is.call(e) && e[[1]] == as.name("/")) {
      e_l[[i]] <- call("/", e[[3]])
      e <- e[[2]]
    } else {
      e_l[[i]] <- call("*", e)
      break
    }
  }
  # print(e_l)
  Filter(Negate(is.null), e_l)
}

expr_multiply <- function(e1, e2) {
  # reduce function
  if (is.call(e1) && e1[[1]] == as.name("*") && length(e1) == 2)
    e1 <- e1[[2]]
  
  if (e2[[1]] == as.name("*"))
    call("*", e1, e2[[2]])
  else if (e2[[1]] == as.name("/"))
    call("/", e1, e2[[2]])
}
