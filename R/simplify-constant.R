
# simplification: constant folding

# custom constant folder function
#' @export
constant_fold <- function(e) {
  # alternative to `codetools::constantFold`
  # keep expr if cannot be folded (constandFold return NULL in that case)
  
  recurse(e, handler = cf_handler)
}

cf_handler <- function(e) {
  if (is.numeric(e)) 
    return(e)
  else if (is.name(e))
    return(e)
  else if (is.call(e)) {
    if (depth(e[[1]]) > 1 && e[[1]][[1]] == quote(gradient))
      return(e)
    else if (as.character(e[[1]]) %in% c("+", "-", "*", "/", "^")) {
      if ((e[[1]] == quote(`+`) || e[[1]] == quote(`-`)) && length(e) == 2)
        e <- as.call(append(as.list(e), 0, 1))
      if (is.numeric(e[[2]]) && is.numeric(e[[3]]))
        e <- eval(e)
    }
  }
  e
}
