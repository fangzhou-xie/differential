
# create gradient generic
# register gradient functions

# TODO: add documentation



#' @export
wrt <- function(...) {
  # package any number of var names and convert into character
  unlist(Map(as.character, substitute(...())), FALSE, FALSE)
}


#' @export
gradient <- function(...) {
  UseMethod("gradient")
}

#' @export
gradient.call <- function(e, wrt_var = wrt()) {
  
  if (is.null(wrt_var) || length(wrt_var) > 1) 
    stop("wrt_var must be of length 1!")
  
  if (is.character(wrt_var))
    w <- str2lang(wrt_var)
  else
    w <- wrt_var
  
  grad(e, w)
}

#' @export
gradient.character <- function(e_chr, wrt_var = wrt()) {
  gradient(str2lang(e_chr), wrt_var)
}

#' @export
gradient.numeric <- function(e_num, wrt_var = wrt()) 0

#' @export
gradient.function <- function(func, wrt_var = wrt()) {
  # main gradient method: with functions
  # 1. match arguments?
  
  # scan all names/operators, insert gradient functions first
  
}


is_simple_func <- function(f_body) {
  if (is.name(f_body) || is.numeric(f_body))
    return(TRUE)
  else if (is.call(f_body)) {
    if (f_body[[1]] != quote(`{`))
      return(TRUE)
    else if (length(f_body) == 2) # f_body[[1]] == quote(`{`)
      return(TRUE)
    else if (length(f_body) == 3) {
      if (is.null(get_assigned_var(f_body[[3]]))) {
        if (is.name(f_body[[3]]))
          return(TRUE)
        else if (is.call(f_body[[3]]) && f_body[[3]][[1]] == quote(return) && 
                 is.name(f_body[[3]][[2]]))
          return(TRUE)
      }
    }
  }
  FALSE
}

# get_assigned_var()
get_assigned_var <- function(e, ass_var = list()) {
  if (is.call(e) && (e[[1]] == quote(`<-`) || e[[1]] == quote(`<-`))) {
    ass_var <- append(ass_var, e[[2]])
    return(get_assigned_var(e[[3]], ass_var))
  } else if (identical(ass_var, list()))
    return(NULL)
  else 
    return(ass_var)
}

# e <- quote(2*x)
# get_assigned_var(e)
