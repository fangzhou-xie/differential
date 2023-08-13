
# generalized recurse call (for traversing nested expression)

recurse <- function(e, leaf) {
  ..recurse <- function(e) {
    e <- recurse(e, leaf)
    leaf(e)
  }
  
  if (is.call(e)) {
    e <- as.call(Map(..recurse, e))
    e <- leaf(e)
  }
  e
}
