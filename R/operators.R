# check precedence of operators

opr_precedence_order <- function(opr) {
  # precedence order:
  # https://stat.ethz.ch/R-manual/R-devel/library/base/html/Syntax.html
  if (class(opr) != "character") stop("operator not character")
  opr <- if (grepl("%.*%", opr)) "%any%" else opr
  opr_order <- switch(
    opr,
    "::" = 18L, ":::" = 18L,
    "$" = 17L, "@" = 17L,
    "[" = 16L, "[[" = 16L,
    "^" = 15L,
    "-1" = 14L, "+1" = 14L, # TODO: convert unary minus and plus
    ":" = 13L,
    "%any%" = 12L, "|>" = 12L,
    "*" = 11L, "/" = 11L,
    "+" = 10L, "-" = 10L,
    "<" = 9L, ">" = 9L, "<=" = 9L, ">=" = 9L, "==" = 9L, "!=" = 9L,
    "!" = 8L,
    "&" = 7L, "&&" = 7L,
    "|" = 6L, "||" = 6L,
    "~" = 5L,
    "->" = 4L, "->>" = 4L,
    "<-" = 3L, "<<-" = 3L,
    "=" = 2L,
    "?" = 1L
  )
  if (is.null(opr_order)) 19L else opr_order
}

is_equal_precedence <- function(opr1, opr2) 
  opr_precedence_order(opr1) == opr_precedence_order(opr2)

is_higher_precedence <- function(opr1, opr2) 
  opr_precedence_order(opr1) > opr_precedence_order(opr2)

is_lower_precedence <- function(opr1, opr2) 
  opr_precedence_order(opr1) < opr_precedence_order(opr2)
