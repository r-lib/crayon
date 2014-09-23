
#' Convert to character
#'
#' @param x Object to be coerced.
#' @param ... Further arguments to pass to methods.
#' @return Character value.
#'
#' @export

chr <- function(x, ...) as.character(x, ...)

#' Concatenate strings
#'
#' @param lhs Left hand side, character scalar.
#' @param rhs Right hand side, character scalar.
#' @return Concatenated string.
#'
#' @name concat
#' @export

`%+%` <- function(lhs, rhs) {
  check_string(lhs)
  check_string(rhs)
  paste0(lhs, rhs)
}
