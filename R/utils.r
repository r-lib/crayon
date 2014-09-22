
check_string <- function(x) {
  stopifnot(is.character(x), length(x) == 1, !is.na(x))
}

chr <- as.character

`%+%` <- function(lhs, rhs) {
  check_string(lhs)
  check_string(rhs)
  paste0(lhs, rhs)
}
