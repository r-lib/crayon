
check_string <- function(x) {
  stopifnot(is.character(x), length(x) == 1, !is.na(x))
}

mypaste <- function(..., sep = " ") {
  args <- list(...)
  if (any(!sapply(args, is.character))) stop("Need character strings")
  len <- setdiff(sapply(args, length), 1)
  if (length(len) > 1) {
    stop("All character vectors must have the same length (or length 1)")
  }

  paste(..., sep = sep)
}
