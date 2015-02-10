
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

scale <- function(x, from = c(0, 255), to = c(0, 5), round = TRUE) {
  y <- (x - from[1]) /
    (from[2] - from[1]) *
    (to[2] - to[1]) +
    to[1]

  if (round) {
    round(y)
  } else {
    y
  }
}

capitalize <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

multicol <- function(x) {
  xs <- strip_style(x)
  max_len <- max(nchar(xs))
  to_add <- max_len - nchar(xs)
  x <- paste0(x, substring("            ", 1, to_add))
  screen_width <- getOption("width")
  num_cols <- trunc(screen_width / max_len)
  num_rows <- ceiling(length(x) / num_cols)
  x <- c(x, rep("", num_cols * num_rows - length(x)))
  xm <- matrix(x, ncol = num_cols, byrow = TRUE)
  apply(xm, 1, paste, collapse = "") %+% "\n"
}

re_table <- function(...) {
  lapply(gregexpr(...), function(x) {
    data.frame(
      start = x,
      end = x + attr(x, "match.length") - 1,
      length = attr(x, "match.length")
    )
  })
}

## Create the non-matching table from the matching table

non_matching <- function(table, str, empty = FALSE) {
  mapply(table, str, SIMPLIFY = FALSE, FUN = function(t, s) {
    if (! nrow(t)) {
      data.frame(start = 1, end = base::nchar(s), length = base::nchar(s))
    } else {
      res <- data.frame(start = c(1, t$end + 1),
                      end = c(t$start - 1, base::nchar(s)))
      res$length <- res$end - res$start + 1
      if (!empty) res[ res$length != 0, , drop = FALSE ]
      res
    }
  })
}
