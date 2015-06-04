
data_frame <- function(...) {

  args <- list(...)

  ## Replicate arguments if needed
  len <- vapply(args, length, numeric(1))
  stopifnot(length(setdiff(len, 1)) <= 1)
  len <- max(0, max(len))
  args <- lapply(args, function(x) rep(x, length.out = len))

  ## Names
  names <- as.character(names(args))
  length(names) <- length(args)
  names <- ifelse(
    is.na(names) | names == "",
    paste0("V", seq_along(args)),
    names)

  structure(args,
            class = "data.frame",
            names = names,
            row.names = seq_along(args[[1]]))
}

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
    res <- data_frame(
      start = x,
      end = x + attr(x, "match.length") - 1,
      length = attr(x, "match.length")
    )
    res <- res[res$start != -1, ]
  })
}

## Create the non-matching table from the matching table

non_matching <- function(table, str, empty = FALSE) {
  mapply(table, str, SIMPLIFY = FALSE, FUN = function(t, s) {
    if (! nrow(t)) {
      data_frame(start = 1, end = base::nchar(s), length = base::nchar(s))
    } else {
      res <- data_frame(start = c(1, t$end + 1),
                        end = c(t$start - 1, base::nchar(s)))
      res$length <- res$end - res$start + 1
      if (!empty) res[ res$length != 0, , drop = FALSE ]
      res
    }
  })
}

myseq <- function(from, to, by = 1) {
  stopifnot(by != 0)
  if (by > 0) {
    if (to < from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  } else {
    if (to > from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  }
}

`%:%` <- myseq

emacs_version <- function() {
  ver <- Sys.getenv("INSIDE_EMACS")
  if (ver == "") return(NA_integer_)

  ver <- strsplit(ver, ",", fixed = TRUE)[[1]]
  ver <- strsplit(ver, ".", fixed = TRUE)[[1]]
  as.numeric(ver)
}

inside_emacs <- function() {
  Sys.getenv("EMACS") != ""
}
