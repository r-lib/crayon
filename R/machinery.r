
## ----------------------------------------------------------------------

crayon_template <- function(...) {
  my_styles <- unlist(attr(sys.function(), "_styles"))
  if (any(!my_styles %in% names(styles))) {
    stop("Unknown styles:",
         paste(setdiff(my_styles, names(styles)), collapse = ","))
  }
  text <- mypaste(...)
  if (has_color()) {
    for (st in rev(my_styles)) {
      open <- styles[[st]]$open
      close <- styles[[st]]$close
      text <- open %+% gsub(close, open, text, fixed = TRUE) %+% close
    }
  }
  text
}

make_style <- function(style) {
  crayon <- crayon_template
  attr(crayon, "_styles") <- list(style)
  class(crayon) <- "crayon"
  crayon
}

#' @export
#' @method "$" crayon

`$.crayon` <- function(crayon, style) {
  attr(crayon, "_styles") <- c(attr(crayon, "_styles"), style)
  crayon
}

#' @include styles.r
#' @param ... Strings to style.
#' @name crayon
#
#' @details
#'
#' Styles: reset bold blurred italic underline inverse hidden strikethrough
#' black red green yellow blue magenta cyan white silver
#' bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite.
#'
#' @aliases reset bold blurred italic underline inverse hidden strikethrough
#'    black red green yellow blue magenta cyan white silver
#'    bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite
#'
#' @export reset bold blurred italic underline inverse hidden strikethrough
#' @export black red green yellow blue magenta cyan white silver
#' @export bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite

sapply(names(styles), function(style) {
  assign(style, make_style(style), envir = asNamespace(packageName()))
})
