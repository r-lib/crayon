
#' Add style to a string
#'
#' See \code{names(styles)}, or the crayon manual for available styles.
#'
#' @param string Character vector to style.
#' @param as Style function to apply, either the function object,
#'   or its name, or an object to pass to \code{\link{make_style}}.
#' @param bg Background style, a style function, or a name that
#'   is passed to \code{\link{make_style}}.
#' @return Styled character vector.
#'
#' @export
#'
#' @examples
#' ## These are equivalent
#' style("foobar", bold)
#' style("foobar", "bold")
#' bold("foobar")

style <- function(string, as, bg = NULL) {

  if (!is.function(as)) {
    as <- use_or_make_style(as)
  }

  if (is.null(bg)) {
    bg <- identity
    class(bg) <- "crayon"

  } else if (!is.function(bg)) {
    bg <- use_or_make_style(bg, bg = TRUE)

  } else {

  }

  if (!is(as, "crayon")) stop("Cannot make style from 'as'")
  if (!is(bg, "crayon")) stop("Cannot make style from 'bg'")

  as(bg(string))
}

use_or_make_style <- function(style, bg = FALSE) {
  if (style %in% names(styles())) {
    make_crayon(styles()[style])
  } else {
    make_style(style, bg = bg)
  }
}
