
#' Add style to a string
#'
#' See \code{names(styles)}, or the crayon manual for available styles.
#'
#' @param string Character vector to style.
#' @param as Style function to apply, either the function object,
#'   or its name.
#' @return Styled character vector.
#'
#' @export
#'
#' @examples
#' ## These are equivalent
#' style("foobar", bold)
#' style("foobar", "bold")
#' bold("foobar")

style <- function(string, as) {
  if (is.function(as)) {
    fun <- as
  } else if (is.character(as)) {
    fun <- get(as)
  }
  if (!is(fun, "crayon")) {
    stop("'as' must be a style function, or its name")
  }
  fun(string)
}
