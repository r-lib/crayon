
## ----------------------------------------------------------------------

#' Does the current R session support ANSI colors?
#'
#' @details
#' The following algorithm is used to detect ANSI support: \itemize{
#'   \item If the \code{crayon.enabled} option is set to \code{TRUE}
#'     with \code{options()}, then \code{TRUE} is returned. If it is
#'     set to something else than \code{TRUE} (typically \code{FALSE}),
#'     then \code{FALSE} is returned.
#'   \item Otherwise, if the standard output is not a terminal, then
#'     \code{FALSE} is returned.
#'   \item Otherwise, if the platform is Windows, \code{FALSE} is returned.
#'   \item Otherwise, if the \code{COLORTERM} environment variable is
#'     set, \code{TRUE} is returned.
#'   \item Otherwise, if the \code{TERM} environment variable starts
#'     with \code{screen}, \code{xterm} or \code{vt100}, or matches
#'     \code{color}, \code{ansi}, \code{cygwin} or \code{linux}
#'     (with case insentive matching), then \code{TRUE} is returned.
#'   \item Otherwise \code{FALSE} is returned.
#' }
#'
#' @return \code{TRUE} if the current R session supports color.
#'
#' @export
#' @examples
#' has_color()

has_color <- function() {

  ## Colors forced?
  enabled <- getOption("crayon.enabled")
  if (!is.null(enabled)) { return(isTRUE(enabled))  }

  ## Are we in a terminal? No?
  if (!isatty(stdout())) { return(FALSE) }

  ## Are we in a windows terminal?
  if (.Platform$OS.type == "windows") { return(FALSE) }

  ## Running in a recent Emacs?
  if (inside_emacs() && emacs_version()[1] >= 23) { return(TRUE) }

  ## COLORTERM set?
  if ("COLORTERM" %in% names(Sys.getenv())) { return(TRUE) }

  ## dumb terminal is not good
  if (Sys.getenv("TERM") == "dumb") { return(FALSE) }

  ## Otherwise try to guess based on TERM
  grepl("^screen|^xterm|^vt100|color|ansi|cygwin|linux",
        Sys.getenv("TERM"), ignore.case = TRUE, perl = TRUE)
}

#' Number of colors the terminal supports
#'
#' @details
#' We use the \code{tput} shell command to detect the
#' number of colors. If \code{tput} is not available,
#' but we think that the terminal supports colors, then
#' eigth colors are assumed.
#'
#'
#' For efficiency, \code{num_colors} caches its result. To
#' re-check the number of colors, set the \code{forget} argument to
#' \code{TRUE}.
#'
#' @param forget Whether to forget the cached result of the color check.
#' @return Numeric scalar, the number of colors the terminal supports.
#' @export
#' @examples
#' num_colors()

num_colors <- function(forget = FALSE) {
  if (forget) memoise::forget(i_num_colors)
  i_num_colors()
}

i_num_colors <- memoise::memoise(function() {
  if (!has_color()) { return(1) }
  if (inside_emacs()) { return(8) }
  cols <- suppressWarnings(try(silent = TRUE,
              as.numeric(system("tput colors", intern = TRUE))))
  if (inherits(cols, "try-error") || !length(cols) || is.na(cols)) { return(8) }
  if (cols %in% c(-1, 0, 1)) { return(1) }
  cols
})
