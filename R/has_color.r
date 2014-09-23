
## ----------------------------------------------------------------------

#' Does the current R session support ANSO colors?
#'
#' The following algorithm is used to detect ANSI support: \itemize{
#'   \item If the \code{crayon.enabled} option is set to \code{TRUE}
#'     with \code{options()}, then \code{TRUE} is returned. If it is
#'     set to something else than \code{TRUE} (typically \code{FALSE}),
#'     then \code{FALSE} is returned.
#'   \item Otherwise, if the standard output is not a terminal, then
#'     \code{FALSE} is returned.
#'   \item Otherwise, if the platform is Windows, \code{TRUE} is returned.
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
  if (.Platform$OS.type == "windows") { return(TRUE) }

  ## COLORTERM set?
  if ("COLORTERM" %in% names(Sys.getenv())) { return(TRUE) }

  ## dumb terminal is not good
  if (Sys.getenv("TERM") == "dumb") { return(FALSE) }

  ## Otherwise try to guess based on TERM
  grepl("^screen|^xterm|^vt100|color|ansi|cygwin|linux",
        Sys.getenv("TERM"), ignore.case = TRUE, perl = TRUE)
}
