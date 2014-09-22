
#' Colored terminal output
#'
#' @docType package
#' @name crayon-package
NULL


## ----------------------------------------------------------------------

#' Does the current R session support color?
#'
#' @return \code{TRUE} is the current R session supports color.
#'
#' @export

has_color <- function() {

  ## Are we in a terminal? No?
  if (!isatty(stdout())) { return(FALSE) }

  ## Are we in a windows terminal?
  ## TODO

  ## COLORTERM set?
  if ("COLORTERM" %in% names(Sys.getenv())) { return(TRUE) }

  ## dumb terminal is not good
  if (Sys.getenv("TERM") == "dumb") { return(FALSE) }

  ## Otherwise try to guess based on TERM
  grepl("^screen|^xterm|^vt100|color|ansi|cygwin|linux",
        Sys.getenv("TERM"), ignore.case = TRUE, perl = TRUE)
}
