
## ----------------------------------------------------------------------

#' Does the current R session support ANSI colors?
#'
#' @details
#' The following algorithm is used to detect ANSI support: \itemize{
#'   \item If the `crayon.enabled` option is set to `TRUE`
#'     with [options()], then `TRUE` is returned. If it is
#'     set to something else than `TRUE` (typically `FALSE`),
#'     then `FALSE` is returned.
#'   \item Otherwise, if the standard output is not a terminal, then
#'     `FALSE` is returned.
#'   \item Otherwise, if the platform is Windows, `TRUE` is returned
#'     if running in ConEmu (\url{https://conemu.github.io/}) or
#'     cmder (\url{http://cmder.net}) with ANSI color support.
#'     Otherwise `FALSE` is returned.
#'   \item Otherwise, if the `COLORTERM` environment variable is
#'     set, `TRUE` is returned.
#'   \item Otherwise, if the `TERM` environment variable starts
#'     with `screen`, `xterm` or `vt100`, or matches
#'     `color`, `ansi`, `cygwin` or `linux`
#'     (with case insentive matching), then `TRUE` is returned.
#'   \item Otherwise `FALSE` is returned.
#' }
#'
#' @section Sinks:
#' Note that `has_color()` returns `FALSE` if a sink is active
#' (see [sink()]). It assumes that the constructed string will be printed
#' to the standard output, and `sink()` redirects to a file, and usually
#' you don't want ANSI colors in the file.
#'
#' The same applies to the case when R's standard output is redirected
#' to a file, from the command line, e.g.:
#' ```
#' R -q -e 'cat(crayon::red("no color here\n"))' > /tmp/crayon-test.txt
#' cat /tmp/crayon-test.txt
#' ```
#'
#' @return `TRUE` if the current R session supports color.
#'
#' @export
#' @examples
#' has_color()

has_color <- function() {

  ## Colors forced?
  enabled <- getOption("crayon.enabled")
  if (!is.null(enabled)) { return(isTRUE(enabled))  }

  ## RStudio with (potential) ANSI support?
  if (rstudio_with_ansi_support() && sink.number() == 0) {
    return(TRUE)
  }

  ## Are we in a terminal? No?
  if (!isatty(stdout())) { return(FALSE) }

  ## Are we in a windows terminal with color support?
  if (os_type() == "windows") {
    if (Sys.getenv("ConEmuANSI") == "ON") { return(TRUE) }
    if (Sys.getenv("CMDER_ROOT") != "") { return(TRUE) }

    ## Are we in another windows terminal or GUI? :(
    return(FALSE)
  }

  ## Running in a recent Emacs?
  if (inside_emacs() && ! is.na(emacs_version()[1]) && emacs_version()[1] >= 23) { return(TRUE) }

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
#' If the `crayon.colors` option is set, then we
#' just use that. It should be an integer number. You can use this
#' option as a workaround if crayon does not detect the number of
#' colors accurately.
#'
#' In Emacs, we report eight colors.
#'
#' Otherwise, we use the `tput` shell command to detect the
#' number of colors. If `tput` is not available,
#' but we think that the terminal supports colors, then
#' eigth colors are assumed.
#'
#' If tput returns 8, but TERM is xterm, we return 256, as most xterm
#' compatible terminals in fact do support 256 colors.
#' There is some discussion about this here:
#' \url{https://github.com/r-lib/crayon/issues/17}
#'
#' For efficiency, `num_colors()` caches its result. To
#' re-check the number of colors, set the `forget` argument to
#' `TRUE`. The cached value is only used if no sinks are active,
#' see also [has_color()] for more information about sinks.
#'
#' @param forget Whether to forget the cached result of the color check.
#' @return Numeric scalar, the number of colors the terminal supports.
#' @export
#' @examples
#' num_colors()

num_colors <- (function() {
  # closure env to store state

  cache <- NULL

  function(forget=FALSE) {
    if (forget || is.null(cache)) {
      result <- i_num_colors()
      if (sink.number() == 0 && rstudio_initialized()) {
        cache <<- result
      }
    } else {
      result <- cache
    }
    result
  }
})()

i_num_colors <- function() {

  ## Number of colors forced
  cols <- getOption("crayon.colors")
  if (!is.null(cols)) { return(as.integer(cols)) }

  ## RStudio
  if (rstudio_with_ansi_support()) { return(256) }

  if (!has_color()) { return(1) }

  ## Emacs
  if (inside_emacs()) { return(8) }

  ## Are we in a windows terminal with color support?
  if (os_type() == "windows") {
    if (Sys.getenv("ConEmuANSI") == "ON" ||
        Sys.getenv("CMDER_ROOT") != "") {
      return(get_terminal_colors())
    }

    ## Are we in another windows terminal or GUI? :(
    return(1)
  }

  ## Otherwise
  get_terminal_colors()
}

get_terminal_colors <- function() {
  ## Try to run tput colors. If it did not run, but has_colors() is TRUE,
  ## then we just report 8 colors
  cols <- suppressWarnings(try(
    silent = TRUE,
    as.numeric(system("tput colors 2>/dev/null", intern = TRUE))[1]
  ))
  if (inherits(cols, "try-error") || !length(cols) || is.na(cols)) { return(8) }
  if (cols %in% c(-1, 0, 1)) { return(1) }

  ## See comment above in docs
  if (cols == 8 && identical(Sys.getenv("TERM"), "xterm")) cols <- 256

  cols
}
