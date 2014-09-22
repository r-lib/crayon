
## ----------------------------------------------------------------------

#' Colored terminal output
#'
#' @include utils.r
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


## ----------------------------------------------------------------------
## Styles

codes <- list(
  reset = c(0, 0),
  bold = c(1, 22), # 21 isn't widely supported and 22 does the same thing
  dim = c(2, 22),
  italic = c(3, 23),
  underline = c(4, 24),
  inverse = c(7, 27),
  hidden = c(8, 28),
  strikethrough = c(9, 29),

  black = c(30, 39),
  red = c(31, 39),
  green = c(32, 39),
  yellow = c(33, 39),
  blue = c(34, 39),
  magenta = c(35, 39),
  cyan = c(36, 39),
  white = c(37, 39),
  gray = c(90, 39),

  bgBlack = c(40, 49),
  bgRed = c(41, 49),
  bgGreen = c(42, 49),
  bgYellow = c(43, 49),
  bgBlue = c(44, 49),
  bgMagenta = c(45, 49),
  bgCyan = c(46, 49),
  bgWhite = c(47, 49)
)

make_style <- function(code) {
  list(
    open <- '\u001b[' %+% chr(codes[[code]][1]) %+% 'm',
    close <- '\u001b[' %+% chr(codes[[code]][2]) %+% 'm'
  )
}

styles <- lapply(names(codes), make_style)
names(styles) <- names(codes)


## ----------------------------------------------------------------------
