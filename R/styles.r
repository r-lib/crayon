
## ----------------------------------------------------------------------
## Styles

codes <- list(
  reset = c(0, 0),
  bold = c(1, 22), # 21 isn't widely supported and 22 does the same thing
  blurred = c(2, 22),
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
  silver = c(90, 39),

  bgBlack = c(40, 49),
  bgRed = c(41, 49),
  bgGreen = c(42, 49),
  bgYellow = c(43, 49),
  bgBlue = c(44, 49),
  bgMagenta = c(45, 49),
  bgCyan = c(46, 49),
  bgWhite = c(47, 49)
)

make_chr_style <- function(code) {
  list(
    open = '\u001b[' %+% chr(codes[[code]][1]) %+% 'm',
    close = '\u001b[' %+% chr(codes[[code]][2]) %+% 'm'
  )
}

#' ANSI escape sequences of crayon styles
#'
#' You can use this object to list all availables crayon styles,
#' via \code{names(styles)}, or to explicitly apply an ANSI
#' escape seauence to a string.
#'
#' @format A named list. Each list element is a list of two
#'   strings, named \sQuote{open} and \sQuote{close}.
#'
#' @seealso \code{\link{crayon}} for the beginning of the crayon manual.
#' @export
#' @examples
#' names(styles)
#' cat(styles[["bold"]]$close)

styles <- lapply(names(codes), make_chr_style)
names(styles) <- names(codes)
