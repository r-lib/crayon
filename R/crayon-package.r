
## ----------------------------------------------------------------------

#' Colored terminal output
#'
#' @include utils.r
#' @include string.r
#' @docType package
#' @name crayon
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
  dark = c(2, 22),
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

styles <- lapply(names(codes), make_chr_style)
names(styles) <- names(codes)


## ----------------------------------------------------------------------
## API design
"
# style a string
blue('Hello world!')

# combine styled and normal strings
blue('Hello') %+% 'World' %+% red('!')

# compose multiple styles using the chainable API
blue$bgRed$bold('Hello world!')

// pass in multiple arguments
blue('Hello', 'World!', 'Foo', 'bar', 'biz', 'baz')

// nest styles
red('Hello', underline.bgBlue('world') %+% '!')

// nest styles of the same type even (color, underline, background)
green(
    'I am a green line ' %+%
    blue$underline$bold('with a blue substring') %+%
    ' that becomes green again!'
)

# Easily define your own themes.

alert <- bold$red
cat(alert('Watch out!'))
"

crayon_template <- function(...) {
  my_styles <- unlist(attr(sys.function(), "_styles"))
  if (any(!my_styles %in% names(styles))) {
      stop("Unknown styles:",
           paste(setdiff(my_styles, names(styles)), collapse = ","))
    }
  open <- sapply(styles[my_styles], "[[", "open")
  close <- sapply(styles[my_styles], "[[", "close")
  paste0(open, collapse = "") %+%
    paste0(..., collapse = "") %+%
    paste0(close, collapse = "")
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

#' @param ... Strings to style.
#' @name crayon
#
#' @details
#'
#' Styles: reset bold dark italic underline inverse hidden strikethrough
#' black red green yellow blue magenta cyan white silver
#' bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite.
#'
#' @aliases reset bold dark italic underline inverse hidden strikethrough
#'    black red green yellow blue magenta cyan white silver
#'    bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite
#'
#' @export reset bold dark italic underline inverse hidden strikethrough
#' @export black red green yellow blue magenta cyan white silver
#' @export bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite

for (style in names(styles)) {
  assign(style, make_style(style), envir = asNamespace(packageName()))
}

#' @export
#' @method print crayon

print.crayon <- function(x, ...) {
  ## TODO
  print.default(x, ...)
}
