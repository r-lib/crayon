
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
#'
#' @usage
#' ## Simple styles
#' red(...)
#' bold(...)
#' ...
#'
#' ## See more styling below
#'
#' @param ... Strings to style.
#' @name crayon
#
#' @details
#'
#' Crayon defines several styles, that can be combined. Each style in the list
#' has a corresponding function with the same name.
#'
#' @section Genaral styles:
#'
#' \itemize{
#'   \item reset
#'   \item bold
#'   \item blurred (usually called \sQuote{dim}, renamed to avoid name clash)
#'   \item italic (not widely supported)
#'   \item underline
#'   \item inverse
#'   \item hidden
#'   \item strikethrough (not widely supported)
#' }
#'
#' @section Text colors:
#'
#' \itemize{
#'   \item black
#'   \item red
#'   \item green
#'   \item yellow
#'   \item blue
#'   \item magenta
#'   \item cyan
#'   \item white
#'   \item silver (usually called \sQuote{gray}, renamed to avoid name clash)
#' }
#'
#' @section Background colors:
#'
#' \itemize{
#'   \item bgBlack
#'   \item bgRed
#'   \item bgGreen
#'   \item bgYellow
#'   \item bgBlue
#'   \item bgMagenta
#'   \item bgCyan
#'   \item bgWhite
#' }
#'
#' @section Styling:
#'
#' The styling functions take any number of character vectors as arguments,
#' and they concatenate and style them: \preformatted{  library(crayon)
#'   cat(blue("Hello", "world!\n"))
#' }
#'
#' Crayon defines the \code{\%+\%} string concatenation operator, to make it easy
#' to assemble stings with different styles. \preformatted{  cat("... to highlight the " \%+\% red("search term") \%+\%
#'       " in a block of text\n")
#' }
#'
#' Styles can be combined using the \code{$} operator: \preformatted{  cat(yellow$bgMagenta$bold('Hello world!\n'))
#' }
#'
#' Styles can also be nested, and then inner style takes
#' precedence: \preformatted{  cat(green(
#'     'I am a green line ' \%+\%
#'     blue$underline$bold('with a blue substring') \%+\%
#'     ' that becomes green again!\n'
#'   ))
#' }
#'
#' It is easy to define your own themes: \preformatted{  error <- red $ bold
#'   warn <- magenta $ underline
#'   note <- cyan
#'   cat(error("Error: subscript out of bounds!\n"))
#'   cat(warn("Warning: shorter argument was recycled.\n"))
#'   cat(note("Note: no such directory.\n"))
#' }
#'
#' @aliases "$.crayon"
#'    reset bold blurred italic underline inverse hidden strikethrough
#'    black red green yellow blue magenta cyan white silver
#'    bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite
#'
#' @export reset bold blurred italic underline inverse hidden strikethrough
#' @export black red green yellow blue magenta cyan white silver
#' @export bgBlack bgRed bgGreen bgYellow bgBlue bgMagenta bgCyan bgWhite
#'
#' @examples
#' cat(blue("Hello", "world!"))
#'
#' cat("... to highlight the " \%+\% red("search term") \%+\%
#'     " in a block of text")
#'
#' cat(yellow$bgMagenta$bold('Hello world!'))
#'
#' cat(green(
#'  'I am a green line ' \%+\%
#'  blue$underline$bold('with a blue substring') \%+\%
#'  ' that becomes green again!'
#' ))
#'
#' error <- red $ bold
#' warn <- magenta $ underline
#' note <- cyan
#' cat(error("Error: subscript out of bounds!"))
#' cat(warn("Warning: shorter argument was recycled."))
#' cat(note("Note: no such directory."))
#'

sapply(names(styles), function(style) {
  assign(style, make_style(style), envir = asNamespace(packageName()))
})
