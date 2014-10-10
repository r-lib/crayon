
## ----------------------------------------------------------------------

crayon_template <- function(...) {
  my_styles <- attr(sys.function(), "_styles")
  text <- mypaste(...)
  if (has_color()) {
    for (st in rev(my_styles)) {
      text <- st$open %+%
        gsub(st$close, st$open, text, fixed = TRUE) %+%
        st$close
    }
  }
  text
}

hash_color_regex <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$"

is_builtin_style <- function(x) {
  x %in% names(builtin_styles)
}

is_r_color <- function(x) {
  x %in% colors() || grepl(hash_color_regex, x)
}

is_rgb_matrix <- function(x) {
  is.matrix(x) && is.numeric(x) && (nrow(x) == 3 || nrow(x) == 4)
}

style_from_r_color <- function(color, bg) {
  style_from_rgb(col2rgb(color), bg)
}

style_8_from_rgb <- function(rgb, bg) {
  ansi_cols <- if (bg) ansi_bg_rgb else ansi_fg_rgb
  dist <- colSums((ansi_cols - as.vector(rgb)) ^ 2 )
  builtin_name <- names(which.min(dist))[1]
  builtin_styles[[builtin_name]]
}

style_from_rgb <- function(rgb, bg) {
  if (num_colors() < 256) { return(style_8_from_rgb(rgb, bg)) }
  ansi256(rgb, bg)
}

#' Create an ANSI color style
#'
#' TODO
#'
#' @param ... The style to create. See details below.
#' @param bg Whether the color applies to the background.
#' @return A function that can be used to color strings.
#'
#' @export

make_style <- function(..., bg = FALSE) {

  args <- list(...)
  stopifnot(length(args) == 1)
  style <- args[[1]]
  style_name <- names(args)[1]

  stopifnot(is.character(style) && length(style) == 1 ||
            is_rgb_matrix(style) && ncol(style) == 1,
            is.logical(bg) && length(bg) == 1)

  ansi_seqs <- if (is_builtin_style(style)) {
    if (bg) { style <- "bg" %+% capitalize(style) }
    if (is.null(style_name)) style_name <- style
    builtin_styles[[style]]

  } else if (is_r_color(style)) {
    if (is.null(style_name)) style_name <- style
    style_from_r_color(style, bg)

  } else if (is_rgb_matrix(style)) {
    style_from_rgb(style, bg)

  } else {
    stop("Unknown style specification: ", style)
  }

  if (!is.null(style_name)) { styles[[style_name]] <<- ansi_seqs }

  crayon <- crayon_template
  attr(crayon, "_styles") <- structure(list(ansi_seqs), names = style_name)
  class(crayon) <- "crayon"
  crayon
}

#' @export
#' @method "$" crayon

`$.crayon` <- function(crayon, style) {
  attr(crayon, "_styles") <- c(attr(crayon, "_styles"), styles[style])
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
NULL

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

styles <- structure(list(), names = character())

sapply(names(builtin_styles), function(style) {
  assign(style, make_style(style), envir = asNamespace(packageName()))
})

.onAttach <- function(libname, pkgname) {
  ub <- unlockBinding
  ub("styles", asNamespace(packageName()))
}
