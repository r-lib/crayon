
## ----------------------------------------------------------------------
## API design
"
# style a string
blue('Hello world!') %>% cat('\n')

# combine styled and normal strings
blue('Hello') %+% ' World' %+% red('!') %>% cat('\n')

# compose multiple styles using the chainable API
blue$bgRed$bold('Hello world!') %>% cat('\n')

// pass in multiple arguments
blue('Hello', 'World!', 'Foo', 'bar', 'biz', 'baz') %>% cat('\n')

// nest styles
red('Hello', underline$bgBlue('world') %+% '!') %>% cat('\n')

// nest styles of the same type even (color, underline, background)
green(
    'I am a green line ' %+%
    blue$underline$bold('with a blue substring') %+%
    ' that becomes green again!'
) %>% cat('\n')

# Easily define your own themes.

alert <- bold$red
alert('Watch out!') %>% cat('\n')
"

crayon_template <- function(...) {
  my_styles <- unlist(attr(sys.function(), "_styles"))
  if (any(!my_styles %in% names(styles))) {
    stop("Unknown styles:",
         paste(setdiff(my_styles, names(styles)), collapse = ","))
  }
  text <- paste(..., collapse = "")
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
