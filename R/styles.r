
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

## ANSI fg color -> R color

ansi_fg_r <- c(
  "black" = "black",
  "red" = "red",
  "green" = "green",
  "yellow" = "yellow",
  "blue" = "blue",
  "magenta" = "magenta",
  "cyan" = "cyan",
  "white" = "white",
  "silver" = "grey"
)

ansi_fg_rgb <- col2rgb(ansi_fg_r)

ansi_bg_r <- c(
  "bgBlack" = "black",
  "bgRed" = "red",
  "bgGreen" = "green",
  "bgYellow" = "yellow",
  "bgBlue" = "blue",
  "bgMagenta" = "magenta",
  "bgCyan" = "cyan",
  "bgWhite" = "white"
)

ansi_bg_rgb <- col2rgb(ansi_bg_r)

make_chr_style <- function(code) {
  list(
    open = '\u001b[' %+% chr(codes[[code]][1]) %+% 'm',
    close = '\u001b[' %+% chr(codes[[code]][2]) %+% 'm'
  )
}

builtin_styles <- lapply(names(codes), make_chr_style)
names(builtin_styles) <- names(codes)
