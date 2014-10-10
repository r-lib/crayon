
fgcodes <- paste0('\x1b[;38;5;', 0:255, 'm')
bgcodes <- paste0('\x1b[;48;5;', 0:255, 'm')

rgb_index <- 17:232
gray_index <- 233:256

ansi256 <- function(rgb, bg = FALSE) {
  codes <- if (bg) bgcodes else fgcodes
  if (rgb[1] == rgb[2] && rgb[2] == rgb[3]) {
    ## Gray
    list(
      open = codes[gray_index][scale(rgb[1], to = c(0, 23))],
      close = builtin_styles$reset$close
    )
    
  } else {
    ## Not gray
    rgb <- scale(rgb)
    list(
      open = codes[rgb_index][rgb[1] * 36 + rgb[2] * 6 + rgb[3] + 1],
      close = builtin_styles$reset$close
    )
  }
}
