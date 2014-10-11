
fgcodes <- c(paste0('\x1b[38;5;', 0:255, 'm'), '\x1b[39m')
bgcodes <- c(paste0('\x1b[48;5;', 0:255, 'm'), '\x1b[49m')

rgb_index <- 17:232
gray_index <- 233:256
reset_index <- 257

ansi256 <- function(rgb, bg = FALSE, grey = FALSE) {
  codes <- if (bg) bgcodes else fgcodes
  if (grey) {
    ## Gray
    list(
      open = codes[gray_index][scale(rgb[1], to = c(0, 23)) + 1],
      close = codes[reset_index]
    )
    
  } else {
    ## Not gray
    rgb <- scale(rgb)
    list(
      open = codes[rgb_index][rgb[1] * 36 + rgb[2] * 6 + rgb[3] + 1],
      close = codes[reset_index]
    )
  }
}
