
context("ansi-256")

## These are really basic cases...

test_that("ansi256", {
  cases <- list(
    list(c(0,0,0), 233),
    list(c(255,0,0), 197),
    list(c(0,255,0), 47),
    list(c(0,0,255), 22),
    list(c(255,255,255), 256)
  )

  for (case in cases) {
    exp1 <- list(
      open = paste0("\033[38;5;", case[[2]] - 1, "m"),
      close = "\033[39m"
    )
    exp2 <- list(
      open = paste0("\033[48;5;", case[[2]] - 1, "m"),
      close = "\033[49m"
    )
    expect_equal(ansi256(case[[1]], bg = FALSE), exp1)
    expect_equal(ansi256(case[[1]], bg = TRUE), exp2)
  }
})

test_that("ansi256, grey", {
  cases <- list(
    list(c(0,0,0), 233),
    list(c(128,128,128), 245),
    list(c(255,255,255), 256)
  )

  for (case in cases) {
    exp1 <- list(
      open = paste0("\033[38;5;", case[[2]] - 1, "m"),
      close = "\033[39m"
    )
    exp2 <- list(
      open = paste0("\033[48;5;", case[[2]] - 1, "m"),
      close = "\033[49m"
    )
    expect_equal(ansi256(case[[1]], grey = TRUE, bg = FALSE), exp1)
    expect_equal(ansi256(case[[1]], grey = TRUE, bg = TRUE), exp2)
  }
})

test_that("ansi256_rgb_index", {
  cases <- list(
    list(c(0,0,0), 233),
    list(c(255,0,0), 197),
    list(c(0,255,0), 47),
    list(c(0,0,255), 22),
    list(c(255,255,255), 256)
  )

  for (case in cases) {
    expect_equal(do.call(ansi256_rgb_index, as.list(case[[1]])), case[[2]])
  }
})
