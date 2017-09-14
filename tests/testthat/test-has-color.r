
context("Color detection")

test_that("has_color, option", {

  withr::with_options(
    c(crayon.enabled = FALSE),
    expect_false(has_color())
  )
  withr::with_options(
    c(crayon.enabled = TRUE),
    expect_true(has_color())
  )
})

test_that("has_color, rstudio", {
  mockery::stub(has_color, "rstudio_with_ansi_support", TRUE)
  mockery::stub(has_color, "rstudioapi::callFun", TRUE)
  expect_true(has_color())
})

test_that("has_color, not a terminal", {
  mockery::stub(has_color, "rstudio_with_ansi_support", FALSE)
  mockery::stub(has_color, "isatty", FALSE)
  withr::with_options(
    list(crayon.enabled = NULL),
    expect_false(has_color())
  )
})

test_that("has_color, windows terminal", {
  mockery::stub(has_color, "rstudio_with_ansi_support", FALSE)
  mockery::stub(has_color, "os_type", "windows")
  withr::with_envvar(
    c(ConEmuANSI = "ON", CMDER_ROOT = ""),
    expect_true(has_color())
  )
  withr::with_envvar(
    c(ConEmuANSI = "OFF", CMDER_ROOT = "/foobar"),
    expect_true(has_color())
  )
  withr::with_options(
    list(crayon.enabled = NULL),
    withr::with_envvar(
      c(ConEmuANSI = "OFF", CMDER_ROOT = NA_character_),
      expect_false(has_color())
    )
  )
})

test_that("number of colors is detected", {

  nc <- num_colors()
  expect_true(nc > 0)
  expect_equal(nc, as.integer(nc))
})

test_that("closure based memoization works", {

  # use `crayon.colors` option to force expected color return
  old.opt <- options(crayon.colors = 42, crayon.enabled=TRUE)
  expect_equal(num_colors(forget = TRUE), 42)
  options(crayon.colors = 43)
  expect_equal(num_colors(), 42)
  expect_equal(num_colors(forget = TRUE), 43)

  # reset options and run one more time
  options(old.opt)
  expect_equal(num_colors(), 43)

  # reset cache to original value
  try(num_colors(forget = TRUE))
})

test_that("tput errors are handled gracefully", {

  ## if tput errors num_colors is 8
  mockery::stub(get_terminal_colors, "system", function(...) stop("Error!"))
  expect_equal(get_terminal_colors(), 8)

  ## if tput returns nothing num_colors is 8
  mockery::stub(get_terminal_colors, "system", function(...) character(0))
  expect_equal(get_terminal_colors(), 8)

  ## if tput returns a non-number num_colors is 8
  mockery::stub(get_terminal_colors, "system", function(...) "no colors!")

  ## if tput returns a number the result is that number
  mockery::stub(get_terminal_colors, "system", function(...) 16)
  expect_equal(get_terminal_colors(), 16)
})
