
context("Color detection")

test_that("Color is detected properly", {

  op <- options()
  on.exit(options(op), add = TRUE)

  ## If disabled, then no
  options(crayon.enabled = FALSE)
  hc <- has_color()
  options(op)
  expect_false(hc)

  ## If enabled, then yes
  op <- options(crayon.enabled = TRUE)
  hc <- has_color()
  options(op)
  expect_true(hc)

})

test_that("number of colors is detected", {

  nc <- num_colors()
  expect_more_than(nc, 0)
  expect_equal(nc, as.integer(nc))

})
