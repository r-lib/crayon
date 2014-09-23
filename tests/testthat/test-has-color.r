
context("Color detection")

test_that("Color is detected properly", {

  op <- options()
  on.exit(options(op), add = TRUE)

  ## If disabled, then no
  options(crayon.enabled = FALSE)
  expect_false(has_color())

  ## If enabled, then yes
  options(crayon.enabled = TRUE)
  expect_true(has_color())

  ## TODO: other cases

})
