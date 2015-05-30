
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

test_that("tput errors are handled gracefully", {

  # if tput errors num_colors is 8
  with_mock(
            `base::system` = function(...) stop("Error!"),

            expect_equal(num_colors(forget = TRUE), 8)
            )

  # if tput returns nothing num_colors is 8
  with_mock(
            `base::system` = function(...) character(0),

            expect_equal(num_colors(forget = TRUE), 8)
            )

  # if tput returns a non-number num_colors is 8
  with_mock(
            `base::system` = function(...) "no colors found!",

            expect_equal(num_colors(forget = TRUE), 8)
            )

  # if tput returns a number the result is that number
  with_mock(
            `base::system` = function(...) "16",

            expect_equal(num_colors(forget = TRUE), 16)
            )
})
