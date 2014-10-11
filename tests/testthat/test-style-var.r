
context("General style function")

op <- options(crayon.enabled = TRUE)
on.exit(options(op))

test_that("style works", {

  x1 <- style("foobar", bold)
  x2 <- style("foobar", "bold")
  x3 <- bold("foobar")

  expect_equal(x1, x2)
  expect_equal(x2, x3)
  
})
