
context("Hyperlinks")

op <- options()
on.exit(options(op))
options(crayon.enabled = TRUE)

test_that("hyperlinks", {
  expect_equal(
    hyperlink("foo", "https://bar"),
    "\033]8;;https://bar\afoo\033]8;;\a"
  )
})
