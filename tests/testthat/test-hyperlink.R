
test_that("hyperlinks", {
  withr::local_options(crayon.hyperlink = TRUE)
  expect_equal(
    hyperlink("foo", "https://bar"),
    "\033]8;;https://bar\afoo\033]8;;\a"
  )
})
