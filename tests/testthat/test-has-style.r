
context("Does a string have ANSI style?")

op <- options()
on.exit(options(op))
options(crayon.enabled = TRUE)

test_that("has_style works", {

  expect_false(has_style("foobar"))
  for (st in names(styles)) {
    expect_true(has_style(style("foobar", st)))
  }

})

context("Strip style from string")

test_that("strip_style works", {

  expect_equal("", strip_style(""))
  expect_equal("foobar", strip_style("foobar"))
  expect_equal("foobar", strip_style(red$underline$bold("foobar")))

  for (st in names(styles)) {
    expect_equal("foobar", strip_style(style("foobar", st)))
  }
})
