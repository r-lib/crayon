
context("Does a string have ANSI style?")

test_that("has_style works", {

  expect_false(has_style("foobar"))
  for (st in names(styles)) {
    expect_true(has_style(style("foobar", st)))
  }

})
