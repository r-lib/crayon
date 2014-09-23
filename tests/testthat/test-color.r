
context("Colors and highlighting")

op <- options()
on.exit(options(op))
options(crayon.enabled = TRUE)

test_that("Coloring and highlighting works", {

  expect_equal(underline("foo"), '\u001b[4mfoo\u001b[24m')
  expect_equal(red('foo'), '\u001b[31mfoo\u001b[39m')
  expect_equal(bgRed('foo'), '\u001b[41mfoo\u001b[49m')

})

test_that("Applying multiple styles at once works", {

  expect_equal(red$bgGreen$underline('foo'),
               '\u001b[31m\u001b[42m\u001b[4mfoo\u001b[24m\u001b[49m\u001b[39m')
  expect_equal(underline$red$bgGreen('foo'),
               '\u001b[4m\u001b[31m\u001b[42mfoo\u001b[49m\u001b[39m\u001b[24m')
})

test_that("Nested styles are supported", {

  expect_equal(
    red('foo' %+% underline$bgBlue('bar') %+% '!'),
    '\u001b[31mfoo\u001b[4m\u001b[44mbar\u001b[49m\u001b[24m!\u001b[39m')
})

test_that("Nested styles of the same type are supported", {

  expect_equal(
    red('a' %+% blue('b' %+% green('c') %+% 'b') %+% 'c'),
    '\u001b[31ma\u001b[34mb\u001b[32mc\u001b[34mb\u001b[31mc\u001b[39m')
})

test_that("Reset all styles", {

  expect_equal(reset(red$bgGreen$underline('foo') %+% 'foo'),
               '\u001b[0m\u001b[31m\u001b[42m\u001b[4mfoo\u001b[24m\u001b[49m\u001b[39mfoo\u001b[0m')
})

test_that("Variable number of arguments", {

  expect_equal(red('foo', 'bar'), '\u001b[31mfoo bar\u001b[39m')

})
