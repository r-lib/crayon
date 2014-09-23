
context("Styling of character vectors")

op <- options()
on.exit(options(op))
options(crayon.enabled = TRUE)

foobar <- c("foo", "bar")
bigyo <- c("bi", "gyo")

test_that("Coloring and highlighting works", {

  expect_equal(underline(foobar),
               c('\u001b[4mfoo\u001b[24m', '\u001b[4mbar\u001b[24m'))
  expect_equal(red(foobar),
               c('\u001b[31mfoo\u001b[39m', '\u001b[31mbar\u001b[39m'))
  expect_equal(bgRed(foobar),
               c('\u001b[41mfoo\u001b[49m', '\u001b[41mbar\u001b[49m'))

})

test_that("Applying multiple styles at once works", {

  expect_equal(red$bgGreen$underline(foobar),
               c('\u001b[31m\u001b[42m\u001b[4mfoo\u001b[24m\u001b[49m\u001b[39m',
                 '\u001b[31m\u001b[42m\u001b[4mbar\u001b[24m\u001b[49m\u001b[39m'))
  expect_equal(underline$red$bgGreen(foobar),
               c('\u001b[4m\u001b[31m\u001b[42mfoo\u001b[49m\u001b[39m\u001b[24m',
                 '\u001b[4m\u001b[31m\u001b[42mbar\u001b[49m\u001b[39m\u001b[24m'))
})

test_that("Nested styles are supported", {

  expect_equal(
    red(foobar %+% underline$bgBlue(bigyo) %+% '!'),
    c('\u001b[31mfoo\u001b[4m\u001b[44mbi\u001b[49m\u001b[24m!\u001b[39m',
      '\u001b[31mbar\u001b[4m\u001b[44mgyo\u001b[49m\u001b[24m!\u001b[39m'))
})

test_that("Nested styles of the same type are supported", {

  aA <- c("a", "A")
  bB <- c("b", "B")
  cC <- c("c", "C")
  expect_equal(
    red(aA %+% blue(bB %+% green(cC) %+% bB) %+% cC),
    c('\u001b[31ma\u001b[34mb\u001b[32mc\u001b[34mb\u001b[31mc\u001b[39m',
      '\u001b[31mA\u001b[34mB\u001b[32mC\u001b[34mB\u001b[31mC\u001b[39m'))
})

test_that("Reset all styles", {

  expect_equal(reset(red$bgGreen$underline(foobar) %+% foobar),
               c('\u001b[0m\u001b[31m\u001b[42m\u001b[4mfoo\u001b[24m\u001b[49m\u001b[39mfoo\u001b[0m',
                 '\u001b[0m\u001b[31m\u001b[42m\u001b[4mbar\u001b[24m\u001b[49m\u001b[39mbar\u001b[0m'))
})

test_that("Variable number of arguments", {

  expect_equal(red(foobar, 'bar'),
               c('\u001b[31mfoo bar\u001b[39m',
                 '\u001b[31mbar bar\u001b[39m'))

})
