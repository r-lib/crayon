
test_that("Coloring and highlighting works", {
  local_colors()
  expect_equal(underline("foo"), '\u001b[4mfoo\u001b[24m')
  expect_equal(red('foo'), '\u001b[31mfoo\u001b[39m')
  expect_equal(bgRed('foo'), '\u001b[41mfoo\u001b[49m')

})

test_that("Applying multiple styles at once works", {
  local_colors()
  expect_equal(red$bgGreen$underline('foo'),
               '\u001b[31m\u001b[42m\u001b[4mfoo\u001b[24m\u001b[49m\u001b[39m')
  expect_equal(underline$red$bgGreen('foo'),
               '\u001b[4m\u001b[31m\u001b[42mfoo\u001b[49m\u001b[39m\u001b[24m')
})

test_that("Nested styles are supported", {
  local_colors()
  expect_equal(
    red('foo' %+% underline$bgBlue('bar') %+% '!'),
    '\u001b[31mfoo\u001b[4m\u001b[44mbar\u001b[49m\u001b[24m!\u001b[39m')
})

test_that("Nested styles of the same type are supported", {
  local_colors()
  expect_equal(
    red('a' %+% blue('b' %+% green('c') %+% 'b') %+% 'c'),
    '\u001b[31ma\u001b[34mb\u001b[32mc\u001b[34mb\u001b[31mc\u001b[39m')
})

test_that("Reset all styles", {
  local_colors()
  expect_equal(reset(red$bgGreen$underline('foo') %+% 'foo'),

'\033[0m\033[31m\033[42m\033[4mfoo\033[24m\033[49m\033[39mfoo\033[0m\033[22m\033[23m\033[24m\033[27m\033[28m\033[29m\033[39m\033[49m'
)

  # reset, but only in middle of string

  expect_equal(
    red$bgGreen$underline('bunny bunny', reset(' foo '), 'foo'),
    '\033[31m\033[42m\033[4mbunny bunny \033[0m foo \033[0m\033[22m\033[23m\033[4m\033[27m\033[28m\033[29m\033[31m\033[42m foo\033[24m\033[49m\033[39m'
  )
})

test_that("Variable number of arguments", {
  local_colors()
  expect_equal(red('foo', 'bar'), '\u001b[31mfoo bar\u001b[39m')

})
