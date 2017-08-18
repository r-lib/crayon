
context("String operations")

str <- c("",
         "plain",
         "\033[31m",
         "\033[39m",
         "\033[31mred\033[39m",
         "\033[31mred\033[39m\033[31mred\033[39m",
         "foo\033[31mred\033[39m",
         "\033[31mred\033[39mfoo")

test_that("col_nchar", {
  for (s in str) {
    expect_equal(col_nchar(s), nchar(strip_style(s)), info = s)
  }
})

test_that("col_substr", {
  for (s in str) {
    for (i in 1 %:% col_nchar(s)) {
      for (j in i %:% col_nchar(s)) {
        expect_equal(strip_style(col_substr(s, i, j)),
                     substr(strip_style(s), i, j), info = paste(s, i, j))
      }
    }
  }
})

test_that("col_substr keeps color", {
  expect_equal(col_substr("\033[31mred\033[39m", 1, 1),
               "\033[31mr\033[39m")
  expect_equal(col_substr("foo\033[31mred\033[39m", 4, 4),
               "\033[31mr\033[39m")
  expect_equal(col_substr("foo\033[31mred\033[39mbar", 4, 4),
               "\033[31mr\033[39m")
  expect_equal(col_substr("\033[31mred\033[39mfoo\033[31mred\033[39mbar", 7, 7),
               "\033[31m\033[39m\033[31mr\033[39m")
})

test_that("col_substr, start after string end", {
  expect_equal(col_substr("red", 4, 4), "")
  expect_equal(col_substr("red", 4, 5), "")
  expect_equal(strip_style(col_substr("\033[31mred\033[39m", 4, 4)), "")
  expect_equal(strip_style(col_substr("\033[31mred\033[39m", 4, 5)), "")

  expect_equal(col_substr("red", 3, 4), "d")
  expect_equal(col_substr("red", 3, 5), "d")
  expect_equal(strip_style(col_substr("\033[31mred\033[39m", 3, 4)), "d")
  expect_equal(strip_style(col_substr("\033[31mred\033[39m", 3, 5)), "d")  
})

test_that("col_substr, multiple strings", {
  set.seed(42)
  for (i in 1:100) {
    strs <- sample(str, 4)
    num_starts <- sample(1:5, 1)
    num_stops <- sample(1:5, 1)
    starts <- sample(1:5, num_starts, replace = TRUE)
    stops <- sample(1:5, num_stops, replace = TRUE)
    r1 <- strip_style(col_substr(strs, starts, stops))
    r2 <- substr(strip_style(strs), starts, stops)
    expect_equal(r1, r2)
  }
})

test_that("col_substr corner cases", {
  # Zero length input

  c0 <- character(0L)
  o0 <- structure(list(), class="abc")
  co0 <- structure(character(0L), class="abc")
  expect_identical(col_substr(c0, 1, 1), substr(c0, 1, 1))
  expect_identical(col_substr(o0, 1, 1), substr(o0, 1, 1))
  expect_identical(col_substr(co0, 1, 1), substr(co0, 1, 1))

  expect_identical(col_substring(c0, 1, 1), substring(c0, 1, 1))
  expect_identical(col_substring(o0, 1, 1), substring(o0, 1, 1))
  expect_identical(col_substring(co0, 1, 1), substring(co0, 1, 1))

  # Character start/stop
  expect_identical(col_substr("abc", "1", 1), substr("abc", "1", 1))
  expect_identical(col_substr("abc", 1, "1"), substr("abc", 1, "1"))

  # non-numeric arguments cause errors; NOTE: this actually "works" 
  # with 'substr' but not implemented in 'col_substr'
  suppressWarnings(
    expect_error(col_substr("abc", "hello", 1), "non-numeric")
  )

})

test_that("col_substring", {
  for (s in str) {
    for (i in 1 %:% col_nchar(s)) {
      for (j in i %:% col_nchar(s)) {
        expect_equal(strip_style(col_substring(s, i, j)),
                     substring(strip_style(s), i, j), info = paste(s, i, j))
      }
    }
  }  
})

test_that("col_substring, multiple strings", {
  set.seed(42)
  for (i in 1:100) {
    strs <- sample(str, 4)
    num_starts <- sample(1:5, 1)
    num_stops <- sample(1:5, 1)
    starts <- sample(1:5, num_starts, replace = TRUE)
    stops <- sample(1:5, num_stops, replace = TRUE)
    r1 <- strip_style(col_substring(strs, starts, stops))
    r2 <- substring(strip_style(strs), starts, stops)
    expect_equal(r1, r2)
  }
})

test_that("col_substring corner cases", {
  # Zero length input

  c0 <- character(0L)
  o0 <- structure(list(), class="abc")
  co0 <- structure(character(0L), class="abc")
  expect_identical(col_substring(c0, 1, 1), substring(c0, 1, 1))
  expect_identical(col_substring(o0, 1, 1), substring(o0, 1, 1))
  expect_identical(col_substring(co0, 1, 1), substring(co0, 1, 1))
})

test_that("col_strsplit", {
  red <- "\033[31mred\033[39m"
  
  str <- "plain-plain"
  expect_equal(col_strsplit(str, "-"), strsplit(str, "-"))
  
  str <- red %+% "-plain"
  expect_equal(strip_style(col_strsplit(str, "-")[[1]]),
               strsplit(strip_style(str), "-")[[1]])

  expect_equal(col_strsplit(str, "e"),
               list(c("\033[31mr\033[39m", "\033[31md\033[39m-plain")))

  str <- red %+% "-" %+% red %+% "-" %+% red
  expect_equal(strip_style(col_strsplit(str, "-")[[1]]),
               strsplit(strip_style(str), "-")[[1]])
  
  # with leading and trailing separators
  str.2 <- "-" %+% red %+% "-" %+% red %+% "-" %+% red %+% "-"
  expect_equal(strip_style(col_strsplit(str.2, "-")[[1]]),
               strsplit(strip_style(str.2), "-")[[1]])

  # greater than length 1
  str.3 <- paste0("-", c(green("hello"), red("goodbye")), "-world-")
  expect_equal(strip_style(unlist(col_strsplit(str.3, "-"))),
               unlist(strsplit(strip_style(str.3), "-")))
})

test_that("col_strsplit multiple strings", {
  red <- "\033[31mred\033[39m"
  str <- c("plain-plain-" %+% red %+% "-plain-" %+% red,
           red %+% "-" %+% red,
           red)

  r1 <- lapply(col_strsplit(str, "-"), strip_style)
  r2 <- strsplit(strip_style(str), "-")
  
})

test_that("col_strsplit edge cases", {
  expect_equal(col_strsplit("", "-"), list(character(0L)))
  expect_equal(
    strip_style(col_strsplit("\033[31m\033[39m", "-")[[1]]), character(0L)
  )
  # special cases
  expect_equal(col_strsplit("", ""), strsplit("", ""))
  expect_equal(col_strsplit("a", "a"), strsplit("a", "a"))
  # this following test isn't working yet
  expect_equal(col_strsplit("a", ""), strsplit("a", ""))
  expect_equal(col_strsplit("", "a"), strsplit("", "a"))
  # Longer strings
  expect_identical(
    col_strsplit(c("", "a", "aa"), "a"), strsplit(c("", "a", "aa"), "a")
  )
  expect_identical(
    col_strsplit(c("abaa", "ababza"), "b."), strsplit(c("abaa", "ababza"), "b.")
  )
})

test_that("Weird length 'split'", {
  expect_error(col_strsplit(c("ab", "bd"), c("b", "d")), "must be character")
  expect_identical(col_strsplit("ab", NULL), strsplit("ab", NULL))
  expect_identical(
    col_strsplit("ab", character(0L)), strsplit("ab", character(0L))
  )
})

test_that("col_align", {
  expect_equal(col_align(character()), character())
  expect_equal(col_align("", 0), "")
  expect_equal(col_align(" ", 0), " ")
  expect_equal(col_align(" ", 1), " ")
  expect_equal(col_align(" ", 2), "  ")
  expect_equal(col_align("a", 1), "a")
  expect_equal(col_align(letters, 1), letters)
  expect_equal(col_align(letters, 0), letters)
  expect_equal(col_align(letters, -1), letters)

  expect_equal(col_align(letters, 2), paste0(letters, " "))
  expect_equal(col_align(letters, 3, "center"), paste0(" ", letters, " "))
  expect_equal(col_align(letters, 2, "right"), paste0(" ", letters))

  expect_equal(
    col_align(c("foo", "foobar", "", "a"), 6, "left"),
    c("foo   ", "foobar", "      ", "a     "))

  expect_equal(
    col_align(c("foo", "foobar", "", "a"), 6, "center"),
    c("  foo ", "foobar", "      ", "   a  "))

  expect_equal(
    col_align(c("foo", "foobar", "", "a"), 6, "right"),
    c("   foo", "foobar", "      ", "     a"))

  # #54: alignment of wide characters
  expect_equal(
    col_align(c("foo", "\u6210\u4ea4\u65e5", "", "a"), 6, "left"),
    c("foo   ", "\u6210\u4ea4\u65e5", "      ", "a     "))

  expect_equal(
    col_align(c("foo", "\u6210\u4ea4\u65e5", "", "a"), 6, "center"),
    c("  foo ", "\u6210\u4ea4\u65e5", "      ", "   a  "))

  expect_equal(
    col_align(c("foo", "\u6210\u4ea4\u65e5", "", "a"), 6, "right"),
    c("   foo", "\u6210\u4ea4\u65e5", "      ", "     a"))
})
