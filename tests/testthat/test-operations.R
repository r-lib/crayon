
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
  
})

test_that("col_strsplit multiple strings", {
  red <- "\033[31mred\033[39m"
  str <- c("plain-plain-" %+% red %+% "-plain-" %+% red,
           red %+% "-" %+% red,
           red)

  r1 <- lapply(col_strsplit(str, "-"), strip_style)
  r2 <- strsplit(strip_style(str), "-")
  
})

test_that("col_strsplit empty string", {

  expect_equal(col_strsplit("", "-"), list(""))
  expect_equal(strip_style(col_strsplit("\033[31m\033[39m", "-")[[1]]), "")
})
