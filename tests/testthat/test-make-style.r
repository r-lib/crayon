
context("Making new ANSI 256 styles")

test_that("make_style without name", {

  st <- styles()
  pink <- make_style("pink")
  expect_true(is(pink, "crayon"))
  expect_identical(st, styles())

})

test_that("make_style with name", {

  st <- styles()
  make_style(foobarnonono = "pink")
  expect_true("foobarnonono" %in% names(styles()))
  drop_style("foobarnonono")
  expect_false("foobarnonono" %in% names(styles()))

})

test_that("hexa color regex works", {

  positive <- c("#000000", "#ffffff", "#0f0f0f", "#f0f0f0",
                "#00000000", "#ffffffff", "#0f0f0f00", "#f0f0f055")

  negative <- c("", "#12345", "123456", "1234567", "12345678",
                "#1234567", "#1234ffg", "#gggggx", "foo#123456",
                "foo#123456bar")

  for (color in positive) {
    expect_true(grepl(hash_color_regex, color))
    expect_true(grepl(hash_color_regex, toupper(color)))
  }

  for (color in negative) {
    expect_false(grepl(hash_color_regex, color))
    expect_false(grepl(hash_color_regex, toupper(color)))
  }

})

test_that("we fall back for ANSI 8 if needed", {

  yellow3 <- make_style("yellow3", colors = 8)
  expect_equal(attr(yellow,  "_styles")[[1]],
               attr(yellow3, "_styles")[[1]])

})

test_that("we can create a style from an R color", {

  red4 <- make_style("red4")
  red_text <- red4("text")
  expect_true(!has_color() || has_style(red_text))

})
