
context("combine_styles")

test_that("one style", {
  expect_equal(combine_styles(red), red)
  expect_equal(combine_styles(bold), bold)
})

test_that("style objects", {
  withr::with_options(
    list(crayon.enabled = TRUE, crayon.colors = 256), {
      expect_equal(
        combine_styles(red, bold)("blah"),
        red(bold("blah"))
      )
      expect_equal(
        combine_styles(red, bold, underline)("foo"),
        red(bold(underline("foo")))
      )
    }
  )
})

test_that("create styles on the fly", {
  withr::with_options(
    list(crayon.enabled = TRUE, crayon.colors = 256), {
      expect_equal(
        combine_styles("darkolivegreen", bold)("blah"),
        make_style("darkolivegreen")((bold("blah")))
      )
      expect_equal(
        combine_styles(bold, "darkolivegreen", underline)("foo"),
        bold(make_style("darkolivegreen")(underline("foo")))
      )
    }
  )
})
