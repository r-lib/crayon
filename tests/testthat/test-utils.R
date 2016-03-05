context("utils")

test_that("non_matching", {
  chr <- "abc"
  splits <- crayon:::re_table("", chr)
  res.df <- data.frame(start=c(1L, 1L:3L), end=0L:3L, length=c(0L, 1L, 1L, 1L))
  expect_equal(crayon:::non_matching(splits, chr, empty=TRUE)[[1L]], res.df)
  expect_equal(crayon:::non_matching(splits, chr)[[1L]], res.df[-1L,])
})
