
if (Sys.getenv("NOT_CRAN") != "") {
  library(testthat)
  library(crayon)
  test_check("crayon")
}
