
if (Sys.getenv("NOT_CRAN") != "") {
  library(crayon)
  library(testthat)
  test_check("crayon")
}
