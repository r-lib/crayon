
context("Defining new styles")

test_that("new styles are local to importing package", {

  skip("This is not implemented, yet.")

  lib_dir <- tempfile()

  on.exit(try(unloadNamespace("foo1"), silent = TRUE), add = TRUE)
  on.exit(try(unloadNamespace("foo2"), silent = TRUE), add = TRUE)
  on.exit(try(unlink(lib_dir, recursive = TRUE), silent = TRUE), add = TRUE)

  make_packages(
    lib_dir = lib_dir,
    imports = "crayon",

    foo1 = {
      f <- function() {
        make_style(pink = "pink")
      }
      g <- function() {
        names(styles())
      }
    },

    foo2 = {
      f <- function() {
        make_style(maroon = "maroon")
      }
      g <- function() {
        names(styles())
      }
    }
  )

  ## Add style in 'foo1', does not effect 'foo2', or attached crayon
  foo1::f()
  expect_true("pink" %in% foo1::g())
  expect_false("pink" %in% foo2::g())
  expect_false("pink" %in% names(styles()))

  ## Attached style change does not affect imports in packages
  on.exit(drop_style("ivory444"), add = TRUE)
  make_style(ivory444 = "ivory")
  expect_true("ivory444" %in% names(styles()))
  expect_false("ivory444" %in% foo1::g())
  expect_false("ivory444" %in% foo2::g())

  ## TODO: what if the package(s) are not attached
})
