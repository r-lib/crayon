# 1.3.1

* Fixed some `R CMD check` problems.

# 1.3.0

* Colors are turned on by default in Emacs ESS 23.x and above.

* Functions to turn on and off a style: `start`, `finish`.

* Really fix `tput` corner cases (@jimhester, #21)

# 1.2.1

* Fix detecting number of colors when `tput` exists, but
  fails with an error and/or does not return anything useful.
  (@jimhester, #18, #19)

# 1.2.0

* Fix detection of number of colors, it was cached from
  installation time (#17).

* Color aware string operations. They are slow and experimental
  currently.

# 1.1.0

* `show_ansi_colors()` prints all supported colors on the screen.

* 256 colors, on terminals that support it.

* Disable colors on Windows, they are not supported in the default setup.

# 1.0.0

* First released version.
