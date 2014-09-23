
# Crayon - stylish terminal output in R

[![Linux Build Status](https://travis-ci.org/gaborcsardi/crayon.png?branch=master)](https://travis-ci.org/gaborcsardi/crayon)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/crayon)](https://ci.appveyor.com/project/gaborcsardi/crayon)

With crayon it is easy to add color to terminal output, create styles
for notes, warnings, errors; and combine styles.

ANSI color support is automatically detected and used. Crayon was largely
inspired by [chalk](https://github.com/sindresorhus/chalk).

## Installation

```r
devtools::install_github("gaborcsardi/crayon")
library(crayon)
```

## Styles

Crayon defines several styles, that can be combined. Each style in the list
has a corresponding function with the same name.

### General styles

* `reset`
* `bold`
* `blurred` (usually called `dim`, renamed to avoid name clash)
* `italic` (not widely supported)
* `underline`
* `inverse`
* `hidden`
* `strikethrough` (not widely supported)

### Text colors

* `black`
* `red`
* `green`
* `yellow`
* `blue`
* `magenta`
* `cyan`
* `white`
* `silver` (usually called `gray`, renamed to avoid name clash)

### Background colors

* `bgBlack`
* `bgRed`
* `bgGreen`
* `bgYellow`
* `bgBlue`
* `bgMagenta`
* `bgCyan`
* `bgWhite`

## Usage

The styling functions take any number of character vectors as arguments,
and they concatenate and style them:

```r
library(crayon)
cat(blue("Hello", "world!\n"))
```

Crayon defines the `%+%` string concatenation operator, to make it easy
to assemble stings with different styles.

```r
cat("... to highlight the " %+% red("search term") %+% " in a block of text\n")
```

Styles can be combined using the `$` operator:

```r
cat(yellow$bgMagenta$bold('Hello world!\n'))
```

Styles can also be nested, and then inner style takes precedence:

```r
cat(green(
  'I am a green line ' %+%
  blue$underline$bold('with a blue substring') %+%
  ' that becomes green again!\n'
))
```
  
It is easy to define your own themes:

```r
error <- red $ bold
warn <- magenta $ underline
note <- cyan
cat(error("Error: subscript out of bounds!\n"))
cat(warn("Warning: shorter argument was recycled.\n"))
cat(note("Note: no such directory.\n"))
```

