% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style-var.R
\name{style}
\alias{style}
\title{Add style to a string}
\usage{
style(string, as = NULL, bg = NULL)
}
\arguments{
\item{string}{Character vector to style.}

\item{as}{Style function to apply, either the function object,
or its name, or an object to pass to \code{\link[=make_style]{make_style()}}.}

\item{bg}{Background style, a style function, or a name that
is passed to \code{\link[=make_style]{make_style()}}.}
}
\value{
Styled character vector.
}
\description{
See \code{names(styles)}, or the crayon manual for available styles.
}
\examples{
## These are equivalent
style("foobar", bold)
style("foobar", "bold")
bold("foobar")
}
