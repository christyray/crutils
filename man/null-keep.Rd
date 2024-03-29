% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/null-arguments.R
\name{null-keep}
\alias{null-keep}
\alias{\%keepnull\%}
\title{Keep NULL value if provided}
\usage{
a \%keepnull\% b
}
\arguments{
\item{a}{The output if its value is \code{NULL}}

\item{b}{The output if \code{a} is not \code{NULL}}
}
\description{
Opposite of \code{\link{null-drop}}. Outputs \code{a} if \code{a} is NULL; otherwise,
outputs \code{b}. Helpful to provide a default value but also a means to override
the default value.
}
\examples{
input <- NULL
x <- input \%||\% sum(c(1,2,3))
}
