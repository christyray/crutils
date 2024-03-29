% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/null-arguments.R
\name{non_null}
\alias{non_null}
\title{Return non-null arguments as symbols that can be evaluated}
\usage{
non_null(...)
}
\arguments{
\item{...}{Function arguments}
}
\value{
Symbols that can be evaluated in R, generated using
\code{\link[rlang]{ensyms}}
}
\description{
Return non-null arguments as symbols that can be evaluated
}
\examples{
x <- NULL
y <- 1
non_null(x,y)
}
