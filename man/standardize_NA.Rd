% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate-inputs.R
\name{standardize_NA}
\alias{standardize_NA}
\title{Replace Inf and NaN values with NA}
\usage{
standardize_NA(x, arg = rlang::caller_arg(x), call = rlang::caller_env())
}
\arguments{
\item{x}{Numeric vector with Inf and/or NaN values to replace}

\item{arg}{An argument name as a string. This argument
will be mentioned in error messages as the input that is at the
origin of a problem.}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}
}
\value{
Explicitly a numeric vector with all non-numeric values replaced with
NA
}
\description{
Replaces all Inf and NaN values with NA in numeric vector
}
