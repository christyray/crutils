% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate-inputs.R
\name{convert_vector}
\alias{convert_vector}
\title{Convert input object to a vector}
\usage{
convert_vector(x, arg = rlang::caller_arg(x), call = rlang::caller_env())
}
\arguments{
\item{x}{Numeric object to be validated and converted to vector}

\item{arg}{An argument name as a string. This argument
will be mentioned in error messages as the input that is at the
origin of a problem.}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}
}
\value{
Numeric vector
}
\description{
Validates that numeric inputs are one-dimensional and converts the input to a
vector if possible
}
