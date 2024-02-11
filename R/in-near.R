
#' Compare values within a tolerance
#'
#' Determines if elements in two numbers or vectors are (pairwise) equal within
#' a relative tolerance value.
#'
#' This function is similar to \code{\link[dplyr]{near}} from
#' \code{\link[dplyr]{dplyr}} except that it uses relative tolerance instead of
#' absolute tolerance. Uses \code{1e-12} as the default tolerance because the
#' \code{ismembertol()} function from MATLAB uses \code{1e-12} as the tolerance
#' for double-precision floating point numbers.
#'
#' @param x,y A pair of numeric vectors to be compared pairwise
#' @param tol Relative tolerance of comparison; default value is \code{1e-12}
#'
#' @return A logical vector the same length as \code{x} and \code{y} indicating
#'   where the paired values are equal within the relative tolerance
#'
#' @seealso [in_near()] and [%~%] for determining if values are members of a
#'   vector within a relative tolerance
#'
#' @export
#'
#' @examples
#' x <- c(1000, 1000, 100, 100, 10, 10, 1, 1)
#' y <- x + x * c(5e-12, 5e-16, 5e-12, 5e-16, 5e-12, 5e-16, 5e-12, 5e-16)
#' pair_near(x, y)

pair_near <- function(x, y, tol = 1e-12) {
  # Standardize the inputs to numeric vectors and replace any Inf or NaN with NA
  x <- standardize_NA(convert_vector(x))
  y <- standardize_NA(convert_vector(y))

  # Check that x and y are the same length
  check_equal_length(x, y)

  # Determine tolerance using the given relative tolerance value
  # `pmax()` determines the parallel maximum - compares matching elements from
  # x and y and returns the maximum at each position
  tol <- pmax(abs(x), abs(y)) * tol

  # Find matches that are within the calculated tolerance
  as.vector(abs(x - y) <= tol)
}

#' Determine if values are members of a vector
#'
#' @description
#' `in_near()` determines if the numbers in one vector occur anywhere in the
#' second vector, checking equality within a relative tolerance value.
#'
#' `%~%` is an infix operator shortcut for `in_near()`.
#'
#' @details
#' These functions are similar to \code{\link[base]{%in%}} except that they
#' check for equality within a relative tolerance rather than exact equality.
#' Uses \code{1e-12} as the default tolerance because the \code{ismembertol}
#' function from MATLAB uses \code{1e-12} as the tolerance for double-precision
#' floating point numbers.
#'
#' @param x Numeric vector with the values to be matched
#' @param y Numeric vector with the values to be matched against
#' @param tol Relative tolerance of comparison; default value is \code{1e-12}
#'
#' @return A logical vector the same length as \code{x} indicating the values of
#'   \code{x} that exist in \code{y} within the relative tolerance
#'
#' @seealso [pair_near()] to determine if values in two vectors are pairwise
#'   equal within a relative tolerance
#'
#' @export
#'
#' @examples
#' x <- c(1, 1 + 1e-2, 1 + 1e-16, 2, 3, 10, 10 + 10e-2, 10 + 10e-16)
#' y <- c(1, 10)
#'
#' in_near(x, y)
#' in_near(x, y, tol = 1e-1)
#'
#' x %~% y

in_near <- function(x, y, tol = 1e-12) {
  # Standardize the inputs to numeric vectors and replace any Inf or NaN with NA
  x <- standardize_NA(convert_vector(x))
  y <- standardize_NA(convert_vector(y))

  # Throw an error if any of the values to be matched against are NA
  check_NA(y)

  # Apply the relative tolerance to each value of the vector being matched
  # against; sets the specific tolerance for the matching to that value
  tol <- abs(y) * tol

  # For each value in the first vector, determine if it is within the tolerance
  # of any values in the second vector; finding where the absolute value of x -
  # y is within the relative tolerance (abs(y) * tol)

  # outer(x, y, "-") creates a matrix where each value of y is subtracted from
  # each value of x; e.g., out[1,1] = x[1] - y[1], out[1,2] = x[1] - y[2],
  # out[2,1] = x[2] - y[1], etc.

  # t() transposes the matrix so the rows correspond to the values being matched
  # against (y); necessary because when comparing a matrix against a vector, R
  # evaluates down the columns

  # colSums() adds the logical output for each value of x; if any values in x
  # are in y within the tolerance, the sum will be greater than 0

  # 0L is the integer 0, faster than double-precision
  colSums(t(abs(outer(x, y, "-"))) <= tol) > 0L
}

#' @rdname in_near
#' @name in_near_infix
#' @export

`%~%` <- function(x, y) {
  in_near(x, y, tol = 1e-12)
}
