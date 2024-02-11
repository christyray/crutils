
#' Convert input object to a vector
#'
#' Validates that numeric inputs are one-dimensional and converts the input to a
#' vector if possible
#'
#' @param x Numeric object to be validated and converted to vector
#' @inheritParams rlang::args_error_context
#'
#' @return Numeric vector

convert_vector <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  # Check that the input is not more than one dimension
  if ((!is.null(nrow(x)) && nrow(x) > 1 && ncol(x) > 1)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a vector",
      "i" = "{.arg {arg}} is {nrow(x) %||% length(x)} by {ncol(x) %||% 1}"
    ), call = call, class = "crutils_error_incompatible_class")
  }

  # Convert input to vector if it was given as table columns
  x <- as.vector(unlist(x))
}

#' Confirm that two vectors are equal length
#'
#' Checks that the two input vectors have the same length and prints a formatted
#' error if they do not
#'
#' @param x,y Numeric vectors to check for size equality
#' @inheritParams rlang::args_error_context

check_equal_length <- function(x, y, call = rlang::caller_env()) {
  # Capture the argument names as symbols for the error message
  xarg <- rlang::ensym(x)
  yarg <- rlang::ensym(y)

  # Check that x and y are the same length
  if (length(x) != length(y)) {
    cli::cli_abort(c(
      "{.arg {xarg}} and {.arg {yarg}} must be the same length",
      "i" = "{.arg {xarg}} is length {length(x)}",
      "i" = "{.arg {yarg}} is length {length(y)}"
    ), call = call, class = "crutils_error_incompatible_size")
  }
}

#' Confirm that vector has no NA values
#'
#' Checks that the input vector does not contain any NAs and prints a formatted
#' error if it does
#'
#' @param x Numeric vectors to check for NAs
#' @inheritParams rlang::args_error_context

check_NA <- function(x,
                     arg = rlang::caller_arg(x),
                     call = rlang::caller_env()) {
  # Check that none of the values in x are NA
  if (anyNA(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must not contain any {.code NA} values",
      "i" = ngettext(
        sum(is.na(x)),
        "{.arg {arg}} contains {sum(is.na(x))} {.code NA} value",
        "{.arg {arg}} contains {sum(is.na(x))} {.code NA} values"
      )
    ), call = call, class = "crutils_error_contains_NA")
  }
}

#' Replace Inf and NaN values with NA
#'
#' Replaces all Inf and NaN values with NA in numeric vector
#'
#' @param x Numeric vector with Inf and/or NaN values to replace
#' @inheritParams rlang::args_error_context
#'
#' @return Explicitly a numeric vector with all non-numeric values replaced with
#' NA

standardize_NA <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  # Replace any Inf, -Inf, and NaN with NA
  is.na(x) <- !is.finite(x)
  return(x)
}
