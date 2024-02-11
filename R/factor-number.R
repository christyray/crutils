
#' Convert factored variable to numeric
#'
#' Wrapper function for the base R method of factor conversion to make the code
#' clearer in other functions
#'
#' @param x Factored vector to be converted
#'
#' @return A numeric vector converted from \code{x}
#'
#' @export
#'
#' @examples
#' x <- factor(c(1, 2, 2, 4, 3, 3), levels = c(1, 2, 3, 4))
#' fct2num(x)

fct2num <- function(x) {
  as.numeric(levels(x))[x]
}

#' Round numbers to the closest values in a list
#'
#' Rounds a numeric vector to the nearest values from a list of arbitrary
#' numbers. Helpful to remove small rounding or floating-point errors when the
#' list of possible values for the vector is known.
#'
#' Used as the first step in converting a numeric vector to a factor in the
#' [factor_number()] function. Originally adapted from a [StackOverflow
#' post](https://stackoverflow.com/questions/12861061/).
#'
#' @param values Numeric vector with values that should be rounded
#' @param round_to Numeric vector with that the `values` should be rounded to
#'
#' @return A numeric vector with the `values` rounded to the closest number in
#'   `round_to`
#'
#' @seealso [factor_number()] converts numeric vectors to factored vectors with
#'   the numbers as the factor levels
#'
#' @export
#'
#' @examples
#' x <- c(3.0003, 4.0002, 1.01, 2.02, 10.1, 5, 5.5, 7)
#' y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' round_list(values = x, round_to = y)

round_list <- function(values, round_to) {
  # Convert inputs to numeric vectors
  values <- convert_vector(values)
  round_to <- convert_vector(round_to)

  # Throw an error if any of the values to be rounded are NA
  check_NA(standardize_NA(values))

  # List of numbers to compare to needs to be sorted for `findInterval`
  round_to <- sort(round_to)

  # `findInterval` returns the interval in round_to that each value belongs to;
  # i.e., interval is the index of the closest number smaller than the value and
  # interval + 1 is the index of the closest number larger than the value
  interval <- findInterval(values, round_to)

  # Use the interval to return the closest numbers on either side of the value;
  # need to include -Inf and Inf for numbers that are outside of the range of
  # the rounding list
  low <- c(-Inf, round_to)[interval + 1]
  high <- c(round_to, Inf)[interval + 1]

  # Determine where the values are closer to the higher number
  # Absolute value is not necessary because low < values < high
  idx_high <- high - values < values - low

  # Replace the low values with the high values where the high values are better
  low[idx_high] <- high[idx_high]
  return(low)
}

#' Convert numeric vector to factored variable
#'
#' Convert a numeric vector to a factored variable with a specific set of
#' levels. Useful in cases where the possible levels for the vector are known
#' and it is necessary to remove small rounding or floating-point errors.
#'
#' Function attempts to convert factor and character inputs to numeric first;
#' throws an error if that conversion introduces NAs. Uses the [round_list()]
#' function to first round each value to the closest number within the set of
#' possible levels, then maps each value to the matching factor level and label.
#'
#' @param x Numeric vector to be converted to factored variable
#' @param levels Levels for the factored variable
#' @param labels Labels for the factored variable levels; defaults to the values
#'   of the levels
#'
#' @return Factored vector created from `x` with input `levels` and `labels`
#'
#' @seealso [round_list()] to round numeric values to the closest numbers from a
#'   given list
#'
#' @export
#'
#' @examples
#' x <- c(3.0003, 4.0002, 1.01, 2.02, 10.1, 5, 5.5, 7)
#' y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' factor_number(x, levels = y, labels = y)

factor_number <- function(x, levels, labels = levels) {
  # Convert inputs to vectors
  x <- convert_vector(x)
  levels <- convert_vector(levels)
  labels <- convert_vector(labels)

  # Check that none of the levels or labels are NA
  check_NA(labels)
  check_NA(levels)

  # Convert factor and character inputs to numeric
  tryCatch({
    if (inherits(x, "factor")) {
      x <- as.numeric(levels(x))[x]
    } else if (inherits(x, "character")) {
      x <- as.numeric(x)
    }
  }, warning = function(warn, name, call = rlang::caller_env(n = 4)) {

    cli::cli_abort(c(
      "{.var x} must be coercible to {.cls numeric}.",
      "x" = "Conversion of {.var x} introduced NAs."
    ), call = call, class = "crutils_error_contains_NA")
  })

  # Convert the numeric input to a factored variable
  factor(
    round_list(x, round_to = levels),
    levels = levels,
    labels = labels
  )
}
