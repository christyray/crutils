
#' Replace NA values in data frame
#'
#' Replace all NA values in a data frame with a specific value (defaults to 0).
#'
#' Applies [tidyr::replace_na()] to all columns in the data frame to replace all
#' NAs with the given value.
#'
#' @param df Data frame with NA values to be replaced
#' @param replace Value to replace all NAs with
#'
#' @return Data frame with NA values replaced with the `replace` value
#'
#' @seealso [tidyr::replace_na()] replaces all NA values in a specific column
#'   with a given value.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   A = c(1, NA, 2, 3),
#'   B = c(NA, 2, NA, 3),
#'   C = c(1, 2, 3, 4),
#'   D = c(NA, NA, NA, 2)
#' )
#' replace_all_na(df, replace = 0)
#'

replace_all_na <- function(df, replace = 0) {
  # Replace all NA values with 0s across all columns of the data frame
  df |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      \(x) tidyr::replace_na(x, replace)
    ))
}
