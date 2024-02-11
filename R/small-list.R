#' Smaller list preview
#'
#' @param x A list or list-type object (e.g., a data frame)
#' @param level The maximum number of list levels to display, defaults to 4
#'
#' @return The output of the `str()` function with specific options
#' @export
#'
#' @examples
#' x <- list(a = 1:3, b = list(c = 5:9))
#' small(x)

small <- function(x, level = 4) {
  utils::str(x, max.level = level, list.len = 3, vec.len = 3, give.attr = FALSE)
}
