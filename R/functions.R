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

#' Defaults for NULL values
#'
#' Outputs `a` unless `a` is NULL, then outputs `b`. Helpful if an argument is
#' optional and there is a means to calculate the default
#'
#' @param a The default output
#' @param b The output if `a` is `NULL`
#'
#' @export
#' @name null-drop
#'
#' @examples
#' input <- NULL
#' x <- input %||% sum(c(1,2,3))

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' Keep NULL value if provided
#'
#' Opposite of \code{\link{null-drop}}. Outputs `a` if `a` is NULL; otherwise,
#' outputs `b`. Helpful to provide a default value but also a means to override
#' the default value.
#'
#' @param a The output if its value is `NULL`
#' @param b The output if `a` is not `NULL`
#'
#' @export
#' @name null-keep
#'
#' @examples
#' input <- NULL
#' x <- input %||% sum(c(1,2,3))

`%keepnull%` <- function(a, b) {
  if (is.null(a)) a else b
}

#' Return non-null arguments as symbols that can be evaluated
#'
#' @param ... Function arguments
#'
#' @return Symbols that can be evaluated in R, generated using
#'   \code{\link[rlang]{ensyms}}
#' @export
#'
#' @examples
#' x <- NULL
#' y <- 1
#' non_null(x,y)

non_null <- function(...) {
  pf <- parent.frame()
  args <- rlang::ensyms(...)

  notnull <- sapply(args, function(arg) { !is.null(eval(arg, envir = pf)) })
  args[notnull]
}
