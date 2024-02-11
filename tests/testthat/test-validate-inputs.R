
# ERROR CHECKING ----------------------------------------------------------

# Test numeric inputs converted to vectors
test_that("Numeric inputs are converted to vectors", {
  x <- list(
    numeric = 1:5,
    matrix = matrix(1:5),
    list = list(1:5),
    tibble = tibble::tibble(1:5),
    df = data.frame(1:5)
  )
  expect_equal(convert_vector(x$numeric), x$numeric)
  expect_equal(convert_vector(x$matrix), x$numeric)
  expect_equal(convert_vector(x$list), x$numeric)
  expect_equal(convert_vector(x$tibble), x$numeric)
  expect_equal(convert_vector(x$df), x$numeric)
})

# Test replacement of NA values in different classes
test_that("NA values are correctly replaced in non-vector inputs", {
  x <- list(
    numeric = c(1:5, NA, NaN, Inf, -Inf, 6:10),
    matrix = matrix(c(1:5, NA, NaN, Inf, -Inf, 6:10)),
    list = list(c(1:5, NA, NaN, Inf, -Inf, 6:10)),
    tibble = tibble::tibble(c(1:5, NA, NaN, Inf, -Inf, 6:10)),
    df = data.frame(c(1:5, NA, NaN, Inf, -Inf, 6:10))
  )
  xNA <- c(1:5, NA, NA, NA, NA, 6:10)
  expect_equal(standardize_NA(convert_vector(x$numeric)), xNA)
  expect_equal(standardize_NA(convert_vector(x$matrix)), xNA)
  expect_equal(standardize_NA(convert_vector(x$list)), xNA)
  expect_equal(standardize_NA(convert_vector(x$tibble)), xNA)
  expect_equal(standardize_NA(convert_vector(x$df)), xNA)
})

# Test errors from non-vector input
test_that("Inputs that cannot be converted to vectors give errors", {
  x <- list(
    matrix = matrix(c(1:5, NA, NaN, Inf, -Inf, 6:10), ncol = 2),
    tibble = tibble::tibble(1:5, c(NA, NaN, Inf, -Inf, NA), 6:10),
    df = data.frame(1:5, c(NA, NaN, Inf, -Inf, NA), 6:10)
  )
  expect_error(
    convert_vector(x$matrix),
    class = "crutils_error_incompatible_class"
  )
  expect_error(
    convert_vector(x$tibble),
    class = "crutils_error_incompatible_class"
  )
  expect_error(
    convert_vector(x$df),
    class = "crutils_error_incompatible_class"
  )
})

# Test errors from incompatible sizes
test_that("Inputs of unequal size throw an error", {
  x <- 1:10
  y <- 11:30

  expect_no_error(check_equal_length(x, x))
  expect_error(
    check_equal_length(x, y),
    class = "crutils_error_incompatible_size"
  )
})

# Test error from NA values
test_that("Inputs containing NA throw an error", {
  a <- c(1, 2, 3, 6, 7, 10)
  b <- c(1, 2, 5, 2, NA, NA)

  expect_no_error(check_NA(a))
  expect_error(check_NA(b), class = "crutils_error_contains_NA")
})
