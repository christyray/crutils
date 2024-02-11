
# REPLACE_ALL_NA() --------------------------------------------------------

test_that("All NAs are replaced with given value", {
  xNA <- tibble::tibble(
    x = c(1:5, NA, NA, Inf, 15, NaN, 6:10),
    y = c(16:30),
    z = c(NA, NA, NA, Inf, NaN, -1:-10)
  )
  x0 <- tibble::tibble(
    x = c(1:5, 0, 0, Inf, 15, 0, 6:10),
    y = c(16:30),
    z = c(0, 0, 0, Inf, 0, -1:-10)
  )
  x10 <- tibble::tibble(
    x = c(1:5, 10, 10, Inf, 15, 10, 6:10),
    y = c(16:30),
    z = c(10, 10, 10, Inf, 10, -1:-10)
  )

  expect_equal(replace_all_na(xNA, 0), x0)
  expect_equal(replace_all_na(xNA, 10), x10)
})
