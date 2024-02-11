
# FCT2NUM() ---------------------------------------------------------------

# Factored variables are converted to numeric
test_that("Factored variables are converted to numeric", {
  x1 <- factor(
    c("10", "100", "100", "10", "1000", "1"),
    levels = c("1", "10", "100", "1000")
  )
  y1 <- c(10, 100, 100, 10, 1000, 1)
  x2 <- factor(
    c("NA", "Inf", "-Inf", "10", "1000", "10", "Inf", "NaN"),
    levels = c("10", "1000", "Inf", "-Inf", "NA", "NaN")
  )
  y2 <- c(NA, Inf, -Inf, 10, 1000, 10, Inf, NaN)

  expect_equal(fct2num(x1), y1)
  expect_equal(suppressWarnings({fct2num(x2)}), y2)
})

# ROUND_LIST() ------------------------------------------------------------

# Values are rounded to the nearest value in the input list
test_that("round_list rounds values to nearest number in input list", {
  x <- c(3.0003, 4.0002, 1.01, -1, -2, 2.02, 10.1, 5, 5.5, 7)
  y <- c(1, 2, 3, -1, 4, 8, -3, 11)
  out <- c(3, 4, 1, -1, -3, 2, 11, 4, 4, 8)

  expect_equal(round_list(x, y), out)
})

# Values outside of range of list are rounded to the max/min
test_that("round_list rounds values outside of list range", {
  x <- c(-6, 2, 3, 8, 11, 15, 20)
  y <- c(-4, 0, 4, 8, 12, 16)
  out <- c(-4, 0, 4, 8, 12, 16, 16)

  expect_equal(round_list(x, y), out)
})

# Values are rounded as expected when exactly in between list values
test_that("round_list rounds down when exactly in between values", {
  x <- c(12.25, 10.5, 6, 5, 4, 3, 2, 1, -1, -2, -3, -4, -5, -6, -10.5, -12.25)
  y <- c(-12.5, 12.5, -12, 12, -11, 11, -10, 10, -5, 5, -3, 3, -1, 1)
  out <- c(12, 10, 5, 5, 3, 3, 1, 1, -1, -3, -3, -5, -5, -5, -11, -12.5)

  expect_equal(round_list(x, y), out)
})

# Handles different input object classes
test_that("round_list accepts vectors and table columns as inputs", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1, 3, 5, 10)
  tbl_x <- tibble::tibble(x = x)
  tbl_y <- tibble::tibble(y = y)

  expect_equal(round_list(x, y), round_list(tbl_x[,1], y))
  expect_equal(round_list(x, y), round_list(x, tbl_y[,1]))
  expect_equal(round_list(x, y), round_list(tbl_x[,1], tbl_y[,1]))
})

# Handles Inf, -Inf, NA, NaN
test_that("round_list handles Inf and NA/NaN values", {
  x1 <- c(3, -3, 5, -5, 10, -10)
  x2 <- c(3, -3, 5, -5, 10, -10, Inf, -Inf)

  y1 <- c(-10, -5, 0, 5, 10)
  y2 <- c(-Inf, -10, -5, 0, 5, 10, Inf)
  y3 <- c(-Inf, -10, -5, NA, 0, NaN, 5, 10, Inf)
  y4 <- c(-Inf, Inf)

  out1 <- c(5, -5, 5, -5, 10, -10)

  expect_equal(round_list(x1, y1), out1)
  expect_equal(round_list(x1, y2), out1)
  expect_equal(round_list(x1, y3), out1)
  expect_equal(round_list(x1, y4), rep(-Inf, length(x1)))

  expect_error(round_list(x2, y1), class = "crutils_error_contains_NA")
  expect_error(round_list(x2, y2), class = "crutils_error_contains_NA")
  expect_error(round_list(x2, y3), class = "crutils_error_contains_NA")
  expect_error(round_list(x2, y4), class = "crutils_error_contains_NA")
})

# FACTOR_NUMBER() ---------------------------------------------------------

# Values are converted to a factor with the specified levels
test_that("factor_number converts input numbers to factors", {
  x <- seq(-2, 2, by = 0.5)
  y <- c(-2, -1, 0, 1, 2)
  out <- factor(c(-2, -2, -1, -1, 0, 0, 1, 1, 2), levels = -2:2)

  expect_equal(factor_number(x, y), out)
})

# Values are converted to a factor with the specified labels
test_that("factor_number assigns the input labels to factors", {
  x <- c(-2, -1, 0, 1, 2, -1.5, -0.5, 0.5, 1.5)
  y <- c(-2, -1, 0, 1, 2)
  labels <- c("-Two", "-One", "Zero", "One", "Two")
  out <- factor(
    c(-2, -1, 0, 1, 2, -2, -1, 0, 1),
    levels = -2:2,
    labels <- c("-Two", "-One", "Zero", "One", "Two")
  )

  expect_equal(factor_number(x, y, labels), out)
})

# Function accepts character and factor input
test_that("factor_number accepts character and factor input", {
  x_num <- c(-3, -2, -1, 3, 2, 1, 0, 0.5, 1.5, 2.5, -2.5, -1.5, -0.5)
  x_chr <- c("-3", "-2", "-1", "3", "2", "1", "0", "0.5", "1.5", "2.5",
             "-2.5", "-1.5", "-0.5")
  x_fac <- factor(
    c(-3, -2, -1, 3, 2, 1, 0, 0.5, 1.5, 2.5, -2.5, -1.5, -0.5),
    levels = c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3)
  )
  x_chf <- factor(
    c("-3", "-2", "-1", "3", "2", "1", "0", "0.5", "1.5", "2.5",
      "-2.5", "-1.5", "-0.5"),
    levels = c("-3", "-2.5", "-2", "-1.5", "-1", "-0.5", "0",
               "0.5", "1", "1.5", "2", "2.5", "3")
  )
  x_err <- factor(c("-3", "-2", "-1", "NA"))

  y <- c(3, 2, 1, 0, -1, -2, -3)
  labels <- c("3+", "2+", "1+", "0", "1-", "2-", "3-")

  out <- factor(
    c(-3, -2, -1, 3, 2, 1, 0, 0, 1, 2, -3, -2, -1),
    levels = c(3, 2, 1, 0, -1, -2, -3),
    labels = c("3+", "2+", "1+", "0", "1-", "2-", "3-")
  )

  expect_equal(factor_number(x_num, y, labels), out)
  expect_equal(factor_number(x_chr, y, labels), out)
  expect_equal(factor_number(x_fac, y, labels), out)
  expect_equal(factor_number(x_chf, y, labels), out)

  expect_error(
    factor_number(x_err, y, labels),
    class = "crutils_error_contains_NA"
  )
})

# Handles different input object classes
test_that("factor_number accepts vectors and table columns as inputs", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1, 3, 5, 10)
  labels <- c("One", "Three", "Five", "Ten")
  tbl_x <- tibble::tibble(x = x)
  tbl_y <- tibble::tibble(y = y)
  tbl_l <- tibble::tibble(labels = labels)

  expect_equal(
    factor_number(x, y, labels),
    factor_number(tbl_x[,1], y, labels)
  )
  expect_equal(
    factor_number(x, y, labels),
    factor_number(x, tbl_y[,1], labels)
  )
  expect_equal(
    factor_number(x, y, labels),
    factor_number(x, y, tbl_l[,1])
  )
  expect_equal(
    factor_number(x, y, labels),
    factor_number(tbl_x[,1], tbl_y[,1], tbl_l[,1])
  )
})

# Handles Inf, -Inf, NA, NaN
test_that("factor_number handles Inf and NA/NaN values", {
  x1 <- c(3, -3, 5, -5, 10, -10)
  x2 <- c(3, -3, 5, -5, 10, -10, Inf, -Inf)

  y1 <- c(-10, -5, 0, 5, 10)
  y2 <- c(-Inf, -10, -5, 0, 5, 10, Inf)
  y3 <- c(-Inf, -10, -5, NA, 0, NaN, 5, 10, Inf)
  y4 <- c(-Inf, Inf)

  out1 <- factor(c(5, -5, 5, -5, 10, -10), levels = c(-10, -5, 0, 5, 10))
  out2 <- factor(
    c(5, -5, 5, -5, 10, -10),
    levels = c(-Inf, -10, -5, 0, 5, 10, Inf)
  )
  out4 <- factor(rep(-Inf, length(x1)), levels = c(-Inf, Inf))

  expect_equal(factor_number(x1, y1), out1)
  expect_equal(factor_number(x1, y2), out2)
  expect_equal(factor_number(x1, y4), out4)

  expect_error(factor_number(x1, y3), class = "crutils_error_contains_NA")
  expect_error(factor_number(x2, y1), class = "crutils_error_contains_NA")
  expect_error(factor_number(x2, y2), class = "crutils_error_contains_NA")
  expect_error(factor_number(x2, y3), class = "crutils_error_contains_NA")
  expect_error(factor_number(x2, y4), class = "crutils_error_contains_NA")
})
