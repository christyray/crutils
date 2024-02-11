
# PAIR_NEAR() -------------------------------------------------------------

# Test values being compared pairwise
test_that("pair_near detects pairwise matches rather than values anywhere", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 3, 4, 2, 5)

  expect_equal(pair_near(x, x), c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(pair_near(x, y), c(TRUE, FALSE, FALSE, FALSE, TRUE))
})

# Test correct outputs for different relative tolerances
test_that("pair_near detects matches for different relative tolerances", {
  x <- 10^rep(-3:3, each = 5)
  y <- x * rep(1 + 2 * 10^(-14:-10), 7)

  expect_equal(
    pair_near(x, y, tol = 1e-9),
    rep(c(TRUE, TRUE, TRUE, TRUE, TRUE), 7)
  )
  expect_equal(
    pair_near(x, y, tol = 1e-10),
    rep(c(TRUE, TRUE, TRUE, TRUE, FALSE), 7)
  )
  expect_equal(
    pair_near(x, y, tol = 1e-11),
    rep(c(TRUE, TRUE, TRUE, FALSE, FALSE), 7)
  )
  expect_equal(
    pair_near(x, y, tol = 1e-12),
    rep(c(TRUE, TRUE, FALSE, FALSE, FALSE), 7)
  )
  expect_equal(
    pair_near(x, y, tol = 1e-13),
    rep(c(TRUE, FALSE, FALSE, FALSE, FALSE), 7)
  )
  expect_equal(
    pair_near(x, y, tol = 1e-14),
    rep(c(FALSE, FALSE, FALSE, FALSE, FALSE), 7)
  )
})

# Test different input object classes
test_that("pair_near correctly handles vectors and table columns", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3, 5, 4)
  tbl <- tibble::tibble(x = x, y = y)

  expect_equal(pair_near(x, y), pair_near(tbl[,1], tbl[,2]))
  expect_equal(pair_near(x, y), pair_near(tbl[,1], y))
})

# Test correct outputs when values are exactly at the relative tolerance
test_that("pair_near correctly handles values exactly at tolerance", {
  x <- c(1000, 1000, 10000, 10000)
  y <- c(900, 990, 9990, 9999)

  expect_equal(pair_near(x, y, tol = 1e-1), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(pair_near(x, y, tol = 1e-2), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(pair_near(x, y, tol = 1e-3), c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(pair_near(x, y, tol = 1e-4), c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(pair_near(x, y, tol = 1e-5), c(FALSE, FALSE, FALSE, FALSE))
})

# Test handling of Inf, -Inf, NA, and NaN
test_that("pair_near correctly outputs NA for non-numeric values", {
  x <- c(10, 11, rep(c(Inf, -Inf, NA, NaN), each = 4))
  y <- c(10, 10, rep(c(Inf, -Inf, NA, NaN), 4))

  expect_equal(pair_near(x, y), c(TRUE, FALSE, rep(NA, 16)))
})

# IN_NEAR() ---------------------------------------------------------------

# Test matching of values that occur within the given list
test_that("in_near detects matches among the list of input values", {
  x1 <- 1:10
  x2 <- rep(x1, each = 3)
  y <- c(1, 2, 3, 5, 8)
  out1 <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  out2 <- rep(out1, each = 3)

  expect_equal(in_near(x1, y), out1)
  expect_equal(in_near(x2, y), out2)
  expect_equal(x1[in_near(x1, y)], y)
})

# Test correct outputs for different relative tolerances
test_that("in_near detects matches for different relative tolerances", {
  x <- 10^rep(-3:3, each = 5) * rep(1 + 2 * 10^(-14:-10), 7)
  y <- 10^(-3:3)

  expect_equal(
    in_near(x, y, tol = 1e-9),
    rep(c(TRUE, TRUE, TRUE, TRUE, TRUE), 7)
  )
  expect_equal(
    in_near(x, y, tol = 1e-10),
    rep(c(TRUE, TRUE, TRUE, TRUE, FALSE), 7)
  )
  expect_equal(
    in_near(x, y, tol = 1e-11),
    rep(c(TRUE, TRUE, TRUE, FALSE, FALSE), 7)
  )
  expect_equal(
    in_near(x, y, tol = 1e-12),
    rep(c(TRUE, TRUE, FALSE, FALSE, FALSE), 7)
  )
  expect_equal(
    in_near(x, y, tol = 1e-13),
    rep(c(TRUE, FALSE, FALSE, FALSE, FALSE), 7)
  )
  expect_equal(
    in_near(x, y, tol = 1e-14),
    rep(c(FALSE, FALSE, FALSE, FALSE, FALSE), 7)
  )
})

# Test different input object classes
test_that("in_near correctly handles vectors and table columns", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1, 3, 5, 10)
  tbl_x <- tibble::tibble(x = x)
  tbl_y <- tibble::tibble(y = y)

  expect_equal(in_near(x, y), in_near(tbl_x[,1], y))
  expect_equal(in_near(x, y), in_near(x, tbl_y[,1]))
  expect_equal(in_near(x, y), in_near(tbl_x[,1], tbl_y[,1]))
})

# Test correct outputs when values are exactly at the relative tolerance
test_that("in_near correctly handles values exactly at tolerance", {
  x <- c(900, 900, 990, 990, 9990, 9990, 9999, 9999)
  y <- c(1000, 10000)

  expect_equal(
    in_near(x, y, tol = 1e-1),
    rep(c(TRUE, TRUE, TRUE, TRUE), each = 2)
  )
  expect_equal(
    in_near(x, y, tol = 1e-2),
    rep(c(FALSE, TRUE, TRUE, TRUE), each = 2)
  )
  expect_equal(
    in_near(x, y, tol = 1e-3),
    rep(c(FALSE, FALSE, TRUE, TRUE), each = 2)
  )
  expect_equal(
    in_near(x, y, tol = 1e-4),
    rep(c(FALSE, FALSE, FALSE, TRUE), each = 2)
  )
  expect_equal(
    in_near(x, y, tol = 1e-5),
    rep(c(FALSE, FALSE, FALSE, FALSE), each = 2)
  )
})

# Test handling of Inf, -Inf, NA, and NaN
test_that("in_near correctly outputs NA for non-numeric values", {
  x <- c(10, 11, rep(c(Inf, -Inf, NA, NaN), each = 4), 20, 15, 10)
  y1 <- c(10, 20)
  y2 <- c(10, 20, NA)

  expect_equal(in_near(x, y1), c(TRUE, FALSE, rep(NA, 16), TRUE, FALSE, TRUE))
  expect_error(in_near(x, y2), class = "crutils_error_contains_NA")
})

# Test that infix operator is equivalent to full function
test_that("%~% is equivalent to in_near", {
  x1 <- 1:10
  x2 <- rep(x1, each = 3)
  y <- c(1, 2, 3, 5, 8)
  out1 <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
  out2 <- rep(out1, each = 3)

  expect_equal(x1 %~% y, out1)
  expect_equal(x1 %~% y, in_near(x1, y))
  expect_equal(x2 %~% y, out2)
  expect_equal(x2 %~% y, in_near(x2, y))
  expect_equal(x1[x1 %~% y], y)
})
