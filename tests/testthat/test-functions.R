
test_that("%||% outputs non-NULL when first argument is NULL", {
  expect_equal(10 %||% 20, 10)
  expect_equal(NULL %||% 20, 20)
})

test_that("%keepnull% outputs NULL when first argument is NULL", {
  expect_null(NULL %keepnull% 10)
  expect_equal(10 %keepnull% 20, 20)
})

test_that("non_null provides argument symbols", {
  x <- NULL
  y <- 10
  z <- NULL
  a <- 20
  expect_equal(non_null(y), rlang::exprs(y))
  expect_equal(non_null(x,y), rlang::exprs(y))
  expect_equal(non_null(x,y,z,a), rlang::exprs(y,a))
  expect_length(non_null(x,z), 0)
})
