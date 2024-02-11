
test_that("small runs without error", {
  x <- list(a = 1:3, b = list(c = 5:9))
  expect_no_error(capture.output(small(x), file = nullfile()))
})
