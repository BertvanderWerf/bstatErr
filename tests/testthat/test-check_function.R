# Helper for indirect testing
test_check_function <- function(fun, ...) {
  check_function(fun, ...)
}

test_that("indirect basic check succeeds", {
  expect_equal(test_check_function(mean), mean)
})

test_that("indirect type error", {
  expect_error(test_check_function(1), "Argument 'fun' in function 'test_check_function' must be a function name")
})

test_that("indirect null error", {
  expect_error(test_check_function(NULL), "Argument 'fun' in function 'test_check_function' must be a non-NULL function name")
})

test_that("indirect allow_null works", {
  expect_equal(test_check_function(NULL, allow_null = TRUE), NULL)
})
