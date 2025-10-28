# Helper for indirect testing
test_check_numeric_vector <- function(num, ...) {
  check_numeric_vector(num, ...)
}

test_that("indirect basic check succeeds", {
  expect_equal(test_check_numeric_vector(3.14), 3.14)
})

test_that("indirect type error", {
  expect_error(test_check_numeric_vector("abc"), "Argument 'num' in function 'test_check_numeric_vector' must be a numeric value.")
})

test_that("indirect null error", {
  expect_error(test_check_numeric_vector(NULL), "Argument 'num' in function 'test_check_numeric_vector' must be a non-NULL numeric value.")
})

test_that("indirect length error", {
  expect_error(test_check_numeric_vector(numeric(0), allow_zero_length=FALSE), "Argument 'num' in function 'test_check_numeric_vector' has zero length, the vector must have at least one value.")
})

test_that("indirect missing value error", {
  expect_error(test_check_numeric_vector(as.numeric(NA)), "Argument 'num' in function 'test_check_numeric_vector' must not contain missing values.")
})

test_that("indirect allow_null works", {
  expect_equal(test_check_numeric_vector(NULL, allow_null = TRUE), NULL)
})

test_that("indirect allow_na works", {
  expect_true(is.na(test_check_numeric_vector(as.character(NA), allow_na = TRUE)))
})

test_that("indirect allow_inf works", {
  expect_true(sum(is.infinite(test_check_numeric_vector(c(1,2, Inf, -Inf ), allow_inf = TRUE)))==2)
})

test_that("indirect must_have_names works", {
  expect_equal(test_check_numeric_vector(c(a=10, b=20)), c(a=10, b=20))
  expect_equal(test_check_numeric_vector(c(a=10, b=20), must_have_names = TRUE), c(a=10, b=20))
  expect_error(test_check_numeric_vector(c(a=10, b=20, 30), must_have_names = TRUE), "Argument 'num' in function 'test_check_numeric_vector' must be a vector with unique names unequal to ''.")
})
