# Helper for indirect testing
test_check_numeric <- function(num, ...) {
  check_numeric(num, ...)
}

test_that("indirect basic check succeeds", {
  expect_equal(test_check_numeric(3.14), 3.14)
})

test_that("indirect type error", {
  expect_error(test_check_numeric("abc"), "Argument 'num' in function 'test_check_numeric' must be a numeric value.")
})

test_that("indirect null error", {
  expect_error(test_check_numeric(NULL), "Argument 'num' in function 'test_check_numeric' must be a non-NULL numeric value.")
})

test_that("indirect length error", {
  expect_error(test_check_numeric(c(1, 2)), "Argument 'num' in function 'test_check_numeric' must be a single numeric value.")
})

test_that("indirect missing value error", {
  expect_error(test_check_numeric(as.numeric(NA)), "Argument 'num' in function 'test_check_numeric' must not contain missing values (NA or NaN).",
               fixed=TRUE)
})

test_that("indirect allow_null works", {
  expect_equal(test_check_numeric(NULL, allow_null = TRUE), NULL)
})

test_that("indirect allow_na works", {
  expect_true(is.na(test_check_numeric(as.character(NA), allow_na = TRUE)))
})

test_that("indirect allow_inf works", {
  expect_true(is.infinite(test_check_numeric(Inf, allow_inf = TRUE)))
})

