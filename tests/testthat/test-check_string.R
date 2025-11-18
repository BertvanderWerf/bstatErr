# Helper for indirect testing
test_check_string <- function(str, ...) {
  check_string(str, ...)
}

test_that("indirect basic check succeeds", {
  expect_equal(test_check_string("abc"), "abc")
})

test_that("indirect type error", {
  expect_error(test_check_string(1), "Argument 'str' in function 'test_check_string' must be a character string.")
})

test_that("indirect null error", {
  expect_error(test_check_string(NULL), "Argument 'str' in function 'test_check_string' must be a non-NULL character string.")
})

test_that("indirect length error", {
  expect_error(test_check_string(c("abc", "def")), "Argument 'str' in function 'test_check_string' must be a single character string.")
})

test_that("indirect missing value error", {
  expect_error(test_check_string(as.character(NA)),
               "Argument 'str' in function 'test_check_string' must not contain missing values (NA or NaN).",
               fixed = TRUE)
  expect_error(test_check_string(NaN), "Argument 'str' in function 'test_check_string' must be a character string.")
  expect_error(test_check_string(NA), "Argument 'str' in function 'test_check_string' must be a character string.")
})

test_that("indirect empty string error", {
  expect_error(test_check_string(''), "Argument 'str' in function 'test_check_string' must not be an empty string.")
})

test_that("indirect allow_null works", {
  expect_equal(test_check_string(NULL, allow_null = TRUE), NULL)
})

test_that("indirect allow_na works", {
  expect_true(is.na(test_check_string(as.character(NA), allow_na = TRUE)))
})

test_that("indirect allow_empty works", {
  expect_equal(test_check_string(c(""), allow_empty = TRUE), '')
})

