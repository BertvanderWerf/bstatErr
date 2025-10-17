# Helper for indirect testing
test_check_logical <- function(boolean, ...) {
  check_logical(boolean, ...)
}

test_that("indirect basic check succeeds", {
  expect_equal(test_check_logical(TRUE), TRUE)
})

test_that("indirect type error", {
  expect_error(test_check_logical(1), "Argument 'boolean' in function 'test_check_logical' must be a logical value.")
})

test_that("indirect null error", {
  expect_error(test_check_logical(NULL), "Argument 'boolean' in function 'test_check_logical' must be a non-NULL logical value.")
})

test_that("indirect length error", {
  expect_error(test_check_logical(c(TRUE, FALSE)), "Argument 'boolean' in function 'test_check_logical' must be a single logical value.")
})

test_that("indirect missing value error", {
  expect_error(test_check_logical(as.logical(NA)), "Argument 'boolean' in function 'test_check_logical' must not be NA or NaN.")
})

test_that("indirect allow_null works", {
  expect_equal(test_check_logical(NULL, allow_null = TRUE), NULL)
})

test_that("indirect allow_na works", {
  expect_true(is.na(test_check_logical(as.character(NA), allow_na = TRUE)))
})
