# Helper for indirect testing
test_check_data_frame <- function(df, ...) {
  check_data_frame(df, ...)
}

test_that("indirect basic check succeeds", {
  expect_equal(test_check_data_frame(data.frame(a=1:10, b="abc")), data.frame(a=1:10, b="abc"))
})

test_that("indirect type error", {
  expect_error(test_check_data_frame(1), "Argument 'df' in function 'test_check_data_frame' must be a data.frame")
})

test_that("indirect null error", {
  expect_error(test_check_data_frame(NULL), "Argument 'df' in function 'test_check_data_frame' must be a non-NULL data.frame")
})

test_that("indirect allow_null works", {
  expect_equal(test_check_data_frame(NULL, allow_null = TRUE), NULL)
})

test_that("indirect allow_zero_rows works", {
  expect_equal(test_check_data_frame(data.frame(a=character(0), b=numeric(0)), allow_zero_rows = TRUE), data.frame(a=character(0), b=numeric(0)))
  expect_error(test_check_data_frame(data.frame(a=character(0), b=numeric(0)), allow_zero_rows = FALSE),
               "Argument 'df' in function 'test_check_data_frame' has zero rows; must have at least one row.")
})
