# Helper for indirect testing
test_check_list <- function(lst, ...) {
  check_list(lst, ...)
}

test_that("indirect basic check succeeds", {
  expect_equal(test_check_list(list(a=1:10, b="abc")), list(a=1:10, b="abc"))
})

test_that("indirect type error", {
  expect_error(test_check_list(1), "Argument 'lst' in function 'test_check_list' must be a list")
})

test_that("indirect null error", {
  expect_error(test_check_list(NULL), "Argument 'lst' in function 'test_check_list' must be a non-NULL list")
})

test_that("indirect allow_null works", {
  expect_equal(test_check_list(NULL, allow_null = TRUE), NULL)
})
