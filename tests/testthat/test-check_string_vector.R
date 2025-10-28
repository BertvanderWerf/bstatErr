# Helper for indirect testing
test_check_string_vector <- function(str, ...) {
  check_string_vector(str, ...)
}

test_that("indirect basic check succeeds", {
  expect_equal(test_check_string_vector(c("a","b","c")), c("a","b","c"))
})

test_that("indirect type error", {
  expect_error(test_check_string_vector(123), "Argument 'str' in function 'test_check_string_vector' must be a string value.")
})

test_that("indirect null error", {
  expect_error(test_check_string_vector(NULL), "Argument 'str' in function 'test_check_string_vector' must be a non-NULL string value.")
})

test_that("indirect length error", {
  expect_error(test_check_string_vector(character(0), allow_zero_length=FALSE), "Argument 'str' in function 'test_check_string_vector' has zero length, the vector must have at least one value.")
})

test_that("indirect missing value error", {
  expect_error(test_check_string_vector(c(NA,"missing")), "Argument 'str' in function 'test_check_string_vector' must not contain missing values.")
})

test_that("indirect allow_null works", {
  expect_equal(test_check_string_vector(NULL, allow_null = TRUE), NULL)
})

test_that("indirect allow_na works", {
  expect_true(all(is.na(test_check_string_vector(as.character(NA), allow_na = TRUE))))
})

test_that("indirect allow_empty works", {
  expect_true(sum(""==test_check_string_vector(c("a", "b", ""), allow_empty = TRUE))==1)
})

test_that("indirect allow_duplicates works", {
  expect_true(sum("a"==test_check_string_vector(c("a", "a", "b"), allow_duplicates = TRUE))==2)
  expect_error(test_check_string_vector(c("a", "a", "b"), allow_duplicates = FALSE), "Argument 'str' in function 'test_check_string_vector' must not contain duplicated values.")
})

test_that("indirect must_have_names works", {
  expect_equal(test_check_string_vector(c(a="xyz", b="abc")), c(a="xyz", b="abc"))
  expect_equal(test_check_string_vector(c(a="xyz", b="abc"), must_have_names = TRUE), c(a="xyz", b="abc"))
  expect_error(test_check_string_vector(c(a="xyz", b="abc", "XYZ"), must_have_names = TRUE), "Argument 'str' in function 'test_check_string_vector' must be a vector with unique names unequal to ''.")
})
