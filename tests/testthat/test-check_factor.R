# Helper for indirect testing
test_check_factor <- function(fct, ...) {
  check_factor(fct, ...)
}

test_that("indirect basic check succeeds", {
  f <- factor(c("a", "b", "c"))
  expect_invisible(test_check_factor(f))
})

test_that("indirect NULL error", {
  expect_error(
    test_check_factor(NULL),
    "Argument 'fct' in function 'test_check_factor' must be a non-NULL factor."
  )
})

test_that("indirect NULL allowed", {
  expect_invisible(test_check_factor(NULL, allow_null = TRUE))
})

test_that("indirect type error (non-factor input)", {
  expect_error(
    test_check_factor(1:3),
    "Argument 'fct' in function 'test_check_factor' must be a factor."
  )
  expect_error(
    test_check_factor(c("a", "b")),
    "Argument 'fct' in function 'test_check_factor' must be a factor."
  )
})

test_that("indirect zero-length error", {
  f0 <- factor(character(0))
  expect_error(
    test_check_factor(f0, allow_zero_length = FALSE),
    "Argument 'fct' in function 'test_check_factor' has zero length; must have at least one element."
  )
})

test_that("indirect zero-length allowed", {
  f0 <- factor(character(0))
  expect_error(test_check_factor(f0, allow_zero_length = TRUE),
               "Argument 'fct' in function 'test_check_factor' cannot have zero levels.")
})

test_that("indirect NA value error", {
  f <- factor(c("a", NA, "b"))
  expect_error(
    test_check_factor(f, allow_na = FALSE),
    "Argument 'fct' in function 'test_check_factor' must not contain missing values \\(NA\\)."
  )
})

test_that("indirect NA values allowed", {
  f <- factor(c("a", NA, "b"))
  expect_invisible(test_check_factor(f, allow_na = TRUE))
})

test_that("indirect one-level error", {
  f <- factor(rep("a", 3))
  expect_error(
    test_check_factor(f, allow_one_level = FALSE),
    "Argument 'fct' in function 'test_check_factor' cannot have only one level."
  )
})

test_that("indirect one-level allowed", {
  f <- factor(rep("a", 3))
  expect_invisible(test_check_factor(f, allow_one_level = TRUE))
})

test_that("indirect zero-level error", {
  f_zero_levels <- factor(integer(), levels = character())
  expect_error(
    test_check_factor(f_zero_levels, allow_zero_levels = FALSE, allow_zero_length = TRUE),
    "Argument 'fct' in function 'test_check_factor' cannot have zero levels."
  )
})

test_that("indirect zero-level allowed", {
  f_zero_levels <- factor(integer(), levels = character())
  expect_invisible(
    test_check_factor(f_zero_levels, allow_zero_levels = TRUE, allow_zero_length = TRUE)
  )
})

test_that("indirect empty levels error", {
  f <- factor(c("a", "b"), levels = c("a", "b", "c"))  # 'c' has no observations
  expect_error(
    test_check_factor(f, allow_empty_levels = FALSE),
    "Argument 'fct' in function 'test_check_factor' cannot have factor levels with no observations."
  )
})

test_that("indirect empty levels allowed", {
  f <- factor(c("a", "b"), levels = c("a", "b", "c"))
  expect_invisible(test_check_factor(f, allow_empty_levels = TRUE))
})
