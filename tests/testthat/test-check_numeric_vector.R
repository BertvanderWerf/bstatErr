# Comprehensive Unit Tests for check_numeric_vector Function
# File: tests/testthat/test-check_numeric_vector_improved.R

# library(testthat)

# Helper function for indirect testing
test_check_numeric_vector <- function(num, ...) {
  check_numeric_vector(num, ...)
}

describe("check_numeric_vector", {

  # ==========================================================================
  # Test 1: Basic Functionality
  # ==========================================================================

  it("accepts valid numeric vectors", {
    expect_equal(test_check_numeric_vector(c(1, 2, 3)), c(1, 2, 3))
    expect_equal(test_check_numeric_vector(c(1.5, 2.7, 3.9)), c(1.5, 2.7, 3.9))
    expect_equal(test_check_numeric_vector(1:10), 1:10)
  })

  it("accepts single numeric value", {
    expect_equal(test_check_numeric_vector(42), 42)
    expect_equal(test_check_numeric_vector(3.14), 3.14)
  })

  it("accepts integers", {
    expect_equal(test_check_numeric_vector(c(1L, 2L, 3L)), c(1L, 2L, 3L))
  })

  it("returns invisibly", {
    result <- withVisible(test_check_numeric_vector(c(1, 2, 3)))
    expect_false(result$visible)
  })

  # ==========================================================================
  # Test 2: NULL Handling
  # ==========================================================================

  it("NULL error when not allowed", {
    expect_error(
      test_check_numeric_vector(NULL),
      "Argument 'num' in function 'test_check_numeric_vector' must be a non-NULL numeric vector."
    )
  })

  it("NULL accepted when allowed", {
    expect_null(test_check_numeric_vector(NULL, allow_null = TRUE))
  })

  # ==========================================================================
  # Test 3: Type Validation
  # ==========================================================================

  it("character rejected", {
    expect_error(
      test_check_numeric_vector("not numeric"),
      "must be a numeric vector"
    )
  })

  it("character vector rejected", {
    expect_error(
      test_check_numeric_vector(c("a", "b", "c")),
      "must be a numeric vector"
    )
  })

  it("logical rejected", {
    expect_error(
      test_check_numeric_vector(TRUE),
      "must be a numeric vector"
    )
  })

  it("list rejected", {
    expect_error(
      test_check_numeric_vector(list(1, 2, 3)),
      "must be a numeric vector"
    )
  })

  it("data.frame rejected", {
    expect_error(
      test_check_numeric_vector(data.frame(x = 1:3)),
      "must be a numeric vector"
    )
  })

  # ==========================================================================
  # Test 4: Zero Length
  # ==========================================================================

  it("zero length error when not allowed", {
    expect_error(
      test_check_numeric_vector(numeric(0)),
      "has zero length; the vector must have at least one value"
    )
  })

  it("zero length accepted when allowed", {
    expect_equal(
      test_check_numeric_vector(numeric(0), allow_zero_length = TRUE),
      numeric(0)
    )
  })

  it("empty integer vector error", {
    expect_error(
      test_check_numeric_vector(integer(0)),
      "has zero length"
    )
  })

  # ==========================================================================
  # Test 5: NA Values
  # ==========================================================================

  it("NA error when not allowed", {
    expect_error(
      test_check_numeric_vector(c(1, 2, NA, 4)),
      "must not contain missing values"
    )
  })

  it("NA accepted when allowed", {
    result <- test_check_numeric_vector(c(1, 2, NA, 4), allow_na = TRUE)
    expect_true(any(is.na(result)))
  })

  it("single NA handled specially", {
    result <- test_check_numeric_vector(NA_real_, allow_na = TRUE)
    expect_true(is.na(result))
    expect_true(is.numeric(result))
  })

  it("all NA vector accepted when allowed", {
    result <- test_check_numeric_vector(as.numeric(c(NA, NA, NA)), allow_na = TRUE)
    expect_true(all(is.na(result)))
  })

  it("NaN rejected when not allowed", {
    expect_error(
      test_check_numeric_vector(c(1, 2, NaN)),
      "must not contain missing values"
    )
  })

  it("NaN accepted when allow_na=TRUE", {
    result <- test_check_numeric_vector(c(1, 2, NaN), allow_na = TRUE)
    expect_true(any(is.nan(result)))
  })

  # ==========================================================================
  # Test 6: Infinite Values
  # ==========================================================================

  it("Inf error when not allowed", {
    expect_error(
      test_check_numeric_vector(c(1, 2, Inf)),
      "must not contain infinite values"
    )
  })

  it("-Inf error when not allowed", {
    expect_error(
      test_check_numeric_vector(c(1, -Inf, 3)),
      "must not contain infinite values"
    )
  })

  it("Inf accepted when allowed", {
    expect_equal(
      test_check_numeric_vector(c(1, Inf, 3), allow_inf = TRUE),
      c(1, Inf, 3)
    )
  })

  it("-Inf accepted when allowed", {
    expect_equal(
      test_check_numeric_vector(c(1, -Inf, 3), allow_inf = TRUE),
      c(1, -Inf, 3)
    )
  })

  it("both Inf and -Inf accepted when allowed", {
    result <- test_check_numeric_vector(c(Inf, 1, -Inf), allow_inf = TRUE)
    expect_equal(sum(is.infinite(result)), 2)
  })

  it("Inf with NA both allowed", {
    result <- test_check_numeric_vector(
      c(1, Inf, NA, -Inf),
      allow_inf = TRUE,
      allow_na = TRUE
    )
    expect_true(any(is.na(result)))
    expect_true(any(is.infinite(result)))
  })

  # ==========================================================================
  # Test 7: Named Vectors
  # ==========================================================================

  it("unnamed vector error when names required", {
    expect_error(
      test_check_numeric_vector(c(1, 2, 3), must_have_names = TRUE),
      "must be a named vector with unique names"
    )
  })

  it("named vector accepted when required", {
    v <- c(a = 1, b = 2, c = 3)
    expect_equal(
      test_check_numeric_vector(v, must_have_names = TRUE),
      v
    )
  })

  it("empty string names error", {
    v <- c(1, 2, 3)
    names(v) <- c("a", "", "c")
    expect_error(
      test_check_numeric_vector(v, must_have_names = TRUE),
      "must have non-empty names"
    )
  })

  it("duplicate names error", {
    v <- c(a = 1, a = 2, c = 3)
    expect_error(
      test_check_numeric_vector(v, must_have_names = TRUE),
      "must have unique names"
    )
  })

  it("partially named vector error", {
    v <- c(a = 1, 2, c = 3)
    expect_error(
      test_check_numeric_vector(v, must_have_names = TRUE),
      "must have non-empty names"
    )
  })

  it("names preserved when present", {
    v <- c(x = 1, y = 2, z = 3)
    result <- test_check_numeric_vector(v)
    expect_equal(names(result), c("x", "y", "z"))
  })

  # ==========================================================================
  # Test 8: Combined Options
  # ==========================================================================

  it("NULL and NA both allowed", {
    expect_null(test_check_numeric_vector(NULL, allow_null = TRUE, allow_na = TRUE))
    result <- test_check_numeric_vector(c(1, NA), allow_null = TRUE, allow_na = TRUE)
    expect_true(any(is.na(result)))
  })

  it("zero length and NA both allowed", {
    expect_equal(
      test_check_numeric_vector(numeric(0), allow_zero_length = TRUE, allow_na = TRUE),
      numeric(0)
    )
  })

  it("NA, Inf, and names all allowed", {
    v <- c(a = 1, b = Inf, c = NA)
    result <- test_check_numeric_vector(
      v,
      allow_na = TRUE,
      allow_inf = TRUE,
      must_have_names = TRUE
    )
    expect_equal(names(result), c("a", "b", "c"))
    expect_true(any(is.na(result)))
    expect_true(any(is.infinite(result)))
  })

  # ==========================================================================
  # Test 9: Edge Cases
  # ==========================================================================

  it("very large values accepted", {
    v <- c(1e308, 1e307, 1e306)
    expect_equal(test_check_numeric_vector(v), v)
  })

  it("very small values accepted", {
    v <- c(1e-308, 1e-307, 1e-306)
    expect_equal(test_check_numeric_vector(v), v)
  })

  it("negative values accepted", {
    v <- c(-1, -2, -3, -4.5)
    expect_equal(test_check_numeric_vector(v), v)
  })

  it("all zeros vector", {
    v <- rep(0, 10)
    expect_equal(test_check_numeric_vector(v), v)
  })

  it("single zero", {
    expect_equal(test_check_numeric_vector(0), 0)
  })

  it("negative zero", {
    expect_equal(test_check_numeric_vector(-0), -0)
  })

  it("mixed integers and doubles", {
    v <- c(1L, 2.5, 3L, 4.7)
    expect_equal(test_check_numeric_vector(v), v)
  })

  # ==========================================================================
  # Test 10: Error Message Quality
  # ==========================================================================

  it("error messages include argument name", {
    my_vector <- "not numeric"
    expect_error(
      test_check_numeric_vector(my_vector),
      "Argument 'num' in function 'test_check_numeric_vector' must be a numeric vector."
    )
  })

  it("error messages include function name", {
    expect_error(
      test_check_numeric_vector("bad"),
      "in function 'test_check_numeric_vector'"
    )
  })

  # ==========================================================================
  # Test 11: Sequence Vectors
  # ==========================================================================

  it("sequence from colon operator", {
    expect_equal(test_check_numeric_vector(1:100), 1:100)
  })

  it("seq() output", {
    v <- seq(0, 1, by = 0.1)
    expect_equal(test_check_numeric_vector(v), v)
  })

  it("rep() output", {
    v <- rep(5, 10)
    expect_equal(test_check_numeric_vector(v), v)
  })
})

describe("Some earlier tests", {

  test_that("indirect basic check succeeds", {
    expect_equal(test_check_numeric_vector(3.14), 3.14)
  })

  test_that("indirect type error", {
    expect_error(test_check_numeric_vector("abc"), "Argument 'num' in function 'test_check_numeric_vector' must be a numeric vector")
  })

  test_that("indirect null error", {
    expect_error(test_check_numeric_vector(NULL), "Argument 'num' in function 'test_check_numeric_vector' must be a non-NULL numeric vector.")
  })

  test_that("indirect length error", {
    expect_error(test_check_numeric_vector(numeric(0), allow_zero_length=FALSE), "Argument 'num' in function 'test_check_numeric_vector' has zero length; the vector must have at least one value.")
  })

  test_that("indirect missing value error", {
    expect_error(test_check_numeric_vector(as.numeric(NA)), "Argument 'num' in function 'test_check_numeric_vector' must not contain missing values",
                 fixed=TRUE)
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
    expect_error(test_check_numeric_vector(c(a=10, b=20, 30), must_have_names = TRUE),
                 "Argument 'num' in function 'test_check_numeric_vector' must have non-empty names (no '').",
                 fixed=TRUE)
  })
})
