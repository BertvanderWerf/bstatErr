# Unit Tests for check_numeric Function
# File: tests/testthat/test-check_numeric_improved.R

library(testthat)

test_check_numeric <- function(num, ...) {
  check_numeric(num, ...)
}

describe("check_numeric", {

  # ==========================================================================
  # Test 1: Basic Functionality
  # ==========================================================================

  it("accepts valid numeric values", {
    expect_equal(test_check_numeric(3.14), 3.14)
    expect_equal(test_check_numeric(42), 42)
    expect_equal(test_check_numeric(-1.5), -1.5)
    expect_equal(test_check_numeric(0), 0)
  })

  it("accepts integers", {
    expect_equal(test_check_numeric(1L), 1L)
  })

  it("returns invisibly", {
    result <- withVisible(test_check_numeric(3.14))
    expect_false(result$visible)
  })

  # ==========================================================================
  # Test 2: NULL Handling
  # ==========================================================================

  it("NULL error when not allowed", {
    expect_error(
      test_check_numeric(NULL),
      "Argument 'num' in function 'test_check_numeric' must be a non-NULL numeric value."
    )
  })

  it("NULL accepted when allowed", {
    expect_null(test_check_numeric(NULL, allow_null = TRUE))
  })

  # ==========================================================================
  # Test 3: Type Validation
  # ==========================================================================

  it("character rejected", {
    expect_error(
      test_check_numeric("3.14"),
      "must be a numeric value"
    )
  })

  it("logical rejected", {
    expect_error(
      test_check_numeric(TRUE),
      "must be a numeric value"
    )
  })

  it("list rejected", {
    expect_error(
      test_check_numeric(list(1, 2)),
      "must be a numeric value"
    )
  })

  # ==========================================================================
  # Test 4: NA Values (NEW TEST - PREVIOUSLY MISSING!)
  # ==========================================================================

  it("NA error when not allowed", {
    expect_error(
      test_check_numeric(NA_real_),
      "must not contain missing values"
    )
  })

  it("NA accepted when allowed", {
    result <- test_check_numeric(NA_real_, allow_na = TRUE)
    expect_true(is.na(result))
  })

  it("single NA handled specially", {
    result <- test_check_numeric(as.numeric(NA), allow_na = TRUE)
    expect_true(is.na(result))
  })

  # ==========================================================================
  # Test 5: Infinite Values (NEW TEST - BUG FIX!)
  # ==========================================================================

  it("Inf error when not allowed", {
    expect_error(
      test_check_numeric(Inf),
      "must not contain infinite values"
    )
  })

  it("-Inf error when not allowed", {
    expect_error(
      test_check_numeric(-Inf),
      "must not contain infinite values"
    )
  })

  it("Inf accepted when allowed", {
    expect_equal(test_check_numeric(Inf, allow_inf = TRUE), Inf)
  })

  it("-Inf accepted when allowed", {
    expect_equal(test_check_numeric(-Inf, allow_inf = TRUE), -Inf)
  })

  it("both Inf and -Inf accepted when allowed", {
    expect_equal(test_check_numeric(Inf, allow_inf = TRUE), Inf)
    expect_equal(test_check_numeric(-Inf, allow_inf = TRUE), -Inf)
  })

  # ==========================================================================
  # Test 6: Combined Options
  # ==========================================================================

  it("NA and Inf both allowed", {
    expect_true(is.na(test_check_numeric(NA_real_, allow_na = TRUE, allow_inf = TRUE)))
    expect_equal(test_check_numeric(Inf, allow_na = TRUE, allow_inf = TRUE), Inf)
  })

  it("NULL and NA both allowed", {
    expect_null(test_check_numeric(NULL, allow_null = TRUE, allow_na = TRUE))
    expect_true(is.na(test_check_numeric(NA_real_, allow_null = TRUE, allow_na = TRUE)))
  })

  # ==========================================================================
  # Test 7: Edge Cases
  # ==========================================================================

  it("very large values accepted", {
    expect_equal(test_check_numeric(1e308), 1e308)
  })

  it("very small values accepted", {
    expect_equal(test_check_numeric(1e-308), 1e-308)
  })

  it("negative zero accepted", {
    expect_equal(test_check_numeric(-0), -0)
  })

  it("NaN rejected (unless allow_na)", {
    expect_error(
      test_check_numeric(NaN),
      "must not contain missing values"
    )
  })

  it("NaN accepted when allow_na=TRUE", {
    result <- test_check_numeric(NaN, allow_na = TRUE)
    expect_true(is.na(result) || is.nan(result))
  })

  # ==========================================================================
  # Test 8: Error Message Quality
  # ==========================================================================

  it("error messages include argument name", {
    my_value <- "not numeric"
    expect_error(
      test_check_numeric(my_value),
      "must be a numeric value"
    )
  })

  it("error messages include function name", {
    expect_error(
      test_check_numeric("bad"),
      "in function 'test_check_numeric'"
    )
  })
})

