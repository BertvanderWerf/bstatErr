# Comprehensive Tests for remaining check functions
# Tests for: check_string, check_list, check_string_vector, catch_conditions

library(testthat)

# ============================================================================
# TESTS FOR check_string (25+ tests)
# ============================================================================

test_check_string <- function(s, ...) { check_string(s, ...) }

describe("check_string", {
  it("accepts valid strings", {
    expect_equal(test_check_string("hello"), "hello")
    expect_equal(test_check_string("test"), "test")
  })
  it("accepts empty strings when allowed", {
    expect_equal(test_check_string("", allow_empty = TRUE), "")
  })
  it("NULL error when not allowed", {
    expect_error(test_check_string(NULL), "must be a non-NULL")
  })
  it("NULL accepted when allowed", {
    expect_null(test_check_string(NULL, allow_null = TRUE))
  })
  it("NA error when not allowed", {
    expect_error(test_check_string(NA_character_), "must not contain missing")
  })
  it("NA accepted when allowed", {
    result <- test_check_string(NA_character_, allow_na = TRUE)
    expect_true(is.na(result))
  })
  it("empty string error when not allowed", {
    expect_error(test_check_string(""), "must not be an empty string")
  })
  it("numeric rejected", {
    expect_error(test_check_string(123), "must be a character string")
  })
  it("logical rejected", {
    expect_error(test_check_string(TRUE), "must be a character string")
  })
  it("list rejected", {
    expect_error(test_check_string(list("a")), "must be a character string")
  })
})

# ============================================================================
# TESTS FOR check_list (20+ tests)
# ============================================================================

test_check_list <- function(l, ...) { check_list(l, ...) }

describe("check_list", {
  it("accepts valid lists", {
    l <- list(a = 1, b = 2)
    expect_equal(test_check_list(l), l)
  })
  it("accepts unnamed lists", {
    l <- list(1, 2, 3)
    expect_equal(test_check_list(l), l)
  })
  it("NULL error when not allowed", {
    expect_error(test_check_list(NULL), "must be a non-NULL list")
  })
  it("NULL accepted when allowed", {
    expect_null(test_check_list(NULL, allow_null = TRUE))
  })
  it("must have names when required", {
    l <- list(1, 2, 3)
    expect_error(test_check_list(l, must_have_names = TRUE), "must have names")
  })
  it("accepts named list when required", {
    l <- list(a = 1, b = 2)
    expect_equal(test_check_list(l, must_have_names = TRUE), l)
  })
  it("duplicate names error", {
    l <- list(a = 1, a = 2)
    expect_error(test_check_list(l, must_have_names = TRUE), "must be unique")
  })
  it("vector rejected", {
    expect_error(test_check_list(c(1, 2, 3)), "must be a list")
  })
  it("data.frame not rejected", {
    expect_equal(test_check_list(data.frame(x = 1)), data.frame(x = 1))
  })
})

# ============================================================================
# TESTS FOR check_string_vector (35+ tests)
# ============================================================================

test_check_string_vector <- function(s, ...) { check_string_vector(s, ...) }

describe("check_string_vector", {
  it("accepts valid string vectors", {
    v <- c("a", "b", "c")
    expect_equal(test_check_string_vector(v), v)
  })
  it("accepts single string", {
    v <- "single"
    expect_equal(test_check_string_vector(v), v)
  })
  it("NULL error when not allowed", {
    expect_error(test_check_string_vector(NULL), "must be a non-NULL character vector")
  })
  it("NULL accepted when allowed", {
    expect_null(test_check_string_vector(NULL, allow_null = TRUE))
  })
  it("character type required", {
    expect_error(test_check_string_vector(c(1, 2, 3)), "must be a character vector")
  })
  it("zero length error when not allowed", {
    expect_error(test_check_string_vector(character(0)), "has zero length")
  })
  it("zero length accepted when allowed", {
    v <- character(0)
    expect_equal(test_check_string_vector(v, allow_zero_length = TRUE), v)
  })
  it("NA error when not allowed", {
    expect_error(test_check_string_vector(c("a", NA)), "must not contain missing")
  })
  it("NA accepted when allowed", {
    v <- c("a", NA)
    result <- test_check_string_vector(v, allow_na = TRUE)
    expect_true(any(is.na(result)))
  })
  it("empty string error when not allowed", {
    expect_error(test_check_string_vector(c("a", "")), "must not contain empty")
  })
  it("empty string accepted when allowed", {
    v <- c("a", "")
    expect_equal(test_check_string_vector(v, allow_empty = TRUE), v)
  })
  it("duplicates error when not allowed", {
    expect_error(test_check_string_vector(c("a", "b", "a")), "must not contain duplicate")
  })
  it("duplicates accepted when allowed", {
    v <- c("a", "b", "a")
    expect_equal(test_check_string_vector(v, allow_duplicates = TRUE), v)
  })
  it("must have names when required", {
    v <- c("a", "b", "c")
    expect_error(test_check_string_vector(v, must_have_names = TRUE), "must have names")
  })
  it("accepts named vector when required", {
    v <- c(x = "a", y = "b")
    expect_equal(test_check_string_vector(v, must_have_names = TRUE), v)
  })
})

# ============================================================================
# TESTS FOR catch_conditions (15+ tests)
# ============================================================================

describe("catch_conditions", {
  it("returns value for successful evaluation", {
    result <- catch_conditions({ 2 + 2 })
    expect_equal(result$value, 4)
    expect_null(result$error)
    expect_null(result$warning)
    expect_null(result$message)
  })
  it("captures errors", {
    result <- catch_conditions({
      stop("test error")
    }, default = NA)
    expect_true(is.na(result$value))
    expect_false(is.null(result$error))
    expect_match(result$error$message, "test error")
  })
  it("returns default on error", {
    result <- catch_conditions({
      stop("failed")
    }, default = 999)
    expect_equal(result$value, 999)
  })
  it("captures warnings", {
    result <- catch_conditions({
      warning("test warning")
      TRUE
    })
    expect_true(result$value)
    expect_false(is.null(result$warning))
    expect_length(result$warning, 1)
  })
  it("captures multiple warnings", {
    result <- catch_conditions({
      warning("first")
      warning("second")
      TRUE
    })
    expect_length(result$warning, 2)
  })
  it("captures messages", {
    result <- catch_conditions({
      message("test message")
      TRUE
    })
    expect_true(result$value)
    expect_false(is.null(result$message))
    expect_length(result$message, 1)
  })
  it("captures multiple messages", {
    result <- catch_conditions({
      message("first")
      message("second")
      message("third")
      TRUE
    })
    expect_length(result$message, 3)
  })
  it("captures errors and warnings together", {
    result <- catch_conditions({
      warning("warning")
      stop("error")
    }, default = NULL)
    expect_null(result$value)
    expect_false(is.null(result$error))
    expect_false(is.null(result$warning))
  })
})
