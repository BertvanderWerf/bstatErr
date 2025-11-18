# Unit Tests for check_numeric_matrix Function
# File: tests/testthat/test-check_numeric_matrix.R

# Helper function for indirect testing (matches pattern in check_numeric_vector tests)
test_check_numeric_matrix <- function(mat, ...) {
  check_numeric_matrix(mat, ...)
}

# =============================================================================
# Test 1: Basic Functionality
# =============================================================================

test_that("basic matrix check succeeds", {
  m <- matrix(1:9, nrow = 3, ncol = 3)
  expect_equal(test_check_numeric_matrix(m), m)
})

test_that("numeric vector is accepted with warning", {
  expect_warning(
    result <- test_check_numeric_matrix(c(1, 2, 3)),
    "is not a matrix; converting"
  )
  expect_true(is.matrix(result))
})

test_that("returns invisibly", {
  m <- matrix(1:4, nrow = 2)
  result <- withVisible(test_check_numeric_matrix(m))
  expect_false(result$visible)
})

# =============================================================================
# Test 2: NULL Handling
# =============================================================================

test_that("NULL error when not allowed", {
  expect_error(
    test_check_numeric_matrix(NULL),
    "Argument 'mat' in function 'test_check_numeric_matrix' must be a non-NULL numeric matrix."
  )
})

test_that("NULL accepted when allowed", {
  expect_null(test_check_numeric_matrix(NULL, allow_null = TRUE))
})

# =============================================================================
# Test 3: Type Validation
# =============================================================================

test_that("character matrix rejected", {
  m <- matrix(c("a", "b", "c", "d"), nrow = 2)
  expect_error(
    test_check_numeric_matrix(m),
    "must be a numeric matrix"
  )
})

test_that("list rejected", {
  expect_error(
    test_check_numeric_matrix(list(1, 2, 3)),
    "Argument 'mat' in function 'test_check_numeric_matrix' must be a numeric matrix"
  )
})

test_that("data.frame rejected", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(
    test_check_numeric_matrix(df),
    "must be a numeric matrix"
  )
})

# =============================================================================
# Test 4: Zero Length
# =============================================================================

test_that("empty matrix error when not allowed", {
  m <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_error(
    test_check_numeric_matrix(m),
    "has zero length; the matrix must have at least one value"
  )
})

test_that("empty matrix accepted when allowed", {
  m <- matrix(numeric(0), nrow = 0, ncol = 0)
  expect_equal(test_check_numeric_matrix(m, allow_zero_length = TRUE), m)
})

test_that("empty matrix with dimensions error", {
  m <- matrix(numeric(0), nrow = 3, ncol = 0)
  expect_error(
    test_check_numeric_matrix(m),
    "has zero length"
  )
})

# =============================================================================
# Test 5: NA Values
# =============================================================================

test_that("NA values error when not allowed", {
  m <- matrix(c(1, 2, NA, 4), nrow = 2)
  expect_error(
    test_check_numeric_matrix(m),
    "must not contain missing values"
  )
})

test_that("NA values accepted when allowed", {
  m <- matrix(c(1, 2, NA, 4), nrow = 2)
  result <- test_check_numeric_matrix(m, allow_na = TRUE)
  expect_true(any(is.na(result)))
})

test_that("all NA matrix accepted when allowed", {
  m <- matrix(as.numeric(rep(NA, 4)), nrow = 2)
  result <- test_check_numeric_matrix(m, allow_na = TRUE)
  expect_true(all(is.na(result)))
})

test_that("single NA value handled specially", {
  result <- test_check_numeric_matrix(as.numeric(NA), allow_na = TRUE)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(1, 1))
  expect_true(is.na(result[1, 1]))
})

# =============================================================================
# Test 6: Infinite Values
# =============================================================================

test_that("Inf values error when not allowed", {
  m <- matrix(c(1, 2, Inf, 4), nrow = 2)
  expect_error(
    test_check_numeric_matrix(m),
    "must not contain infinite values"
  )
})

test_that("-Inf values error when not allowed", {
  m <- matrix(c(1, 2, -Inf, 4), nrow = 2)
  expect_error(
    test_check_numeric_matrix(m),
    "must not contain infinite values"
  )
})

test_that("Inf values accepted when allowed", {
  m <- matrix(c(1, 2, Inf, 4), nrow = 2)
  result <- test_check_numeric_matrix(m, allow_inf = TRUE)
  expect_true(any(is.infinite(result)))
})

test_that("mixed Inf and -Inf accepted when allowed", {
  m <- matrix(c(1, Inf, -Inf, 4), nrow = 2)
  result <- test_check_numeric_matrix(m, allow_inf = TRUE)
  expect_equal(sum(is.infinite(result)), 2)
})

# =============================================================================
# Test 7: Square Matrix Requirement
# =============================================================================

test_that("square matrix passes when required", {
  m <- matrix(1:9, nrow = 3, ncol = 3)
  expect_equal(test_check_numeric_matrix(m, must_be_square = TRUE), m)
})

test_that("non-square matrix fails when required", {
  m <- matrix(1:6, nrow = 2, ncol = 3)
  expect_error(
    test_check_numeric_matrix(m, must_be_square = TRUE),
    "must be a square matrix \\(nrow=2, ncol=3\\)"
  )
})

test_that("1x1 matrix is square", {
  m <- matrix(5, nrow = 1, ncol = 1)
  expect_equal(test_check_numeric_matrix(m, must_be_square = TRUE), m)
})

# =============================================================================
# Test 8: Symmetric Matrix Requirement
# =============================================================================

test_that("symmetric matrix passes", {
  m <- matrix(c(1, 2, 3,
                2, 4, 5,
                3, 5, 6), nrow = 3, byrow = TRUE)
  expect_equal(test_check_numeric_matrix(m, must_be_symmetric = TRUE), m)
})

test_that("non-symmetric matrix fails", {
  m <- matrix(c(1, 2, 3,
                4, 5, 6,
                7, 8, 9), nrow = 3, byrow = TRUE)
  expect_error(
    test_check_numeric_matrix(m, must_be_symmetric = TRUE),
    "must be a symmetric matrix"
  )
})

test_that("identity matrix is symmetric", {
  m <- diag(3)
  expect_equal(test_check_numeric_matrix(m, must_be_symmetric = TRUE), m)
})

test_that("symmetric requirement forces square check", {
  # Non-square matrix should fail even without explicit must_be_square
  m <- matrix(1:6, nrow = 2, ncol = 3)
  expect_error(
    test_check_numeric_matrix(m, must_be_symmetric = TRUE),
    "must be a square matrix"
  )
})

test_that("nearly symmetric matrix passes with tolerance", {
  # Create symmetric matrix with tiny numerical error
  m <- matrix(c(1, 2, 3,
                2, 4, 5,
                3, 5, 6), nrow = 3, byrow = TRUE)
  m[1, 2] <- m[1, 2] + 1e-15  # Add tiny error

  # Should still pass due to isSymmetric() tolerance
  expect_equal(test_check_numeric_matrix(m, must_be_symmetric = TRUE), m)
})

# =============================================================================
# Test 9: Combined Requirements
# =============================================================================

test_that("square and no NA", {
  m <- matrix(c(1, 2, NA, 4), nrow = 2)
  expect_error(
    test_check_numeric_matrix(m, must_be_square = TRUE),
    "must not contain missing values"
  )
})

test_that("symmetric with NA allowed", {
  m <- matrix(c(1, 2, NA, 3), nrow = 2)
  # Will fail symmetry check because NA != 2
  expect_error(
    test_check_numeric_matrix(m, must_be_symmetric = TRUE, allow_na = TRUE),
    "must be a symmetric matrix"
  )
})

test_that("square with Inf allowed", {
  m <- matrix(c(1, Inf, -Inf, 4), nrow = 2)
  expect_equal(
    test_check_numeric_matrix(m, must_be_square = TRUE, allow_inf = TRUE),
    m
  )
})

# =============================================================================
# Test 10: Edge Cases
# =============================================================================

test_that("single value matrix", {
  m <- matrix(42, nrow = 1, ncol = 1)
  expect_equal(test_check_numeric_matrix(m), m)
})

test_that("large matrix passes", {
  m <- matrix(rnorm(10000), nrow = 100, ncol = 100)
  expect_equal(test_check_numeric_matrix(m), m)
})

test_that("matrix with very small values", {
  m <- matrix(c(1e-100, 1e-200, 1e-300, 1e-400), nrow = 2)
  expect_equal(test_check_numeric_matrix(m), m)
})

test_that("matrix with very large values", {
  m <- matrix(c(1e100, 1e200, 1e100, 1e200), nrow = 2)
  expect_equal(test_check_numeric_matrix(m), m)
})

test_that("negative values allowed", {
  m <- matrix(c(-1, -2, -3, -4), nrow = 2)
  expect_equal(test_check_numeric_matrix(m), m)
})

test_that("all zeros matrix", {
  m <- matrix(0, nrow = 3, ncol = 3)
  expect_equal(test_check_numeric_matrix(m), m)
})

# =============================================================================
# Test 11: Dimension Preservation
# =============================================================================

test_that("row and column names preserved", {
  m <- matrix(1:4, nrow = 2)
  rownames(m) <- c("r1", "r2")
  colnames(m) <- c("c1", "c2")

  result <- test_check_numeric_matrix(m)
  expect_equal(rownames(result), c("r1", "r2"))
  expect_equal(colnames(result), c("c1", "c2"))
})

test_that("dimensions preserved", {
  m <- matrix(1:12, nrow = 3, ncol = 4)
  result <- test_check_numeric_matrix(m)
  expect_equal(dim(result), c(3, 4))
})

# =============================================================================
# Test 12: Logical Parameter Validation
# =============================================================================

test_that("non-logical allow_null rejected", {
  m <- matrix(1:4, nrow = 2)
  expect_error(
    test_check_numeric_matrix(m, allow_null = "yes"),
    "logical value"
  )
})

test_that("non-logical must_be_square rejected", {
  m <- matrix(1:4, nrow = 2)
  expect_error(
    test_check_numeric_matrix(m, must_be_square = 1),
    "logical value"
  )
})

# =============================================================================
# Test 13: Error Message Quality
# =============================================================================

test_that("error messages include argument name", {
  my_matrix <- "not a matrix"
  expect_error(
    test_check_numeric_matrix(my_matrix),
    "Argument 'mat' in function 'test_check_numeric_matrix' must be a numeric matrix"
  )
})

test_that("error messages include function name", {
  m <- "not a matrix"
  expect_error(
    test_check_numeric_matrix(m),
    "in function 'test_check_numeric_matrix'"
  )
})

# =============================================================================
# Test 14: Special Matrices
# =============================================================================

test_that("diagonal matrix is symmetric", {
  m <- diag(c(1, 2, 3, 4))
  expect_equal(test_check_numeric_matrix(m, must_be_symmetric = TRUE), m)
})

test_that("covariance matrix is symmetric", {
  set.seed(123)
  x <- matrix(rnorm(100), ncol = 5)
  cov_matrix <- cov(x)

  expect_equal(
    test_check_numeric_matrix(cov_matrix, must_be_symmetric = TRUE),
    cov_matrix
  )
})

test_that("correlation matrix is symmetric", {
  set.seed(123)
  x <- matrix(rnorm(100), ncol = 5)
  cor_matrix <- cor(x)

  expect_equal(
    test_check_numeric_matrix(cor_matrix, must_be_symmetric = TRUE),
    cor_matrix
  )
})

