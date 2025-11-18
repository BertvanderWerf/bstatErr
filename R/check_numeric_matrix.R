#' Check Numeric Matrix for Typical Errors
#'
#' Validates a numeric matrix argument for type, dimensions, NA values, infinity,
#' and structural properties (square, symmetric). Provides consistent error messages
#' that include the argument name and calling function.
#'
#' @param numeric_matrix_arg Matrix or numeric to check. Can be a matrix, or a
#'   numeric vector that will be treated as a matrix.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value. Default is FALSE.
#' @param allow_na Logical. If TRUE, NA values in the matrix are allowed. Default is FALSE.
#' @param allow_inf Logical. If TRUE, -Inf or Inf values in the matrix are allowed.
#'   Default is FALSE.
#' @param allow_zero_length Logical. If TRUE, zero-length matrices (empty matrices)
#'   are allowed. Default is FALSE.
#' @param must_be_square Logical. If TRUE, the matrix must have equal number of
#'   rows and columns (square matrix). Default is FALSE.
#' @param must_be_symmetric Logical. If TRUE, the matrix must be symmetric
#'   (i.e., M\[i,j] == M\[j,i] for all i,j). Automatically sets must_be_square to TRUE.
#'   Default is FALSE.
#'
#' @details
#' This function performs a sequence of validation checks:
#' \enumerate{
#'   \item Validates all logical parameters
#'   \item If must_be_symmetric=TRUE, automatically sets must_be_square=TRUE
#'   \item Checks if NULL when not allowed
#'   \item Returns NULL early if allowed
#'   \item Handles single NA value specially when allowed
#'   \item Checks if input is numeric and/or matrix
#'   \item Checks for zero length when not allowed
#'   \item Checks for NA values when not allowed
#'   \item Checks for infinite values when not allowed
#'   \item Checks if matrix is square when required
#'   \item Checks if matrix is symmetric when required
#' }
#'
#' **Matrix Type Checking:**
#' The function accepts inputs that are either:
#' \itemize{
#'   \item Already a matrix (is.matrix() returns TRUE)
#'   \item Numeric vectors (will be accepted but user should be aware)
#' }
#'
#' **Symmetry Checking:**
#' Uses base R's isSymmetric() function with default tolerance for numerical
#' precision issues.
#'
#' @return Invisibly returns \code{numeric_matrix_arg} if all checks pass;
#'   otherwise throws an error with a descriptive message.
#'
#' @note
#' Error messages include:
#' \itemize{
#'   \item The argument name (from substitute())
#'   \item The calling function name (from sys.call())
#'   \item Specific reason for failure
#' }
#'
#' This makes debugging easier in nested function calls.
#'
#' @examples
#' # Valid matrix
#' m <- matrix(1:9, nrow = 3)
#' check_numeric_matrix(m)
#'
#' # Allow NULL
#' check_numeric_matrix(NULL, allow_null = TRUE)
#'
#' # Square matrix required
#' m_square <- matrix(1:9, nrow = 3)
#' check_numeric_matrix(m_square, must_be_square = TRUE)
#'
#' # Symmetric matrix
#' m_sym <- matrix(c(1,2,3, 2,4,5, 3,5,6), nrow = 3)
#' check_numeric_matrix(m_sym, must_be_symmetric = TRUE)
#'
#' \dontrun{
#' # These will fail:
#' check_numeric_matrix("not a matrix")  # Type error
#' check_numeric_matrix(NULL)  # NULL not allowed
#' m_rect <- matrix(1:6, nrow = 2, ncol = 3)
#' check_numeric_matrix(m_rect, must_be_square = TRUE)  # Not square
#' }
#'
#' @export
check_numeric_matrix <- function(numeric_matrix_arg,
                                 allow_null = FALSE,
                                 allow_na = FALSE,
                                 allow_inf = FALSE,
                                 allow_zero_length = FALSE,
                                 must_be_square = FALSE,
                                 must_be_symmetric = FALSE) {

  # ============================================================================
  # VALIDATE LOGICAL PARAMETERS
  # ============================================================================

  allow_null <- check_logical(allow_null)
  allow_na <- check_logical(allow_na)
  allow_inf <- check_logical(allow_inf)
  allow_zero_length <- check_logical(allow_zero_length)
  allow_must_be_square <- check_logical(must_be_square)
  allow_must_be_symmetric <- check_logical(must_be_symmetric)

  # If must be symmetric, it must also be square
  if (isTRUE(must_be_symmetric)) {
    must_be_square <- TRUE
  }

  # ============================================================================
  # CHECK FOR NULL
  # ============================================================================

  # Check for NULL if not allowed
  if (is.null(numeric_matrix_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL numeric matrix.",
        as.character(substitute(numeric_matrix_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # NULL does not have a type, return NULL early - no further tests needed
  if (is.null(numeric_matrix_arg)) {
    return(invisible(NULL))
  }

  # ============================================================================
  # HANDLE SPECIAL CASE: SINGLE NA VALUE
  # ============================================================================

  # NA or NaN can be of any type
  # If single NA value is allowed, convert to 1x1 matrix and return
  if (length(numeric_matrix_arg) == 1 && isTRUE(allow_na) && is.na(numeric_matrix_arg)) {
    return(invisible(matrix(as.numeric(numeric_matrix_arg), nrow = 1, ncol = 1)))
  }

  # ============================================================================
  # CHECK TYPE
  # ============================================================================

  # Check if input is numeric
  # Accept both numeric vectors and matrices
  if (!is.numeric(numeric_matrix_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a numeric matrix",
        as.character(substitute(numeric_matrix_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # If not already a matrix but is numeric, convert to matrix
  # This handles numeric vectors passed as arguments
  if (!is.matrix(numeric_matrix_arg)) {
    # Warn user that conversion is happening
    warning(
      sprintf(
        "Argument '%s' in function '%s' is not a matrix; converting numeric vector to matrix.",
        as.character(substitute(numeric_matrix_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
    # Convert to single-column matrix
    numeric_matrix_arg <- as.matrix(numeric_matrix_arg)
  }

  # ============================================================================
  # CHECK LENGTH (MATRIX SIZE)
  # ============================================================================

  # Check if matrix has zero length (empty matrix)
  if (length(numeric_matrix_arg) == 0 && isFALSE(allow_zero_length)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' has zero length; the matrix must have at least one value.",
        as.character(substitute(numeric_matrix_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR NA VALUES
  # ============================================================================

  # Check for NA values if not allowed
  if (isFALSE(allow_na) && any(is.na(numeric_matrix_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain missing values (NA).",
        as.character(substitute(numeric_matrix_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR INFINITE VALUES
  # ============================================================================

  # Check for non-finite values (Inf, -Inf) if not allowed
  if (isFALSE(allow_inf) && any(!is.na(numeric_matrix_arg) & is.infinite(numeric_matrix_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain infinite values (Inf, -Inf).",
        as.character(substitute(numeric_matrix_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK IF MATRIX MUST BE SQUARE
  # ============================================================================

  # Check if matrix must be square (nrow == ncol)
  if (isTRUE(must_be_square)) {
    n_rows <- nrow(numeric_matrix_arg)
    n_cols <- ncol(numeric_matrix_arg)

    if (n_rows != n_cols) {
    stop(
      sprintf(
          "Argument '%s' in function '%s' must be a square matrix (nrow=%d, ncol=%d).",
        as.character(substitute(numeric_matrix_arg)),
          deparse(sys.call(sys.parent())[[1]]),
          n_rows,
          n_cols
      ),
      call. = FALSE
    )
  }
  }

  # ============================================================================
  # CHECK IF MATRIX MUST BE SYMMETRIC
  # ============================================================================

  # Check if matrix must be symmetric
  # Note: This check only runs if must_be_symmetric is TRUE
  # and the matrix is square (enforced above)
  if (isTRUE(must_be_symmetric)) {
    # Use isSymmetric from base R - handles numerical precision
    if (!isSymmetric(numeric_matrix_arg)) {
    stop(
      sprintf(
          "Argument '%s' in function '%s' must be a symmetric matrix (M[i,j] == M[j,i] for all i,j).",
        as.character(substitute(numeric_matrix_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }
  }

  # ============================================================================
  # ALL CHECKS PASSED - RETURN MATRIX
  # ============================================================================

  # Return the validated matrix invisibly
  invisible(numeric_matrix_arg)
}
