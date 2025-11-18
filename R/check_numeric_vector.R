#' Check a Numeric Vector for Typical Errors
#'
#' Validates a numeric vector argument for type, length, NA values, infinity,
#' and optional naming requirements. Provides consistent error messages that
#' include the argument name and calling function name.
#'
#' @details
#' This function performs validation checks in sequence:
#' \enumerate{
#'   \item Validates all logical parameters
#'   \item Checks if NULL when not allowed
#'   \item Returns NULL early if allowed (no further checks)
#'   \item Handles single NA value specially when allowed
#'   \item Checks if input is numeric type
#'   \item Checks for zero length when not allowed
#'   \item Checks for NA values when not allowed
#'   \item Checks for infinite values when not allowed
#'   \item Checks for required names and uniqueness
#' }
#'
#' **Numeric Vectors in R:**
#' A numeric vector can contain integers, doubles, or a mix of both.
#' All of these are numeric vectors: \code{c(1, 2, 3)}, \code{c(1.5, 2.7)},
#' \code{1:10}, \code{seq(0, 1, 0.1)}.
#'
#' **NA vs Inf:**
#' \itemize{
#'   \item NA represents missing data (unknown value)
#'   \item Inf/-Inf represents mathematical infinity
#'   \item Both can occur in numeric vectors but mean different things
#' }
#'
#' @param numeric_arg Numeric vector to check.
#' @param allow_null Logical. If TRUE, NULL is accepted as valid. Default FALSE.
#' @param allow_na Logical. If TRUE, NA values allowed in vector. Default FALSE.
#' @param allow_inf Logical. If TRUE, -Inf or Inf values allowed. Default FALSE.
#' @param allow_zero_length Logical. If TRUE, zero-length vectors allowed. Default FALSE.
#' @param must_have_names Logical. If TRUE, all elements must have unique names. Default FALSE.
#'
#' @return Invisibly returns \code{numeric_arg} if all checks pass;
#'   otherwise throws an error with descriptive message.
#'
#' @note
#' **Error messages include:**
#' \itemize{
#'   \item The argument name (from substitute())
#'   \item The calling function name (from sys.call())
#'   \item Specific reason for failure
#' }
#'
#' **Name checking:**
#' When \code{must_have_names = TRUE}, the function checks:
#' \itemize{
#'   \item All elements have names (not NULL)
#'   \item No empty string names ("")
#'   \item All names are unique
#' }
#'
#' @examples
#' # Valid numeric vectors
#' check_numeric_vector(c(1, 2, 3))
#' check_numeric_vector(c(1.5, 2.7, 3.9))
#' check_numeric_vector(1:10)
#'
#' # Allow NULL
#' check_numeric_vector(NULL, allow_null = TRUE)
#'
#' # Allow NA values
#' check_numeric_vector(c(1, 2, NA, 4), allow_na = TRUE)
#'
#' # Allow Inf values
#' check_numeric_vector(c(1, Inf, -Inf), allow_inf = TRUE)
#'
#' # Require names
#' check_numeric_vector(c(a = 1, b = 2, c = 3), must_have_names = TRUE)
#'
#' \dontrun{
#' # These will fail:
#' check_numeric_vector("not numeric")  # Type error
#' check_numeric_vector(NULL)  # NULL not allowed
#' check_numeric_vector(numeric(0))  # Zero length not allowed
#' check_numeric_vector(c(1, NA))  # NA not allowed
#' check_numeric_vector(c(1, Inf))  # Inf not allowed
#' check_numeric_vector(c(1, 2), must_have_names = TRUE)  # No names
#' }
#'
#' @export
check_numeric_vector <- function(numeric_arg,
                                 allow_null = FALSE,
                                 allow_na = FALSE,
                                 allow_inf = FALSE,
                                 allow_zero_length = FALSE,
                                 must_have_names = FALSE) {

  # ============================================================================
  # VALIDATE LOGICAL PARAMETERS
  # ============================================================================

  allow_null <- check_logical(allow_null)
  allow_na <- check_logical(allow_na)
  allow_inf <- check_logical(allow_inf)
  allow_zero_lengh <- check_logical(allow_zero_length)
  must_have_names <- check_logical(must_have_names)

  # ============================================================================
  # CHECK FOR NULL
  # ============================================================================

  # Check if NULL when not allowed
  if (is.null(numeric_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL numeric vector.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # NULL does not have a type - return NULL early, no further checks needed
  if (is.null(numeric_arg)) {
    return(invisible(NULL))
  }

  # ============================================================================
  # HANDLE SPECIAL CASE: SINGLE NA VALUE
  # ============================================================================

  # NA/NaN can be of any type. If single NA is allowed, convert and return
  if (length(numeric_arg) == 1 && isTRUE(allow_na) && is.na(numeric_arg)) {
    return(invisible(as.numeric(numeric_arg)))
  }

  # ============================================================================
  # CHECK TYPE
  # ============================================================================

  # Check if input is numeric (includes integers and doubles)
  if (!is.numeric(numeric_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a numeric vector.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR ZERO LENGTH
  # ============================================================================

  # Check if vector has zero length
  if (length(numeric_arg) == 0 && isFALSE(allow_zero_length)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' has zero length; the vector must have at least one value.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR NA VALUES
  # ============================================================================

  # Check for NA values if not allowed
  if (isFALSE(allow_na) && any(is.na(numeric_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain missing values (NA or NaN).",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR INFINITE VALUES
  # ============================================================================

  # Check for non-finite values (Inf, -Inf) if not allowed
  # Note: Check only non-NA values to avoid issues when allow_na=TRUE
  if (isFALSE(allow_inf) && any(!is.na(numeric_arg) & is.infinite(numeric_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain infinite values (Inf, -Inf).",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR REQUIRED NAMES
  # ============================================================================

  # Check if vector must have names
  if (isTRUE(must_have_names)) {
    vec_names <- names(numeric_arg)

    # Check if names exist
    if (is.null(vec_names)) {
    stop(
      sprintf(
          "Argument '%s' in function '%s' must be a named vector with unique names.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

    # Check for empty string names (after removing NA names)
    # NA names are treated as unnamed
    if (any(!is.na(vec_names) & vec_names == '')) {
      stop(
        sprintf(
          "Argument '%s' in function '%s' must have non-empty names (no '').",
          as.character(substitute(numeric_arg)),
          deparse(sys.call(sys.parent())[[1]])
        ),
        call. = FALSE
      )
    }

    # Check for duplicate names
    if (any(duplicated(vec_names))) {
    stop(
      sprintf(
          "Argument '%s' in function '%s' must have unique names (no duplicates).",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }
  }

  # ============================================================================
  # ALL CHECKS PASSED - RETURN VECTOR
  # ============================================================================

  invisible(numeric_arg)
}
