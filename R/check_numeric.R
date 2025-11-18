#' Check a Numeric Value for Typical Errors
#'
#' Validates a single numeric argument for type, NA values, and infinity.
#' Provides consistent error messages that include the argument name and
#' calling function name.
#'
#' @details
#' This function performs validation checks in the following sequence:
#' \enumerate{
#'   \item Checks if NULL when not allowed
#'   \item Returns NULL early if allowed (no further checks)
#'   \item Handles single NA value specially when allowed
#'   \item Checks if input is numeric type
#'   \item Checks for NA values when not allowed
#'   \item Checks for infinite values when not allowed
#' }
#'
#' **Important:** A numeric value in R includes integers and doubles.
#' Both \code{1}, \code{1L}, and \code{1.0} are numeric.
#'
#' **NA vs Inf:** NA values are missing data; Inf/-Inf are infinite values.
#' Both can occur in numeric data but represent different issues.
#'
#' @param numeric_arg Numeric value to check. Can be a single value or vector
#'   of length 1.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value.
#'   Default is FALSE.
#' @param allow_na Logical. If TRUE, NA values are allowed. Default is FALSE.
#' @param allow_inf Logical. If TRUE, -Inf or Inf values are allowed.
#'   Default is FALSE.
#'
#' @return Invisibly returns \code{numeric_arg} if all checks pass;
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
#' # Valid numeric values
#' check_numeric(3.14)
#' check_numeric(42)
#' check_numeric(-1.5)
#'
#' # Allow NULL
#' check_numeric(NULL, allow_null = TRUE)
#'
#' # Allow NA
#' check_numeric(NA_real_, allow_na = TRUE)
#'
#' # Allow Inf
#' check_numeric(Inf, allow_inf = TRUE)
#'
#' \dontrun{
#' # These will fail:
#' check_numeric("not a number")  # Type error
#' check_numeric(NULL)  # NULL not allowed
#' check_numeric(NA)  # NA not allowed
#' check_numeric(Inf)  # Inf not allowed
#' }
#'
#' @export
check_numeric <- function(numeric_arg,
                          allow_null = FALSE,
                          allow_na = FALSE,
                          allow_inf = FALSE) {

  # ============================================================================
  # VALIDATE LOGICAL PARAMETERS
  # ============================================================================

  allow_null <- check_logical(allow_null)
  allow_na <- check_logical(allow_na)
  allow_inf <- check_logical(allow_inf)

  # ============================================================================
  # CHECK FOR NULL
  # ============================================================================

  # Check if NULL when not allowed
  if (is.null(numeric_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL numeric value.",
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

  # NA can be of any type. If single NA is allowed, return it early
  if (length(numeric_arg) == 1 && isTRUE(allow_na) && is.na(numeric_arg)) {
    return(invisible(as.numeric(numeric_arg)))
  }

  # ============================================================================
  # CHECK TYPE
  # ============================================================================

  # Check if input is numeric
  if (!is.numeric(numeric_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a numeric value.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR SINGLE NUMERIC VALUE
  # ============================================================================

  # Check length == 1 if not NULL
  if (!is.null(numeric_arg) && length(numeric_arg) != 1) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a single numeric value.",
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
  if (isFALSE(allow_na) && is.na(numeric_arg)) {
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
  # Note: We check only non-NA values to avoid error when allow_na=TRUE
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
  # ALL CHECKS PASSED - RETURN VALUE
  # ============================================================================

    invisible(numeric_arg)
}
