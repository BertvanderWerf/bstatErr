#' Check a Character String for Typical Errors
#'
#' Validates a single character string argument for type, NA, NULL, and empty string.
#'
#' @details
#' Validates in sequence:
#' \enumerate{
#'   \item Checks if NULL when not allowed
#'   \item Handles single NA specially
#'   \item Checks if character type
#'   \item Checks for NA value when not allowed
#'   \item Checks for empty string when not allowed
#' }
#'
#' @param string_arg Character string to check.
#' @param allow_null Logical. If TRUE, NULL is accepted. Default FALSE.
#' @param allow_na Logical. If TRUE, NA values allowed. Default FALSE.
#' @param allow_empty Logical. If TRUE, empty strings allowed. Default FALSE.
#'
#' @return Invisibly returns \code{string_arg} if all checks pass.
#'
#' @examples
#' check_string("hello")
#' check_string("", allow_empty = TRUE)
#' check_string(NA_character_, allow_na = TRUE)
#'
#' @export
check_string <- function(string_arg,
                         allow_null = FALSE,
                         allow_na = FALSE,
                         allow_empty = FALSE) {

  # ============================================================================
  # VALIDATE LOGICAL PARAMETERS
  # ============================================================================

  allow_null <- check_logical(allow_null)
  allow_na <- check_logical(allow_na)
  allow_empty <- check_logical(allow_empty)

  # ============================================================================
  # CHECK FOR NULL
  # ============================================================================

  if (is.null(string_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL character string.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  if (is.null(string_arg)) {
    return(invisible(NULL))
  }

  # ============================================================================
  # HANDLE SPECIAL CASE: SINGLE NA VALUE
  # ============================================================================

  if (length(string_arg) == 1 && isTRUE(allow_na) && (is.na(string_arg) | is.nan(string_arg))) {
    return(invisible(string_arg))
  }

  # ============================================================================
  # CHECK TYPE
  # ============================================================================

  if (!is.character(string_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a character string.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR SINGLE CHARACTER STRING
  # ============================================================================

  # Check length == 1 if not NULL
  if (!is.null(string_arg) && length(string_arg) != 1) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a single character string.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR NA VALUES
  # ============================================================================

  if (isFALSE(allow_na) && (is.na(string_arg) | is.nan(string_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain missing values (NA or NaN).",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR EMPTY STRINGS
  # ============================================================================

  if (isFALSE(allow_empty) && string_arg == "") {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not be an empty string.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # RETURN VALIDATED VALUE
  # ============================================================================

  invisible(string_arg)
}
