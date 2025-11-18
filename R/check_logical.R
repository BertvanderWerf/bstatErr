#' Check a Logical Value for Typical Errors
#'
#' Validates a single logical argument for type, NA values, and NULL.
#' Provides consistent error messages that include the argument name and
#' calling function name.
#'
#' @details
#' This function performs validation checks in sequence:
#' \enumerate{
#'   \item Validates logical parameters
#'   \item Checks if NULL when not allowed
#'   \item Returns NULL early if allowed
#'   \item Handles single NA value specially
#'   \item Checks if input is logical type
#'   \item Checks for NA values when not allowed
#' }
#'
#' @param logical_arg Logical value to check (TRUE, FALSE, or NA).
#' @param allow_null Logical. If TRUE, NULL is accepted. Default FALSE.
#' @param allow_na Logical. If TRUE, NA values are allowed. Default FALSE.
#'
#' @return Invisibly returns \code{logical_arg} if all checks pass;
#'   otherwise throws an error.
#'
#' @examples
#' check_logical(TRUE)
#' check_logical(FALSE)
#' check_logical(NULL, allow_null = TRUE)
#' check_logical(NA, allow_na = TRUE)
#'
#' @export
check_logical <- function(logical_arg,
                          allow_null = FALSE,
                          allow_na = FALSE) {

  # ============================================================================
  # VALIDATE LOGICAL PARAMETERS
  # ============================================================================

  if (!is.logical(allow_null)) {
    stop("Parameter 'allow_null' must be logical", call. = FALSE)
  }
  if (!is.logical(allow_na)) {
    stop("Parameter 'allow_na' must be logical", call. = FALSE)
  }

  # ============================================================================
  # CHECK FOR NULL
  # ============================================================================

  if (is.null(logical_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL logical value.",
        as.character(substitute(logical_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  if (is.null(logical_arg)) {
    return(invisible(NULL))
  }

  # NA or NaN can be of any type return as.logical(logical_arg), no further test should be done
  # ============================================================================
  # HANDLE SPECIAL CASE: SINGLE NA VALUE
  # ============================================================================

  if (length(logical_arg) == 1 && isTRUE(allow_na) && is.na(logical_arg)) {
    return(invisible(logical_arg))
  }

   # ============================================================================
  # CHECK TYPE
  # ============================================================================

  if (!is.logical(logical_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a logical value (TRUE or FALSE).",
        as.character(substitute(logical_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR SINGLE LOGICAL VALUE
  # ============================================================================

  if (length(logical_arg) != 1) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a single logical value.",
        as.character(substitute(logical_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR NA VALUES
  # ============================================================================

  if (isFALSE(allow_na) && any(is.na(logical_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain missing values (NA or NaN).",
        as.character(substitute(logical_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # RETURN VALIDATED VALUE
  # ============================================================================

  invisible(logical_arg)
}
