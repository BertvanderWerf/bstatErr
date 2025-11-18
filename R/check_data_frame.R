#' Check a Data Frame for Typical Errors
#'
#' Validates a data frame argument for type, NULL, and number of rows.
#'
#' @details
#' Validates in sequence:
#' \enumerate{
#'   \item Validates logical parameters
#'   \item Checks if NULL when not allowed
#'   \item Checks if input is data.frame type
#'   \item Checks for zero rows when not allowed
#' }
#'
#' @param data_frame_arg Data frame to check.
#' @param allow_null Logical. If TRUE, NULL is accepted. Default FALSE.
#' @param allow_zero_rows Logical. If TRUE, empty data frames allowed. Default FALSE.
#'
#' @return Invisibly returns \code{data_frame_arg} if all checks pass.
#'
#' @examples
#' check_data_frame(mtcars)
#' check_data_frame(NULL, allow_null = TRUE)
#'
#' @export
check_data_frame <- function(data_frame_arg,
                             allow_null = FALSE,
                             allow_zero_rows = FALSE) {

  # ============================================================================
  # VALIDATE LOGICAL PARAMETERS
  # ============================================================================

  check_logical(allow_null)
  check_logical(allow_zero_rows)

  # ============================================================================
  # CHECK FOR NULL
  # ============================================================================

  if (is.null(data_frame_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL data.frame.",
        as.character(substitute(data_frame_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  if (is.null(data_frame_arg)) {
    return(invisible(NULL))
  }

  # ============================================================================
  # CHECK TYPE
  # ============================================================================

  if (!is.data.frame(data_frame_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a data.frame.",
        as.character(substitute(data_frame_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR ZERO ROWS
  # ============================================================================

  if (nrow(data_frame_arg) == 0 && isFALSE(allow_zero_rows)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' has zero rows; must have at least one row.",
        as.character(substitute(data_frame_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # RETURN VALIDATED VALUE
  # ============================================================================

  invisible(data_frame_arg)
}
