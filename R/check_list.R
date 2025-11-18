#' Check a List for Typical Errors
#'
#' Validates a list argument for type, NULL, and optional naming requirements.
#'
#' @details
#' Validates in sequence:
#' \enumerate{
#'   \item Validates logical parameters
#'   \item Checks if NULL when not allowed
#'   \item Checks if input is list type
#'   \item Checks for required names and uniqueness
#' }
#'
#' @param list_arg List to check.
#' @param allow_null Logical. If TRUE, NULL is accepted. Default FALSE.
#' @param must_have_names Logical. If TRUE, all elements must have names. Default FALSE.
#'
#' @return Invisibly returns \code{list_arg} if all checks pass.
#'
#' @examples
#' check_list(list(a = 1, b = 2))
#' check_list(list(1, 2, 3))
#' check_list(NULL, allow_null = TRUE)
#'
#' @export
check_list <- function(list_arg,
                       allow_null = FALSE,
                       must_have_names = FALSE) {

  # ============================================================================
  # VALIDATE LOGICAL PARAMETERS
  # ============================================================================

  allow_null <- check_logical(allow_null)
  must_have_names <- check_logical(must_have_names)

  # ============================================================================
  # CHECK FOR NULL
  # ============================================================================

  if (is.null(list_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL list.",
        as.character(substitute(list_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  if (is.null(list_arg)) {
    return(invisible(NULL))
  }

  # ============================================================================
  # CHECK TYPE
  # ============================================================================

  if (!is.list(list_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a list.",
        as.character(substitute(list_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR REQUIRED NAMES
  # ============================================================================

  if (isTRUE(must_have_names)) {
    list_names <- names(list_arg)

    # Check if list has names
    if (is.null(list_names) || any(list_names == "")) {
    stop(
      sprintf(
          "Argument '%s' in function '%s' must have names for all elements.",
        as.character(substitute(list_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

    # Check if names are unique
    if (any(duplicated(list_names))) {
      stop(
        sprintf(
          "Argument '%s' in function '%s' names must be unique.",
          as.character(substitute(list_arg)),
          deparse(sys.call(sys.parent())[[1]])
        ),
        call. = FALSE
      )
    }
  }

  # ============================================================================
  # RETURN VALIDATED VALUE
  # ============================================================================

  invisible(list_arg)
}
