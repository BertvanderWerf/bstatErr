#' Check a logical for typical errors
#'
#' Validates a single logical argument for type, length, NA and NULL logical value.
#'
#' @param logical_arg logical to check.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value. Default is FALSE.
#' @param allow_na Logical. If TRUE, NA logicals are allowed. Default is FALSE.
#'
#' @return Invisibly returns \code{logical_arg} if all checks pass; otherwise throws an error.
#' @examples
#' check_logical(TRUE)
#' check_logical(NULL, allow_null = TRUE)
#' @export
check_logical <- function(logical_arg,
                          allow_null = FALSE,
                          allow_na = FALSE) {

  # Check for NULL if not allowed
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

  # NULL does not have a type, return NULL, no further tests should be done
  if (is.null(logical_arg)) return(NULL)

  # NA or NaN can be of any type return as.logical(logical_arg), no further test should be done
  if (!is.null(logical_arg) && length(logical_arg)==1 && isTRUE(allow_na) && is.na(logical_arg))
    return(as.logical(logical_arg))

  # Check type if not NULL
  if (!is.null(logical_arg) && !is.logical(logical_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a logical value.",
        as.character(substitute(logical_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check length == 1 if not NULL
  if (!is.null(logical_arg) && length(logical_arg) != 1) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a single logical value.",
        as.character(substitute(logical_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check for NA values if not allowed
  if (!is.null(logical_arg) && isFALSE(allow_na) && is.na(logical_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not be NA or NaN.",
        as.character(substitute(logical_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # All checks passed; return the logical argument
  invisible(logical_arg)
}
