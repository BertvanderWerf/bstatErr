#' Check a character string for typical errors
#'
#' Validates a single character string argument for type, NA, NULL, and empty string.
#'
#' @param string_arg Character string to check.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value. Default is FALSE.
#' @param allow_na Logical. If TRUE, NA strings are allowed. Default is FALSE.
#' @param allow_empty Logical. If TRUE, empty strings "" are allowed. Default is FALSE.
#'
#' @return Invisibly returns \code{string_arg} if all checks pass; otherwise throws an error.
#' @examples
#' check_string("abc")
#' check_string(NULL, allow_null = TRUE)
#' @export
check_string <- function(string_arg,
                         allow_null = FALSE,
                         allow_na = FALSE,
                         allow_empty = FALSE) {

  # Check for NULL if not allowed
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

  # NULL does not have a type, return NULL, no further tests should be done
  if (is.null(string_arg)) return(NULL)

  # NA or NaN can be of any type return as.character(string_arg), no further test should be done
  if (!is.null(string_arg) && length(string_arg)==1 && isTRUE(allow_na) && is.na(string_arg))
    return(as.character(string_arg))

  # Check type if not NULL
  if (!is.null(string_arg) && !is.character(string_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a character string.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

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

  # Check for NA values if not allowed
  if (!is.null(string_arg) && isFALSE(allow_na) && is.na(string_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not be NA or NaN.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check for empty string if not allowed
  if (!is.null(string_arg) && isFALSE(allow_empty) && string_arg == '') {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not be an empty character string.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # All checks passed; return the string argument
  invisible(string_arg)
}
