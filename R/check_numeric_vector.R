#' Check a numeric for typical errors
#'
#' Validates a single numeric argument for type, length, NA and NULL numeric value.
#'
#' @param numeric_arg numeric to check.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value. Default is FALSE.
#' @param allow_na Logical. If TRUE, NA numerics are allowed. Default is FALSE.
#' @param allow_inf Logical. If TRUE, -Inf or Inf numerics are allowed. Default is FALSE.
#' @param allow_zero_length Logical. If TRUE, zero length vectors are allowed. Default is FALSE
#' @param must_have_names Logical. If TRUE, vector elements must have unique names. Default is FALSE.
#'
#' @return Invisibly returns \code{numeric_arg} if all checks pass; otherwise throws an error.
#' @examples
#' check_numeric_vector(TRUE)
#' check_numeric_vector(NULL, allow_null = TRUE)
#' @export
check_numeric_vector <- function(numeric_arg,
                                 allow_null = FALSE,
                                 allow_na = FALSE,
                                 allow_inf = FALSE,
                                 allow_zero_length = FALSE,
                                 must_have_names = FALSE) {

  # Check for NULL if not allowed
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

  # NULL does not have a type, return NULL, no further tests should be done
  if (is.null(numeric_arg)) return(NULL)

  # NA or NaN can be of any type return as.numeric(numeric_arg), no further test should be done
  if (!is.null(numeric_arg) && length(numeric_arg)==1 && isTRUE(allow_na) && is.na(numeric_arg))
    return(as.numeric(numeric_arg))

  # Check type if not NULL
  if (!is.null(numeric_arg) && !is.numeric(numeric_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a numeric value.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check length == 0 if not NULL
  if (!is.null(numeric_arg) && length(numeric_arg) == 0 && isFALSE(allow_zero_length)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' has zero length, the vector must have at least one value.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check for NA values if not allowed
  if (!is.null(numeric_arg) && isFALSE(allow_na) && any(is.na(numeric_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain missing values.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check for non finite values if not allowed
  if (!is.null(numeric_arg) && isFALSE(allow_inf) && any(!is.na(numeric_arg) & is.infinite(numeric_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain missing values.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }


  # Check if list must have names
  if (!is.null(numeric_arg) && isTRUE(must_have_names) &&
      (is.null(names(numeric_arg)) || any(!is.na(names(numeric_arg)) & names(numeric_arg)=='') || any(duplicated(names(numeric_arg))))
  ) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a vector with unique names unequal to ''.",
        as.character(substitute(numeric_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # All checks passed; return the numeric argument
  invisible(numeric_arg)
}
