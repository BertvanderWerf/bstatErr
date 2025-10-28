#' Check a string vector for typical errors
#'
#' Validates a single string argument for type, length, NA, "" and NULL string value.
#'
#' @param string_arg string to check.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value. Default is FALSE.
#' @param allow_na Logical. If TRUE, NA strings are allowed. Default is FALSE.
#' @param allow_zero_length Logical. If TRUE, zero length vectors are allowed. Default is FALSE
#' @param allow_empty Logical. If TRUE, empty strings ("") are allowed. Default is FALSE
#' @param allow_duplicates Logical. If TRUE, duplicate values are allowed. Default is FALSE
#' @param must_have_names Logical. If TRUE, vector elements must have unique names. Default is FALSE.
#'
#' @return Invisibly returns \code{string_arg} if all checks pass; otherwise throws an error.
#' @examples
#' check_string_vector(c("a","b","c",NaN))
#' check_string_vector(NULL, allow_null = TRUE)
#' @export
check_string_vector <- function(string_arg,
                                allow_null = FALSE,
                                allow_na = FALSE,
                                allow_zero_length = FALSE,
                                allow_empty = FALSE,
                                allow_duplicates = FALSE,
                                must_have_names = FALSE) {

  # Check for NULL if not allowed
  if (is.null(string_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL string value.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # NULL does not have a type, return NULL, no further tests should be done
  if (is.null(string_arg)) return(NULL)

  # NA or NaN can be of any type return as.string(string_arg), no further test should be done
  if (!is.null(string_arg) && length(string_arg)==1 && isTRUE(allow_na) && is.na(string_arg))
    return(as.character(string_arg))

  # Check type if not NULL
  if (!is.null(string_arg) && !is.character(string_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a string value.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check length == 0 if not NULL
  if (!is.null(string_arg) && length(string_arg) == 0 && isFALSE(allow_zero_length)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' has zero length, the vector must have at least one value.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check for NA values if not allowed
  if (!is.null(string_arg) && isFALSE(allow_na) && any(is.na(string_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain missing values.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check for non finite values if not allowed
  if (!is.null(string_arg) && isFALSE(allow_empty) && any(!is.na(string_arg) & string_arg=="")) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain empty ('') values.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check for non finite values if not allowed
  if (!is.null(string_arg) && isFALSE(allow_duplicates) && any(duplicated(string_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain duplicated values.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check if list must have names
  if (!is.null(string_arg) && isTRUE(must_have_names) &&
      (is.null(names(string_arg)) || any(!is.na(names(string_arg)) & names(string_arg)=='') || any(duplicated(names(string_arg))))
  ) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a vector with unique names unequal to ''.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # All checks passed; return the string argument
  invisible(string_arg)
}



