#' Check a function for typical errors
#'
#' Validates a single function argument for type and NULL function.
#'
#' @param function_arg function to check.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value. Default is FALSE.
#'
#' @return Returns \code{function_arg} if all checks pass; otherwise throws an error.
#' @examples
#' check_function(mean)
#' check_function(NULL, allow_null = TRUE)
#' @export
check_function <- function(function_arg,
                          allow_null = FALSE) {

  # Check for NULL if not allowed
  if (is.null(function_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL function name",
        as.character(substitute(function_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # NULL does not have a type, return NULL, no further tests should be done
  if (is.null(function_arg)) return(NULL)

  # Check type if not NULL
  if (!is.null(function_arg) && !is.function(function_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a function name",
        as.character(substitute(function_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # All checks passed; return the function argument
  function_arg
}
