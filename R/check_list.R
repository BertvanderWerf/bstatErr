#' Check a list for typical errors
#'
#' Validates a single list argument for type, length, NA and NULL list value.
#'
#' @param list_arg list to check.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value. Default is FALSE.
#'
#' @return Returns \code{list_arg} if all checks pass; otherwise throws an error.
#' @examples
#' check_list(list("abc",c(1,2,3)))
#' check_list(NULL, allow_null = TRUE)
#' @export
check_list <- function(list_arg,
                          allow_null = FALSE) {

  # Check for NULL if not allowed
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

  # NULL does not have a type, return NULL, no further tests should be done
  if (is.null(list_arg)) return(NULL)

  # Check type if not NULL
  if (!is.null(list_arg) && !is.list(list_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a list.",
        as.character(substitute(list_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # All checks passed; return the list argument
  list_arg
}
