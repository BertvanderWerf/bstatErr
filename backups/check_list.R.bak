#' Check a list for typical errors
#'
#' Validates a single list argument for type and NULL list value.
#'
#' @param list_arg list to check.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value. Default is FALSE.
#' @param must_have_names Logical. If TRUE, list must have unique names. Default is FALSE.
#'
#' @return Invisibly returns \code{list_arg} if all checks pass; otherwise throws an error.
#' @examples
#' check_list(list("abc",c(1,2,3)))
#' check_list(NULL, allow_null = TRUE)
#' @export
check_list <- function(list_arg,
                       allow_null = FALSE,
                       must_have_names = FALSE) {

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

  # Check if list must have names
  if (!is.null(list_arg) && isTRUE(must_have_names) &&
      (is.null(names(list_arg)) || any(!is.na(names(list_arg)) & names(list_arg)=='') || any(duplicated(names(list_arg))))
     ) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a list with unique names unequal to ''.",
        as.character(substitute(list_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # All checks passed; return the list argument
  invisible(list_arg)
}
