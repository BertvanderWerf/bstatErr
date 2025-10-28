#' Check a data.frame for typical errors
#'
#' Validates a single data.frame argument for type, length, NA and NULL data.frame value.
#'
#' @param data.frame_arg data.frame to check.
#' @param allow_null Logical. If TRUE, NULL is accepted as a valid value. Default is FALSE.
#' @param allow_zero_length Logical. If TRUE, a data frame with zero number of rows is accepted as a valid value. Default is FALSE.
#'
#' @return Invisibly returns \code{data_frame_arg} if all checks pass; otherwise throws an error.
#' @examples
#' check_data.frame(data.frame("abc",c(1,2,3)))
#' check_data.frame(NULL, allow_null = TRUE)
#' @export
check_data_frame <- function(data_frame_arg, allow_null = FALSE, allow_zero_length = FALSE) {

  # Check for NULL if not allowed
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

  # NULL does not have a type, return NULL, no further tests should be done
  if (is.null(data_frame_arg)) return(NULL)

  # Check type if not NULL
  if (!is.null(data_frame_arg) && !is.data.frame(data_frame_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a data.frame.",
        as.character(substitute(data_frame_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # Check data_frame has rows
  if (!is.null(data_frame_arg) && isFALSE(allow_zero_length) && nrow(data_frame_arg)==0) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must at least have 1 row in the data.frame.",
        as.character(substitute(data_frame_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # All checks passed; return the data.frame argument
  invisible(data_frame_arg)
}
