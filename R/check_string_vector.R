#' Check a String Vector for Typical Errors
#'
#' Validates a character vector for type, NA, empty strings, duplicates, length,
#' and optional naming requirements.
#'
#' @details
#' Validates in sequence:
#' \enumerate{
#'   \item Checks if NULL when not allowed
#'   \item Checks if character type
#'   \item Checks for zero length when not allowed
#'   \item Checks for NA values when not allowed
#'   \item Checks for empty strings when not allowed
#'   \item Checks for duplicates when not allowed
#'   \item Checks for required names and uniqueness
#' }
#'
#' @param string_arg Character vector to check.
#' @param allow_null Logical. If TRUE, NULL is accepted. Default FALSE.
#' @param allow_na Logical. If TRUE, NA values allowed. Default FALSE.
#' @param allow_zero_length Logical. If TRUE, zero-length vectors allowed. Default FALSE.
#' @param allow_empty Logical. If TRUE, empty strings allowed. Default FALSE.
#' @param allow_duplicates Logical. If TRUE, duplicate values allowed. Default FALSE.
#' @param must_have_names Logical. If TRUE, vector must be named. Default FALSE.
#'
#' @return Invisibly returns \code{string_arg} if all checks pass.
#'
#' @examples
#' check_string_vector(c("a", "b", "c"))
#' check_string_vector(c("x", "y"), allow_duplicates = TRUE)
#' check_string_vector(NULL, allow_null = TRUE)
#'
#' @export
check_string_vector <- function(string_arg,
                                allow_null = FALSE,
                                allow_na = FALSE,
                                allow_zero_length = FALSE,
                                allow_empty = FALSE,
                                allow_duplicates = FALSE,
                                must_have_names = FALSE) {

  # ============================================================================
  # VALIDATE LOGICAL PARAMETERS
  # ============================================================================

  allow_null <- check_logical(allow_null)
  allow_na <- check_logical(allow_na)
  allow_zero_length <- check_logical(allow_zero_length)
  allow_empty <- check_logical(allow_empty)
  allow_duplicates <- check_logical(allow_duplicates)
  must_have_names <- check_logical(must_have_names)

  # ============================================================================
  # CHECK FOR NULL
  # ============================================================================

  if (is.null(string_arg) && isFALSE(allow_null)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a non-NULL character vector.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  if (is.null(string_arg)) {
    return(invisible(NULL))
  }

  # ============================================================================
  # HANDLE SPECIAL CASE: SINGLE NA VALUE
  # ============================================================================

  # NA can be of any type. If single NA is allowed, return it early
  if (!is.null(string_arg) && length(string_arg)==1 && isTRUE(allow_na) && is.na(string_arg))
    return(as.character(string_arg))

  # ============================================================================
  # CHECK TYPE
  # ============================================================================

  if (!is.character(string_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a character vector.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR ZERO LENGTH
  # ============================================================================

  if (length(string_arg) == 0 && isFALSE(allow_zero_length)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' has zero length; must have at least one element.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR NA VALUES
  # ============================================================================

  if (isFALSE(allow_na) && any(is.na(string_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain missing values (NA).",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR EMPTY STRINGS
  # ============================================================================

  if (isFALSE(allow_empty) && any(string_arg == "", na.rm = TRUE)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain empty strings.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR DUPLICATES
  # ============================================================================

  if (isFALSE(allow_duplicates) && any(duplicated(string_arg, na.rm = TRUE))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain duplicate values.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ============================================================================
  # CHECK FOR REQUIRED NAMES
  # ============================================================================

  if (isTRUE(must_have_names)) {
    vec_names <- names(string_arg)

    if (is.null(vec_names) || any(vec_names == "")) {
    stop(
      sprintf(
          "Argument '%s' in function '%s' must have names for all elements.",
        as.character(substitute(string_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

    if (any(duplicated(vec_names))) {
      stop(
        sprintf(
          "Argument '%s' in function '%s' names must be unique.",
          as.character(substitute(string_arg)),
          deparse(sys.call(sys.parent())[[1]])
        ),
        call. = FALSE
      )
    }
  }

  # ============================================================================
  # RETURN VALIDATED VALUE
  # ============================================================================

  invisible(string_arg)
}
