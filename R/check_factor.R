#' Check a factor for typical errors
#'
#' Validates a factor for type, missing values, length, number of levels,
#' and presence of empty levels.
#'
#' @details
#' Validates in sequence:
#' \enumerate{
#'   \item Checks if NULL when not allowed.
#'   \item Checks argument is a factor.
#'   \item Checks for zero length when not allowed.
#'   \item Checks for NA values when not allowed (or allowed).
#'   \item Checks for zero levels when not allowed.
#'   \item Checks for only one level when not allowed.
#'   \item Checks for levels with no observations (empty levels) when not allowed.
#' }
#'
#' @param factor_arg Factor to check.
#' @param allow_null Logical. If TRUE, NULL is accepted. Default FALSE.
#' @param allow_na Logical. If TRUE, NA values are allowed. Default FALSE.
#' @param allow_zero_length Logical. If TRUE, zero-length factors are allowed. Default FALSE.
#' @param allow_one_level Logical. If TRUE, factor with 1 level is allowed. Default FALSE.
#' @param allow_zero_levels Logical. If TRUE, factor with 0 levels is allowed. Default FALSE.
#' @param allow_empty_levels Logical. If TRUE, factor is allowed to have levels with no observations. Default FALSE.
#'
#' @return Invisibly returns \code{factor_arg} if all checks pass.
#'
#' @examples
#' check_factor(factor(c("a", "b", "c")))
#' check_factor(factor(c("x", "y"), levels = c("x", "y", "z")),
#'              allow_empty_levels = TRUE)
#' check_factor(NULL, allow_null = TRUE)
#'
#' @export
check_factor <- function(
    factor_arg,
    allow_null = FALSE,
    allow_na = FALSE,
    allow_zero_length = FALSE,
    allow_one_level = FALSE,
    allow_zero_levels = FALSE,
    allow_empty_levels = FALSE
) {

  # Validate logical control parameters (using bstatErr helpers)
  allow_null         <- bstatErr::check_logical(allow_null)
  allow_na           <- bstatErr::check_logical(allow_na)
  allow_zero_length  <- bstatErr::check_logical(allow_zero_length)
  allow_one_level    <- bstatErr::check_logical(allow_one_level)
  allow_zero_levels  <- bstatErr::check_logical(allow_zero_levels)
  allow_empty_levels <- bstatErr::check_logical(allow_empty_levels)

  # ---------------------------------------------------------------------------
  # NULL check
  # ---------------------------------------------------------------------------
  if (is.null(factor_arg)) {
    if (!allow_null) {
      stop(
        sprintf(
          "Argument '%s' in function '%s' must be a non-NULL factor.",
          as.character(substitute(factor_arg)),
          deparse(sys.call(sys.parent())[[1]])
        ),
        call. = FALSE
      )
    }
    return(invisible(NULL))
  }

  # ---------------------------------------------------------------------------
  # Type check
  # ---------------------------------------------------------------------------
  if (!is.factor(factor_arg)) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must be a factor.",
        as.character(substitute(factor_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Zero length check
  # ---------------------------------------------------------------------------
  if (length(factor_arg) == 0L && !allow_zero_length) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' has zero length; must have at least one element.",
        as.character(substitute(factor_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # NA values check
  # ---------------------------------------------------------------------------
  if (!allow_na && any(is.na(factor_arg))) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' must not contain missing values (NA).",
        as.character(substitute(factor_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Number of levels: zero levels
  # ---------------------------------------------------------------------------
  if (!allow_zero_levels && nlevels(factor_arg) == 0L) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' cannot have zero levels.",
        as.character(substitute(factor_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Number of levels: only one level
  # ---------------------------------------------------------------------------
  if (!allow_one_level && nlevels(factor_arg) == 1L) {
    stop(
      sprintf(
        "Argument '%s' in function '%s' cannot have only one level.",
        as.character(substitute(factor_arg)),
        deparse(sys.call(sys.parent())[[1]])
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Empty levels check (levels with zero observations)
  # ---------------------------------------------------------------------------
  if (!allow_empty_levels) {
    freq <- table(factor_arg)
    if (any(freq == 0L)) {
      stop(
        sprintf(
          "Argument '%s' in function '%s' cannot have factor levels with no observations.",
          as.character(substitute(factor_arg)),
          deparse(sys.call(sys.parent())[[1]])
        ),
        call. = FALSE
      )
    }
  }

  invisible(factor_arg)
}
