#' Evaluate an Expression and Capture Condition Objects
#'
#' Evaluates an expression while capturing and returning any warning, message,
#' or error conditions as structured objects. This allows advanced inspection of
#' errors and warnings instead of storing only their messages.
#'
#' @details
#' This function uses R's condition handling system to capture three types of
#' conditions:
#' \itemize{
#'   \item **Errors** (class inherits from "error"): Stops execution
#'   \item **Warnings** (class inherits from "warning"): Non-fatal issues
#'   \item **Messages** (class inherits from "message"): Informational output
#' }
#'
#' Unlike \code{tryCatch()}, this captures the condition *objects* themselves,
#' which include:
#' \itemize{
#'   \item The message text
#'   \item The call that generated it
#'   \item The condition class
#'   \item Any custom fields
#' }
#'
#' Multiple warnings and messages are collected in a list.
#'
#' @param expr Expression to evaluate (enclosed in braces or as a call).
#' @param default Value to return if an error occurs. Default: \code{NULL}.
#'
#' @return A list with elements:
#' \itemize{
#'   \item \code{value}: The evaluation result or the \code{default} if error occurred
#'   \item \code{error}: Error condition object (or NULL if none)
#'   \item \code{warning}: List of warning condition objects (or NULL if none)
#'   \item \code{message}: List of message condition objects (or NULL if none)
#' }
#'
#' @examples
#' # Normal evaluation with no conditions
#' result <- catch_conditions({ 2 + 2 })
#' print(result$value)  # 4
#'
#' # Capture warnings
#' result <- catch_conditions({
#'   x <- c(1, 2, NA)
#'   mean(x)
#' })
#'
#' # Capture error with default value
#' result <- catch_conditions({
#'   stop("Something went wrong!")
#' }, default = NA)
#' print(result$error$message)
#'
#' @export
catch_conditions <- function(expr, default = NULL) {

  # ============================================================================
  # INITIALIZE RESULT STRUCTURE
  # ============================================================================

  # Result list to hold all captured conditions
  result <- list(
    value = NULL,
    error = NULL,
    warning = NULL,
    message = NULL
  )

  # ============================================================================
  # EVALUATE EXPRESSION WITH CONDITION HANDLERS
  # ============================================================================

  # Use tryCatch to evaluate expression and capture conditions
  result$value <- tryCatch(
    # EXPRESSION TO EVALUATE
    {
      # Capture multiple warnings and messages using withCallingHandlers
      withCallingHandlers(
        expr,
        # HANDLE WARNINGS
        warning = function(w) {
          # Store warning condition object (not just message)
          if (is.null(result$warning)) {
            result$warning <<- list()
  }
          result$warning <<- c(result$warning, list(w))
          # Call invokeRestart to continue execution
          invokeRestart("muffleWarning")
        },
        # HANDLE MESSAGES
        message = function(m) {
          # Store message condition object
          if (is.null(result$message)) {
            result$message <<- list()
          }
          result$message <<- c(result$message, list(m))
          # Call invokeRestart to continue execution
    invokeRestart("muffleMessage")
  }
      )
    },
    # HANDLE ERRORS
    error = function(e) {
      # Store error condition object
      result$error <<- e
      # Return default value instead of stopping
      return(default)
  }
  )

  # ============================================================================
  # RETURN CAPTURED CONDITIONS
  # ============================================================================

  invisible(result)
}
