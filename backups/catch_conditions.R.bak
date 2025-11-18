#' Evaluate an Expression and Capture Condition Objects
#'
#' Evaluates an expression while capturing and returning any warning, message, or error conditions as structured objects.
#' This allows advanced inspection of errors and warnings (including class, call, and metadata) instead of storing only their messages.
#'
#' @param expr Expression to evaluate (enclosed in braces or as a call).
#' @param default Value to return if an error occurs. Default: `NULL`.
#'
#' @returns
#' A list with elements:
#' \itemize{
#'   \item `value` — The evaluation result or the fallback `default` if an error occurred.
#'   \item `warning` — A list of captured warning condition objects (or `NULL` if none).
#'   \item `message` — A list of captured message condition objects (or `NULL` if none).
#'   \item `error` — An error condition object if an error occurred (or `NULL` otherwise).
#' }
#'
#' @examples
#' # 1. Normal evaluation
#' catch_conditions({ 2 + 2 })
#'
#' # 2. Capture warnings and messages
#' catch_conditions({
#'   message("Computation started")
#'   log(-1)
#' })
#'
#' # 3. Capture error and return fallback
#' res <- catch_conditions(stop("Critical failure"), default = NA)
#' res$error[[1]]$message    # Inspect full error object
#'
#' @export
catch_conditions <- function(expr, default = NULL) {
  warnings_list <- list()
  messages_list <- list()
  error_obj <- NULL

  # Warning handler – collect condition objects, not strings
  warning_handler <- function(w) {
    warnings_list <<- append(warnings_list, list(w))
    invokeRestart("muffleWarning")  # Continue execution quietly
  }

  # Message handler – collect message objects
  message_handler <- function(m) {
    messages_list <<- append(messages_list, list(m))
    invokeRestart("muffleMessage")
  }

  # Error handler – capture the error object, return fallback
  error_handler <- function(e) {
    error_obj <<- e
    default
  }

  # Execute expression inside withCallingHandlers + tryCatch
  result <- withCallingHandlers(
    tryCatch(expr, error = error_handler),
    warning = warning_handler,
    message = message_handler
  )

  # Return structured result containing full condition objects
  list(
    value = result,
    warning = if (length(warnings_list)) warnings_list else NULL,
    message = if (length(messages_list)) messages_list else NULL,
    error = error_obj
  )
}
