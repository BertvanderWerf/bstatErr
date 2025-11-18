#' bstatErr: Input Validation Functions for R Packages
#'
#' @description
#' The bstatErr package provides a comprehensive suite of input validation
#' functions designed to standardize argument checking in R packages. Each
#' function validates specific data types and provides consistent, informative
#' error messages that include both the argument name and calling function name.
#'
#' @details
#' ## Overview
#'
#' Input validation is critical for creating robust R packages. The bstatErr
#' package simplifies this process by providing ready-to-use validation functions
#' that handle common data types and edge cases.
#'
#' ## Main Features
#'
#' - **Consistent API**: All functions follow the same pattern with `allow_*` parameters
#' - **Informative errors**: Error messages include argument names and calling functions
#' - **Comprehensive validation**: Checks for NULL, NA, type, length, and special requirements
#' - **Edge case handling**: Properly handles Inf, NaN, zero-length vectors, etc.
#' - **Production ready**: Thoroughly tested with extensive unit test coverage
#'
#' ## Core Validation Functions
#'
#' ### Single Value Checks
#' - \code{\link{check_numeric}}: Validates a single numeric value
#' - \code{\link{check_logical}}: Validates a single logical value
#' - \code{\link{check_string}}: Validates a single character string
#'
#' ### Vector Checks
#' - \code{\link{check_numeric_vector}}: Validates numeric vectors
#' - \code{\link{check_string_vector}}: Validates character vectors
#'
#' ### Matrix Checks
#' - \code{\link{check_numeric_matrix}}: Validates numeric matrices
#'
#' ### Object Checks
#' - \code{\link{check_list}}: Validates list objects
#' - \code{\link{check_data_frame}}: Validates data frames
#' - \code{\link{check_function}}: Validates function objects
#'
#' ### Utility Functions
#' - \code{\link{catch_conditions}}: Captures errors, warnings, and messages
#'
#' ## Common Parameters
#'
#' Most validation functions share these parameters:
#'
#' - `allow_null`: If TRUE, NULL is accepted (default: FALSE)
#' - `allow_na`: If TRUE, NA values are allowed (default: FALSE)
#' - `allow_inf`: If TRUE, Inf/-Inf values are allowed (default: FALSE, numeric only)
#' - `allow_zero_length`: If TRUE, empty vectors/matrices allowed (default: FALSE)
#' - `must_have_names`: If TRUE, requires unique names (default: FALSE)
#'
#' ## Usage Pattern
#'
#' All validation functions follow this pattern:
#'
#' 1. Check if NULL (when not allowed)
#' 2. Check data type
#' 3. Check for NA values (when not allowed)
#' 4. Check for special conditions (Inf, empty, duplicates, etc.)
#' 5. Return input invisibly if all checks pass
#' 6. Throw informative error if any check fails
#'
#' ## Error Messages
#'
#' Error messages follow a consistent format:
#' \preformatted{
#' Error: Argument 'param_name' in function 'calling_function'
#'        must be [requirement description].
#' }
#'
#' This includes:
#' - The exact parameter name that failed
#' - The function where the error occurred
#' - A clear description of what was expected
#'
#' @examples
#' \dontrun{
#' # Example function using bstatErr validation
#' my_function <- function(x, y, data) {
#'   # Validate inputs
#'   check_numeric_vector(x)
#'   check_string(y)
#'   check_data_frame(data)
#'
#'   # Function logic here...
#' }
#'
#' # Valid call
#' my_function(c(1, 2, 3), "test", mtcars)
#'
#' # Invalid calls with clear error messages
#' my_function("not numeric", "test", mtcars)
#' # Error: Argument 'x' in function 'my_function' must be a numeric vector.
#'
#' my_function(c(1, 2), 123, mtcars)
#' # Error: Argument 'y' in function 'my_function' must be a character string.
#'
#' my_function(c(1, 2), "test", NULL)
#' # Error: Argument 'data' in function 'my_function' must be a non-NULL data.frame.
#' }
#'
#' @section Quick Start:
#'
#' \preformatted{
#' # Install package
#' # install.packages("bstatErr")
#'
#' # Load package
#' library(bstatErr)
#'
#' # Basic usage - single values
#' check_numeric(3.14)              # Pass
#' check_logical(TRUE)              # Pass
#' check_string("hello")            # Pass
#'
#' # Vectors
#' check_numeric_vector(c(1, 2, 3))         # Pass
#' check_string_vector(c("a", "b", "c"))   # Pass
#'
#' # With options
#' check_numeric(NULL, allow_null = TRUE)                    # Pass
#' check_numeric_vector(c(1, NA, 3), allow_na = TRUE)       # Pass
#' check_numeric_vector(c(1, Inf), allow_inf = TRUE)        # Pass
#'
#' # Named vectors
#' check_numeric_vector(
#'   c(a = 1, b = 2),
#'   must_have_names = TRUE
#' )  # Pass
#' }
#'
#' @section Package Philosophy:
#'
#' bstatErr is designed with these principles:
#'
#' 1. **Fail Fast**: Validation happens immediately at function entry
#' 2. **Fail Clearly**: Errors explain exactly what went wrong
#' 3. **Fail Consistently**: All functions follow the same patterns
#' 4. **Be Flexible**: Allow options for different use cases
#' 5. **Be Thorough**: Handle edge cases properly
#'
#' @section Performance:
#'
#' Validation functions are lightweight with minimal overhead. They use
#' base R functions only and avoid expensive operations. Typical overhead
#' is < 1ms per validation call.
#'
#' @section See Also:
#'
#' Other R validation packages:
#' - assertthat: Assertion framework
#' - checkmate: Fast argument checks
#' - validate: Data validation infrastructure
#'
#' bstatErr complements these by providing:
#' - Better error messages with function names
#' - Consistent API across all data types
#' - Comprehensive edge case handling
#'
#' @section Development:
#'
#' This package was developed with AI assistance from Perplexity AI for
#' documentation, testing, and best practices implementation.
#'
#' @author Bert van der Werf
#'
#' @keywords package
#'
#' @docType package
#'
#' @name bstatErr-package
#' @aliases bstatErr
#'
"_PACKAGE"
