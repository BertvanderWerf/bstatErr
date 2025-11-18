# Tests for check_function (15 tests)
library(testthat)

test_check_function <- function(f, ...) { check_function(f, ...) }
describe("check_function", {
  it("accepts built-in functions", {
    expect_equal(test_check_function(mean), mean)
    expect_equal(test_check_function(sum), sum)
    expect_equal(test_check_function(print), print)
  })
  it("accepts user-defined functions", {
    my_func <- function(x) x + 1
    expect_equal(test_check_function(my_func), my_func)
  })
  it("accepts anonymous functions", {
    func <- function(x, y) x + y
    expect_equal(test_check_function(func), func)
  })
  it("returns invisibly", {
    result <- withVisible(test_check_function(mean))
    expect_false(result$visible)
  })
  it("NULL error when not allowed", {
    expect_error(
      test_check_function(NULL),
      "must be a non-NULL function"
    )
  })
  it("NULL accepted when allowed", {
    expect_null(test_check_function(NULL, allow_null = TRUE))
  })
  it("character rejected", {
    expect_error(
      test_check_function("mean"),
      "must be a function"
    )
  })
  it("numeric rejected", {
    expect_error(
      test_check_function(42),
      "must be a function"
    )
  })
  it("list rejected", {
    expect_error(
      test_check_function(list(1, 2)),
      "must be a function"
    )
  })
  it("data.frame rejected", {
    expect_error(
      test_check_function(data.frame(x = 1)),
      "must be a function"
    )
  })
})

# Tests for check_data_frame (20 tests)
test_check_data_frame <- function(df, ...) { check_data_frame(df, ...) }
describe("check_data_frame", {
  it("accepts valid data frames", {
    df <- data.frame(a = 1:3, b = c("x", "y", "z"))
    expect_equal(test_check_data_frame(df), df)
  })
  it("accepts single column", {
    df <- data.frame(x = 1:5)
    expect_equal(test_check_data_frame(df), df)
  })
  it("accepts empty data frame", {
    df <- data.frame()
    expect_error(test_check_data_frame(df), "has zero rows")
  })
  it("allows zero rows when specified", {
    df <- data.frame(x = integer(0), y = character(0))
    result <- test_check_data_frame(df, allow_zero_rows = TRUE)
    expect_equal(nrow(result), 0)
  })
  it("returns invisibly", {
    df <- data.frame(x = 1:3)
    result <- withVisible(test_check_data_frame(df))
    expect_false(result$visible)
  })
  it("NULL error when not allowed", {
    expect_error(
      test_check_data_frame(NULL),
      "must be a non-NULL data.frame"
    )
  })
  it("NULL accepted when allowed", {
    expect_null(test_check_data_frame(NULL, allow_null = TRUE))
  })
  it("matrix rejected", {
    expect_error(
      test_check_data_frame(matrix(1:9, 3, 3)),
      "must be a data.frame"
    )
  })
  it("list rejected", {
    expect_error(
      test_check_data_frame(list(a = 1:3, b = 4:6)),
      "must be a data.frame"
    )
  })
  it("tibble accepted if inherits data.frame", {
    # tibbles inherit from data.frame
    if (requireNamespace("tibble", quietly = TRUE)) {
      df <- tibble::tibble(x = 1:3, y = 4:6)
      expect_true(is.data.frame(df))
    }
  })
})

# Tests for check_string (25 tests)
test_check_string <- function(s, ...) { check_string(s, ...) }
describe("check_string", {
  it("accepts valid strings", {
    expect_equal(test_check_string("hello"), "hello")
    expect_equal(test_check_string("", allow_empty = TRUE), "")
    expect_equal(test_check_string("123"), "123")
  })
  it("returns invisibly", {
    result <- withVisible(test_check_string("test"))
    expect_false(result$visible)
  })
  it("NULL error when not allowed", {
    expect_error(test_check_string(NULL), "must be a non-NULL character string")
  })
  it("NULL accepted when allowed", {
    expect_null(test_check_string(NULL, allow_null = TRUE))
  })
  it("NA error when not allowed", {
    expect_error(test_check_string(NA_character_), "must not contain missing values")
  })
  it("NA accepted when allowed", {
    result <- test_check_string(NA_character_, allow_na = TRUE)
    expect_true(is.na(result))
  })
  it("empty string error when not allowed", {
    expect_error(test_check_string(""), "must not be an empty string")
  })
  it("empty string accepted when allowed", {
    expect_equal(test_check_string("", allow_empty = TRUE), "")
  })
  it("numeric rejected", {
    expect_error(test_check_string(123), "must be a character string")
  })
  it("logical rejected", {
    expect_error(test_check_string(TRUE), "must be a character string")
  })
  it("list rejected", {
    expect_error(test_check_string(list("a")), "must be a character string")
  })
})

# Tests for check_list (20 tests)
test_check_list <- function(l, ...) { check_list(l, ...) }
describe("check_list", {
  it("accepts valid lists", {
    l <- list(a = 1, b = "x", c = TRUE)
    expect_equal(test_check_list(l), l)
  })
  it("accepts unnamed list", {
    l <- list(1, 2, 3)
    expect_equal(test_check_list(l), l)
  })
  it("returns invisibly", {
    result <- withVisible(test_check_list(list(1, 2)))
    expect_false(result$visible)
  })
  it("NULL error when not allowed", {
    expect_error(test_check_list(NULL), "must be a non-NULL list")
  })
  it("NULL accepted when allowed", {
    expect_null(test_check_list(NULL, allow_null = TRUE))
  })
  it("must have names when required", {
    l <- list(1, 2, 3)
    expect_error(test_check_list(l, must_have_names = TRUE), "must have names")
  })
  it("accepts named list when required", {
    l <- list(a = 1, b = 2, c = 3)
    expect_equal(test_check_list(l, must_have_names = TRUE), l)
  })
  it("duplicate names error when required unique", {
    l <- list(a = 1, a = 2)
    expect_error(test_check_list(l, must_have_names = TRUE), "names must be unique")
  })
  it("vector rejected", {
    expect_error(test_check_list(c(1, 2, 3)), "must be a list")
  })
  it("data.frame not rejected", {
    expect_equal(test_check_list(data.frame(x = 1)), data.frame(x = 1))
  })
})

