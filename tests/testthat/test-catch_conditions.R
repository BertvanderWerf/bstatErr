test_that("returns correct structure and captures condition objects", {
  res <- catch_conditions({ warning("test warning"); message("msg") })
  expect_type(res, "list")
  expect_true(is.list(res$warning))
  expect_true(is.list(res$message))
  expect_s3_class(res$warning[[1]], "simpleWarning")
  expect_s3_class(res$message[[1]], "simpleMessage")
})

test_that("captures error condition and returns default", {
  res <- catch_conditions(stop("fail"), default = "fallback")
  expect_equal(res$value, "fallback")
  expect_s3_class(res$error, "simpleError")
  expect_match(res$error$message, "fail")
})

test_that("handles multiple warnings and messages", {
  res <- catch_conditions({
    warning("first"); warning("second")
    message("alpha"); message("beta")
    42
  })
  expect_equal(res$value, 42)
  expect_length(res$warning, 2)
  expect_length(res$message, 2)
})

test_that("returns no conditions for normal evaluation", {
  res <- catch_conditions({ sqrt(4) })
  expect_null(res$warning)
  expect_null(res$message)
  expect_null(res$error)
  expect_equal(res$value, 2)
})
