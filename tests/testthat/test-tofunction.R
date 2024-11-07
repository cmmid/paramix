
#' `to_function`: takes a function OR a data.frame-like object (+ interpolation options),
#' with a lower and upper bound
#'
#' Test that:
#'  - error if arguments are not a function or data.table with 2+ columns, the first two numerics
#'  - if function: error if the lb/ub are na
#'  - if table, warn if first column not sorted
#'  - if table, after sorting first column: warn if first row not <= lb
#'  - ... : if last row not <=
#'  - stop if arguments don't share endpoints (after sorting)
#'  - expand when arguments don't share endpoints
#'  - return value has all the input values, *except* possibly endpoints
#'  - return value is sorted, unique

test_that("`to_function` errors for invalid arguments.", {
  notfunordf <- 5
  junkopts <- list(a = 5)

  # just not a function or data.frame
  expect_error(to_function(notfunordf, c(1, 10), interpolate_opts(function(x) x)))
  # if a function, error if bad df columns
  expect_error(to_function(data.frame(x = 0:10), c(1, 10), interpolate_opts(function(x) x)))
  expect_error(to_function(data.frame(x = 0:10, y = LETTERS[0:10]), c(1, 10), interpolate_opts(function(x) x)))
  # if a data.frame, not covering lb, ub
  # if a data.frame, junk interpolate_opts
})

test_that("`to_function` yields a function", {
  testfun <- function(x) x
  testdf <- data.frame(x = 0:10, y = 0:10)
  span <- 1:10
  testops <- interpolate_opts(fun = stats::approxfun, method = "linear", kind = "point")
  res <- expect_no_error(to_function(testfun, span, testops))
  expect_true(is.function(res))
  res2 <- expect_no_error(to_function(testdf, span, testops))
  expect_true(is.function(res2))
  expect_equal(res2(0:10), res(0:10))
})
