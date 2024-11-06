




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
  expect_error()
})
