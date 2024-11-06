
#' `make_partition`: takes a model and output partition
#' returns the combined partition. Though internal, used directly with user
#' inputs, so also does type checking
#'
#' Test that:
#'  - error if arguments are non-numeric or empty
#'  - warn if arguments unsorted and/or non-unique (per argument, allowed across args)
#'  - stop if arguments don't share endpoints (after sorting)
#'  - expand when arguments don't share endpoints
#'  - return value has all the input values, *except* possibly endpoints
#'  - return value is sorted, unique

test_that("Error on non-numeric, missing, or empty partitions.", {
  # no arguments
  expect_error(make_partition())
  # empty arguments
  expect_error(make_partition(c(), c()))
  # non-numeric argument
  expect_error(make_partition("A"))
})

test_that(
  "`make_partition` errors when partition endpoints do not match.", {
  one <- c(1, 2)
  two <- c(2, 3)
  expect_error(make_partition(c(1, 2), c(2, 3)))
})

test_that(
  "`make_partition` warns partitions are not in order.", {
  expect_warning(make_partition(1:3, 3:1))
})

test_that(
  "`make_partition` warns about non-unique partitions.", {
  expect_warning(make_partition(1:3, c(1, 1:3)))
})

test_that(
  "`make_partition` contains all input partitions.", {
  mp <- c(1, 5)
  op <- 1:5
  mixp <- make_partition(mp, op)
  expect_contains(mixp, mp)
  expect_contains(mixp, op)
})

test_that(
  "`make_partition` results in sorted, unique values.", {
  mp <- c(1, 1, 5)
  op <- 5:1
  mixp <- expect_warning(make_partition(mp, op))
  expect_true(!is.unsorted(mixp))
  expect_equal(length(unique(mixp)), length(mixp))
})
