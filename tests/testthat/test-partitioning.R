
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
#'

test_that("Error on non-numeric, missing, or empty partitions.", {
  # no arguments
  expect_error(make_partition())
  # empty arguments
  expect_error(make_partition(c()))
  # non-numeric argument
  expect_error(make_partition("A"))
})

test_that(
  "`make_partition` errors when partition endpoints do not match.", {
  one <- c(1, 2)
  two <- c(2, 3)
  expect_warning(res <- make_partition(c(1, 2), c(2, 3)))
  expect_equal(head(res, 1) == min(one, two))
  expect_equal(tail(res, 1) == max(one, two))
  expect_equal(length(res) == 2)
})

test_that(
  "`make_partition` warns partitions are not in order.", {
})

test_that(
  "`make_partition` warns partitions are not in order.", {
})

test_that("`make_partition` properly combines input.", {
  model_part <- c(0, 5, 10); new_part <- do.call(seq, as.list(range(model_part)))
  expect_equal(make_partition(model_part), model_part)
  expect_equal(make_partition(new_part), new_part)
  expect_equal(make_partition(model_part, new_part, open_partition = FALSE), new_part)
})

test_that("`make_partition` output is sorted and unique.", {
  model_part <- c(0, 5, 10); new_part <- do.call(seq, as.list(range(model_part)))
  example <- make_partition(new_part, model_part, open_partition = TRUE)
  expect_equal(example, example[order(example)])
  expect_equal(example, unique(example))
})

test_that("`to_function` will properly error when forbidden negative inputs", {

})
