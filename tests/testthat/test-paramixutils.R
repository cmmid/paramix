
# make_partition: takes a model and output partition
# should error if those are arguments are non-numeric or empty
# should warn if those arguments are out of order
# should warn if those arguments don't share end points (after assuring order)
# return value should be: numeric, sorted, unique, contain all of the
# (valid) inputs *except possibly endpoints* (when they don't match)

test_that("`make_partition` complains about invalid input.", {
  # no arguments
  expect_error(make_partition())
  # non-numeric argument
  expect_error(make_partition("A"))
})

test_that(
  "`make_partition` warns when partition endpoints do not match, and expands.", {
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
