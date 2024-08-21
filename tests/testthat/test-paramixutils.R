test_that("`make_partition` complains about invalid input.", {
  # no arguments
  expect_error(make_partition())
  # non-numeric argument
  expect_error(make_partition("A"))
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
