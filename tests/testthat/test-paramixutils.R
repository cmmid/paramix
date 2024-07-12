test_that("`make_partition` complains about invalid input.", {
  # no arguments
  expect_error(make_partition())
  # non-numeric argument
  expect_error(make_partition("A"))
  # non-parsable open_partition
  expect_error(make_partition(1:5, open_partition = "A"))
  # empty open_partition
  expect_error(make_partition(1:5, open_partition = c()))
  # too much open_partition
  expect_error(make_partition(1:5, open_partition = rep(FALSE, 3)))
})

test_that("`make_partition` properly combines input.", {
  model_part <- c(0, 5, 10); new_part <- do.call(seq, as.list(range(model_part)))
  expect_equal(make_partition(model_part, open_partition = FALSE), model_part)
  expect_equal(make_partition(new_part, open_partition = FALSE), new_part)
  expect_equal(make_partition(model_part, new_part, open_partition = FALSE), new_part)
})

test_that("`make_partition` output is sorted and unique.", {
  model_part <- c(0, 5, 10); new_part <- do.call(seq, as.list(range(model_part)))
  example <- make_partition(new_part, model_part, open_partition = TRUE)
  expect_equal(example, example[order(example)])
  expect_equal(example, unique(example))
})

test_that("`make_partition` handles open partitions.", {
  model_part <- c(0, 5, 10); new_part <- do.call(seq, as.list(range(model_part)))
  example_lo <- make_partition(new_part, model_part, open_partition = c(TRUE, FALSE))
  example_ro <- make_partition(new_part, model_part, open_partition = c(FALSE, TRUE))
  example_b1 <- make_partition(new_part, model_part, open_partition = TRUE)
  example_b2 <- make_partition(new_part, model_part, open_partition = rep(TRUE, 2))
  example_n1 <- make_partition(new_part, model_part, open_partition = TRUE)
  example_n2 <- make_partition(new_part, model_part, open_partition = rep(TRUE, 2))
  expect_equal(example, example[order(example)])
  expect_equal(example, unique(example))
})
