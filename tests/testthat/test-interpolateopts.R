
test_that("`interpolate_opts` errors on invalid inputs.", {
  notfun <- 5
  noargsfun <- function() print()
  okayfun <- function(x) print(x)
  # must provide a function
  expect_error(interpolate_opts(notfun))
  # ... with arguments
  expect_error(interpolate_opts(noargsfun))
  # kind must match point or integral
  expect_error(interpolate_opts(okayfun, "invalidkind"))
  # additional args must be named
  expect_error(interpolate_opts(okayfun, "point", 5))
  # additional args must be in signature of fun
  expect_error(interpolate_opts(okayfun, y = 5))
  # ... except, not the first arg
  expect_error(interpolate_opts(okayfun, x = 5))
})

test_that("`interpolate_opts` works for valid inputs.", {
  okayfun <- function(x) print(x)
  complexfun <- function(x, other) print(x, other)
  usekindfun <- function(x, kind) print(x, kind)
  otherval <- 5

  expect_no_error(interpolate_opts(okayfun))
  expect_no_error(interpolate_opts(okayfun, kind = "integral"))
  expect_no_error(interpolate_opts(complexfun, kind = "integral"))
  expect_no_error(interpolate_opts(complexfun, kind = "integral", other = otherval))

  res <- expect_no_error(interpolate_opts(complexfun, other = otherval))
  expect_equal(res$fun, complexfun)
  expect_equal(res$other, otherval)
  expect_false(res$`.usekind`)

  res <- expect_no_error(interpolate_opts(usekindfun))
  expect_equal(res$fun, usekindfun)
  expect_true(res$`.usekind`)

})
