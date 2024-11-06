

check_not_numeric <- function(var) {
  if (!is.numeric(var)) {
    stop(sprintf(
      "`%s` is not numeric", toString(var)
    ))
  }
  if (any(is.na(var))) {
    stop(sprintf(
      "`%s` contains NA/NaN values", toString(var)
    ))
  }
}

check_sorted <- function(var) {
  if (is.unsorted(var)) {
    warning(sprintf(
      "`%s` is not sorted: %s", deparse(substitute(var)), toString(var)
    ))
    return(sort(var))
  } else {
    return(var)
  }
}

check_unique <- function(var) {
  ret <- unique(var)
  if (length(ret) != length(var)) {
    warning(sprintf(
      "`%s` is not unique: %s", deparse(substitute(var)), toString(var)
    ))
  }
  return(ret)
}

check_bound <- function(vlen, var, res, what) {
  wtf <- switch(
    what, lower = utils::head, upper = utils::tail
  )

  if (vlen) {
    if (wtf(var, 1) != wtf(res, 1)) {
      return(sprintf(
        "`%s` %s bound does not match mixture: %s vs %s",
        deparse(substitute(var)), what, toString(var[1]), toString(res[1])
      ))
    }
  } else invisible()
}

#' @title Create a Least-Common-Interval Partition
#'
#' @description
#' Internal utility method for creating partitions, possibly from multiple
#' distinct partitions. Validates inputs.
#'
#' @param m_part the model partition
#'
#' @param o_part the output partition
#'
#' @return a sorted numeric vector with unique values
#' @keywords internal
make_partition <- function(
  model_partition = numeric(0), output_partition = numeric(0)
) {

  check_not_numeric(model_partition)
  check_not_numeric(output_partition)

  mlen <- length(model_partition)
  olen <- length(output_partition)

  if (mlen + olen == 0L) {
    stop("`model_partition` and `output_partition` cannot both be empty.")
  }

  model_partition <- check_sorted(model_partition)
  model_partition <- check_unique(model_partition)
  output_partition <- check_sorted(output_partition)
  output_partition <- check_unique(output_partition)

  res <- unique(sort(c(model_partition, output_partition)))

  checked_bounds <- c(
    check_bound(mlen, model_partition, res, "lower"),
    check_bound(mlen, model_partition, res, "upper"),
    check_bound(olen, output_partition, res, "lower"),
    check_bound(olen, output_partition, res, "upper")
  )

  if (!is.null(checked_bounds)) {
    checked_bounds <- c(
      "Partition boundaries must match. If these partitions are correct, for example if the output partitions drop results from the total simulated population, consider supplying a dummy partition and then discarding it.",
      checked_bounds
    )
    stop(paste(checked_bounds, collapse = "\n\t"))
  }


  return(res)
}

#' @title Internal Conversion of Data to Function
#'
#' @param x a function or the single argument version of `x` in
#' [xy.coords()] (as per [approxfun()] or [splinefun()] inputs)
#'
#' @param interp_opts if `x` is function, ignored. Otherwise,
#' an interpolating function and its arguments.
#'
#' @return a function
#' @keywords internal
to_function <- function(x, interp_opts, lb, ub) {
  if (is.function(x)) {
    bcheck <- c(x(lb), x(ub))
    if (any(is.na(bcheck))) {
      stop(sprintf())
    }
    return(x)
  } else {
    callargs <- interp_opts
    callfun <- interp_opts$fun
    callargs$fun <- NULL
    callargs$x <- x
    return(do.call(callfun, args = callargs))
  }
}
