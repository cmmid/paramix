

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
#' @param model_partition the model partition
#'
#' @param output_partition the output partition
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

sanitize_data <- function(x, bounds) {
  if (dim(x)[2] < 2) {
    stop(sprintf("data must have at least two columns."))
  } else if (dim(x)[2] != 2) {
    warning(sprintf(
      "data has more than two columns; only using %s",
      toString(names(x)[1:2])
    ))
  }
  x <- x[, c(1, 2)]
  if (!is.numeric(x[[1]]) || !is.numeric(x[[2]])) {
    stop(sprintf(
      "first two data columns must satisfy is.numeric; got classes %s, %s",
      toString(class(x[[1]])),
      toString(class(x[[2]]))
    ))
  }
  if (is.unsorted(x[[1]])) {
    warning(sprintf(
      "data column 1 is unsorted: %s; will be sorted before interpolation",
      toString(x[[1]])
    ))
    x <- x[order(x[[1]]), ]
  }
  if (bounds[1] < x[[1]][1]) {
    warning(sprintf(
      "data may not support bounds: lower bound %s < first column 1 entry %s",
      bounds[1], x[[1]][1]
    ))
  }
  if (bounds[2] > tail(x[[1]], 1)) {
    warning(sprintf(
      "data may not support bounds: upper bound %s > last column 1 entry %s",
      bounds[1], x[[1]][1]
    ))
  }
  if (length(unique(x[[1]])) != dim(x)[1]) {
    warning(sprintf(
      "data column 1 is not unique: %s; ensure interpolation function can handle this.",
      toString(x[[1]])
    ))
  }
  return(x)
}

#' @title Internal Conversion of Data to Function
#'
#' @param x a function or the single argument version of `x` in
#' [xy.coords()] (as per [approxfun()] or [splinefun()] inputs). Pass through
#' from user input, must be checked
#'
#' @param bounds numeric vector, length 2: the partition lower bound; not checked,
#' result of `range(make_partition(...))`.
#'
#' @param interp_opts if `x` is function, ignored. Otherwise,
#' an interpolating function and its arguments.
#'
#' @return a function
#' @keywords internal
to_function <- function(x, bounds, interp_opts) {
  if (is.function(x)) {
    bcheck <- c(x(bounds[1]), x(bounds[2]))
    if (any(is.na(bcheck))) {
      stop(c(sprintf(
        "The (lower, upper) bounds of the mixing partition evaluate to (%s, %s); the function %s must be defined on the whole partition.", deparse(substitute(x)), toString(bcheck[1]), toString(bcheck[2])
      )))
    }
    return(x)
  } else if (is.data.frame(x)) {
    x <- sanitize_data(x, bounds)

    callargs <- do.call(interpolate_opts, interp_opts)
    callfun <- interp_opts$fun
    callargs$fun <- NULL
    if (!callargs$.usekind) {
      kind <- callargs$kind
      callargs$kind <- NULL
      if (kind == "integral") {
        if (tail(x[[2]], 1) != 0) {
          stop(sprintf(
            "for integral data, the final entry should be 0; got instead: %s",
            toString(tail(x[[2]], 1))
          ))
        }
        ws <- diff(x[[1]])
        x[[2]] <- c(x[[2]]/ws, 0)
      }
    }
    callargs$.usekind <- NULL
    callargs$x <- x
    return(do.call(callfun, args = callargs))
  } else {
    stop(sprintf("%s must be a function or data.frame; received %s.", deparse(substitute(x)), class(x)))
  }
}

#' @title Compose Parameter & Density Functions
#'
#' @description
#' Purely internal, called after `to_function`, so no direct user arguments.#'
#'
#' @param f_param a function; the parameter function, varying with the aggregate
#'
#' @param f_pop a function; the density function, varying with the aggregate
#'
#' @return a new function, f(x) = f_param(x)*f_pop(x)
#'
#' @keywords internal
make_weight <- function(f_param, f_pop) {
  return(function(x) f_param(x) * f_pop(x))
}
