
#' @title Create a Least-Common-Interval Partition
#'
#' @description
#' Internal utility method for creating partitions, possibly from multiple
#' distinct partitions. Validates inputs and potential creates open ends.
#'
#' @param ... any number of numeric vectors
#'
#' @return a sorted numeric vector with unique values
#'
#' @examples
#' paramix:::make_partition(1:5)
#' paramix:::make_partition(5:1, 4, 6)
#'
make_partition <- function(
  ...
) {
  partition <- suppressWarnings(as.numeric(c(...)))

  stopifnot(
    "Must provide some partition points." = length(partition) != 0,
    "May not provide any `NA` values for partition." = !any(is.na(partition))
  )

  return(unique(sort(partition)))
}


#' @param x a function or the single argument version of `x` in
#' [xy.coords()] (as per [approxfun()] or [splinefun()] inputs)
#'
#' @param interp_opts if `x` is function, ignored. Otherwise,
#' an interpolating function and its arguments.
#'
#' @return a function
to_function <- function(x, interp_opts) {
  if (is.function(x)) {
    return(x)
  } else {
    callargs <- interp_opts
    callfun <- interp_opts$fun
    callargs$fun <- NULL
    callargs$x <- x
    return(do.call(callfun, args = callargs))
  }
}

#' @title Compose Parameter & Density Functions
make_weight <- function(f_param, f_dense) {
  return(function(x) f_param(x) * f_dense(x))
}

#' @title Blend Parameters
#'
#' @description
#' `blend` extracts aggregate parameters from an `alembic` object.
#'
#' @param alembic_dt an [alembic()] return value
#'
#' @return a `data.table` of with two columns: `model_from` (partition lower
#' bounds) and `value` (parameter values for those partitions)
#'
#' @examples
#' ifr_levin <- function(age_in_years) {
#'   scaled <- exp(-7.56 + 0.121 * age_in_years)
#'   scaled / (100 + scaled)
#' }
#' age_limits <- c(seq(0, 69, by = 5), 70, 80, 100)
#' age_pyramid <- data.frame(
#'   from = 0:99, weight = ifelse(0:99 < 65, 1, .99^(0:99-64))
#' ) # flat age distribution, then 1% annual deaths
#' alembic_dt <- alembic(ifr_levin, age_pyramid, age_limits, 0:100)
#'
#' ifr_blend <- blend(alembic_dt)
#' # the actual function
#' plot(
#'   60:100, ifr_levin(60:100),
#'   xlab = "age (years)", ylab = "IFR", type = "l"
#' )
#' # the properly aggregated blocks
#' lines(
#'   age_limits, c(ifr_blend$value, tail(ifr_blend$value, 1)),
#'   type = "s", col = "dodgerblue"
#' )
#' # naively aggregated blocks
#' ifr_naive <- ifr_levin(head(age_limits, -1) + diff(age_limits)/2)
#' lines(
#'   age_limits, c(ifr_naive, tail(ifr_naive, 1)),
#'   type = "s", col = "firebrick"
#' )
#' # properly aggregated, but not accounting for age distribution
#' bad_alembic_dt <- alembic(
#'   ifr_levin, within(age_pyramid, weight <- 1), age_limits, 0:100
#' )
#' ifr_unif <- blend(bad_alembic_dt)
#' lines(
#'   age_limits, c(ifr_unif$value, tail(ifr_unif$value, 1)),
#'   type = "s", col = "darkgreen"
#' )
#' @export
blend <- function(
  alembic_dt
) {
  return(alembic_dt[, .(value = sum(weight) / sum(density)), by = model_from])
}

#' @title Create the Blending and Distilling Object
#'
#' @param f_param a function, `f(x)` which transforms the feature (e.g. age),
#' and yields the parameter value. Alternatively, a `data.frame` where the first
#' column is the feature (x) and the second is the parameter (y); see
#' [xy.coords()] for details. If the latter, combined with `pars_interp_opts`,
#' and defaulting to spline interpolation.
#'
#' @param f_dense like `f_param`, either a density function (though it does
#' not have to integrate to 1 like a pdf) or a `data.frame` of values. If the
#' latter, combined with `dens_interp_opts` and defaulting to constant density
#' from each x to the next.
#'
#' @param model_partition a numeric vector of cut points, which define the
#' partitioning that will be used in the model
#'
#' @param new_partition the partition of the underlying feature
#'
#' @param pars_interp_opts a list, minimally with an element `fun`,
#' corresponding to an interpolation function. Defaults to [splinefun()]
#' "natural" interpolation
#'
#' @param dens_interp_opts ibid, but for density. Defaults to [approxfun()]
#' "constant" interpolation
#'
#' @return a `data.frame` which maps fractions of the original model partitions
#' to the desired partitions, according to underlying relative outcome rates and
#' densities
#'
#' @examples
#' ifr_levin <- function(age_in_years) {
#'   scaled <- exp(-7.56 + 0.121 * age_in_years)
#'   scaled / (100 + scaled)
#' }
#' age_limits <- c(seq(0, 69, by = 5), 70, 80, 100)
#' age_pyramid <- data.frame(
#'   from = 0:100, weight = ifelse(0:99 < 65, 1, .99^(0:100-64))
#' ) # flat age distribution, then 1% annual deaths
#' ifr_alembic <- alembic(ifr_levin, age_pyramid, age_limits, 0:100)
#'
#' @export
alembic <- function(
  f_param,
  f_dense,
  model_partition,
  new_partition,
  pars_interp_opts = list(
    fun = stats::splinefun, method = "natural"
  ),
  dens_interp_opts = list(
    fun = stats::approxfun, method = "constant",
    yleft = 0, yright = 0
  )
) {

  overall_partition <- make_partition(model_partition, new_partition)

  lowers <- head(overall_partition, -1)
  uppers <- tail(overall_partition, -1)

  f_param <- to_function(f_param, pars_interp_opts)
  f_dense <- to_function(f_dense, dens_interp_opts)

  f <- make_weight(f_param, f_dense)

  return(data.table::data.table(from = lowers)[, {
    model_part <- findInterval(from, model_partition)
    new_part <- findInterval(from, new_partition)
    weight <- numeric(length(from))
    dense <- numeric(length(from))
    for (i in seq_along(weight)) {
      weight[i] <- integrate(
        f, lowers[i], uppers[i], subdivisions = 1000L
      )$value
      dense[i] <- integrate(
        f_dense, lowers[i], uppers[i], subdivisions = 1000L
      )$value
    }
    .(
      model_from = model_partition[model_part],
      new_from = new_partition[new_part], weight = weight, density = dense
    )
  }])

}

#' @title Distill Outcomes
#'
#' @description
#' `distill` takes a low-age resolution outcome, for example deaths,
#' and proportionally distributes that outcome into a higher age resolution for
#' use in subsequent analyses like years-life-lost style calculations.
#'
#' @inheritParams blend
#'
#' @param outcomes a long-format `data.frame` with columns `model_from` and
#' `value`, optionally others that will be preserved
#'
#' @return a `data.frame` mirroring `outcomes`, but with
#'
#' @export
distill <- function(
  outcomes, alembic_dt
) {

  setnames(setDT(outcomes), "from", "model_from", skip_absent = TRUE)

  mapping <- alembic_dt[, .(
    new_from, model_fraction = weight / sum(weight)),
    by = model_from
  ]

  return(outcomes[mapping, on = .(model_from)][, .(
    value = sum(value * model_fraction)
  ), by = new_from])

}
