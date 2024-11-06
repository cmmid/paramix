
utils::globalVariables(c(
  "weight", "density", "from", "model_from", "model_fraction"
))

#' @title Compose Parameter & Density Functions
#'
#' @param f_param a function; the parameter function, varying with the aggregate
#'
#' @param f_pop a function; the density function, varying with the aggregate
#'
#' @return a new function, f(x) = f_param(x)*f_density(x)
#'
#' @keywords internal
make_weight <- function(f_param, f_pop) {
  return(function(x) f_param(x) * f_pop(x))
}

#' @title Create the Blending and Distilling Object
#'
#' @param f_param a function, `f(x)` which transforms the feature (e.g. age),
#' to yield the parameter values. Alternatively, a `data.frame` where the first
#' column is the feature and the second is the parameter; see
#' [xy.coords()] for details. If the latter, combined with `pars_interp_opts`,
#' and defaulting to spline interpolation.
#'
#' @param f_pop like `f_param`, either a density function (though it does
#' not have to integrate to 1 like a pdf) or a `data.frame` of values. If the
#' latter, it is treated as a series of populations within intervals, and
#' then interpolated with `pop_interp_opts` to create a density function.
#'
#' @param model_partition a numeric vector of cut points, which define the
#' partitioning that will be used in the model
#'
#' @param output_partition the partition of the underlying feature
#'
#' @param pars_interp_opts a list, minimally with an element `fun`,
#' corresponding to an interpolation function. Defaults to [splinefun()]
#' "natural" interpolation.
#'
#' @param pop_interp_opts ibid, but for density. Defaults to [approxfun()]
#' "constant" interpolation.
#'
#' @return a `data.table` with columns `model_part`, `out_part`, `weight` and
#' `relpop`. The first two columns identify which partitions, for both the model
#' and output, the other values are associated with; the combination of
#' `model_part` and `out_part` forms a unique identifier, but individually they
#' may appear multiple times. which maps fractions of the original model partitions
#' to the desired partitions, according to underlying relative outcome rates and
#' densities
#'
#' @examples
#' ifr_levin <- function(age_in_years) {
#'   (10^(-3.27 + 0.0524 * age_in_years))/100
#' }
#' age_limits <- c(seq(0, 69, by = 5), 70, 80, 100)
#' age_pyramid <- data.frame(
#'   from = 0:100, weight = ifelse(0:100 < 65, 1, .99^(0:100-64))
#' ) # flat age distribution, then 1% annual deaths
#' ifr_alembic <- alembic(ifr_levin, age_pyramid, age_limits, 0:100)
#'
#' @importFrom utils head tail
#' @importFrom stats integrate
#' @import data.table
#' @export
alembic <- function(
    f_param,
    f_pop,
    model_partition,
    output_partition,
    pars_interp_opts = list(
      fun = stats::splinefun, method = "natural"
    ),
    pop_interp_opts = list(
      fun = stats::approxfun, method = "constant",
      yleft = 0, yright = 0
    )
) {

  overall_partition <- make_partition(model_partition, output_partition)

  lowers <- head(overall_partition, -1)
  uppers <- tail(overall_partition, -1)

  f_param <- to_function(f_param, pars_interp_opts)
  f_pop <- to_function(f_pop, pop_interp_opts)

  f <- make_weight(f_param, f_pop)

  ret <- data.table(from = lowers)[, {
    model_part <- findInterval(from, model_partition)
    out_part <- findInterval(from, output_partition)
    weight <- numeric(length(from))
    relpop <- numeric(length(from))
    for (i in seq_along(lowers)) {
      weight[i] <- integrate(
        f, lowers[i], uppers[i], subdivisions = 1000L
      )$value
      relpop[i] <- integrate(
        f_pop, lowers[i], uppers[i], subdivisions = 1000L
      )$value
    }
    .(
      model_from = model_partition[model_part],
      new_from = output_partition[out_part],
      weight = weight, relpop = relpop
    )
  }]

  setattr(ret, "f_param", f_param)
  setattr(ret, "f_pop", f_pop)

  return(ret)

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
#'   (10^(-3.27 + 0.0524 * age_in_years))/100
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

#' @title Distill Outcomes
#'
#' @description
#' `distill` takes a low-age resolution outcome, for example deaths,
#' and proportionally distributes that outcome into a higher age resolution for
#' use in subsequent analyses like years-life-lost style calculations.
#'
#' @inheritParams blend
#'
#' @param outcomes_dt a long-format `data.frame` with a column either named
#' `from` or `model_from` and a column `value` (other columns will be silently
#' ignored)
#'
#' @details
#' When the `value` column is re-calculated, note that it will aggregate all
#' matching `from` / `model_from` rows in `outcomes_dt`. If you need to group
#' by other features in your input data (e.g. if you need to distill outcomes
#' across multiple simulation outputs), that has to be done outside of any call
#' to `distill()`.
#'
#'
#' @return a `data.frame`, with `new_from` and recalculated `value` column
#'
#' @import data.table
#' @export
distill <- function(
  alembic_dt, outcomes_dt
) {

  setnames(setDT(outcomes_dt), "from", "model_from", skip_absent = TRUE)

  mapping <- alembic_dt[, .(
    new_from, model_fraction = weight / sum(weight)
    ), by = model_from
  ]

  return(outcomes_dt[mapping, on = .(model_from)][,
    .(value = sum(value * model_fraction)),
    by = new_from
  ])

}
