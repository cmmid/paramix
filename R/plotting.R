
#' @title Parameter Calculation Comparison Summary
#'
#' @description
#' Implements several approaches to computing partition-aggregated parameters,
#' then tables them up for convenient plotting.
#'
#' @inheritParams alembic
#'
#' @param resolution the number of points to calculate for the underlying
#' `f_param` function. The default 101 points means 100 partitions.
#'
#' @return a `data.table`, columns:
#'  - `model_category`, a integer corresponding to which of the intervals of
#'  `model_partition` the `x` value is in
#'  - `x`, a numeric series from the first to last elements of `model_partition`
#'  with length `resolution`
#'  - `method`, a factor with levels:
#'    * `f_val`: `f_param(x)`
#'    * `f_mid`: `f_param(x_mid)`, where `x_mid` is the midpoint x of the
#'    `model_category`
#'    * `f_mean`: `f_param(weighted.mean(x, w))`, where `w` defined by
#'    `densities` and `model_category`
#'    * `mean_f`: `weighted.mean(f_param(x), w)`, same as previous
#'    * `wm_f`: the result as if having used `paramix::blend()`; this should be
#'    very similar to `mean_f`, though will be slightly different since `blend`
#'    uses `integrate()`
#'
#' @examples
#' # COVID IFR from Levin et al 2020 https://doi.org/10.1007/s10654-020-00698-1
#' f_param <- function(age_in_years) {
#'   (10^(-3.27 + 0.0524 * age_in_years))/100
#' }
#'
#' densities <- data.frame(
#'   from = 0:101,
#'   weight = c(rep(1, 66), exp(-0.075 * 1:35), 0)
#' )
#'
#' model_partition <- c(0, 5, 20, 65, 101)
#'
#' ps_dt <- parameter_summary(f_param, densities, model_partition)
#' ps_dt
#'
#' @examplesIf require(ggplot2)
#' ggplot(ps_dt) + aes(x, y = value, color = method) +
#'   geom_line(data = \(dt) subset(dt, method == "f_val")) +
#'   geom_step(data = \(dt) subset(dt, method != "f_val")) +
#'   theme_bw() + theme(
#'     legend.position = "inside", legend.position.inside = c(0.05, 0.95),
#'     legend.justification = c(0, 1)
#'   ) + scale_color_discrete(
#'     "Method", labels = c(
#'       f_val = "f(x)", f_mid = "f(mid(x))", f_mean = "f(E[x])",
#'       mean_f = "discrete E[f(x)]", wm_f = "integrated E[f(x)]"
#'     )
#'   ) +
#'   scale_x_continuous("Age", breaks = seq(0, 100, by = 10)) +
#'   scale_y_log10("IFR", breaks = 10^c(-6, -4, -2, 0), limits = 10^c(-6, 0))
#'
#' @importFrom data.table melt.data.table
#' @export
parameter_summary <- function(
  f_param, f_pop, model_partition,
  resolution = 101L
) {

  output_partition <- seq(model_partition[1], tail(model_partition, 1), length.out = resolution)

  alembic_dt <- alembic(
    f_param, f_pop, model_partition, output_partition
  )

  plot_dt <- data.table::data.table(
    x = output_partition
  )[, f_val := attr(alembic_dt, "f_param")(x)]

  plot_dt[,
    model_category := findInterval(x, model_partition, all.inside = TRUE)
  ][alembic_dt, on = .(x = output_partition), relpop := relpop]
  plot_dt[is.na(relpop), relpop := 0]

  blended <- blend(alembic_dt)

  plot_dt[, c("f_mid", "f_mean", "mean_f", "wm_f") := .(
    f_param(mean(x)),
    f_param(weighted.mean(x, relpop)),
    weighted.mean(f_param(x), relpop),
    blended[.GRP, value]
  ), by = model_category]

  plot_dt$relpop <- NULL

  return(melt.data.table(
    plot_dt,
    id.vars = c("model_category", "x"), variable.name = "method"
  ))

}

utils::globalVariables(c(
  "f_val", "x", "model_category", "new_from", "density", "value"
))

#' @title Distillation Calculation Comparison Summary
#'
#' @description
#' Implements several approaches to imputing higher resolution outcomes, then
#' tables them up for convenient plotting.
#'
#' @inheritParams distill
#'
#' @return a `data.table`, columns:
#'  - `partition`, the feature point corresponding to the value
#'  - `value`, the translated `outcomes_dt$value`
#'  - `method`, a factor with levels indicating how feature points are selected,
#'  and how `value` is weighted to those features:
#'    * `f_mid`: features at the `alembic_dt` outcome partitions, each with
#'    value corresponding to the total value of the corresponding model
#'    partition, divided by the number of outcome partitions in that model
#'    partition
#'    * `f_mean`: ...
#'    * `mean_f`: ...
#'    * `wm_f`: the
#'
#' @examplesIf require(data.table)
#'
#' library(data.table)
#' f_param <- function(age_in_years) {
#'   (10^(-3.27 + 0.0524 * age_in_years))/100
#' }
#'
#' model_partition <- c(0, 5, 20, 65, 101)
#' density_dt <- data.table(
#'   from = 0:101, weight = c(rep(1, 66), exp(-0.075 * 1:35), 0)
#' )
#' alembic_dt <- alembic(
#'   f_param, density_dt, model_partition, seq(0, 101, by = 1L)
#' )
#'
#' # for simplicity, assume a uniform force-of-infection across ages =>
#' # infections proportion to population density.
#' model_outcomes_dt <- density_dt[from != max(from), .(value = sum(f_param(from) * weight)),
#'   by = .(model_from = model_partition[findInterval(from, model_partition)])
#' ]
#'
#' ds_dt <- distill_summary(alembic_dt, model_outcomes_dt)
#'
#' @import data.table
#' @importFrom stats weighted.mean
#' @export
distill_summary <- function(
  alembic_dt, outcomes_dt, groupcol = names(outcomes_dt)[1]
) {
  setnames(setDT(outcomes_dt), groupcol, "model_partition")

  distilled_dt <- alembic_dt[outcomes_dt, on = .(model_partition)]
  distilled_dt[, weigh_at := output_partition + c(diff(output_partition) / 2, 0)]

  return(rbind(
    # approach 1: all outcomes at mean value
    distilled_dt[, .(
        partition = weighted.mean(weigh_at, relpop),
        value = value[1], method = "f_mean"
      ), by = model_partition
    ][, .SD, .SDcols = -c("model_partition")],

    # approach 2: outcomes spread uniformly within group
    distilled_dt[, .(
      partition = output_partition, value = value / .N, method = "f_mid"
    ), by = model_partition][, .SD, .SDcols = -c("model_partition")],

    # TODO need to aggregate density_dt to new_from

    # approach 3: proportionally to age distribution within the group
    distilled_dt[, .(
      partition = output_partition, value = value * relpop / sum(relpop),
      method = "mean_f"
    ), by = model_partition][, .SD, .SDcols = -c("model_partition")],

    # approach 4: proportionally to age *and* relative mortality rates
    setnames(
      distill(alembic_dt, outcomes_dt)[, method := "wm_f"],
      "output_partition", "partition"
    )
  )[, method := factor(method)])

}

utils::globalVariables(c("model_from", "weigh_at", "method"))
