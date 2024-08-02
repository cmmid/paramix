
#' @title Parameter Calculation Comparison Summary
#'
#' @description
#' Implements several approaches to computing partition-aggregated parameters,
#' then tables them up for convenient plotting.
#'
#'
#' @inheritParams blend
#'
#' @param resolution the number of points to calculate for the underlying
#' `f_param` function.
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
#' # from Levin et al 2020 https://doi.org/10.1007/s10654-020-00698-1
#' f_param <- function(age_in_years) {
#'   scaled <- exp(-7.56 + 0.121 * age_in_years)
#'   scaled / (100 + scaled)
#' }
#'
#' densities <- data.frame(
#'   from = 0:100,
#'   weight = c(rep(1, 66), exp(-0.075 * 1:35))
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
  f_param, densities, model_partition, ...,
  resolution = diff(range(model_partition)) + 1L
) {

  partition <- make_partition(model_partition, open_partition = c(FALSE, FALSE))

  alembic_dt <- alembic(
    f_param, densities, partition,
    seq(partition[1], tail(partition, 1), length.out = resolution)
  )

  plot_dt <- data.table::data.table(
    x = head(
      seq(partition[1], tail(partition, 1), length.out = resolution), -1
    )
  )[, f_val := f_param(x)]

  plot_dt[,
    model_category := findInterval(x, model_partition, all.inside = TRUE)
  ][alembic_dt, on = .(x = new_from), density := density]

  blended <- blend(alembic_dt)

  plot_dt[, c("f_mid", "f_mean", "mean_f", "wm_f") := .(
    f_param(mean(x)),
    f_param(weighted.mean(x, density)),
    weighted.mean(f_param(x), density),
    blended[.GRP, value]
  ), by = model_category]

  plot_dt$density <- NULL

  return(melt.data.table(
    plot_dt,
    id.vars = c("model_category", "x"), variable.name = "method"
  ))

}

#' @title Convenient Distillation Comparison Summary
#'
#' @description
#' Calculates the outcomes for various distillation assumptions.
#'
#' @param model_outcomes_dt a data.table (or convertable to such) with columns
#' `from` and `value`
#'
#' @param model_upper the upper limit of the last partition
#'
#'
#' @export
distill_summary <- function(
  model_outcomes_dt,
  density_dt,
  mapping_dt
) {
  setDT(model_outcomes_dt)
  model_partitions <- c(
    model_outcomes_dt[, unique(model_from)],
    mapping_dt[, max(new_from)]
  )

  density_dt <- density_dt[, {
    new_from <- mapping_dt$new_from[
      findInterval(from, mapping_dt$new_from, rightmost.closed = FALSE)
    ]
    .(new_from, weight)
  }][, .(weight = sum(weight)), by = new_from]

  return(rbind(
    # approach 1: all outcomes at mean age
    model_outcomes_dt[order(model_from), .(
      partition = (head(model_partitions, -1) + tail(model_partitions, -1)) / 2,
      value, method = "mean_partition"
    )],

    # approach 2: outcomes spread uniformly within group
    model_outcomes_dt[
      mapping_dt, on = .(model_from), allow.cartesian = TRUE
    ][, {
      parts <- new_from
      .(partition = parts, value = value / length(parts))
    }, by = model_from][, .(
      partition, value, method = "uniform_model")
    ],

    # TODO need to aggregate density_dt to new_from

    # approach 3: proportionally to age distribution within the group
    density_dt[
      mapping_dt, on = .(new_from)
    ][
      model_outcomes_dt, on = .(model_from)
    ][, .(
        partition = new_from, value = value * weight / sum(weight)
      ),
      by = .(model_from)
    ][, .(partition, value, method = "proportional_density")],

    # approach 4: proportionally to age *and* relative mortality rates
    setnames(
      distill(model_outcomes_dt, mapping_dt)[, method := "alembic_weighted"],
      "new_from", "partition"
    )
  ))

}
