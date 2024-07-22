
#' @title Convenient Parameter Comparison Summary
#'
#' @inheritParams blend
#'
#' @param resolution the number of points to calculate for the underlying
#' `f_param` line
#'
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
    x = seq(partition[1], tail(partition, 1), length.out = resolution)
  )[, f_val := f_param(x)]

  plot_dt[,
    model_category := findInterval(x, model_partition, all.inside = TRUE)
  ]

  blended <- blend(alembic_dt)

  plot_dt[, c("f_mean", "mean_f", "wm_f") := .(
    f_param(mean(x)), mean(f_param(x)), blended[.GRP, value]
  ), by = model_category]

  data.table::melt.data.table(
    plot_dt, id.vars = c("model_category", "x"),
    variable.factor = FALSE, variable.name = "method"
  )

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
