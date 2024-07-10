
#' @title Convenient Parameter Comparison Summary
#'
#' @inheritParams blend
#'
#' @param resolution the number of points to calculate for the underlying
#' `f_param` line
#'
#' @export
parameter_summary <- function(
  f_param, densities, model_partition, ..., resolution = diff(range(model_partition)) + 1L
) {
  partition <- make_partition(model_partition, open_partition = c(FALSE, FALSE))

  plot_dt <- data.table::data.table(
    x = seq(partition[1], tail(partition, 1), length.out = resolution)
  )[, f_val := f_param(x) ]

  plot_dt[, model_category := findInterval(x, model_partition)]
  plot_dt[, f_mean := f_param(mean(x)), by = model_category]
  plot_dt[, mean_f := mean(f_param(x)), by = model_category]

  blended <- blend(f_param, densities, model_partition, ...)
  plot_dt[, blend_f := blended[.GRP], by = model_category]

  data.table::melt.data.table(plot_dt, id.vars = c("model_category", "x"))

}
