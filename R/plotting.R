
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

  plot_dt <- data.table::data.table(
    x = seq(partition[1], tail(partition, 1), length.out = resolution)
  )[, f_val := f_param(x) ]

  plot_dt[, model_category := findInterval(x, model_partition, all.inside = TRUE)]

  blended <- blend(f_param, densities, partition, ...)

  plot_dt[, c("f_mean", "mean_f", "wm_f") := .(
    f_param(mean(x)), mean(f_param(x)), blended[.GRP]
  ), by = model_category]

  data.table::melt.data.table(plot_dt, id.vars = c("model_category", "x"), variable.factor = FALSE)

}
