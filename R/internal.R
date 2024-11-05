
#' @title Create a Least-Common-Interval Partition
#'
#' @description
#' Internal utility method for creating partitions, possibly from multiple
#' distinct partitions. Validates inputs.
#'
#' @param ... any number of numeric vectors
#'
#' @return a sorted numeric vector with unique values
#' @keywords internal
make_partition <- function(
  model_part, output_part
) {
  # want an error 1) in the parent context,
  # 2) using variable names passed from that context
  # (assuming they are generally just the argument names)

  partition <- suppressWarnings(as.numeric(c(model_part, output_part)))

  stopifnot(
    "Must provide some partition points." = length(partition) != 0,
    "May not provide any `NA` values for partition." = !any(is.na(partition))
  )

  return(unique(sort(partition)))
}
