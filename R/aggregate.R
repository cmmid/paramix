
#' @title Create a Least-Common-Interval Partition
#'
#' @description
#' Internal utility method for creating partitions, possibly from multiple
#' distinct partitions. Validates inputs and potential creates open ends.
#'
#' @param ... any number of numeric vectors
#'
#' @param open_partition a value coerceable to logical vector; length 1 or 2
#'
#' @return a sorted numeric vector with unique values
#'
#' @examples
#' paramix:::make_partition(1:5, open_partition = FALSE)
#' paramix:::make_partition(5:1, 4, 6, open_partition = c(TRUE, FALSE))
#'
make_partition <- function(
  ..., open_partition
) {
  partition <- suppressWarnings(as.numeric(c(...)))

  stopifnot(
    "Must provide some partition points." = length(partition) != 0,
    "May not provide any `NA` values for partition." = !any(is.na(partition)),
    "Must indicate if partition is open." = !missing(open_partition)
  )

  open_partition <- as.logical(open_partition)

  stopifnot(
    "Open indicator must be length 1 or 2." = length(open_partition) %in% c(1, 2),
    "Must provide values interpretable as logical." = !any(is.na(open_partition))
  )

  if (length(open_partition) == 1) open_partition[2] <- open_partition[1]

  if (open_partition[1]) partition <- c(-Inf, partition)
  if (open_partition[2]) partition <- c(partition, Inf)

  return(unique(sort(partition)))
}

make_density <- function(densities, interpolater = approxfun, ...) {
  if (is.function(densities)) {
    return(densities)
  } else {
    return(interpolater(densities$from, densities$weight, ...))
  }
}

make_weight <- function(f_param, f_density) {
  return(function(x, ...) f_param(x, ...) * f_density(x))
}

#' @title Transform Parameter Function into Values by Partition
#'
#' @description
#' `blend` integrates a functional relationship between some model
#' feature, for example case fatality ratio, into static parameter
#' values for use in a lower-resolution partitioned compartmental model.
#'
#' @param f_param a function, `f(x, ...)` which operates on the feature
#' (e.g. age), and optionally additional arguments, and yields the parameter
#' value.
#'
#' @param model_partition a numeric vector of cut points, which define the partition
#'
#' @param densities a `data.frame` with numeric columns `from` and `weight`. The
#' `weight` column must be positive, but need not be normalized. Alternatively:
#' a density function
#'
#' @param ... optional arguments to `f_param`
#'
#' @param open_partitions a logical vector, `length(open_partition) == 2`.
#' Whether the `partitions` argument should be evaluated as open on lower and/or
#' upper ends - i.e. should `-Inf` and/or `Inf` be pre/appended to the
#' `partition` argument. n.b., this can also be accomplished by directly
#' including the `Inf`s in the partition.
#'
#' @return a vector of parameter values, one for each partition
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
#' ifr_blend <- blend(ifr_levin, age_limits, age_pyramid)
#' # the actual function
#' plot(
#'   60:100, ifr_levin(60:100),
#'   xlab = "age (years)", ylab = "IFR", type = "l"
#' )
#' # the properly aggregated blocks
#' lines(
#'   age_limits, c(ifr_blend, tail(ifr_blend, 1)),
#'   type = "s", col = "dodgerblue"
#' )
#' # naively aggregated blocks
#' ifr_naive <- ifr_levin(head(age_limits, -1) + diff(age_limits)/2)
#' lines(
#'   age_limits, c(ifr_naive, tail(ifr_naive, 1)),
#'   type = "s", col = "firebrick"
#' )
#' # properly aggregated, but not accounting for distribution
#' ifr_unif <- blend(ifr_levin, age_limits, within(age_pyramid, weight <- 1))
#' lines(
#'   age_limits, c(ifr_unif, tail(ifr_unif, 1)),
#'   type = "s", col = "darkgreen"
#' )
#' @export
blend <- function(
  f_param, densities,
  model_partition,
  ...,
  open_partition = c(lower = FALSE, upper = FALSE)
) {

  partitions <- make_partition(model_partition, open_partition = open_partition)

  # TODO argument checking

  lowers <- head(partitions, -1)
  uppers <- tail(partitions, -1)

  approxrule <- c(1, 1)
  if (!open_partition[1] & partitions[1] < densities[, min(from)]) {
    approxrule[1] <- 2
  }

  if (!open_partition[2] & tail(partitions, 1) > densities[, max(from)]) {
    approxrule[2] <- 2
  }

  f_density <- make_density(densities, method = "constant", rule = approxrule)

  f <- make_weight(f_param, f_density)

  out <- as.numeric(mapply(
    integrate, lower = lowers, upper = uppers, MoreArgs = c(list(f = f), list(..., subdivisions = 1000L))
  )["value",]) / as.numeric(mapply(
    integrate, lower = lowers, upper = uppers, MoreArgs = list(f = f_density, subdivisions = 1000L)
  )["value",])

  return(out)

}

#' @title Create an Model Outcome Re-mapping
#'
#' @inheritParams blend
#'
#' @param new_partition the new partition of the underlying feature
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
#'   from = 0:99, weight = ifelse(0:99 < 65, 1, .99^(0:99-64))
#' ) # flat age distribution, then 1% annual deaths
#' ifr_alembic <- alembic(ifr_levin, age_pyramid, age_limits, 0:100)
#'
#' @export
alembic <- function(
  f_param,
  densities,
  model_partition,
  new_partition,
  ...,
  open_partition = c(lower = FALSE, upper = FALSE)
) {

  overall_partition <- make_partition(
    model_partition, new_partition, open_partition = open_partition
  )

  lowers <- head(overall_partition, -1)
  uppers <- tail(overall_partition, -1)

  approxrule <- c(1, 1)
  if (!open_partition[1] & overall_partition[1] < densities[, min(from)]) {
    approxrule[1] <- 2
  }

  if (!open_partition[2] & tail(overall_partition, 1) > densities[, max(from)]) {
    approxrule[2] <- 2
  }

  f_density <- make_density(densities, method = "constant", rule = approxrule)

  f <- make_weight(f_param, f_density)

  return(data.table::data.table(from = lowers)[, {
    model_part <- findInterval(from, model_partition)
    new_part <- findInterval(from, new_partition)
    weight <- numeric(length(from))
    for (i in seq_along(weight)) {
      weight[i] <- integrate(f, lowers[i], uppers[i])$value
    }
    weight <- weight / aggregate(weight, by = list(model_part), FUN = sum)[model_part,]$x
    .(model_from = model_partition[model_part],
      new_from = new_partition[new_part], model_fraction = weight)
  }])

}

#' @title Re-Map Compartmental Model Outcomes
#'
#' @description
#' `distill` takes a low-age resolution outcome, for example deaths,
#' and proportionally distributes that outcome into a higher age resolution for
#' use in subsequent analyses like years-life-lost style calculations.
#'
#' @param outcomes a long-format `data.frame` with columns `model_part` and
#' `value`, optionally others that will be preserved
#'
#' @param mapping see [alembic()]: a `data.frame` in accordance with the return
#' of that function.
#'
#' @return a `data.frame` mirroring `outcomes`, but with
#'
#' @export
distill <- function(
  outcomes, mapping
) {

  setDT(outcomes)
  setDT(mapping)

  return(outcomes[
    mapping, on = .(model_partition)
  ][, .(value = sum(value*model_fraction)),
    by = c("new_partition", setdiff(names(outcomes), c("model_partition", "value")))
  ])

}
