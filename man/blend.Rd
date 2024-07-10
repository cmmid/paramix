% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate.R
\name{blend}
\alias{blend}
\title{Transform Parameter Function into Values by Partition}
\usage{
blend(
  f_param,
  model_partition,
  densities,
  ...,
  open_partition = c(lower = FALSE, upper = FALSE)
)
}
\arguments{
\item{f_param}{a function, \code{f(x, ...)} which operates on the feature
(e.g. age), and optionally additional arguments, and yields the parameter
value.}

\item{model_partition}{a numeric vector of cut points, which define the partition}

\item{densities}{a \code{data.frame} with numeric columns \code{from} and \code{weight}. The
\code{weight} column must be positive, but need not be normalized. Alternatively:
a density function}

\item{...}{optional arguments to \code{f_param}}

\item{open_partitions}{a logical vector, \code{length(open_partition) == 2}.
Whether the \code{partitions} argument should be evaluated as open on lower and/or
upper ends - i.e. should \code{-Inf} and/or \code{Inf} be pre/appended to the
\code{partition} argument. n.b., this can also be accomplished by directly
including the \code{Inf}s in the partition.}
}
\value{
a vector of parameter values, one for each partition
}
\description{
\code{blend} integrates a functional relationship between some model
feature, for example case fatality ratio, into static parameter
values for use in a lower-resolution partitioned compartmental model.
}
\examples{
ifr_levin <- function(age_in_years) {
  scaled <- exp(-7.56 + 0.121 * age_in_years)
  scaled / (100 + scaled)
}
age_limits <- c(seq(0, 69, by = 5), 70, 80, 100)
age_pyramid <- data.frame(
  from = 0:99, weight = ifelse(0:99 < 65, 1, .99^(0:99-64))
) # flat age distribution, then 1\% annual deaths
ifr_blend <- blend(ifr_levin, age_limits, age_pyramid)
# the actual function
plot(
  60:100, ifr_levin(60:100),
  xlab = "age (years)", ylab = "IFR", type = "l"
)
# the properly aggregated blocks
lines(
  age_limits, c(ifr_blend, tail(ifr_blend, 1)),
  type = "s", col = "dodgerblue"
)
# naively aggregated blocks
ifr_naive <- ifr_levin(head(age_limits, -1) + diff(age_limits)/2)
lines(
  age_limits, c(ifr_naive, tail(ifr_naive, 1)),
  type = "s", col = "firebrick"
)
# properly aggregated, but not accounting for distribution
ifr_unif <- blend(ifr_levin, age_limits, within(age_pyramid, weight <- 1))
lines(
  age_limits, c(ifr_unif, tail(ifr_unif, 1)),
  type = "s", col = "darkgreen"
)
}