% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal.R
\name{make_partition}
\alias{make_partition}
\title{Create a Least-Common-Interval Partition}
\usage{
make_partition(model_partition = numeric(0), output_partition = numeric(0))
}
\arguments{
\item{model_partition}{the model partition}

\item{output_partition}{the output partition}
}
\value{
a sorted numeric vector with unique values
}
\description{
Internal utility method for creating partitions, possibly from multiple
distinct partitions. Validates inputs.
}
\keyword{internal}
