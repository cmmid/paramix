% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate.R
\name{make_weight}
\alias{make_weight}
\title{Compose Parameter & Density Functions}
\usage{
make_weight(f_param, f_dense)
}
\arguments{
\item{f_param}{a function; the parameter function, varying with the aggregate}

\item{f_dense}{a function; the density function, varying with the aggregate}
}
\value{
a new function, f(x) = f_param(x)*f_density(x)
}
\description{
Compose Parameter & Density Functions
}
\keyword{internal}
