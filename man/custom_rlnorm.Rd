% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_rlnorm.R
\name{custom_rlnorm}
\alias{custom_rlnorm}
\title{Custom log-normal random number generator}
\usage{
custom_rlnorm(n, m, s)
}
\arguments{
\item{n}{number of returned observations}

\item{m}{mean in lognormal scale}

\item{s}{sd in lognormal scale}
}
\value{
Values drawn from a lognormal distribution with mean \code{m} and standard
deviation \code{s} in the lognormal scale.
}
\description{
A wrapper for the rlnorm function that converts the normal mean and sd inputs
so that the output lognormal samples have the specified mean and sd at the
lognormal scale
}
\examples{
custom_rlnorm(10, 5, 3)

}
