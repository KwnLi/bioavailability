% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step2.R
\name{step2}
\alias{step2}
\title{Step 2 function}
\usage{
step2(
  AsPb = NULL,
  tot.n = NULL,
  ivba.n = NULL,
  minFrcAct = NULL,
  maxFrcAct = NULL,
  numbins = 20,
  ...
)
}
\arguments{
\item{AsPb}{define contaminant}

\item{tot.n}{number of total conc. samples, passed to simDU}

\item{ivba.n}{number of ivba samples, passed to simDU}

\item{minFrcAct}{minimum fraction of action level to simulate}

\item{maxFrcAct}{maximum fraction of action level to simulate}

\item{numbins}{nr. divisions over range of contaminant levels}

\item{...}{simDU parameters}
}
\value{
A list with error probabilities at the simulated quantiles and a
character string indicating As or Pb
}
\description{
Vary the contaminant level
}
\examples{
test.step2.2 <- step2(AsPb = "Pb", tot.n = 5, ot.incr = 10, ivba.n = 3,
ivba.incr = 10, minFrcAct = -0.01, maxFrcAct = -0.99, coeV.tot = 0.5,
coeV.rba = 0.05, error_tot = TRUE, error_ivb = TRUE, error_ivb_cv = 0.05,
ivba_model = TRUE)

}
