% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step3.R
\name{step3}
\alias{step3}
\title{Step 3}
\usage{
step3(
  meas.tot = NULL,
  meas.ivba = NULL,
  AsPb = NULL,
  tot.incr = NULL,
  ivba.incr = NULL,
  useMeanTot = T,
  actLvl = 400,
  actLvlRBA = 60
)
}
\arguments{
\item{meas.tot}{actual total concentration measurements}

\item{meas.ivba}{actual ivba measurements}

\item{AsPb}{"As" or "Pb"}

\item{tot.incr}{number of increments for total conc. samples}

\item{ivba.incr}{number of increments for ivb samples}

\item{useMeanTot}{Use mean of upper 95\% interval in estimation of total
contaminant? T = use the mean; F = use 95\% interval}

\item{actLvl}{What is the site-specific soil contaminant action level? mg/kg
(action level: limit of biovalability above which site has to be remedied)}

\item{actLvlRBA}{\% RBA assumed in the action level}
}
\description{
Calculate actual DU statistics
}
\examples{
teststep3 = step3(c(340,580,209,300,333), meas.ivba = c(78,76,45),
      tot.incr = 5, ivba.incr = 5,
      "Pb")
teststep3 <-  step3(testcv(rnorm(n = 10000, mean = 300, sd = 20),100),
      meas.ivba = testcv(rnorm(n = 10000, mean = 75, sd = 5),100),
      tot.incr = 100, ivba.incr = 100, "Pb") # simulates increments of 10
}
