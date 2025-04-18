% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simDU.R
\name{simDU}
\alias{simDU}
\title{Simulate DU function}
\usage{
simDU(
  AsPb = NULL,
  actLvl = 400,
  actLvlRBA = 60,
  frcAct = NULL,
  useMeanTot = T,
  useMeanIVBA = T,
  tot.n = NULL,
  tot.incr = 1,
  ivba.n = NULL,
  ivba.incr = 1,
  coeV.tot = NULL,
  coeV.rba = NULL,
  mn.rba = 60,
  dist_tot = "lognorm",
  dist_rba = "normal",
  custom_sepred = NULL,
  error_tot = FALSE,
  error_tot_cv = NULL,
  error_ivb = FALSE,
  error_ivb_cv = NULL,
  ivba_model = FALSE,
  post_mean = FALSE,
  iter = 1000,
  outputLvl = 1
)
}
\arguments{
\item{AsPb}{"As" or "Pb"}

\item{actLvl}{Site-specific soil contaminant action level: mg/kg  (action
level: limit of biovalability above which site has to be remedied)}

\item{actLvlRBA}{Site-specific soil contaminant action level: \% RBA assumed
in the action level}

\item{frcAct}{Site-specific soil contaminant action level: fraction of the
action level threshold}

\item{useMeanTot}{Use mean of upper 95\% interval in estimation of total
contaminant? T = use the mean; F = use 95\% interval}

\item{useMeanIVBA}{Use mean or upper 95\% interval in estimation of IVBA? T =
use the mean; F = use 95\% interval}

\item{tot.n}{sampling protocol input: number of total metal concentration
samples}

\item{tot.incr}{sampling protocol input: number of increments in total
samples}

\item{ivba.n}{sampling protocol input: number of samples analyzed for IVBA}

\item{ivba.incr}{sampling protocol input: number of increments in ivba
samples}

\item{coeV.tot}{simulated DU parameter: coefficient of variation of total
metal concentration samples}

\item{coeV.rba}{simulated DU parameter: coefficient of varation of RBA}

\item{mn.rba}{mean "true" RBA}

\item{dist_tot}{distribution of total concentration (input parameter for
\code{simDist_tot})}

\item{dist_rba}{distribution of RBA (input parameter for \code{simDist_rba})}

\item{custom_sepred}{simulation parameter: custom sepred value, overrides
defaults}

\item{error_tot}{simulation parameter: (T/F) include total conc. measurement
error}

\item{error_tot_cv}{simulation parameter: (T/F) total conc. measurement error
coefficient of variation}

\item{error_ivb}{simulation parameter: (T/F) include IVBA measurement error}

\item{error_ivb_cv}{simulation parameter: (T/F) IVBA measurement error
coefficient of variation}

\item{ivba_model}{simulation parameter: (T/F) include IVBA model error}

\item{post_mean}{simulation parameter: (T/F) calculate IVBA model error after
summarizing across samples}

\item{iter}{simulation parameter: number of simulations}

\item{outputLvl}{Output detail: \code{1}=just error rate and summary across sims;
\code{2}=level1 + summary of each simulation; \code{3}=level2 + total and ivb samples
for each iteration; \code{4}=level3 + uncomposited increments for each iteration}
}
\description{
Simulate DU function
}
\examples{
test <- simDU(AsPb = "As", frcAct = 0.15, tot.n = 5, ivba.n = 3,
coeV.tot = 0.5, coeV.rba = 0.05, error_tot = TRUE, error_ivb = TRUE,
error_ivb_cv = 0.05, ivba_model = TRUE, outputLvl = 2)

}
