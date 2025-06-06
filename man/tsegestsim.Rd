% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{tsegestsim}
\alias{tsegestsim}
\title{Simulate Survival Data for Two-Stage Estimation Method Using
g-estimation}
\usage{
tsegestsim(
  n = 500L,
  allocation1 = 2L,
  allocation2 = 1L,
  pbprog = 0.5,
  trtlghr = -0.5,
  bprogsl = 0.3,
  shape1 = 1.8,
  scale1 = 360,
  shape2 = 1.7,
  scale2 = 688,
  pmix = 0.5,
  admin = 5000,
  pcatnotrtbprog = 0.5,
  pcattrtbprog = 0.25,
  pcatnotrt = 0.2,
  pcattrt = 0.1,
  catmult = 0.5,
  tdxo = 1,
  ppoor = 0.1,
  pgood = 0.04,
  ppoormet = 0.4,
  pgoodmet = 0.2,
  xomult = 1.4188308,
  milestone = 546,
  outputRawDataset = 1L,
  seed = NA_integer_
)
}
\arguments{
\item{n}{The total sample size for two treatment arms combined.}

\item{allocation1}{The number of subjects in the active treatment group
in a randomization block.}

\item{allocation2}{The number of subjects in the control group in
a randomization block.}

\item{pbprog}{The probability of having poor prognosis at baseline.}

\item{trtlghr}{The treatment effect in terms of log hazard ratio.}

\item{bprogsl}{The poor prognosis effect in terms of log hazard ratio.}

\item{shape1}{The shape parameter for the Weibull event distribution
for the first component.}

\item{scale1}{The scale parameter for the Weibull event distribution
for the first component.}

\item{shape2}{The shape parameter for the Weibull event distribution
for the second component.}

\item{scale2}{The scale parameter for the Weibull event distribution
for the second component.}

\item{pmix}{The mixing probability of the first component Weibull
distribution.}

\item{admin}{The administrative censoring time.}

\item{pcatnotrtbprog}{The probability of developing metastatic disease
on control treatment with poor baseline prognosis.}

\item{pcattrtbprog}{The probability of developing metastatic disease
on active treatment with poor baseline prognosis.}

\item{pcatnotrt}{The probability of developing metastatic disease
on control treatment with good baseline prognosis.}

\item{pcattrt}{The probability of developing metastatic disease
on active treatment with good baseline prognosis.}

\item{catmult}{The impact of metastatic disease on shortening remaining
survival time.}

\item{tdxo}{Whether treatment crossover depends on time-dependent
covariates between disease progression and treatment switching.}

\item{ppoor}{The probability of switching for poor baseline prognosis
with no metastatic disease.}

\item{pgood}{The probability of switching for good baseline prognosis
with no metastatic disease.}

\item{ppoormet}{The probability of switching for poor baseline prognosis
after developing metastatic disease.}

\item{pgoodmet}{The probability of switching for good baseline prognosis
after developing metastatic disease.}

\item{xomult}{The direct effect of crossover on extending remaining
survival time.}

\item{milestone}{The milestone to calculate restricted mean survival
time.}

\item{outputRawDataset}{Whether to output the raw data set.}

\item{seed}{The seed to reproduce the simulation results.
The seed from the environment will be used if left unspecified.}
}
\value{
A list with two data frames.
\itemize{
\item \code{sumdata}: A summary data frame with the following variables:
\itemize{
\item \code{simtrueconstmean}: The true control group restricted mean
survival time (RMST).
\item \code{simtrueconstlb}: The lower bound for control group RMST.
\item \code{simtrueconstub}: The upper bound for control group RMST.
\item \code{simtrueconstse}: The standard error for control group RMST.
\item \code{simtrueexpstmean}: The true experimental group restricted
mean survival time (RMST).
\item \code{simtrueexpstlb}: The lower bound for experimental group RMST.
\item \code{simtrueexpstub}: The upper bound for experimental group RMST.
\item \code{simtrueexpstse}: The standard error for experimental group
RMST.
\item \code{simtrue_coxwbprog_hr}: The treatment hazard ratio from the
Cox model adjusting for baseline prognosis.
\item \code{simtrue_cox_hr}: The treatment hazard ratio from the Cox
model without adjusting for baseline prognosis.
}
\item \code{paneldata}: A counting process style subject-level data frame
with the following variables:
\itemize{
\item \code{id}: The subject ID.
\item \code{trtrand}: The randomized treatment arm.
\item \code{bprog}: Whether the patient had poor baseline prognosis.
\item \code{tstart}: The left end of time interval.
\item \code{tstop}: The right end of time interval.
\item \code{event}: Whether the patient died at the end of the interval.
\item \code{timeOS}: The observed survival time.
\item \code{died}: Whether the patient died during the study.
\item \code{progressed}: Whether the patient had disease progression.
\item \code{timePFSobs}: The observed time of disease progression at
regular scheduled visits.
\item \code{progtdc}: The time-dependent covariate for progression.
\item \code{catevent}: Whether the patient developed metastatic disease.
\item \code{cattime}: When the patient developed metastatic disease.
\item \code{cattdc}: The time-dependent covariate for cat event.
\item \code{catlag}: The lagged value of \code{cattdc}.
\item \code{xo}: Whether the patient switched treatment.
\item \code{xotime}: When the patient switched treatment.
\item \code{xotdc}: The time-dependent covariate for treatment
switching.
\item \code{xotime_upper}: The upper bound of treatment switching time.
\item \code{censor_time}: The administrative censoring time.
}
}
}
\description{
Obtains the simulated data for baseline prognosis,
disease progression, treatment switching, death, and
time-dependent covariates.
}
\examples{

sim1 <- tsegestsim(
  n = 500, allocation1 = 2, allocation2 = 1, pbprog = 0.5, 
  trtlghr = -0.5, bprogsl = 0.3, shape1 = 1.8, 
  scale1 = 360, shape2 = 1.7, scale2 = 688, 
  pmix = 0.5, admin = 5000, pcatnotrtbprog = 0.5, 
  pcattrtbprog = 0.25, pcatnotrt = 0.2, pcattrt = 0.1, 
  catmult = 0.5, tdxo = 1, ppoor = 0.1, pgood = 0.04, 
  ppoormet = 0.4, pgoodmet = 0.2, xomult = 1.4188308, 
  milestone = 546, outputRawDataset = 1, seed = 2000)

}
\references{
NR Latimer, IR White, K Tilling, and U Siebert.
Improved two-stage estimation to adjust for treatment switching in
randomised trials: g-estimation to address time-dependent confounding.
Statistical Methods in Medical Research. 2020;29(10):2900-2918.
}
\author{
Kaifeng Lu, \email{kaifenglu@gmail.com}
}
