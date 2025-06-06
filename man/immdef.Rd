% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{immdef}
\alias{immdef}
\title{Simulated CONCORDE trial data from the rpsftm package}
\format{
An object of class \code{data.frame} with 1000 rows and 9 columns.
}
\usage{
immdef
}
\description{
Patients were randomly assigned to receive treatment immediately
or deferred, and those in the deferred arm could cross over and
receive treatment. The primary endpoint was time to disease progression.
\describe{
\item{\code{id}}{Patient identification number}
\item{\code{def}}{Indicator that the participant was assigned to
the deferred treatment arm}
\item{\code{imm}}{Indicator that the participant was assigned to
the immediate treatment arm}
\item{\code{censyrs}}{The censoring time, in years, corresponding to
the close of study minus the time of entry for each patient}
\item{\code{xo}}{Indicator that crossover occurred}
\item{\code{xoyrs}}{The time, in years, from entry to switching, or
0 for patients in the immediate arm}
\item{\code{prog}}{Indicator of disease progression (1), or
censoring (0)}
\item{\code{progyrs}}{Time, in years, from entry to disease
progression or censoring}
\item{\code{entry}}{The time of entry into the study, measured in years
from the date of randomisation}
}
}
\keyword{datasets}
