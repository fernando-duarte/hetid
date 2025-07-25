% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation_utils.R
\name{validate_min_observations}
\alias{validate_min_observations}
\title{Validate Minimum Observations}
\usage{
validate_min_observations(n, min_obs = HETID_CONSTANTS$MIN_OBSERVATIONS)
}
\arguments{
\item{n}{Number of observations to validate}

\item{min_obs}{Minimum required observations}
}
\value{
Invisible TRUE if valid, stops with informative error if invalid
}
\description{
Validates that sufficient observations are available for statistical estimation.
}
\keyword{internal}
