% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_paths.R
\name{check_data_file_exists}
\alias{check_data_file_exists}
\title{Check Data File Exists}
\usage{
check_data_file_exists(filename)
}
\arguments{
\item{filename}{Character string with filename (including extension)}
}
\value{
Logical indicating whether file exists
}
\description{
Checks if a data file exists in the package data directory.
Useful for workflows that depend on external data.
}
\keyword{internal}
