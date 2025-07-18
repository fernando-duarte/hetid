% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_yield_curve.R
\name{download_yield_curve}
\alias{download_yield_curve}
\title{Download Federal Reserve Yield Curve Data}
\usage{
download_yield_curve(
  dataset = c("feds200628", "feds200533", "both"),
  force = FALSE,
  quiet = FALSE
)
}
\arguments{
\item{dataset}{Character string specifying which dataset to download.
Either "feds200628" (Svensson parameters) or "feds200533" (smoothed yields).
Can also use "both" to download both datasets.}

\item{force}{Logical. If TRUE, forces re-download even if data exists. Default is FALSE.}

\item{quiet}{Logical. If TRUE, suppresses download progress messages. Default is FALSE.}
}
\value{
Invisibly returns the path(s) to the saved CSV file(s).
}
\description{
Downloads yield curve data from the Federal Reserve Board website.
Two datasets are available: the Svensson yield curve parameters (feds200628)
and the smoothed yield curve (feds200533).
}
\examples{
\dontrun{
# Download Svensson yield curve parameters
download_yield_curve("feds200628")

# Download smoothed yield curve
download_yield_curve("feds200533")

# Download both datasets
download_yield_curve("both")

# Force re-download
download_yield_curve("feds200628", force = TRUE)
}

}
\references{
Gürkaynak, R. S., Sack, B., and Wright, J. H. (2007).
"The U.S. Treasury yield curve: 1961 to the present."
Journal of Monetary Economics, 54(8), 2291-2304.

Svensson, L. E. (1994).
"Estimating and interpreting forward interest rates: Sweden 1992-1994."
NBER Working Paper No. 4871.
}
