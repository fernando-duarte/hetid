% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bond_calculations.R
\name{compute_n_hat}
\alias{compute_n_hat}
\title{Compute Expected Log Bond Price Estimator (n_hat)}
\usage{
compute_n_hat(yields, term_premia, i)
}
\arguments{
\item{yields}{Data frame with columns y1, y2, ..., containing yields}

\item{term_premia}{Data frame with columns tp1, tp2, ..., containing term premia}

\item{i}{Integer, the horizon (must be >= 1)}
}
\value{
Numeric vector of n_hat(i,t) values
}
\description{
Computes the time series n_hat(i,t) which is an estimator for
E_t[p_(t+i)^(1)] = -E_t[y_(t+i)^(1)]
}
\details{
The formula is:
n_hat(i,t) = i*y_t^(i) - (i+1)*y_t^(i+1) + (i+1)\emph{TP_t^(i+1) - i}TP_t^(i)
}
\examples{
\dontrun{
# Extract ACM data
data <- extract_acm_data(data_types = c("yields", "term_premia"))

# Compute n_hat for i=5
n_hat_5 <- compute_n_hat(
  yields = data[, grep("^y", names(data))],
  term_premia = data[, grep("^tp", names(data))],
  i = 5
)
}

}
