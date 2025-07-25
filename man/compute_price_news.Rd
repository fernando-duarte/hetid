% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_price_news.R
\name{compute_price_news}
\alias{compute_price_news}
\title{Compute Price News}
\usage{
compute_price_news(
  yields,
  term_premia,
  i,
  return_yield_news = FALSE,
  return_df = FALSE,
  dates = NULL
)
}
\arguments{
\item{yields}{Data frame with columns y1, y2, ..., containing yields}

\item{term_premia}{Data frame with columns tp1, tp2, ..., containing term premia}

\item{i}{Integer specifying the maturity index (column) to analyze.
Must be between 1 and ncol(yields). Corresponds to maturity in years
for ACM dataset (i=1 is 1-year, i=2 is 2-year, ..., i=10 is 10-year).
The maturity index determines which yield curve point is used in the analysis.}

\item{return_yield_news}{Logical, if TRUE returns yield news instead of log price news}

\item{return_df}{Logical, if TRUE returns a data frame with dates (default FALSE)}

\item{dates}{Optional vector of dates corresponding to the rows in yields/term_premia.
If not provided and return_df = TRUE, will use row indices.}
}
\value{
Numeric vector of computed values, or data frame with dates if return_df = TRUE
}
\description{
Computes the time series of price news Delta_(t+1)p_(t+i)^(1) or Delta_(t+1)y_(t+i)^(1)
}
\details{
The price news for log prices is:
Delta_(t+1)p_(t+i)^(1) = n_hat(i-1,t+1) - n_hat(i,t)

The price news for yields is:
Delta_(t+1)y_(t+i)^(1) = -Delta_(t+1)p_(t+i)^(1)
}
\examples{
\dontrun{
# Extract ACM data - need maturities 1, i-1, i, i+1 for maturity i
# For i=5, we need maturities 1, 4, 5, 6
data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  maturities = c(1, 4, 5, 6)
)
yields <- data[, grep("^y", names(data))]
term_premia <- data[, grep("^tp", names(data))]

# Compute price news for i=5 (log price news)
price_news_5 <- compute_price_news(yields, term_premia, i = 5)

# Compute yield news with dates
yield_news_5_df <- compute_price_news(
  yields, term_premia,
  i = 5,
  return_yield_news = TRUE,
  return_df = TRUE,
  dates = data$date
)
}

}
