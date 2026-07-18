# Stationarity and summary-statistics execution policy.

PAPER_STATIONARITY_CONTROL <- list(
  summary_acf_max = 2L,
  adf = list(
    type = "drift",
    select_lags = "AIC",
    response_surface_trend = "c",
    response_surface_statistic = "t"
  ),
  kpss = list(type = "mu"),
  ljung_box = list(lag = 8L, type = "Ljung-Box")
)
