# Shared inference and rendering policy for published paper tables.

PAPER_REPORTING_CONTROL <- list(
  significance = c(
    one_star = 0.10,
    two_stars = 0.05,
    three_stars = 0.01
  ),
  mean_ols = list(
    hac_lags = 4L,
    prewhite = FALSE,
    adjust = FALSE
  ),
  logvar_logols = list(
    hac_lags = 4L,
    prewhite = FALSE,
    adjust = FALSE
  ),
  ppml = list(
    se_type = "hac",
    hac_lags = 4L
  ),
  harvey = list(
    se_type = "hac",
    hac_lags = 4L
  )
)

stopifnot(
  identical(
    names(PAPER_REPORTING_CONTROL$significance),
    c("one_star", "two_stars", "three_stars")
  ),
  all(diff(PAPER_REPORTING_CONTROL$significance) < 0),
  PAPER_REPORTING_CONTROL$mean_ols$hac_lags >= 0L,
  PAPER_REPORTING_CONTROL$logvar_logols$hac_lags >= 0L
)
