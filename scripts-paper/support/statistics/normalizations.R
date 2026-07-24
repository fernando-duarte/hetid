# Named distributional normalizations shared by execution and prose.

LOGVAR_NORMAL_CHISQ1_MEDIAN <- stats::qchisq(0.5, df = 1)
LOGVAR_NORMAL_LOG_SQUARE_MEDIAN <- log(LOGVAR_NORMAL_CHISQ1_MEDIAN)
LOGVAR_NORMAL_LOG_SQUARE_MEAN <- digamma(0.5) + log(2)
LOGVAR_NORMAL_LOG_SQUARE_GAP <- -LOGVAR_NORMAL_LOG_SQUARE_MEAN
LOGVAR_NORMAL_MEDIAN_MEANLOG_GAP <-
  LOGVAR_NORMAL_LOG_SQUARE_MEDIAN - LOGVAR_NORMAL_LOG_SQUARE_MEAN

logvar_normal_gap_text <- function(
  digits,
  gap = LOGVAR_NORMAL_LOG_SQUARE_GAP
) {
  formatC(
    gap,
    format = "f",
    digits = as.integer(digits)
  )
}
