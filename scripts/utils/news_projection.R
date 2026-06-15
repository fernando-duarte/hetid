# News-projection hook: decides whether the pipeline imposes the exact-news
# projection B = 0 (residual Y2 = the raw news) or estimates B by regressing
# Y2 on the common conditioning vector ("let the data speak", the default).
# HETID_IMPOSE_NEWS_PROJECTION_ZERO overrides the IMPOSE_NEWS_PROJECTION_ZERO
# script constant; resolution is centralized here so the stages that build the
# W2 residuals cannot drift. Mirrors baseline_gamma_method() in gamma_source.R.

impose_news_projection_zero <- function() {
  raw <- trimws(Sys.getenv("HETID_IMPOSE_NEWS_PROJECTION_ZERO", ""))

  if (identical(raw, "")) {
    # No override: fall back to the script constant (the global set by
    # common_settings.R), read the way N_Y1_LAGS is at
    # identification_utils.R:108. Absent the constant, default to FALSE.
    const <- get0(
      "IMPOSE_NEWS_PROJECTION_ZERO",
      envir = globalenv(), ifnotfound = FALSE
    )
    return(isTRUE(const))
  }

  if (raw %in% c("TRUE", "true", "1")) {
    return(TRUE)
  }
  if (raw %in% c("FALSE", "false", "0")) {
    return(FALSE)
  }

  stop(
    "HETID_IMPOSE_NEWS_PROJECTION_ZERO must be one of ",
    "TRUE/true/1 (impose exact-news B = 0) or ",
    "FALSE/false/0 (estimate B from the data); got '", raw, "'"
  )
}
