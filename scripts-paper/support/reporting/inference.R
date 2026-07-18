# Shared significance, row-layout, and Newey-West table helpers.

paper_source_once(paper_path("config", "reporting.R"))

interleave <- function(a, b) {
  as.vector(rbind(a, b))
}

paper_significance_level <- function(
  level,
  thresholds = PAPER_REPORTING_CONTROL$significance
) {
  stopifnot(
    length(level) == 1L,
    is.character(level),
    level %in% names(thresholds)
  )
  unname(thresholds[[level]])
}

paper_significance_legend <- function(
  style = c("descending_p", "ascending_colon", "ascending_percent"),
  thresholds = PAPER_REPORTING_CONTROL$significance
) {
  style <- match.arg(style)
  stopifnot(identical(
    names(thresholds),
    c("one_star", "two_stars", "three_stars")
  ))
  one <- thresholds[["one_star"]]
  two <- thresholds[["two_stars"]]
  three <- thresholds[["three_stars"]]
  switch(style,
    descending_p = sprintf(
      "$^{***}p<%.2f$, $^{**}p<%.2f$, $^{*}p<%.2f$",
      three, two, one
    ),
    ascending_colon = sprintf(
      "$^{*}$: $p<%.2f$; $^{**}$: $p<%.2f$; $^{***}$: $p<%.2f$",
      one, two, three
    ),
    ascending_percent = sprintf(
      "$^{*}$/$^{**}$/$^{***}$ at %s\\%%",
      paste(format(100 * c(one, two, three), trim = TRUE), collapse = "/")
    )
  )
}

paper_newey_west_description <- function(policy) {
  sprintf(
    paste(
      "Newey--West heteroskedasticity- and autocorrelation-consistent",
      "standard errors with %d lags (Bartlett kernel, %s)"
    ),
    policy$hac_lags,
    if (isTRUE(policy$prewhite)) "prewhitening" else "no prewhitening"
  )
}

sig_stars <- function(
  p,
  thresholds = PAPER_REPORTING_CONTROL$significance
) {
  stopifnot(
    identical(names(thresholds), c(
      "one_star", "two_stars", "three_stars"
    ))
  )
  ifelse(
    !is.finite(p),
    "",
    ifelse(
      p < thresholds[["three_stars"]],
      "^{***}",
      ifelse(
        p < thresholds[["two_stars"]],
        "^{**}",
        ifelse(p < thresholds[["one_star"]], "^{*}", "")
      )
    )
  )
}

paper_newey_west_statistics <- function(
  fit,
  estimates,
  coef_names,
  policy
) {
  stopifnot(
    length(estimates) == length(coef_names),
    length(policy$hac_lags) == 1L,
    policy$hac_lags >= 0L,
    is.logical(policy$prewhite),
    length(policy$prewhite) == 1L,
    is.logical(policy$adjust),
    length(policy$adjust) == 1L
  )
  se <- sqrt(diag(sandwich::NeweyWest(
    fit,
    lag = policy$hac_lags,
    prewhite = policy$prewhite,
    adjust = policy$adjust
  )))[coef_names]
  statistic <- estimates / se
  p_value <- 2 * stats::pt(
    -abs(statistic),
    df = stats::df.residual(fit)
  )
  list(
    se = se,
    statistic = statistic,
    p_value = p_value,
    stars = sig_stars(p_value)
  )
}
