# Shared significance, row-layout, and Newey-West table helpers.

paper_source_once(paper_path("config", "reporting.R"))

interleave <- function(a, b) {
  as.vector(rbind(a, b))
}

paper_inference_labels <- function(
  inference = PAPER_ANALYSIS_CONTRACT$inference
) {
  alpha <- inference$nominal_alpha
  stopifnot(alpha > 0, alpha < 1)
  probs <- 100 * c(lower = alpha / 2, upper = 1 - alpha / 2)
  list(
    coverage_percent = 100 * (1 - alpha),
    lower_percent = unname(probs[["lower"]]),
    upper_percent = unname(probs[["upper"]]),
    minimum_valid_draw_percent =
      100 * inference$minimum_valid_draw_share
  )
}

paper_inference_metadata_frame <- function(
  rows,
  inference = PAPER_ANALYSIS_CONTRACT$inference
) {
  stopifnot(rows >= 0L)
  data.frame(
    inference_version = rep(inference$version, rows),
    nominal_alpha = rep(inference$nominal_alpha, rows),
    minimum_valid_draw_share = rep(
      inference$minimum_valid_draw_share,
      rows
    ),
    stringsAsFactors = FALSE
  )
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
      "$^{***}p<%s$, $^{**}p<%s$, $^{*}p<%s$",
      paper_format_number(
        three,
        PAPER_REPORTING_CONTROL$cells$statistic_digits,
        "na"
      ),
      paper_format_number(
        two,
        PAPER_REPORTING_CONTROL$cells$statistic_digits,
        "na"
      ),
      paper_format_number(
        one,
        PAPER_REPORTING_CONTROL$cells$statistic_digits,
        "na"
      )
    ),
    ascending_colon = sprintf(
      "$^{*}$: $p<%s$; $^{**}$: $p<%s$; $^{***}$: $p<%s$",
      paper_format_number(
        one,
        PAPER_REPORTING_CONTROL$cells$statistic_digits,
        "na"
      ),
      paper_format_number(
        two,
        PAPER_REPORTING_CONTROL$cells$statistic_digits,
        "na"
      ),
      paper_format_number(
        three,
        PAPER_REPORTING_CONTROL$cells$statistic_digits,
        "na"
      )
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
