# Checks for shared inference-reporting policy and helpers.

paper_source_once(paper_path("support", "reporting", "inference.R"))

reporting_p <- c(
  0.20,
  PAPER_REPORTING_CONTROL$significance[["one_star"]] / 2,
  PAPER_REPORTING_CONTROL$significance[["two_stars"]] / 2,
  PAPER_REPORTING_CONTROL$significance[["three_stars"]] / 2,
  NA_real_
)
check(
  "significance stars derive from the reporting contract",
  identical(
    sig_stars(reporting_p),
    c("", "^{*}", "^{**}", "^{***}", "")
  )
)
check(
  "interleave has one canonical row-order implementation",
  identical(interleave(c("a", "b"), c("A", "B")), c("a", "A", "b", "B"))
)
check(
  "LaTeX sidecar matcher derives from extension control",
  all(grepl(
    paper_latex_sidecar_pattern(),
    paste0("fixture.", PAPER_LATEX_CONTROL$sidecar_extensions)
  ))
)
check(
  "thousands formatting groups a caption replication count",
  identical(paper_format_thousands(10000L), "10{,}000")
)
check(
  "thousands formatting leaves a sub-thousand count alone",
  identical(paper_format_thousands(25L), "25")
)

reporting_alt_thresholds <- c(
  one_star = 0.20,
  two_stars = 0.10,
  three_stars = 0.04
)
check(
  "significance prose executes a supplied nondefault threshold policy",
  grepl(
    "20/10/4",
    paper_significance_legend(
      "ascending_percent",
      reporting_alt_thresholds
    ),
    fixed = TRUE
  )
)
reporting_alt_hac <- list(hac_lags = 7L, prewhite = TRUE)
check(
  "Newey-West prose executes a supplied nondefault policy",
  identical(
    paper_newey_west_description(reporting_alt_hac),
    paste(
      "Newey--West heteroskedasticity- and autocorrelation-consistent",
      "standard errors with 7 lags (Bartlett kernel, prewhitening)"
    )
  )
)

reporting_fit <- stats::lm(
  c(1.0, 1.8, 3.2, 3.7, 5.1, 5.8) ~
    c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)
)
reporting_coef <- stats::coef(reporting_fit)
reporting_policy <- PAPER_REPORTING_CONTROL$mean_ols
reporting_nw <- paper_newey_west_statistics(
  reporting_fit,
  reporting_coef,
  names(reporting_coef),
  reporting_policy
)
reporting_expected_se <- sqrt(diag(sandwich::NeweyWest(
  reporting_fit,
  lag = reporting_policy$hac_lags,
  prewhite = reporting_policy$prewhite,
  adjust = reporting_policy$adjust
)))
check(
  "Newey-West helper executes the named reporting policy",
  isTRUE(all.equal(
    unname(reporting_nw$se),
    unname(reporting_expected_se),
    tolerance = 1e-12
  ))
)
check(
  "Newey-West helper returns aligned statistics and stars",
  length(reporting_nw$statistic) == length(reporting_coef) &&
    length(reporting_nw$stars) == length(reporting_coef) &&
    identical(names(reporting_nw$se), names(reporting_coef))
)
check(
  "coefficient-table layout has one reporting-policy owner",
  PAPER_TABLE_STYLE$coefficient$row_stride == 2L &&
    grepl(
      "tabcolsep",
      PAPER_TABLE_STYLE$coefficient$fontsize,
      fixed = TRUE
    )
)

rm(
  reporting_p,
  reporting_alt_thresholds,
  reporting_alt_hac,
  reporting_fit,
  reporting_coef,
  reporting_policy,
  reporting_nw,
  reporting_expected_se
)
