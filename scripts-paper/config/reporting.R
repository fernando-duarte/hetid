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
  ),
  cells = list(
    log_variance = list(
      digits = 3L,
      numeric_missing = "nonfinite",
      status_mode = "unreliable",
      na_as_status = TRUE,
      infinite_bounds = TRUE,
      degenerate_rtol = 0
    ),
    structural = list(
      digits = 3L,
      numeric_missing = "na",
      status_mode = "not_bounded",
      na_as_status = FALSE,
      infinite_bounds = FALSE,
      degenerate_rtol = 0,
      confidence_brackets = "open"
    ),
    variance_share = list(
      digits = 2L,
      numeric_missing = "na",
      status_mode = "not_bounded",
      na_as_status = FALSE,
      infinite_bounds = FALSE
    ),
    statistic_digits = 2L
  ),
  precision = list(
    tau_significant = 2L,
    console_significant = 3L,
    descriptive_summary = 3L,
    descriptive_correlation = 2L,
    diagnostic_table = 3L,
    figure_annotation = 3L,
    caption_percent = 3L,
    caption_endpoint = 2L
  )
)

PAPER_TABLE_STYLE <- list(
  coefficient = list(
    fontsize = "\\footnotesize\\setlength{\\tabcolsep}{3pt}",
    row_stride = 2L
  ),
  variance_share = list(
    fontsize = "\\footnotesize\\setlength{\\tabcolsep}{4pt}"
  )
)

PAPER_LATEX_CONTROL <- list(
  sidecar_extensions = c(
    "aux", "log", "fls", "fdb_latexmk", "out", "toc",
    "nav", "snm", "vrb", "synctex.gz"
  ),
  cleanup_attempts = 8L,
  cleanup_wait_seconds = 0.25
)

stopifnot(
  identical(
    names(PAPER_REPORTING_CONTROL$significance),
    c("one_star", "two_stars", "three_stars")
  ),
  all(diff(PAPER_REPORTING_CONTROL$significance) < 0),
  PAPER_REPORTING_CONTROL$mean_ols$hac_lags >= 0L,
  PAPER_REPORTING_CONTROL$logvar_logols$hac_lags >= 0L,
  PAPER_TABLE_STYLE$coefficient$row_stride >= 1L,
  !anyDuplicated(PAPER_LATEX_CONTROL$sidecar_extensions),
  PAPER_LATEX_CONTROL$cleanup_attempts >= 1L,
  PAPER_LATEX_CONTROL$cleanup_wait_seconds >= 0,
  all(vapply(
    PAPER_REPORTING_CONTROL$cells[c(
      "log_variance", "structural", "variance_share"
    )],
    function(policy) policy$digits >= 0L,
    logical(1)
  )),
  all(vapply(
    PAPER_REPORTING_CONTROL$precision,
    function(digits) {
      is.integer(digits) &&
        length(digits) == 1L &&
        !is.na(digits) &&
        digits >= 0L
    },
    logical(1)
  ))
)

paper_format_general <- function(value, digits) {
  stopifnot(
    length(digits) == 1L,
    is.finite(digits),
    digits >= 0,
    digits == as.integer(digits)
  )
  sprintf(
    paste0("%.", as.integer(digits), "g"),
    value
  )
}

paper_latex_sidecar_pattern <- function(
  control = PAPER_LATEX_CONTROL
) {
  extensions <- gsub(
    ".",
    "[.]",
    control$sidecar_extensions,
    fixed = TRUE
  )
  paste0("[.](", paste(extensions, collapse = "|"), ")$")
}
