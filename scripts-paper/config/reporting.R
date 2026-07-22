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
    caption_endpoint = 2L,
    variance_bound_sci = 2L
  )
)

# Shared presentation tokens: the missing/not-applicable cell placeholder and
# the table-notes label, so a single edit retargets every consumer.
PAPER_NA_TOKEN <- "--"
PAPER_TABLE_NOTES_LABEL <- "Notes:"

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

# Integer with a LaTeX math-mode thousands separator, e.g. a B=10{,}000
# bootstrap replication count in a caption. "{,}" groups digits without the
# wide list-separator spacing a bare comma gets in math mode.
paper_format_thousands <- function(value) {
  stopifnot(
    length(value) == 1L,
    is.finite(value),
    value == as.integer(value)
  )
  gsub(",", "{,}", formatC(as.integer(value), format = "d", big.mark = ","), fixed = TRUE)
}

# Canonical slack (tau) rendering: the significant-digit idiom repeated across
# every table, caption, and figure annotation that prints a tau value.
paper_format_tau <- function(tau) {
  paper_format_general(
    tau,
    PAPER_REPORTING_CONTROL$precision$tau_significant
  )
}

# The estimation-sample date span, formatted as "start--end" for a caption's
# sample note. Takes the result's `$sample` sub-list.
paper_sample_span <- function(sample) {
  paste(format(sample$span), collapse = "--")
}

# Plain-math scientific-notation cell (no siunitx \num) shared by the
# diagnostics and variance-bound summary tables. `na_token` (when not NULL)
# substitutes for non-finite inputs, otherwise every value is formatted.
paper_format_sci <- function(value, digits, format = "e",
                             na_token = NULL) {
  txt <- formatC(value, format = format, digits = digits)
  has_e <- grepl("e", txt, fixed = TRUE)
  mant <- sub("e.*", "", txt)
  expo <- suppressWarnings(as.integer(sub(".*e", "", txt)))
  cell <- ifelse(
    has_e,
    sprintf("$%s \\times 10^{%d}$", mant, expo),
    txt
  )
  if (is.null(na_token)) {
    return(cell)
  }
  ifelse(is.finite(value), cell, na_token)
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
