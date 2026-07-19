# Shared, policy-driven rendering primitives for publication-table cells.

paper_source_once(paper_path("config", "reporting.R"))

.paper_fixed_format <- function(digits) {
  paste0("%.", as.integer(digits), "f")
}

# One rendering of an identified-set / confidence interval in LaTeX, shared by
# the set, confidence, and endpoint-envelope cell formatters below. `marks`
# selects the finite-interval brackets; `side` renders a half-infinite interval
# ("upper" reports the finite upper bound, "lower" the finite lower bound).
.interval_latex <- function(lower, upper, digits, marks = c("[", "]"),
                            side = "none") {
  fmt <- .paper_fixed_format(digits)
  switch(side,
    upper = sprintf(paste0("$(-\\infty,\\,", fmt, "]$"), upper),
    lower = sprintf(paste0("$[", fmt, ",\\,\\infty)$"), lower),
    none = sprintf(
      paste0("$", marks[[1L]], fmt, ",\\,", fmt, marks[[2L]], "$"),
      lower, upper
    )
  )
}

paper_format_number <- function(
  value,
  digits,
  missing = c("nonfinite", "na"),
  missing_token = PAPER_NA_TOKEN
) {
  missing <- match.arg(missing)
  unavailable <- if (missing == "nonfinite") {
    !is.finite(value)
  } else {
    is.na(value)
  }
  ifelse(
    unavailable,
    missing_token,
    sprintf(.paper_fixed_format(digits), value)
  )
}

paper_format_set_interval <- function(
  lower,
  upper,
  status,
  digits,
  status_mode = c("not_bounded", "unreliable"),
  na_as_status = FALSE,
  infinite_bounds = FALSE,
  degenerate_rtol = 0
) {
  status_mode <- match.arg(status_mode)
  status_cell <- if (status_mode == "not_bounded") {
    status != PAPER_ENDPOINT_STATUS[["bounded"]]
  } else {
    status == PAPER_ENDPOINT_STATUS[["unreliable"]]
  }
  status_cell <- status_cell |
    (isTRUE(na_as_status) & (is.na(lower) | is.na(upper)))
  degenerate <- if (degenerate_rtol == 0) {
    lower == upper
  } else {
    abs(upper - lower) <= degenerate_rtol * (1 + abs(upper))
  }
  fixed <- .interval_latex(lower, upper, digits)
  interval <- if (isTRUE(infinite_bounds)) {
    ifelse(
      is.infinite(lower) & is.infinite(upper),
      PAPER_ENDPOINT_STATUS[["unbounded"]],
      ifelse(
        is.infinite(lower),
        .interval_latex(lower, upper, digits, side = "upper"),
        ifelse(
          is.infinite(upper),
          .interval_latex(lower, upper, digits, side = "lower"),
          fixed
        )
      )
    )
  } else {
    fixed
  }
  ifelse(status_cell, status, ifelse(degenerate, "", interval))
}

paper_format_confidence_interval <- function(
  lower,
  upper,
  digits,
  blank = FALSE,
  brackets = c("open", "closed")
) {
  brackets <- match.arg(brackets)
  marks <- if (brackets == "open") c("(", ")") else c("[", "]")
  value <- .interval_latex(lower, upper, digits, marks = marks)
  ifelse(blank | !is.finite(lower) | !is.finite(upper), "", value)
}

paper_format_endpoint_envelope <- function(lower, upper, side, digits) {
  side <- ifelse(is.na(side), "none", side)
  ifelse(
    side == "none" | is.na(lower) | is.na(upper),
    "",
    ifelse(
      side == "upper",
      .interval_latex(lower, upper, digits, side = "upper"),
      ifelse(
        side == "lower",
        .interval_latex(lower, upper, digits, side = "lower"),
        .interval_latex(lower, upper, digits)
      )
    )
  )
}

# Shared column headers for the exact-set tables: the OLS and tau = 0 columns
# followed by one header per displayed slack.
paper_tau_col_headers <- function(tau_display) {
  c(
    "OLS", "$\\tau{=}0$",
    sprintf("$\\tau{=}%s$", paper_format_tau(tau_display))
  )
}
