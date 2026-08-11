# Cell formatters and PPML table-part assembly shared by every log-variance
# estimator panel: the "--" non-finite formatter, the status-aware
# identified-set cell, the coefficient/statistic row interleaver, and the
# canonical PPML rows and columns. Definitions only.

paper_source_once(paper_path(
  "support", "reporting", "inference.R"
))
paper_source_once(paper_path("support", "reporting", "cells.R"))

# NA and non-finite values render "--"; upstream diagnostics retain the reason
# a value is unavailable rather than hiding it behind a numeric token.
logvar_fmt <- function(
  x,
  cell_policy = PAPER_REPORTING_CONTROL$cells$log_variance
) {
  paper_math_negative(
    paper_format_number(
      x,
      cell_policy$digits,
      cell_policy$numeric_missing
    )
  )
}

# an unreliable or upstream-propagated (NA-endpoint) cell renders its status
# word; certified one-sided divergence renders a half-infinite range; a
# degenerate interval (point-identified) is left blank as in the structural
# table
set_cell <- function(
  lo,
  hi,
  status,
  cell_policy = PAPER_REPORTING_CONTROL$cells$log_variance
) {
  paper_format_set_interval(
    lo,
    hi,
    status,
    digits = cell_policy$digits,
    status_mode = cell_policy$status_mode,
    na_as_status = cell_policy$na_as_status,
    infinite_bounds = cell_policy$infinite_bounds,
    degenerate_rtol = cell_policy$degenerate_rtol
  )
}

# Side-aware confidence-envelope cell: the moving-block bootstrap outer
# containment interval for one set endpoint (log_var_eq_set_boot), rendered on
# the row beneath its set_cell. A two-sided finite envelope renders in round
# parentheses, following the confidence-interval convention shared with the
# mean panel's CI rows and distinguishing it from the square-bracket identified
# set above. A genuinely one-sided set (side "upper"/"lower") keeps its live
# endpoint bracket-closed and the unattained infinite endpoint parenthesized,
# exactly like set_cell's own infinite-bound cells; a suppressed cell (side
# "none", or a non-finite endpoint) renders blank. Vectorized like set_cell.
envelope_cell <- function(
  ci_lo,
  ci_hi,
  side,
  cell_policy = PAPER_REPORTING_CONTROL$cells$log_variance
) {
  paper_format_endpoint_envelope(
    ci_lo,
    ci_hi,
    side,
    cell_policy$digits
  )
}

# One coefficient vector must match the reference row order exactly, so every
# rendered cell lines up with its coefficient label; a mismatch is a wiring bug,
# never data. Shared by the estimator panel, the log-OLS fragment, and the
# envelope columns so the row-order invariant has a single owner.
logvar_assert_coef_aligned <- function(coef, reference) {
  stopifnot(identical(coef, reference))
}

# The combined estimator-panel column headers: the estimator's reference column,
# the tau = 0 Lewbel-point column, then one header per displayed slack. The lone
# builder for the panel path, so the header vector is never assembled twice and
# reconciled at runtime.
logvar_estimator_headers <- function(reference_header, tau_display) {
  c(
    reference_header,
    "$\\tau{=}0$",
    sprintf("$\\tau{=}%s$", paper_format_tau(tau_display))
  )
}

# The identified-set table columns shared by logvar_ppml_table_parts and the
# Harvey panel: each column is set_cell over one tau's hull with the bootstrap
# envelope_cell on the row beneath when an envelope frame is supplied. The
# slicing and coef-alignment validation live here so the two estimator panels
# cannot drift on the envelope-column rule (as logvar_se_point_col does for the
# point column). envelope NULL keeps every column byte-identical to the
# pre-envelope renderer; a supplied envelope keys on the same taus and aligns to
# tab_coef.
logvar_set_envelope_cols <- function(
  sets,
  envelope,
  keys,
  tab_coef,
  n_obs,
  cell_policy = PAPER_REPORTING_CONTROL$cells$log_variance
) {
  env <- if (is.null(envelope)) vector("list", length(sets)) else envelope[keys]
  stopifnot(
    length(env) == length(sets),
    is.null(envelope) ||
      (!any(vapply(env, is.null, logical(1))) &&
        all(vapply(env, function(e) identical(e$coef, tab_coef), logical(1))))
  )
  set_col <- function(st, e) {
    logvar_assert_coef_aligned(st$coef, tab_coef)
    stat_row <- if (is.null(e)) {
      ""
    } else {
      envelope_cell(
        e$ci_lower,
        e$ci_upper,
        e$side,
        cell_policy
      )
    }
    c(
      interleave(
        set_cell(
          st$set_lower,
          st$set_upper,
          st$status,
          cell_policy
        ),
        stat_row
      ),
      PAPER_NA_TOKEN, sprintf("%d", n_obs)
    )
  }
  unname(Map(set_col, sets, env))
}

# the point column itself, with its analytic and bootstrap statistic branches
paper_source_once(paper_path(
  "log_variance", "tables", "point_column.R"
))

paper_source_once(paper_path(
  "log_variance", "tables", "ppml_table_parts.R"
))
