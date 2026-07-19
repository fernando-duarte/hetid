# Cell formatters and PPML table-part assembly shared by the primary
# render_ppml_table.R and the combined estimator panels: the "--" non-finite
# formatter, the status-aware identified-set cell, the coefficient/statistic
# row interleaver, and the canonical PPML rows and columns. Definitions only.

paper_source_once(paper_path(
  "log_variance", "tables", "panel_block.R"
))
paper_source_once(paper_path(
  "support", "reporting", "inference.R"
))
paper_source_once(paper_path("support", "reporting", "cells.R"))

# NA and non-finite values render "--"; upstream diagnostics retain the reason
# a value is unavailable rather than hiding it behind a numeric token.
fmt <- function(x) {
  policy <- PAPER_REPORTING_CONTROL$cells$log_variance
  paper_format_number(x, policy$digits, policy$numeric_missing)
}

# an unreliable or upstream-propagated (NA-endpoint) cell renders its status
# word; certified one-sided divergence renders a half-infinite range; a
# degenerate interval (point-identified) is left blank as in the structural
# table
set_cell <- function(lo, hi, status) {
  policy <- PAPER_REPORTING_CONTROL$cells$log_variance
  paper_format_set_interval(
    lo,
    hi,
    status,
    digits = policy$digits,
    status_mode = policy$status_mode,
    na_as_status = policy$na_as_status,
    infinite_bounds = policy$infinite_bounds,
    degenerate_rtol = policy$degenerate_rtol
  )
}

# Side-aware confidence-envelope cell: the moving-block bootstrap outer
# containment interval for one set endpoint (log_var_eq_set_boot), rendered on
# the row beneath its set_cell. A genuinely one-sided set (side "upper"/
# "lower") keeps its live endpoint bracket-closed and the unattained infinite
# endpoint parenthesized, exactly like set_cell's own infinite-bound cells; a
# suppressed cell (side "none", or a non-finite endpoint) renders blank.
# Vectorized like set_cell.
envelope_cell <- function(ci_lo, ci_hi, side) {
  paper_format_endpoint_envelope(
    ci_lo,
    ci_hi,
    side,
    PAPER_REPORTING_CONTROL$cells$log_variance$digits
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
logvar_set_envelope_cols <- function(sets, envelope, keys, tab_coef, n_obs) {
  env <- if (is.null(envelope)) vector("list", length(sets)) else envelope[keys]
  stopifnot(
    length(env) == length(sets),
    is.null(envelope) ||
      (!any(vapply(env, is.null, logical(1))) &&
        all(vapply(env, function(e) identical(e$coef, tab_coef), logical(1))))
  )
  set_col <- function(st, e) {
    stopifnot(identical(st$coef, tab_coef))
    stat_row <- if (is.null(e)) "" else envelope_cell(e$ci_lower, e$ci_upper, e$side)
    c(
      interleave(set_cell(st$set_lower, st$set_upper, st$status), stat_row),
      PAPER_NA_TOKEN, sprintf("%d", n_obs)
    )
  }
  unname(Map(set_col, sets, env))
}

# A point-estimate table column shared by the PPML parts and the Harvey panel:
# with se_type NULL (default) the interleaved statistic rows stay blank, exactly
# as before SEs. With se_type set, the stored SE frame must be present and
# aligned to tab_coef (fail loud rather than silently blank while the notes claim
# SEs are reported); values then carry t = coef/se in the stat row with stars
# from the standard-normal (QMLE) approximation. An all-NA frame (a point not
# certified feasible) keeps the key column and renders a blank stat row per cell.
# se_types is the estimator's validated variant vector.
logvar_se_point_col <- function(vals, se_frame, se_type, se_types, tab_coef,
                                n_obs) {
  if (is.null(se_type)) {
    return(c(interleave(fmt(vals), ""), PAPER_NA_TOKEN, sprintf("%d", n_obs)))
  }
  key <- match.arg(se_type, se_types) # loud on an unknown type
  stopifnot(
    !is.null(se_frame), key %in% names(se_frame),
    identical(se_frame$coef, tab_coef) # row order aligned to coefs
  )
  se <- se_frame[[key]]
  t_stat <- vals / se
  stars <- sig_stars(2 * stats::pnorm(-abs(t_stat)))
  cells <- ifelse(
    stars == "" | !is.finite(t_stat), fmt(vals),
    sprintf("%s$%s$", fmt(vals), stars)
  )
  # a finite coefficient whose SE failed the conditioning gate has no t-stat:
  # mark it "--" (SE unavailable), never a blank stat row, which beside the
  # star-less coefficient would read as "tested, not significant"
  stat_row <- ifelse(
    is.finite(t_stat), sprintf("(%.2f)", t_stat),
    ifelse(is.finite(vals) & !is.finite(se), PAPER_NA_TOKEN, "")
  )
  c(interleave(cells, stat_row), PAPER_NA_TOKEN, sprintf("%d", n_obs))
}

# The point-column conditioning caveat shared verbatim by both estimators' SE
# notes: tau = 0 conditions on the plug-in news vector, while the set columns
# carry either a separate endpoint envelope or an explicit deferral.
logvar_se_note_caveat <- function(set_endpoint_inference = FALSE) {
  prefix <- paste(
    "The $\\tau{=}0$ statistics condition on the plug-in Lewbel news vector",
    "$b_N$ and do not propagate its first-stage sampling error; $\\tau{>}0$",
    "set columns are identified-set ranges, not point estimates."
  )
  if (isTRUE(set_endpoint_inference)) {
    return(paste(
      prefix,
      "Their moving-block-bootstrap outer confidence envelopes are reported",
      "separately beneath the set cells."
    ))
  }
  paste(
    prefix,
    "No standard error is attached; moving-block-bootstrap set-endpoint",
    "uncertainty is deferred."
  )
}

# Canonical PPML table parts: the quasi-Poisson reference and Lewbel-point
# columns followed by exact-keyed display-tau hulls. Both the primary table and
# the combined panels consume this one assembly path so their PPML cells cannot
# drift. The statistic slots and R-squared row are blank by construction, unless
# envelope supplies a per-tau (paper_tau_key-keyed) confidence-envelope
# frame (log_var_eq_set_boot$ppml), in which case the blank row beneath each set
# cell instead renders that tau's per-coef envelope_cell. NULL (the default)
# keeps every column byte-identical to the pre-envelope renderer.
paper_source_once(paper_path(
  "log_variance", "tables", "estimator_panel.R"
))

logvar_ppml_table_parts <- function(ppml, tau_display, n_pc_r, se_type = NULL,
                                    envelope = NULL) {
  model <- PAPER_ANALYSIS_CONTRACT$model
  expected_coef <- c(
    model$intercept_col,
    model$return_pc_cols[seq_len(n_pc_r)]
  )
  stopifnot(length(expected_coef) == n_pc_r + 1L)
  logvar_estimator_panel_parts(
    ppml,
    ppml$sample$n,
    tau_display,
    list(
      intercept_label = "$\\theta_0$",
      slope_template = "$\\theta_{%d,R}$",
      reference_header = "OLS",
      expected_coef = expected_coef
    ),
    se_type = se_type,
    se_types = LOGVAR_PPML_SE_TYPES,
    envelope = envelope
  )
}
