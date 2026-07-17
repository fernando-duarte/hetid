# Cell formatters and PPML table-part assembly shared by the primary
# render_ppml_table.R and the combined estimator panels: the "--" non-finite
# formatter, the status-aware identified-set cell, the coefficient/statistic
# row interleaver, and the canonical PPML rows and columns. Definitions only.

# NA and non-finite values render "--"; upstream diagnostics retain the reason
# a value is unavailable rather than hiding it behind a numeric token.
fmt <- function(x) ifelse(!is.finite(x), "--", sprintf("%.3f", x))

# significance stars from a p-value at the shared 10/5/1% thresholds
sig_stars <- function(p) {
  ifelse(!is.finite(p), "",
    ifelse(p < 0.01, "^{***}",
      ifelse(p < 0.05, "^{**}", ifelse(p < 0.10, "^{*}", ""))
    )
  )
}
# an unreliable or upstream-propagated (NA-endpoint) cell renders its status
# word; certified one-sided divergence renders a half-infinite range; a
# degenerate interval (point-identified) is left blank as in the structural
# table
set_cell <- function(lo, hi, status) {
  ifelse(
    status == "unreliable" | is.na(lo) | is.na(hi), status,
    ifelse(
      is.infinite(lo) & is.infinite(hi), "unbounded",
      ifelse(
        is.infinite(lo), sprintf("$(-\\infty,\\,%.3f]$", hi),
        ifelse(
          is.infinite(hi), sprintf("$[%.3f,\\,\\infty)$", lo),
          ifelse(lo == hi, "", sprintf("$[%.3f,\\,%.3f]$", lo, hi))
        )
      )
    )
  )
}

# coefficient rows interleaved with statistic rows (blank where inference is
# not reported)
interleave <- function(a, b) as.vector(rbind(a, b))

# Side-aware confidence-envelope cell: the moving-block bootstrap outer
# containment interval for one set endpoint (log_var_eq_set_boot), rendered on
# the row beneath its set_cell. A genuinely one-sided set (side "upper"/
# "lower") keeps its live endpoint bracket-closed and the unattained infinite
# endpoint parenthesized, exactly like set_cell's own infinite-bound cells; a
# suppressed cell (side "none", or a non-finite endpoint) renders blank.
# Vectorized like set_cell.
envelope_cell <- function(ci_lo, ci_hi, side) {
  side <- ifelse(is.na(side), "none", side)
  ifelse(
    side == "none" | is.na(ci_lo) | is.na(ci_hi), "",
    ifelse(
      side == "upper", sprintf("$(-\\infty,\\,%.3f]$", ci_hi),
      ifelse(
        side == "lower", sprintf("$[%.3f,\\,\\infty)$", ci_lo),
        sprintf("$[%.3f,\\,%.3f]$", ci_lo, ci_hi)
      )
    )
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
      "--", sprintf("%d", n_obs)
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
    return(c(interleave(fmt(vals), ""), "--", sprintf("%d", n_obs)))
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
    ifelse(is.finite(vals) & !is.finite(se), "--", "")
  )
  c(interleave(cells, stat_row), "--", sprintf("%d", n_obs))
}

# The point-column conditioning caveat shared verbatim by both estimators' SE
# notes: the tau = 0 statistics condition on the plug-in Lewbel news vector and
# the tau > 0 set columns carry no standard error (the moving-block bootstrap is
# deferred).
logvar_se_note_caveat <- function() {
  paste(
    "The $\\tau{=}0$ statistics condition on the plug-in Lewbel news vector",
    "$b_N$ and do not propagate its first-stage sampling error; $\\tau{>}0$",
    "set columns are identified-set ranges, not point estimates, so no",
    "standard error is attached (the moving-block bootstrap for set-endpoint",
    "uncertainty is deferred)."
  )
}

# Canonical PPML table parts: the quasi-Poisson reference and Lewbel-point
# columns followed by exact-keyed display-tau hulls. Both the primary table and
# the combined panels consume this one assembly path so their PPML cells cannot
# drift. The statistic slots and R-squared row are blank by construction, unless
# envelope supplies a per-tau (sprintf("%.17g", tau)-keyed) confidence-envelope
# frame (log_var_eq_set_boot$ppml), in which case the blank row beneath each set
# cell instead renders that tau's per-coef envelope_cell. NULL (the default)
# keeps every column byte-identical to the pre-envelope renderer.
logvar_ppml_table_parts <- function(ppml, tau_display, n_pc_r, se_type = NULL,
                                    envelope = NULL) {
  tab <- ppml$table
  expected_coef <- c("(Intercept)", paste0("l.pc", seq_len(n_pc_r)))
  keys <- sprintf("%.17g", tau_display)
  sets <- ppml$sets[keys]
  stopifnot(
    identical(tab$coef, expected_coef),
    length(sets) == length(tau_display),
    !any(vapply(sets, is.null, logical(1))),
    all(vapply(sets, function(st) identical(st$coef, tab$coef), logical(1)))
  )
  n_obs <- ppml$sample$n
  coef_labels <- c(
    "$\\theta_0$", sprintf("$\\theta_{%d,R}$", seq_len(n_pc_r))
  )
  rows <- c(interleave(coef_labels, ""), "$R^2$", "$N$")
  point_col <- function(vals, se_frame) {
    logvar_se_point_col(
      vals, se_frame, se_type, LOGVAR_PPML_SE_TYPES, tab$coef, n_obs
    )
  }
  columns <- c(
    list(
      point_col(tab$reference, ppml$se$reference),
      point_col(tab$point, ppml$se$point)
    ),
    logvar_set_envelope_cols(sets, envelope, keys, tab$coef, n_obs)
  )
  list(
    table = tab, sets = sets, rows = rows, columns = columns,
    headers = c(
      "OLS", "$\\tau{=}0$", sprintf("$\\tau{=}%.2g$", tau_display)
    ),
    n_obs = n_obs
  )
}
