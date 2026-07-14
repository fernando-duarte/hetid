# Cell formatters and PPML table-part assembly shared by the primary
# log_var_eq_table.R and the combined estimator panels: the "--" non-finite
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

# Canonical PPML table parts: the quasi-Poisson reference and Lewbel-point
# columns followed by exact-keyed display-tau hulls. Both the primary table and
# the combined panels consume this one assembly path so their PPML cells cannot
# drift. The statistic slots and R-squared row are blank by construction.
logvar_ppml_table_parts <- function(ppml, tau_display, n_pc_r, se_type = NULL) {
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
  # a point column: with se_type NULL (default) the stat rows are blank, exactly
  # as before. With se_type set, the stored SE frame must be present and aligned
  # (fail loud rather than silently blank while the notes claim SEs are
  # reported); values then carry t = coef/se in the interleaved stat row with
  # stars from the standard-normal approximation. An all-NA frame (tau = 0 point
  # not feasible) keeps the key column and renders a blank stat row per cell.
  point_col <- function(vals, se_frame) {
    if (is.null(se_type)) {
      return(c(interleave(fmt(vals), ""), "--", sprintf("%d", n_obs)))
    }
    key <- match.arg(se_type, LOGVAR_PPML_SE_TYPES) # loud on an unknown type
    stopifnot(
      !is.null(se_frame), key %in% names(se_frame),
      identical(se_frame$coef, tab$coef) # row order aligned to coefs
    )
    se <- se_frame[[key]]
    t_stat <- vals / se
    p_val <- 2 * stats::pnorm(-abs(t_stat))
    stars <- sig_stars(p_val)
    cells <- ifelse(
      stars == "" | !is.finite(t_stat), fmt(vals),
      sprintf("%s$%s$", fmt(vals), stars)
    )
    stat_row <- ifelse(is.finite(t_stat), sprintf("(%.2f)", t_stat), "")
    c(interleave(cells, stat_row), "--", sprintf("%d", n_obs))
  }
  columns <- c(
    list(
      point_col(tab$reference, ppml$se$reference),
      point_col(tab$point, ppml$se$point)
    ),
    unname(lapply(sets, function(st) {
      c(
        interleave(set_cell(st$set_lower, st$set_upper, st$status), ""),
        "--", sprintf("%d", n_obs)
      )
    }))
  )
  list(
    table = tab, sets = sets, rows = rows, columns = columns,
    headers = c(
      "OLS", "$\\tau{=}0$", sprintf("$\\tau{=}%.2g$", tau_display)
    ),
    n_obs = n_obs
  )
}
