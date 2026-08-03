# The point-estimate table column shared by the PPML parts and the Harvey panel,
# and the two statistic constructions it renders. Split out of
# table_formatting.R, which owns the remaining cell formatters.
#
# The two constructions never mix within a column. The analytic branch divides
# the estimate by a stored quasi-maximum-likelihood standard error; the
# estimator's reference column always uses it. The bootstrap branch reads a
# point_t_statistic frame -- the estimate over the robust scale of its own
# bootstrap draws, with the frame's own two-sided normal p-value -- and the
# tau = 0 column uses it in every table, because the pipeline now publishes
# every log-variance table after the bootstrap stage.
#
# Which branch a column takes is decided by point_stat alone, never by the
# presence of a tau > 0 confidence envelope: a table can print a bootstrap
# tau = 0 statistic under bare identified-set ranges, and does.

# vals over the stored analytic SE, with the SE column selected by se_type. An
# absent frame or a misaligned row order is a wiring bug: fail loud rather than
# silently blank while the notes claim standard errors are reported.
logvar_analytic_point_stat <- function(vals, se_frame, se_type, se_types,
                                       tab_coef) {
  key <- match.arg(se_type, se_types) # loud on an unknown type
  stopifnot(
    !is.null(se_frame), key %in% names(se_frame),
    identical(se_frame$coef, tab_coef) # row order aligned to coefs
  )
  se <- se_frame[[key]]
  statistic <- vals / se
  list(
    statistic = statistic,
    p_value = 2 * stats::pnorm(-abs(statistic)),
    missing = !is.finite(se)
  )
}

# One estimator's tau = 0 statistic frame from the unified bootstrap stage. A
# missing frame is a wiring error and must not be tolerated: point_stat NULL
# falls back to the analytic ratio, so a silent NULL would restore exactly the
# mixed convention this construction removes.
logvar_boot_point_stat <- function(boot, estimator_id) {
  frame <- boot$point_t[[estimator_id]]
  stopifnot(is.data.frame(frame))
  frame
}

# the statistic and p-value a point_t_statistic frame already carries. Its
# statistic is NA for every coefficient its gate refused, which is exactly the
# "attempted but unavailable" case the renderer marks with the NA token.
#
# The frame's own point is the bootstrap anchor's full-sample evaluation, while
# vals is the estimator's published point column. They are one number by
# construction, because the anchor reads each estimator's published recipe
# (logvar_point_record). Assert it rather than trust it: a drift would print a
# ratio beneath a number the ratio does not describe, which is the silent error
# this whole construction exists to remove. A coefficient the gate refused keeps
# a finite point and an NA statistic, so the refusal path does not trip this.
logvar_bootstrap_point_stat <- function(point_stat, tab_coef, vals) {
  stopifnot(
    is.data.frame(point_stat),
    all(c("coef", "point", "statistic", "p_value") %in% names(point_stat)),
    identical(point_stat$coef, tab_coef), # row order aligned to coefs
    length(point_stat$point) == length(vals)
  )
  finite_vals <- unname(is.finite(vals))
  stopifnot(
    identical(finite_vals, unname(is.finite(point_stat$point))),
    !any(finite_vals) || isTRUE(all.equal(
      unname(vals[finite_vals]),
      unname(point_stat$point[finite_vals])
    ))
  )
  list(
    statistic = point_stat$statistic,
    p_value = point_stat$p_value,
    missing = !is.finite(point_stat$statistic)
  )
}

# With se_type NULL and no point_stat the interleaved statistic rows stay blank,
# exactly as before SEs. Otherwise values carry their statistic in the stat row
# with stars from the standard-normal approximation. point_stat wins where both
# are supplied, so the tau = 0 column reports the bootstrap statistic while the
# reference column beside it keeps the analytic ratio. An all-NA statistic (a
# point not certified feasible) keeps the key column and blanks its stat rows.
# se_types is the estimator's validated variant vector.
logvar_se_point_col <- function(
  vals,
  se_frame,
  se_type,
  se_types,
  tab_coef,
  n_obs,
  cell_policy = PAPER_REPORTING_CONTROL$cells$log_variance,
  point_stat = NULL
) {
  stat <- if (!is.null(point_stat)) {
    logvar_bootstrap_point_stat(point_stat, tab_coef, vals)
  } else if (!is.null(se_type)) {
    logvar_analytic_point_stat(vals, se_frame, se_type, se_types, tab_coef)
  }
  if (is.null(stat)) {
    return(c(
      interleave(logvar_fmt(vals, cell_policy), ""),
      PAPER_NA_TOKEN,
      sprintf("%d", n_obs)
    ))
  }
  stars <- sig_stars(stat$p_value)
  cells <- ifelse(
    stars == "" | !is.finite(stat$statistic),
    logvar_fmt(vals, cell_policy),
    sprintf("%s$%s$", logvar_fmt(vals, cell_policy), stars)
  )
  # a finite coefficient whose statistic failed its gate has no t-stat: mark it
  # "--" (statistic unavailable), never a blank stat row, which beside the
  # star-less coefficient would read as "tested, not significant"
  stat_row <- ifelse(
    is.finite(stat$statistic),
    sprintf(
      "(%s)",
      paper_format_number(
        stat$statistic,
        PAPER_REPORTING_CONTROL$cells$statistic_digits,
        "na"
      )
    ),
    ifelse(is.finite(vals) & stat$missing, PAPER_NA_TOKEN, "")
  )
  c(interleave(cells, stat_row), PAPER_NA_TOKEN, sprintf("%d", n_obs))
}
