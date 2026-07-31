# Estimator-neutral coefficient-panel assembly. Estimator notation, the
# reference header, and standard-error capability remain explicit inputs.
#
# envelope and point_stat are independent switches and must stay that way.
# envelope decides whether a confidence row appears beneath each tau > 0 set
# cell; point_stat decides whether the tau = 0 column reports the bootstrap
# statistic instead of the analytic ratio. A table can take the second without
# the first, which is what the published document does per estimator.

# Estimator notation for the panels the per-estimator document stacks. They live
# beside the assembler, not inside the document, so the checks that pin a panel's
# row labels read the same literal the published page does. The intercepts are
# distinct normalizations: theta^H_0 is the Gaussian multiplicative-variance one
# and theta^0.5_0 the median one, never shared with each other or theta^log_0.
LOGVAR_HARVEY_PANEL_SPEC <- list(
  intercept_label = "$\\theta^{H}_0$",
  slope_template = "$\\theta^{H}_{%d,R}$",
  reference_header = "Reference"
)
LOGVAR_LAD_PANEL_SPEC <- list(
  intercept_label = "$\\theta^{0.5}_0$",
  slope_template = "$\\theta^{0.5}_{%d,R}$",
  reference_header = "Reference"
)

logvar_estimator_panel_parts <- function(
  result,
  n_obs,
  tau_display,
  spec,
  se_type = NULL,
  se_types = NULL,
  envelope = NULL,
  cell_policy = PAPER_REPORTING_CONTROL$cells$log_variance,
  point_stat = NULL
) {
  required <- c(
    "intercept_label",
    "slope_template",
    "reference_header"
  )
  stopifnot(
    is.list(result),
    is.list(spec),
    all(required %in% names(spec)),
    length(n_obs) == 1L,
    n_obs >= 1L
  )
  tab <- result$table
  if (!is.null(spec$expected_coef)) {
    logvar_assert_coef_aligned(tab$coef, spec$expected_coef)
  }
  keys <- vapply(tau_display, paper_tau_key, character(1))
  sets <- result$sets[keys]
  stopifnot(
    !any(vapply(sets, is.null, logical(1))),
    all(vapply(
      sets,
      function(set) identical(set$coef, tab$coef),
      logical(1)
    ))
  )
  n_slope <- length(tab$coef) - 1L
  labels <- c(
    spec$intercept_label,
    sprintf(spec$slope_template, seq_len(n_slope))
  )
  rows <- c(interleave(labels, ""), "$R^2$", "$N$")
  se <- if (is.null(result$se)) {
    list(reference = NULL, point = NULL)
  } else {
    result$se
  }
  point_col <- function(values, frame, stat = NULL) {
    logvar_se_point_col(
      values,
      frame,
      se_type,
      se_types,
      tab$coef,
      n_obs,
      cell_policy,
      point_stat = stat
    )
  }
  columns <- c(
    list(
      point_col(tab$reference, se$reference),
      point_col(tab$point, se$point, point_stat)
    ),
    logvar_set_envelope_cols(
      sets,
      envelope,
      keys,
      tab$coef,
      n_obs,
      cell_policy
    )
  )
  list(
    table = tab,
    sets = sets,
    rows = rows,
    columns = columns,
    headers = logvar_estimator_headers(spec$reference_header, tau_display),
    n_obs = n_obs
  )
}
