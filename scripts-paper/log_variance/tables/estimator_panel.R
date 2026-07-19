# Estimator-neutral coefficient-panel assembly. Estimator notation, the
# reference header, and standard-error capability remain explicit inputs.

logvar_estimator_panel_parts <- function(
  result,
  n_obs,
  tau_display,
  spec,
  se_type = NULL,
  se_types = NULL,
  envelope = NULL
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
    stopifnot(identical(tab$coef, spec$expected_coef))
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
  point_col <- function(values, frame) {
    logvar_se_point_col(
      values,
      frame,
      se_type,
      se_types,
      tab$coef,
      n_obs
    )
  }
  columns <- c(
    list(
      point_col(tab$reference, se$reference),
      point_col(tab$point, se$point)
    ),
    logvar_set_envelope_cols(
      sets,
      envelope,
      keys,
      tab$coef,
      n_obs
    )
  )
  list(
    table = tab,
    sets = sets,
    rows = rows,
    columns = columns,
    headers = c(
      spec$reference_header,
      "$\\tau{=}0$",
      sprintf(
        "$\\tau{=}%s$",
        paper_format_tau(tau_display)
      )
    ),
    n_obs = n_obs
  )
}

logvar_estimator_panel_fragment <- function(
  result,
  n_obs,
  tau_display,
  spec,
  caption,
  label,
  se_type = NULL,
  se_types = NULL,
  envelope = NULL
) {
  parts <- logvar_estimator_panel_parts(
    result,
    n_obs,
    tau_display,
    spec,
    se_type,
    se_types,
    envelope
  )
  style <- PAPER_TABLE_STYLE$coefficient
  build_simple_latex_table(
    parts$rows,
    parts$columns,
    col_headers = parts$headers,
    caption = caption,
    label = label,
    fontsize = style$fontsize,
    rule_after = style$row_stride
  )
}
