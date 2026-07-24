logvar_set_boot_draw_from_est <- function(dat, est, shared_geometry, logvar_spec) {
  bootstrap_stage_search_control_guard(logvar_spec)
  stopifnot(
    identical(shared_geometry$tau0_slot, 1L),
    is.character(shared_geometry$key_col),
    length(shared_geometry$key_col) == 1L
  )
  rows <- bootstrap_stage_logvar_rows(
    dat, est, logvar_spec, shared_geometry$key_col
  )
  pcr <- paper_normalize_model_matrix(rows$pc_data, logvar_spec$pc_preprocessing)
  colnames(pcr) <- logvar_spec$pc_cols
  point <- if (is.null(est$point0)) NULL else est$point0$theta
  tables <- shared_geometry$tables
  tau0 <- shared_geometry$tau0_slot
  tables[[tau0]] <- coef_interval_tables_from_quadratic(
    shared_geometry$quadratics[[tau0]], est$beta1r, est$beta2r
  )
  boxes <- lapply(tables, function(interval) interval$theta)
  built <- list()
  for (id in logvar_spec$estimator_ids) {
    dependencies <- logvar_spec$estimator_dependencies[[id]]
    stopifnot(all(dependencies %in% names(built)))
    inputs <- built[dependencies]
    built[[id]] <- tryCatch(
      logvar_spec$builders[[id]](
        rows$w1, rows$w2, pcr, rows$key, point, inputs
      ),
      error = function(error) NULL
    )
  }
  stats::setNames(lapply(logvar_spec$estimator_ids, function(id) {
    logvar_run_estimator(
      built[[id]], logvar_spec, boxes, shared_geometry$quadratics, point,
      shared_geometry$taus
    )
  }), logvar_spec$estimator_ids)
}

logvar_set_boot_draw <- function(dat, spec) {
  spec <- logvar_set_boot_compat_spec(spec)
  stopifnot(
    length(spec$taus) > 1L,
    identical(spec$taus[[1L]], 0),
    all(spec$taus[-1L] > 0)
  )
  est <- estimate_set_id_system(dat, spec)
  geometry <- set_id_boot_geometry(est, spec$gamma, spec$taus, spec$taus[-1L])
  geometry$key_col <- spec$key_col
  logvar_set_boot_draw_from_est(dat, est, geometry, spec)
}
