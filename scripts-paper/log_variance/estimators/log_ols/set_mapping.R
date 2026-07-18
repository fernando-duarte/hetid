# Map the log-OLS estimator over one mean-equation identified set.

logvar_logols_set_mapper <- function(
  estimator,
  mean_eq,
  b_point,
  grid_n,
  grid_floor,
  qtr,
  w2_names
) {
  function(tau, b_tab) {
    stopifnot(identical(b_tab$coef, w2_names))
    qs <- tau_quadratic_system(mean_eq$gamma, tau, mean_eq$moments)
    result <- logvar_engine_set_at_tau(
      estimator,
      qs,
      b_tab,
      b_seed = b_point,
      grid_n = grid_n,
      grid_floor = grid_floor,
      cold_start_check = FALSE,
      tau = tau
    )
    crossings <- result$domain_info$cross_all
    list(
      table = result$table,
      n_cross = result$n_cross,
      n_feasible = result$n_feasible,
      cross_qtr = if (is.null(crossings)) NULL else qtr[crossings],
      schema = result$schema
    )
  }
}

logvar_logols_sets <- function(
  estimator,
  mean_eq,
  b_point,
  grid_n,
  grid_floor,
  qtr,
  w2_names
) {
  mapper <- logvar_logols_set_mapper(
    estimator, mean_eq, b_point, grid_n, grid_floor, qtr, w2_names
  )
  sets <- Map(
    mapper,
    mean_eq$tau_display,
    lapply(mean_eq$set_tables, `[[`, "theta")
  )
  names(sets) <- names(mean_eq$set_tables)
  sets
}
