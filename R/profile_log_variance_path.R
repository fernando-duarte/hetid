#' Evaluate a Log-Variance Path with Nesting Checks
#'
#' @param sets Result of profile_log_variance_map().
#' @param quadratics,theta_tables Explicit systems and containing tables by tau key.
#' @param taus Increasing finite tau grid strictly between zero and one.
#' @param control Search controls from log_variance_search_control().
#' @details Retained display searches must match the supplied systems, tables
#'   and controls; incompatible aggregates raise a structured argument error.
#' @return Numeric rows with source labels and diagnostics. Thin-lattice bounded
#'   endpoints are downgraded; remaining nesting violations are unreliable.
#'   Known pre-grid closures retain NA endpoints/counts and are identified by
#'   tau in diagnostics$pre_grid_closures for grid rows and
#'   diagnostics$display_pre_grid_closures for retained display rows.
#'   Missing counts otherwise raise an error.
#' @examples
#' ids <- seq_len(24L)
#' w1 <- rep(c(-2, -1, 1, 2), 6L)
#' w2 <- cbind(news = rep(c(-1, 1), 12L))
#' sample_data <- prepare_log_variance_search(
#'   w1, w2, cbind(pc1 = ids), ids, ids
#' )
#' taus <- c(0.05, 0.1)
#' keys <- sprintf("%.17g", taus)
#' quadratics <- stats::setNames(lapply(taus, function(tau) {
#'   list(A_i = list(matrix(1, 1L, 1L)), b_i = list(0), c_i = -tau^2)
#' }), keys)
#' theta_tables <- stats::setNames(lapply(taus, function(tau) {
#'   data.frame(coef = "news", status = "bounded", outer_lower = -tau, outer_upper = tau)
#' }), keys)
#' control <- log_variance_search_control()
#' control$search$grid_n <- 7L
#' control$search$grid_floor <- 3L
#' sets <- profile_log_variance_map(
#'   sample_data, quadratics, theta_tables, taus[1L], "logols",
#'   point = c(news = 0), control = control
#' )
#' path <- profile_log_variance_path(sets, quadratics, theta_tables, taus, control)
#' path$rows[c("tau", "coef", "lower_status", "upper_status", "source")]
#' @export
profile_log_variance_path <- function(sets, quadratics, theta_tables, taus,
                                      control = log_variance_search_control()) {
  lv_set_validate_aggregate(sets)
  lv_set_validate_sample(sets$sample)
  lv_set_validate_path(sets$sample, quadratics, theta_tables, taus, sets$point)
  assert_bad_argument_ok(all(diff(taus) > 0), "taus must increase", "taus")
  lv_set_validate_control(control)
  lv_set_check_request(
    sets, sets$sample, quadratics, theta_tables, sets$taus,
    sets$request$point, control
  )
  path <- list(
    quadratics = quadratics, point = sets$point,
    sample_id = sets$sample$sample_id
  )
  bounds <- list(theta = theta_tables, grid = taus)
  lv_set_bounds_by_tau(sets, path, bounds, control)
}
