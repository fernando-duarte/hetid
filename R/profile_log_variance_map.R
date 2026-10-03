#' Profile and Audit Candidate-Indexed Log-Variance Maps
#'
#' Runs caller-ordered tau searches using supplied mean systems and containing
#' tables. PPML gets a scaling pilot and an independent Morton-grid coverage audit.
#' Harvey uses the PPML start map, recession checks and an independent sensitivity
#' audit. Log-OLS scans the full bounded lattice with residual-crossing checks.
#'
#' @param sample Output of prepare_log_variance_search().
#' @param quadratics Named list of quadratic systems keyed by sprintf("%.17g", tau).
#' @param theta_tables Containing tables with the same keys and news-column order.
#'   PPML and Harvey require a bounded containing table at the first tau.
#' @param taus Distinct finite tau values strictly between zero and one,
#'   searched in the supplied order.
#' @param method One of ppml, harvey or logols.
#' @param point Optional finite tau-zero point. An infeasible point is not published.
#' @param control Search controls from log_variance_search_control().
#' @param ppml Optional PPML result on the identical sample. A PPML request
#'   requires the identical completed search specification; Harvey reuses only
#'   its estimator and start inputs and searches the requested systems anew.
#' @return A list carrying estimator, sample, seed, point, cache, grid_cap,
#'   results, primary, audit, taus, and PPML pilot or Harvey PPML inputs.
#'   Harvey also returns stability_precheck with passed and named reasons from
#'   precheck_harvey_starts(). Tentative numerical failures can reach the existing
#'   fitter's recovery, while passed remains FALSE. This screen neither establishes
#'   fit failure nor accepts an endpoint; the fitting and endpoint gates still apply.
#'   This contains numerical search evidence, not a global-optimality certificate.
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
#'   sample_data, quadratics, theta_tables, taus, "logols",
#'   point = c(news = 0), control = control
#' )
#' sets$results[[keys[1L]]]$schema[c("coef", "lower", "upper")]
#' @export
profile_log_variance_map <- function(sample, quadratics, theta_tables, taus,
                                     method = c("ppml", "harvey", "logols"),
                                     point = NULL, control = log_variance_search_control(),
                                     ppml = NULL) {
  method <- lv_set_method(method)
  lv_set_validate_sample(sample)
  lv_set_validate_control(control)
  lv_set_validate_path(sample, quadratics, theta_tables, taus, point)
  if (!is.null(ppml)) {
    assert_bad_argument_ok(
      is.list(ppml) && is.list(ppml$estimator) && is.list(ppml$estimator$metadata) &&
        identical(ppml$estimator$metadata$estimator, "ppml") &&
        identical(ppml$estimator$metadata$sample_id, sample$sample_id),
      "ppml must use the identical sample", "ppml"
    )
    if (method == "ppml") {
      lv_set_check_request(ppml, sample, quadratics, theta_tables, taus, point, control)
      return(ppml)
    }
  }
  id <- lv_set_request_id(sample, quadratics, theta_tables, taus, point, control)
  path <- list(quadratics = quadratics, point = point, sample_id = sample$sample_id)
  bounds <- list(theta = theta_tables)
  tau_control <- list(baseline = taus[[1L]], display = taus)
  result <- lv_set_fixed_rng(lv_set_build_map(
    method, sample, path, bounds,
    tau_control, control, ppml
  ))
  lv_set_bind_request(result, id, point)
}

lv_set_validate_path <- function(sample, quadratics, theta_tables, taus, point) {
  validate_profile_taus(taus, "taus")
  keys <- profile_tau_key(taus)
  assert_bad_argument_ok(!anyDuplicated(keys) && is.list(quadratics) &&
    is.list(theta_tables) && all(keys %in% names(quadratics)) &&
    all(keys %in% names(theta_tables)), "all taus need systems and tables", "taus")
  for (key in keys) {
    quadratic_validate_system(quadratics[[key]])
    assert_bad_argument_ok(
      length(quadratics[[key]]$b_i[[1L]]) == ncol(sample$w2),
      "quadratic theta dimension must match the sample", "quadratics"
    )
    assert_bad_argument_ok(
      is.data.frame(theta_tables[[key]]) &&
        identical(theta_tables[[key]]$coef, colnames(sample$w2)),
      "theta_tables must follow the news-column order", "theta_tables"
    )
  }
  if (!is.null(point)) {
    lv_set_axis(point, colnames(sample$w2), "point")
  }
  invisible(TRUE)
}

lv_set_map_context <- function(sample, path, theta_tables, tau_control, grid_cap, control) {
  tab <- theta_tables[[profile_tau_key(tau_control$baseline)]]
  lv_set_assert(
    !is.null(tab), all(tab$status == "bounded"),
    identical(colnames(sample$w2), tab$coef)
  )
  quadratic <- lv_set_quadratic(path, tau_control$baseline)
  point <- unname(path$point)
  point_feasible <- !is.null(point) && lv_set_point_feasible(quadratic, point)
  bounds <- profile_containing_box(tab)
  mesh <- lv_set_coarsen_grid(lv_set_feasible_grid(
    quadratic, bounds$lower, bounds$upper,
    control$search$grid_n, control
  ), grid_cap)
  lv_set_assert(nrow(mesh) > 0L)
  anchor <- if (point_feasible) point else mesh[1L, ]
  list(
    point = if (point_feasible) point else NULL, grid = mesh, anchor = anchor,
    anchor_source = if (point_feasible) "tau_zero_point" else "baseline_grid_first",
    seed = anchor
  )
}
