#' Search a Log-Variance Estimator over a Quadratic Mean Set
#'
#' Scans a feasible containing-box lattice, polishes several starts and checks
#' endpoints with cold fits. A bounded status records passed numerical checks,
#' not a proof of global optimality. Budget exhaustion closes all sides.
#'
#' @param estimator A map from make_log_variance_map(), or an equivalent custom map.
#' @param quadratic A validated quadratic system.
#' @param theta_table Coordinate table with status and outer_lower/outer_upper.
#' @param seed Optional finite mean-parameter seed.
#' @param extra_starts Optional nested lists of additional candidates.
#' @param max_grid_points Optional positive grid cap.
#' @param max_fit_evals Maximum actual fit evaluations. Cache hits do not consume it.
#' @param cache Optional environment bound to estimator, sample and specification.
#' @param budget Optional shared budget environment returned by an earlier result.
#'   Its stored limit applies when supplied, independently of max_fit_evals.
#' @param cold_start_check Whether to verify accepted endpoints without cache use.
#' @param tau Finite numeric scalar label, or NA for an unlabeled result.
#' @param starts_per_side Number of separated scan starts per side.
#' @param grid_selector Optional selector returning a unique feasible subset and
#'   selector_id. Selected rows are visited in their returned order.
#' @param control Search and solver controls from log_variance_search_control().
#'   sets$GRID_POINTS_LIMIT bounds each raw lattice before allocation. Log-OLS
#'   also uses its resolved FULL_GRID_SAFETY_CAP; the smaller bound applies.
#' @return A list with schema, n_feasible and diagnostics. The schema preserves
#'   coefficient identity, side statuses, attaining points, residuals and sources.
#'   Unreliable sides may retain diagnostic values; they are not accepted endpoints.
#'   cache and budget carry reusable state, with cold checks bypassing cached fits.
#'   A mean_domain_unbounded closure reason with NA endpoints records an
#'   unavailable search on the mean domain, not mapped-side divergence.
#'   An everywhere-zero log-OLS residual closes as empty_log_domain with
#'   unreliable sides. Unproved residual-zero approaches retain geometric
#'   uncertainty. A search may proceed only when every direction they could
#'   threaten is independently certified divergent and the required aggregate
#'   signs are certified; otherwise sides remain unreliable. Opposite grid signs
#'   alone do not establish divergence.
#' @examples
#' ids <- seq_len(24L)
#' w1 <- rep(c(-2, -1, 1, 2), 6L)
#' w2 <- cbind(news = rep(c(-1, 1), 12L))
#' sample_data <- prepare_log_variance_search(
#'   w1, w2, cbind(pc1 = ids), ids, ids
#' )
#' map <- make_log_variance_map(sample_data, "logols")
#' quadratic <- list(A_i = list(matrix(1, 1L, 1L)), b_i = list(0), c_i = -0.1^2)
#' theta_table <- data.frame(
#'   coef = "news", status = "bounded", outer_lower = -0.1, outer_upper = 0.1
#' )
#' control <- log_variance_search_control()
#' control$search$GRID_N <- 7L
#' control$search$GRID_FLOOR <- 3L
#' result <- search_log_variance_map(
#'   map, quadratic, theta_table,
#'   seed = c(news = 0), max_grid_points = 7L,
#'   max_fit_evals = 100L, cold_start_check = FALSE, tau = 0.1, control = control
#' )
#' result$schema[c("coef", "lower", "upper", "lower_status", "upper_status")]
#' @export
search_log_variance_map <- function(estimator, quadratic, theta_table, seed = NULL,
                                    extra_starts = NULL, max_grid_points = NULL,
                                    max_fit_evals = Inf, cache = NULL, budget = NULL,
                                    cold_start_check = NULL, tau = NA_real_,
                                    starts_per_side = NULL, grid_selector = NULL,
                                    control = log_variance_search_control()) {
  if (is.null(cache)) cache <- new.env(parent = emptyenv())
  if (is.null(budget)) budget <- lv_set_budget(max_fit_evals)
  result <- lv_set_fixed_rng(lv_set_search(
    estimator, quadratic, theta_table, seed, extra_starts,
    max_grid_points, max_fit_evals, cache, budget, cold_start_check, tau,
    starts_per_side, grid_selector, control
  ))
  c(result, list(cache = cache, budget = budget))
}
