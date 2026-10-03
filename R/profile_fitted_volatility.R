#' Profile Fitted Volatility over a Quadratic Mean Set
#'
#' Searches the fitted log variance separately at each response date, with the
#' intercept removed. Variance and volatility are exponential transformations of
#' those dated endpoints. A bounded status records numerical checks and does not
#' certify a global optimum. Each date can have a different attaining mean point.
#'
#' @param sets Completed PPML or Harvey aggregate from profile_log_variance_map().
#'   The prepared sample must carry sorted, unique period-end response Dates.
#'   Its source cache, seed and grid cap are reused without a display refit.
#' @param quadratic Quadratic mean system for the supplied tau.
#' @param theta_table Containing coefficient table on the source news axis.
#' @param tau Positive finite slack labeling the set.
#' @param point Optional finite mean point, defaulting to the original request.
#' @param control Controls from log_variance_search_control(). The envelope fit
#'   budget must be a positive finite integer.
#' @return A list with dated data, raw search schema, point_status, metadata and
#'   diagnostics. The intercept is excluded on every scale. Unavailable mean
#'   domains retain missing endpoints and their closure diagnostics. Finite
#'   exponential overflow demotes the affected displayed side to unreliable;
#'   the raw log endpoints and attaining points remain in schema.
#' @examples
#' dates <- as.Date(c(
#'   "2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30",
#'   "2020-05-31", "2020-06-30", "2020-07-31", "2020-08-31",
#'   "2020-09-30", "2020-10-31", "2020-11-30", "2020-12-31"
#' ))
#' t <- seq_along(dates)
#' sample_data <- prepare_log_variance_search(
#'   rep(c(-2, -1, 1, 2), 3L), cbind(news = rep(c(-1, 1), 6L)),
#'   cbind(pc1 = sin(t)), dates, dates,
#'   response_ids = dates
#' )
#' tau <- 0.05
#' key <- sprintf("%.17g", tau)
#' quadratic <- list(A_i = list(matrix(1, 1L, 1L)), b_i = list(0), c_i = -tau^2)
#' theta_table <- data.frame(
#'   coef = "news", status = "bounded", outer_lower = -tau, outer_upper = tau
#' )
#' control <- log_variance_search_control()
#' control$search$grid_n <- 7L
#' control$search$grid_floor <- 3L
#' control$search$primary_grid_cap <- 7L
#' control$search$coverage_grid_cap <- 7L
#' control$search$primary_fit_budget <- 50L
#' control$search$coverage_fit_budget <- 50L
#' control$search$envelope_fit_budget <- 100L
#' control$search$primary_starts_per_side <- 1L
#' control$search$audit_starts_per_side <- 1L
#' sets <- profile_log_variance_map(
#'   sample_data, stats::setNames(list(quadratic), key),
#'   stats::setNames(list(theta_table), key), tau, "ppml",
#'   point = c(news = 0), control = control
#' )
#' envelope <- profile_fitted_volatility(
#'   sets, quadratic, theta_table, tau,
#'   control = control
#' )
#' envelope$data[c("date", "volatility_lower", "volatility_upper")]
#' @export
profile_fitted_volatility <- function(sets, quadratic, theta_table, tau,
                                      point = sets$request$point,
                                      control = log_variance_search_control()) {
  fitted_volatility_validate_set(sets, quadratic, theta_table, tau, point, control)
  design <- sets$sample$x_mat
  design[, LOG_VARIANCE_INTERCEPT_LABEL] <- 0
  target_labels <- sprintf("date_%04d", seq_along(sets$sample$response_date))
  adapter <- fitted_volatility_adapter(sets$estimator, design, target_labels, sets$cache)
  result <- search_log_variance_map(
    adapter, quadratic, theta_table,
    seed = sets$seed,
    max_grid_points = sets$grid_cap, cache = new.env(parent = emptyenv()),
    budget = lv_set_budget(control$search$envelope_fit_budget),
    starts_per_side = control$search$envelope_starts_per_side, tau = tau, control = control
  )
  point_eta <- rep(NA_real_, length(target_labels))
  point_status <- "not_in_set"
  if (!is.null(point) && lv_set_point_feasible(quadratic, point)) {
    fit <- adapter$fit_at_b(point, phase = "extra_start")
    point_status <- if (log_variance_fit_ok(fit)) "ok" else fit$fit_status
    if (identical(point_status, "ok")) point_eta <- unname(fit$coef)
  }
  fitted_volatility_result(sets, adapter, result, tau, point_eta, point_status, control)
}
