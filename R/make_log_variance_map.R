#' Controls for a Candidate-Indexed Log-Variance Estimator
#'
#' @param method One of ppml, harvey or logols.
#' @return The frozen estimator controls, separate from search controls.
#' @examples
#' control <- log_variance_map_control("logols")
#' control$SCAN_CHUNK_SIZE <- 32L
#' control$SCAN_CHUNK_SIZE
#' @export
log_variance_map_control <- function(method = c("ppml", "harvey", "logols")) {
  method <- lv_set_method(method)
  switch(method,
    ppml = lv_set_ppml_control(),
    harvey = lv_set_harvey_control(),
    logols = lv_set_logols_control()
  )
}

#' Build a Candidate-Indexed Log-Variance Estimator
#'
#' PPML and Harvey delegate response fitting to make_log_variance_fitter().
#' Log-OLS delegates to evaluate_log_projection() with method log. These maps
#' expose analytic Jacobians and cache identities for search_log_variance_map().
#'
#' @param sample Output of prepare_log_variance_search().
#' @param method One of ppml, harvey or logols.
#' @param point Optional tau-zero point. Named theta vectors must match the news
#'   columns exactly in order; unnamed vectors are positional.
#' @param anchor PPML response-scale and fallback anchor. Defaults to point;
#'   required when point is NULL for PPML.
#' @param anchor_source Nonempty label describing the anchor.
#' @param response_scale Positive fixed PPML response scale.
#' @param ppml PPML estimator on the identical sample, required by Harvey.
#' @param logols_coef OLS coefficients of log squared benchmark residuals,
#'   required by Harvey, intercept first.
#' @param control Estimator controls from log_variance_map_control(). Log-OLS
#'   SCAN_CHUNK_SIZE must be a positive finite integer.
#' @return A list with metadata, coef_labels, fit_at_b, jacobian_at_b, and optional
#'   point/start, batch-scan, objective and domain hooks. Raw fit_status is distinct
#'   from the projection evaluator's status. Log-OLS coefficients are projections,
#'   not conditional-variance coefficients without additional assumptions.
#' @examples
#' ids <- seq_len(24L)
#' w1 <- rep(c(-2, -1, 1, 2), 6L)
#' w2 <- cbind(news = rep(c(-1, 1), 12L))
#' sample_data <- prepare_log_variance_search(
#'   w1, w2, cbind(pc1 = ids), ids, ids
#' )
#' map <- make_log_variance_map(sample_data, "logols")
#' fit <- map$fit_at_b(c(news = 0.02))
#' fit$coef
#' map$jacobian_at_b(c(news = 0.02))
#' @export
make_log_variance_map <- function(sample, method = c("ppml", "harvey", "logols"),
                                  point = NULL, anchor = point,
                                  anchor_source = "supplied", response_scale = 1,
                                  ppml = NULL, logols_coef = NULL,
                                  control = log_variance_map_control(method)) {
  method <- lv_set_method(method)
  lv_set_validate_sample(sample)
  validate_point <- function(value, arg) lv_set_axis(value, colnames(sample$w2), arg)
  if (!is.null(point)) validate_point(point, "point")
  defaults <- log_variance_map_control(method)
  assert_bad_argument_ok(
    is.list(control) && identical(names(control), names(defaults)),
    "control must have the estimator's default fields", "control"
  )
  if (method == "ppml") {
    validate_point(anchor, "anchor")
    lv_set_positive(response_scale, "response_scale")
    assert_bad_argument_ok(is.character(anchor_source) && length(anchor_source) == 1L &&
      !is.na(anchor_source) && nzchar(anchor_source), "invalid anchor_source", "anchor_source")
    return(lv_set_ppml_estimator(sample, point, anchor, anchor_source, response_scale, control))
  }
  if (method == "harvey") {
    lv_set_validate_harvey_start(ppml, sample, logols_coef)
    return(lv_set_harvey_estimator(sample, point, ppml, logols_coef, control))
  }
  lv_set_positive(control$SCAN_CHUNK_SIZE, "SCAN_CHUNK_SIZE", integer = TRUE)
  lv_set_logols_estimator(sample, control)
}

lv_set_validate_sample <- function(sample) {
  assert_bad_argument_ok(
    is.list(sample) && is.character(sample$sample_id),
    "sample must come from prepare_log_variance_search", "sample"
  )
  validate_hetid_log_projection_prep(sample$prep)
  sample_id <- sample$sample_id
  sample$sample_id <- NULL
  assert_bad_argument_ok(
    identical(sample_id, lv_set_hash(sample)),
    "sample contents changed after preparation", "sample"
  )
  invisible(TRUE)
}
