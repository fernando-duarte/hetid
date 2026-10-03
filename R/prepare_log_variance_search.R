#' Prepare Aligned Samples for a Log-Variance Search
#'
#' Keeps the full mean sample in the existing projection preparation and selects
#' volatility rows by identifiers. Regressors are centered over those rows.
#' The supplied residuals must obey the contract of prepare_log_projection().
#'
#' @param w1,w2 Full mean-sample response and news residuals.
#' @param x_var Volatility regressors on volatility_ids, without an intercept.
#' @param mean_ids,volatility_ids Unique identifiers in matching classes. Date
#'   identifiers must already be calendar period ends; use to_period_end() first.
#' @param ols_residuals Optional OLS benchmark residuals on the full mean sample.
#'   Required only by the audited Harvey path.
#' @param response_ids Identifiers for the response dates, in volatility-row order.
#' @param impose_null Passed to prepare_log_projection().
#' @return A sample list containing both samples, centered design, identifiers,
#'   the fixed projection preparation and an identity binding all retained values.
#' @examples
#' mean_ids <- seq_len(24L)
#' volatility_ids <- 13:24
#' w1 <- rep(c(-2, -1, 1, 2), 6L)
#' w2 <- cbind(news = rep(c(-1, 1), 12L))
#' sample_data <- prepare_log_variance_search(
#'   w1, w2, cbind(pc1 = seq_along(volatility_ids)), mean_ids, volatility_ids
#' )
#' sample_data$prep$volatility_rows
#' @export
prepare_log_variance_search <- function(w1, w2, x_var, mean_ids, volatility_ids,
                                        ols_residuals = NULL,
                                        response_ids = volatility_ids,
                                        impose_null = FALSE) {
  prep <- prepare_log_projection(w1, w2, x_var, mean_ids, volatility_ids, impose_null)
  assert_bad_argument_ok(
    is.atomic(response_ids) && is.null(dim(response_ids)) &&
      length(response_ids) == length(volatility_ids) && !anyNA(response_ids) &&
      identical(class(response_ids), class(volatility_ids)),
    "response_ids must match volatility_ids in length and class", "response_ids"
  )
  if (!is.null(ols_residuals)) {
    assert_bad_argument_ok(
      is.numeric(ols_residuals) && is.null(dim(ols_residuals)) &&
        length(ols_residuals) == length(w1) && all(is.finite(ols_residuals)),
      "ols_residuals must be finite on the full mean sample", "ols_residuals"
    )
  }
  pcr <- scale(as.matrix(x_var), center = TRUE, scale = FALSE)
  colnames(pcr) <- colnames(prep$x_centered)
  sample_data <- list(
    w1 = prep$w1, w2 = prep$w2, pcr = pcr,
    x_mat = log_variance_design(pcr), date = volatility_ids, response_date = response_ids,
    ols_residuals = if (is.null(ols_residuals)) NULL else ols_residuals[prep$volatility_rows],
    prep = prep
  )
  sample_data$sample_id <- lv_set_hash(sample_data)
  sample_data
}
