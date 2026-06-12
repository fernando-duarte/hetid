#' Compute Variance Bound
#'
#' Computes the empirical upper bound for Var(error(i,t+1))
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return Numeric value of the variance bound (1/4)*c_hat_i*k_hat_i
#'
#' @details
#' The variance bound is:
#' Var(error(i,t+1)) <= (1/4)*c_hat_i*k_hat_i
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step}
#'   (108 for standard ACM data with the default annual step), because this
#'   function requires data at maturity \code{i + step}. \code{i} must be
#'   a positive multiple of \code{step} (enforced by
#'   \code{\link{compute_k_hat}}).
#'
#' @export
#'
#' @examples
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#' yields <- data[, paste0("y", seq(12, 120, 12))]
#' term_premia <- data[, paste0("tp", seq(12, 120, 12))]
#'
#' # Compute variance bound for the 5-year (60-month) maturity
#' var_bound_60 <- compute_variance_bound(yields, term_premia, i = 60)
#'
compute_variance_bound <- function(yields, term_premia, i,
                                   step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  validate_row_alignment(yields, term_premia)

  # Compute components
  c_hat <- compute_c_hat(yields, term_premia, i, step = step)
  k_hat <- compute_k_hat(yields, term_premia, i, step = step)

  # Return variance bound
  0.25 * c_hat * k_hat
}
