#' Compute the Projection Variance Bound for the Expected SDF
#'
#' Computes \eqn{\widehat U_{P,i}^{(0)}}, the empirical projection bound on
#' the unconditional variance of the expected-SDF level-approximation error
#' \eqn{\varepsilon_{i,t}^{(0)}} produced by \code{\link{compute_expected_sdf}}.
#' It is the sample variance (divisor \eqn{N}) of the gap series
#' \eqn{g_t = e^{-y^{(1)}_{t+s}} - e^{n\_hat(i,t)}}, \eqn{s = i / step}.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return Numeric scalar \eqn{\widehat U_{P,i}^{(0)} \ge 0}, or
#'   \code{NA_real_} when no finite paired observations remain.
#'
#' @section Mathematical Formula:
#' With \eqn{s = i / step} and \eqn{T_i = \{1, \dots, T - s\}},
#' \deqn{\widehat U_{P,i}^{(0)}
#'   = \frac{1}{N_i} \sum_{t \in T_i}
#'     \left( g_t - \bar g \right)^2,
#'   \qquad
#'   g_t = e^{-y^{(step)}_{t+s} \cdot m(step) / 100} - e^{n\_hat(i,t)},
#'   \quad \bar g = \frac{1}{N_i}\sum_{t \in T_i} g_t.}
#' \eqn{\bar g} is exactly the centering correction added by
#' \code{\link{compute_expected_sdf}}; the divisor is \eqn{N_i} (the spec's
#' \eqn{1/N}), computed with \code{\link{centered_cov}}, not the
#' \eqn{1/(N-1)} of \code{stats::var}.
#'
#' @section Time units:
#' The realized one-period yield is led \code{i / step} rows; rows count
#' news periods, not calendar time, so \code{i} must be a positive multiple
#' of \code{step} and the row frequency must equal the news period.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step}
#'   (108 with the default annual step), because \code{n_hat(i, t)} requires
#'   data at maturity \code{i + step}.
#'
#' @seealso \code{\link{compute_expected_sdf}}, \code{\link{compute_variance_bound}}
#'
#' @export
#'
#' @examples
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#' yields <- data[, paste0("y", seq(12, 120, 12))]
#' term_premia <- data[, paste0("tp", seq(12, 120, 12))]
#'
#' # Projection variance bound for the 5-year (60-month) horizon
#' bound_60 <- compute_expected_sdf_variance_bound(yields, term_premia, i = 60)
#'
compute_expected_sdf_variance_bound <- function(yields, term_premia, i,
                                                step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  validate_row_alignment(yields, term_premia)

  # compute_expected_sdf_gap owns the multiple-of-step contract (it does
  # the i/step row shift), so it is validated there, not here. It returns
  # the gap series already filtered to finite paired observations.
  components <- compute_expected_sdf_gap(yields, term_premia, i, step = step)
  gap <- components$gap

  # Degenerate data (no finite paired observations): no bound to estimate.
  # Matches the NA_real_ contract of compute_c_hat / compute_k_hat.
  if (length(gap) == 0) {
    return(NA_real_)
  }

  # Projection bound: 1/N centered variance of the finite gap series (spec
  # divisor N), the empirical counterpart of Var(epsilon_{i,t}^{(0)}). A
  # constant or single-observation gap gives 0 (the literal 1/N variance).
  centered_cov(gap, gap)[1, 1]
}
