#' Solve Quadratic Equation for Theta
#'
#' Finds the roots of the quadratic equation in theta given time series of PC_jt,
#' W_1,t+1, W_2,i,t+1, and a parameter tau.
#'
#' @param pc_j Numeric vector, time series of PC_jt (principal component j at time t)
#' @param w1 Numeric vector, time series of W_1,t+1 (reduced form residual for Y1)
#' @param w2 Numeric vector, time series of W_2,i,t+1 (reduced form residual for Y2 at maturity i)
#' @param tau Numeric, parameter between 0 and 1
#' @param use_t_minus_1 Logical, if TRUE uses n-1 in variance/covariance denominators (unbiased),
#'                      if FALSE uses n (biased). Note: W1 and W2 residuals are pre-computed
#'                      using the appropriate timing alignment, so this parameter does NOT
#'                      affect the PC timing.
#' @param dates Optional vector of dates corresponding to the time series. If provided, the function
#'              will return information about which dates were used after removing NA values.
#'
#' @return A named list containing:
#' \describe{
#'   \item{roots}{Numeric vector of length 2 containing the roots theta^(1) and theta^(2),
#'                ordered so the root with smallest real part is first}
#'   \item{coefficients}{Named vector with quadratic coefficients a, b, c where
#'                        a*theta^2 + b*theta + c = 0}
#'   \item{discriminant}{The discriminant b^2 - 4ac}
#'   \item{components}{List containing all computed covariances and variances
#'                      used in the calculation}
#'   \item{dates_used}{If dates parameter was provided, returns the dates corresponding to
#'                     observations used after removing NA values}
#' }
#'
#' @details
#' The function solves the quadratic equation:
#' a*theta^2 + b*theta + c = 0
#'
#' where:
#' - a = (1 - tau^2)
#' - b = 2*(Cov(W1*W2, W2^2)/Var(W2^2) * tau^2 - Cov(W1*W2, PC_j)/Cov(W2^2, PC_j))
#' - c = \\[Cov(W1*W2, PC_j)\\]^2 / \\[Cov(W2^2, PC_j)\\]^2 - Var(W1*W2)/Var(W2^2) * tau^2
#'
#' Note: The input residuals W1 and W2 are expected to be pre-computed using the
#' appropriate timing alignment (typically using lagged PCs to predict future values).
#' The covariances and variances are computed using empirical estimators with either
#' n-1 or n in the denominator based on use_t_minus_1.
#'
#' @importFrom stats complete.cases
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate sample data
#' set.seed(123)
#' n <- 100
#' pc_j <- rnorm(n)
#' w1 <- rnorm(n)
#' w2 <- rnorm(n)
#'
#' # Solve quadratic for tau = 0.5
#' result <- solve_theta_quadratic(pc_j, w1, w2, tau = 0.5)
#' print(result$roots)
#'
#' # Use biased estimator (n instead of n-1 in denominators)
#' result_biased <- solve_theta_quadratic(pc_j, w1, w2, tau = 0.5, use_t_minus_1 = FALSE)
#'
#' # With dates to track which observations were used
#' dates <- seq(as.Date("2020-01-01"), length.out = n, by = "month")
#' result_dates <- solve_theta_quadratic(pc_j, w1, w2, tau = 0.5, dates = dates)
#' print(result_dates$dates_used) # Shows dates after removing NA values
#' }
#'
solve_theta_quadratic <- function(pc_j, w1, w2, tau, use_t_minus_1 = TRUE, dates = NULL) {
  # Validate inputs and clean data
  validated <- validate_theta_inputs(pc_j, w1, w2, tau, dates) # nolint: object_usage_linter
  pc_j <- validated$pc_j
  w1 <- validated$w1
  w2 <- validated$w2
  n <- validated$n
  dates_used <- if (!is.null(dates)) validated$dates else NULL

  # Compute moments using unified function
  moments <- compute_theta_moments_unified(pc_j, w1, w2, use_t_minus_1) # nolint: object_usage_linter

  # Compute quadratic coefficients using unified function
  coeffs <- compute_quadratic_coefficients(moments, tau) # nolint: object_usage_linter
  a <- coeffs$a
  b <- coeffs$b
  c <- coeffs$c
  discriminant <- coeffs$discriminant

  # Solve quadratic equation using unified function
  roots <- solve_quadratic(a, b, c, discriminant) # nolint: object_usage_linter

  # Prepare results
  result <- list(
    roots = roots,
    coefficients = c(a = a, b = b, c = c),
    discriminant = discriminant,
    components = c(
      moments,
      list(
        a = a,
        b = b,
        c = c,
        discriminant = discriminant,
        n_obs = n
      )
    )
  )

  # Add dates_used if dates were provided
  if (!is.null(dates_used)) {
    result$dates_used <- dates_used
  }

  result
}
