#' Solve Quadratic Equation for Gamma_1
#'
#' Finds the roots of the quadratic equation in gamma_1 given time series of PC_jt,
#' W_1,t+1, W_2,i,t+1, and a parameter tau.
#'
#' @param pc_j Numeric vector, time series of PC_jt (principal component j at time t)
#' @param w1 Numeric vector, time series of W_1,t+1 (reduced form residual for Y1)
#' @param w2 Numeric vector, time series of W_2,i,t+1 (reduced form residual for Y2 at maturity i)
#' @param tau Numeric, parameter between 0 and 1
#' @param use_t_minus_1 Logical, if TRUE uses T-1 in denominators (default), if FALSE uses T
#'
#' @return A named list containing:
#' \describe{
#'   \item{roots}{Numeric vector of length 2 containing the roots gamma_1^(1) and gamma_1^(2),
#'                ordered so the root with smallest real part is first}
#'   \item{coefficients}{Named vector with quadratic coefficients a, b, c where
#'                        a*gamma_1^2 + b*gamma_1 + c = 0}
#'   \item{discriminant}{The discriminant b^2 - 4ac}
#'   \item{components}{List containing all computed covariances and variances
#'                      used in the calculation}
#' }
#'
#' @details
#' The function solves the quadratic equation:
#' a*gamma_1^2 + b*gamma_1 + c = 0
#'
#' where:
#' - a = (1 - tau^2)
#' - b = 2*(Cov(W1*W2, W2^2)/Var(W2^2) * tau^2 - Cov(W1*W2, PC_j)/Cov(W2^2, PC_j))
#' - c = \\[Cov(W1*W2, PC_j)\\]^2 / \\[Cov(W2^2, PC_j)\\]^2 - Var(W1*W2)/Var(W2^2) * tau^2
#'
#' The covariances and variances are computed using empirical estimators with either
#' T-1 or T in the denominator based on use_t_minus_1.
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
#' result <- solve_gamma_quadratic(pc_j, w1, w2, tau = 0.5)
#' print(result$roots)
#'
#' # Use T instead of T-1 in denominators
#' result_t <- solve_gamma_quadratic(pc_j, w1, w2, tau = 0.5, use_t_minus_1 = FALSE)
#' }
#'
solve_gamma_quadratic <- function(pc_j, w1, w2, tau, use_t_minus_1 = TRUE) {
  # Validate inputs and clean data
  validated <- validate_gamma_inputs(pc_j, w1, w2, tau)
  pc_j <- validated$pc_j
  w1 <- validated$w1
  w2 <- validated$w2
  n <- validated$n

  # Compute moments
  moments <- compute_gamma_moments(pc_j, w1, w2, use_t_minus_1)

  # Compute quadratic coefficients
  a <- 1 - tau^2

  b <- 2 * (moments$cov_w1w2_w2sq / moments$var_w2sq * tau^2 -
    moments$cov_w1w2_pcj / moments$cov_w2sq_pcj)

  c <- (moments$cov_w1w2_pcj^2) / (moments$cov_w2sq_pcj^2) -
    moments$var_w1w2 / moments$var_w2sq * tau^2

  # Compute discriminant
  discriminant <- b^2 - 4 * a * c

  # Solve quadratic equation
  if (abs(a) < .Machine$double.eps) {
    # Linear equation case (a ≈ 0, which means tau ≈ 1)
    if (abs(b) < .Machine$double.eps) {
      stop("Both a and b coefficients are effectively zero")
    }
    roots <- c(-c / b, NA)
  } else if (discriminant < 0) {
    # Complex roots
    real_part <- -b / (2 * a)
    imag_part <- sqrt(-discriminant) / (2 * a)
    roots <- complex(
      real = c(real_part, real_part),
      imaginary = c(-imag_part, imag_part)
    )
  } else {
    # Real roots
    sqrt_disc <- sqrt(discriminant)
    root1 <- (-b - sqrt_disc) / (2 * a)
    root2 <- (-b + sqrt_disc) / (2 * a)
    roots <- c(root1, root2)
  }

  # Order roots by real part (smallest first)
  if (!is.na(roots[2])) {
    if (Re(roots[2]) < Re(roots[1])) {
      roots <- roots[c(2, 1)]
    }
  }

  # Return results
  list(
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
}
