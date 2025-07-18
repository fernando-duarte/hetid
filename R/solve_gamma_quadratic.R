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
#'   \item{coefficients}{Named vector with quadratic coefficients a, b, c where a*gamma_1^2 + b*gamma_1 + c = 0}
#'   \item{discriminant}{The discriminant b^2 - 4ac}
#'   \item{components}{List containing all computed covariances and variances used in the calculation}
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
  # Input validation
  if (!is.numeric(pc_j) || !is.numeric(w1) || !is.numeric(w2)) {
    stop("pc_j, w1, and w2 must be numeric vectors")
  }

  # Check equal lengths
  n <- length(pc_j)
  if (length(w1) != n || length(w2) != n) {
    stop("All input time series must have the same length")
  }

  # Check tau
  if (!is.numeric(tau) || length(tau) != 1 || tau < 0 || tau > 1) {
    stop("tau must be a single numeric value between 0 and 1")
  }

  # Remove any rows with NA values
  complete_idx <- complete.cases(pc_j, w1, w2)
  if (sum(complete_idx) < 3) {
    stop("Not enough complete observations (need at least 3)")
  }

  pc_j <- pc_j[complete_idx]
  w1 <- w1[complete_idx]
  w2 <- w2[complete_idx]
  n <- length(pc_j)

  # Set denominator for variance/covariance calculations
  denom <- ifelse(use_t_minus_1, n - 1, n)

  # Compute derived series
  w1_w2 <- w1 * w2
  w2_sq <- w2^2

  # Compute means
  mean_pc_j <- mean(pc_j)
  mean_w1_w2 <- mean(w1_w2)
  mean_w2_sq <- mean(w2_sq)

  # Compute covariances and variances
  # Cov(W1*W2, PC_j)
  cov_w1w2_pcj <- sum((w1_w2 - mean_w1_w2) * (pc_j - mean_pc_j)) / denom

  # Cov(W2^2, PC_j)
  cov_w2sq_pcj <- sum((w2_sq - mean_w2_sq) * (pc_j - mean_pc_j)) / denom

  # Var(W1*W2)
  var_w1w2 <- sum((w1_w2 - mean_w1_w2)^2) / denom

  # Var(W2^2)
  var_w2sq <- sum((w2_sq - mean_w2_sq)^2) / denom

  # Cov(W1*W2, W2^2)
  cov_w1w2_w2sq <- sum((w1_w2 - mean_w1_w2) * (w2_sq - mean_w2_sq)) / denom

  # Check for zero denominators
  if (abs(cov_w2sq_pcj) < .Machine$double.eps) {
    stop("Cov(W2^2, PC_j) is effectively zero, cannot compute quadratic coefficients")
  }
  if (abs(var_w2sq) < .Machine$double.eps) {
    stop("Var(W2^2) is effectively zero, cannot compute quadratic coefficients")
  }

  # Compute quadratic coefficients
  # a * gamma_1^2 + b * gamma_1 + c = 0

  a <- 1 - tau^2

  b <- 2 * (cov_w1w2_w2sq / var_w2sq * tau^2 - cov_w1w2_pcj / cov_w2sq_pcj)

  c <- (cov_w1w2_pcj^2) / (cov_w2sq_pcj^2) - var_w1w2 / var_w2sq * tau^2

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
    components = list(
      cov_w1w2_pcj = cov_w1w2_pcj,
      cov_w2sq_pcj = cov_w2sq_pcj,
      var_w1w2 = var_w1w2,
      var_w2sq = var_w2sq,
      cov_w1w2_w2sq = cov_w1w2_w2sq,
      n_obs = n,
      denominator = denom
    )
  )
}
