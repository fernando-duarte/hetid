#' Unified Moment Computation for Gamma Quadratic Functions
#'
#' Single source of truth for computing moments (means, variances, covariances)
#' used throughout the theta quadratic solver functions.
#'
#' @param x First vector
#' @param y Second vector (optional for covariance)
#' @param use_t_minus_1 Logical, if TRUE uses n-1 denominator (unbiased),
#'                      if FALSE uses n denominator (biased)
#' @param na_rm Logical, if TRUE removes NA values
#'
#' @return For compute_mean: scalar mean
#'         For compute_var: scalar variance
#'         For compute_cov: scalar covariance
#'
#' @name moment_computation
#' @keywords internal
NULL

#' @rdname moment_computation
compute_mean <- function(x, na_rm = FALSE) {
  if (na_rm) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) {
    return(NA_real_)
  }
  sum(x) / length(x)
}

#' @rdname moment_computation
compute_var <- function(x, use_t_minus_1 = TRUE, na_rm = FALSE) {
  if (na_rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  if (n < 2) {
    return(NA_real_)
  }

  mean_x <- compute_mean(x, na_rm = FALSE)
  denom <- ifelse(use_t_minus_1, n - 1, n)

  sum((x - mean_x)^2) / denom
}

#' @rdname moment_computation
compute_cov <- function(x, y, use_t_minus_1 = TRUE, na_rm = FALSE) {
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }

  if (na_rm) {
    complete_idx <- complete.cases(x, y)
    x <- x[complete_idx]
    y <- y[complete_idx]
  }

  n <- length(x)
  if (n < 2) {
    return(NA_real_)
  }

  mean_x <- compute_mean(x, na_rm = FALSE)
  mean_y <- compute_mean(y, na_rm = FALSE)
  denom <- ifelse(use_t_minus_1, n - 1, n)

  sum((x - mean_x) * (y - mean_y)) / denom
}

#' Compute All Moments for Gamma Quadratic
#'
#' Computes all required moments for the theta quadratic equation
#' using the unified moment functions.
#'
#' @param pc Vector of principal component values
#' @param w1 Vector of W1 residuals
#' @param w2 Vector of W2 residuals
#' @param use_t_minus_1 Logical, if TRUE uses n-1 denominator
#'
#' @return List containing:
#' \describe{
#'   \item{cov_w1w2_pc}{Covariance between W1\\*W2 and PC}
#'   \item{cov_w2sq_pc}{Covariance between W2^2 and PC}
#'   \item{var_w1w2}{Variance of W1\\*W2}
#'   \item{var_w2sq}{Variance of W2^2}
#'   \item{cov_w1w2_w2sq}{Covariance between W1\\*W2 and W2^2}
#'   \item{var_pc}{Variance of PC (for validation)}
#' }
#'
#' @keywords internal
compute_theta_moments_unified <- function(pc, w1, w2, use_t_minus_1 = TRUE) {
  # Check equal lengths
  if (length(pc) != length(w1) || length(pc) != length(w2)) {
    stop("All input vectors must have the same length")
  }

  # Compute derived series
  w1w2 <- w1 * w2
  w2_sq <- w2^2

  # Compute all moments using unified functions
  moments <- list(
    cov_w1w2_pc = compute_cov(w1w2, pc, use_t_minus_1 = use_t_minus_1),
    cov_w2sq_pc = compute_cov(w2_sq, pc, use_t_minus_1 = use_t_minus_1),
    var_w1w2 = compute_var(w1w2, use_t_minus_1 = use_t_minus_1),
    var_w2sq = compute_var(w2_sq, use_t_minus_1 = use_t_minus_1),
    cov_w1w2_w2sq = compute_cov(w1w2, w2_sq, use_t_minus_1 = use_t_minus_1),
    var_pc = compute_var(pc, use_t_minus_1 = use_t_minus_1)
  )

  # Validate critical moments using academic constants
  if (abs(moments$cov_w2sq_pc) < HETID_CONSTANTS$MACHINE_EPSILON) {
    stop("Cov(W2^2, PC) is effectively zero, cannot compute quadratic coefficients")
  }
  if (abs(moments$var_w2sq) < HETID_CONSTANTS$MACHINE_EPSILON) {
    stop("Var(W2^2) is effectively zero, cannot compute quadratic coefficients")
  }

  moments
}

#' Compute Quadratic Coefficients
#'
#' Computes the coefficients a, b, c for the theta quadratic equation
#' given the moments.
#'
#' @param moments List of moments from compute_theta_moments_unified
#' @param tau Quantile parameter between 0 and 1
#'
#' @return List containing:
#' \describe{
#'   \item{a}{Coefficient of theta^2}
#'   \item{b}{Coefficient of theta}
#'   \item{c}{Constant term}
#'   \item{discriminant}{b^2 - 4ac}
#' }
#'
#' @keywords internal
compute_quadratic_coefficients <- function(moments, tau) {
  # Coefficient of theta^2
  a <- 1 - tau^2

  # Coefficient of theta
  b <- 2 * ((moments$cov_w1w2_w2sq / moments$var_w2sq) * tau^2 -
    (moments$cov_w1w2_pc / moments$cov_w2sq_pc))

  # Constant term
  c <- (moments$cov_w1w2_pc^2 / moments$cov_w2sq_pc^2) -
    (moments$var_w1w2 / moments$var_w2sq) * tau^2

  # Discriminant
  discriminant <- b^2 - 4 * a * c

  list(a = a, b = b, c = c, discriminant = discriminant)
}

#' Solve Quadratic Equation
#'
#' Solves ax^2 + bx + c = 0 and returns ordered roots.
#'
#' @param a Coefficient of x^2
#' @param b Coefficient of x
#' @param c Constant term
#' @param discriminant Pre-computed discriminant (optional)
#'
#' @return Vector of two roots (possibly complex), ordered by real part
#'
#' @keywords internal
solve_quadratic <- function(a, b, c, discriminant = NULL) {
  if (is.null(discriminant)) {
    discriminant <- b^2 - 4 * a * c
  }

  # Handle special cases using academic constants
  if (abs(a) < HETID_CONSTANTS$MACHINE_EPSILON) {
    # Linear equation case
    if (abs(b) < HETID_CONSTANTS$MACHINE_EPSILON) {
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
  if (!is.na(roots[2]) && Re(roots[2]) < Re(roots[1])) {
    roots <- roots[c(2, 1)]
  }

  roots
}
