#' Solve Quadratic Equation for gamma_1 using Linear Combination of PCs
#'
#' Solves the quadratic equation for the identification parameter gamma_1
#' using a linear combination of principal components with normalized weights.
#'
#' @param pc_matrix Matrix of principal components (n x J)
#' @param weights Vector of weights for linear combination (length J)
#' @param w1 Vector of reduced form residuals for Y1
#' @param w2 Vector of reduced form residuals for Y2
#' @param tau Quantile parameter between 0 and 1
#' @param use_t_minus_1 Logical indicating whether to use lagged PCs
#' @param return_df Logical, if TRUE includes dates in the linear_comb output (default FALSE)
#' @param dates Optional vector of dates corresponding to the rows in pc_matrix
#'
#' @return List containing:
#'   \item{roots}{Vector of two roots (possibly complex)}
#'   \item{linear_comb}{The linear combination of PCs used (data frame with dates if return_df=TRUE)}
#'   \item{normalized_weights}{The normalized weights}
#'   \item{variance}{Variance of the linear combination (should be 1)}
#'   \item{error}{Error message if computation failed}
#'
#' @importFrom stats var cov
#' @export
solve_gamma_quadratic_lincomb <- function(pc_matrix, weights, w1, w2, tau,
                                          use_t_minus_1 = TRUE, return_df = FALSE, dates = NULL) {
  # Input validation
  if (!is.matrix(pc_matrix)) {
    pc_matrix <- as.matrix(pc_matrix)
  }

  if (ncol(pc_matrix) != length(weights)) {
    return(list(
      roots = c(NA, NA),
      error = "Number of weights must match number of PCs"
    ))
  }

  # Create linear combination
  linear_comb <- as.vector(pc_matrix %*% weights)

  # Normalize to unit variance
  var_lc <- var(linear_comb, na.rm = TRUE)
  if (var_lc <= 0) {
    return(list(
      roots = c(NA, NA),
      error = "Linear combination has zero or negative variance"
    ))
  }

  normalized_weights <- weights / sqrt(var_lc)
  linear_comb <- linear_comb / sqrt(var_lc)

  # Handle lagged PC if requested
  if (use_t_minus_1) {
    # Create lagged version
    n <- length(linear_comb)
    linear_comb_lag <- c(NA, linear_comb[1:(n - 1)])

    # Align all series
    valid_idx <- complete.cases(w1, w2, linear_comb_lag)
    w1_aligned <- w1[valid_idx]
    w2_aligned <- w2[valid_idx]
    lc_aligned <- linear_comb_lag[valid_idx]
  } else {
    # Align without lag
    valid_idx <- complete.cases(w1, w2, linear_comb)
    w1_aligned <- w1[valid_idx]
    w2_aligned <- w2[valid_idx]
    lc_aligned <- linear_comb[valid_idx]
  }

  # Check for sufficient observations
  if (length(w1_aligned) < 10) {
    return(list(
      roots = c(NA, NA),
      error = "Insufficient observations after alignment"
    ))
  }

  # Compute required moments
  w1w2 <- w1_aligned * w2_aligned
  w2_sq <- w2_aligned^2

  # Covariances
  cov_w1w2_lc <- cov(w1w2, lc_aligned, use = "complete.obs")
  cov_w2sq_lc <- cov(w2_sq, lc_aligned, use = "complete.obs")
  cov_w1w2_w2sq <- cov(w1w2, w2_sq, use = "complete.obs")

  # Variances
  var_w1w2 <- var(w1w2, na.rm = TRUE)
  var_w2sq <- var(w2_sq, na.rm = TRUE)

  # Check for numerical issues
  if (abs(cov_w2sq_lc) < 1e-10 || var_w2sq < 1e-10) {
    return(list(
      roots = c(NA, NA),
      error = "Near-zero variance or covariance detected"
    ))
  }

  # Compute quadratic coefficients

  # Coefficient of gamma_1^2
  a <- 1 - tau^2

  # Coefficient of gamma_1
  b <- 2 * ((cov_w1w2_w2sq / var_w2sq) * tau^2 -
    (cov_w1w2_lc / cov_w2sq_lc))

  # Constant term
  c <- (cov_w1w2_lc^2 / cov_w2sq_lc^2) - (var_w1w2 / var_w2sq) * tau^2

  # Solve quadratic equation
  discriminant <- b^2 - 4 * a * c

  if (discriminant >= 0) {
    # Real roots
    sqrt_disc <- sqrt(discriminant)
    root1 <- (-b + sqrt_disc) / (2 * a)
    root2 <- (-b - sqrt_disc) / (2 * a)
    roots <- c(root1, root2)
  } else {
    # Complex roots
    real_part <- -b / (2 * a)
    imag_part <- sqrt(-discriminant) / (2 * a)
    roots <- complex(
      real = c(real_part, real_part),
      imaginary = c(imag_part, -imag_part)
    )
  }

  # Handle linear_comb output format
  if (return_df) {
    # Create dates if not provided
    if (is.null(dates)) {
      dates <- 1:length(linear_comb)
    }

    # Ensure dates match length
    if (length(dates) != length(linear_comb)) {
      stop("Length of dates must match number of rows in pc_matrix")
    }

    linear_comb_output <- data.frame(
      date = dates,
      linear_comb = linear_comb,
      stringsAsFactors = FALSE
    )
  } else {
    linear_comb_output <- linear_comb
  }

  list(
    roots = roots,
    linear_comb = linear_comb_output,
    normalized_weights = normalized_weights,
    variance = var(linear_comb, na.rm = TRUE),
    error = NULL
  )
}
