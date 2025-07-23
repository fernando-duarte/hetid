#' Solve Quadratic Equation for Theta using Linear Combination of PCs
#'
#' Solves the quadratic equation for the identification parameter theta
#' using a linear combination of principal components with normalized weights.
#'
#' @param pcs Matrix of principal components (n x J)
#' @param weights Vector of weights for linear combination (length J)
#' @param w1 Vector of reduced form residuals for Y1
#' @param w2 Vector of reduced form residuals for Y2
#' @param tau Quantile parameter between 0 and 1
#' @param normalize_by Character, how to normalize weights: "norm" (L2 norm) or "variance"
#' @param use_t_minus_1 Logical, if TRUE uses n-1 in variance/covariance denominators
#' @param return_df Logical, if TRUE includes dates in the linear_comb output
#' @param dates Optional vector of dates corresponding to the rows in pcs
#'
#' @return List containing:
#'   \item{roots}{Vector of two roots (possibly complex)}
#'   \item{coefficients}{Named vector with quadratic coefficients a, b, c}
#'   \item{discriminant}{The discriminant b^2 - 4ac}
#'   \item{linear_comb}{The linear combination of PCs used}
#'   \item{normalized_weights}{The normalized weights}
#'   \item{variance}{Variance of the linear combination}
#'   \item{components}{List of all computed moments for transparency}
#'   \item{dates_used}{If dates provided, dates after alignment}
#'   \item{error}{Error message if computation failed}
#'
#' @details
#' This function creates a linear combination of principal components and solves
#' the theta quadratic equation. It provides options for:
#' - Weight normalization: by L2 norm (sum of squares = 1) or by variance
#' - Denominator choice: n-1 (unbiased) or n (biased)
#'
#' The function always uses lagged PCs for consistency with residual computation.
#'
#' @importFrom stats var complete.cases
#' @export
solve_theta_quadratic_lincomb <- function(pcs,
                                          weights,
                                          w1,
                                          w2,
                                          tau,
                                          normalize_by = c("norm", "variance"),
                                          use_t_minus_1 = TRUE,
                                          return_df = FALSE,
                                          dates = NULL) {
  # Input validation
  normalize_by <- match.arg(normalize_by)

  if (!is.matrix(pcs)) {
    pcs <- as.matrix(pcs)
  }

  if (ncol(pcs) != length(weights)) {
    return(list(
      roots = c(NA, NA),
      error = "Number of weights must match number of PCs"
    ))
  }

  # Create linear combination with raw weights first
  linear_comb_raw <- as.vector(pcs %*% weights)

  # Normalize weights based on chosen method
  if (normalize_by == "norm") {
    # Normalize by L2 norm (sum of squares = 1)
    weights_norm <- sqrt(sum(weights^2))
    if (weights_norm < .Machine$double.eps) {
      return(list(
        roots = c(NA, NA),
        error = "Weights cannot all be zero"
      ))
    }
    normalized_weights <- weights / weights_norm
    linear_comb <- linear_comb_raw / weights_norm
  } else { # normalize_by == "variance"
    # Normalize to unit variance
    var_lc <- var(linear_comb_raw, na.rm = TRUE)
    if (var_lc <= .Machine$double.eps) {
      return(list(
        roots = c(NA, NA),
        error = "Linear combination has zero or negative variance"
      ))
    }
    normalized_weights <- weights / sqrt(var_lc)
    linear_comb <- linear_comb_raw / sqrt(var_lc)
  }

  # Always use lagged PCs for consistency with residual computation
  n <- length(linear_comb)
  linear_comb_lag <- c(NA, linear_comb[1:(n - 1)])

  # Track which dates are used if provided
  dates_for_calc <- NULL
  if (!is.null(dates)) {
    if (length(dates) != n) {
      stop("Length of dates must match number of rows in pcs")
    }
    dates_for_calc <- dates
  }

  # Align all series
  valid_idx <- complete.cases(w1, w2, linear_comb_lag)
  w1_aligned <- w1[valid_idx]
  w2_aligned <- w2[valid_idx]
  lc_aligned <- linear_comb_lag[valid_idx]

  if (!is.null(dates_for_calc)) {
    dates_used <- dates_for_calc[valid_idx]
  } else {
    dates_used <- NULL
  }

  # Check for sufficient observations
  if (length(w1_aligned) < 10) {
    return(list(
      roots = c(NA, NA),
      error = "Insufficient observations after alignment"
    ))
  }

  # Compute moments using unified function
  moments <- compute_theta_moments_unified(
    pc = lc_aligned,
    w1 = w1_aligned,
    w2 = w2_aligned,
    use_t_minus_1 = use_t_minus_1
  )

  # Compute quadratic coefficients using unified function
  coeffs <- compute_quadratic_coefficients(moments, tau)

  # Solve quadratic equation using unified function
  roots <- solve_quadratic(coeffs$a, coeffs$b, coeffs$c, coeffs$discriminant)

  # Handle linear_comb output format
  if (return_df) {
    if (is.null(dates)) {
      dates <- seq_len(n)
    }

    linear_comb_output <- data.frame(
      date = dates,
      linear_comb = linear_comb,
      stringsAsFactors = FALSE
    )
  } else {
    linear_comb_output <- linear_comb
  }

  # Prepare comprehensive output
  result <- list(
    roots = roots,
    coefficients = c(a = coeffs$a, b = coeffs$b, c = coeffs$c),
    discriminant = coeffs$discriminant,
    linear_comb = linear_comb_output,
    normalized_weights = normalized_weights,
    variance = var(linear_comb, na.rm = TRUE),
    components = moments, # Include all moments for transparency
    error = NULL
  )

  # Add dates_used if applicable
  if (!is.null(dates_used)) {
    result$dates_used <- dates_used
  }

  # Add metadata about settings used
  result$settings <- list(
    normalize_by = normalize_by,
    use_t_minus_1 = use_t_minus_1,
    n_obs_used = length(w1_aligned)
  )

  result
}
