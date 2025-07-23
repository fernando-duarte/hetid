#' Quadratic Solver Base Functions
#'
#' Common functionality for solving theta quadratic equations
#'
#' @name quadratic_solver_base
#' @keywords internal
NULL

#' Core Theta Quadratic Solver
#'
#' Common logic for solving theta quadratic equations from aligned data
#'
#' @param pc_aligned Aligned principal component data
#' @param w1_aligned Aligned W1 residuals
#' @param w2_aligned Aligned W2 residuals
#' @param tau Quantile parameter
#' @param use_t_minus_1 Whether to use n-1 in denominators
#' @param dates_used Optional dates corresponding to aligned data
#' @param additional_components Additional components to include in output
#' @return List with roots, coefficients, discriminant, and components
#' @keywords internal
solve_theta_quadratic_core <- function(pc_aligned, w1_aligned, w2_aligned, tau,
                                       use_t_minus_1 = TRUE, dates_used = NULL,
                                       additional_components = list()) {
  # Compute moments using unified function
  moments <- compute_theta_moments_unified(
    pc = pc_aligned,
    w1 = w1_aligned,
    w2 = w2_aligned,
    use_t_minus_1 = use_t_minus_1
  )

  # Compute quadratic coefficients using unified function
  coeffs <- compute_quadratic_coefficients(moments, tau)

  # Solve quadratic equation using unified function
  roots <- solve_quadratic(coeffs$a, coeffs$b, coeffs$c, coeffs$discriminant)

  # Prepare base result structure
  result <- list(
    roots = roots,
    coefficients = c(a = coeffs$a, b = coeffs$b, c = coeffs$c),
    discriminant = coeffs$discriminant,
    components = c(
      moments,
      list(
        a = coeffs$a,
        b = coeffs$b,
        c = coeffs$c,
        discriminant = coeffs$discriminant,
        n_obs = length(w1_aligned)
      ),
      additional_components
    )
  )

  # Add dates_used if provided
  if (!is.null(dates_used)) {
    result$dates_used <- dates_used
  }

  result
}

#' Prepare Linear Combination Output
#'
#' Common logic for preparing linear combination output in different formats
#'
#' @param linear_comb Linear combination series
#' @param return_df Whether to return data frame
#' @param dates Optional dates vector
#' @param n Total number of observations
#' @return Either the series or a data frame
#' @keywords internal
prepare_linear_comb_output <- function(linear_comb, return_df, dates, n) {
  if (!return_df) {
    return(linear_comb)
  }

  if (is.null(dates)) {
    dates <- seq_len(n)
  }

  data.frame(
    date = dates,
    linear_comb = linear_comb,
    stringsAsFactors = FALSE
  )
}

#' Validate Aligned Data
#'
#' Common validation for aligned time series data
#'
#' @param w1_aligned Aligned W1 residuals
#' @param w2_aligned Aligned W2 residuals
#' @param pc_aligned Aligned principal component data
#' @param min_obs Minimum required observations
#' @return Invisible TRUE if valid, stops with error if invalid
#' @keywords internal
validate_aligned_data <- function(w1_aligned, w2_aligned, pc_aligned, min_obs = 10) {
  n_obs <- length(w1_aligned)

  if (n_obs != length(w2_aligned) || n_obs != length(pc_aligned)) {
    stop("All aligned series must have the same length")
  }

  if (n_obs < min_obs) {
    stop("Insufficient observations after alignment (need at least ", min_obs, ")")
  }

  invisible(TRUE)
}

#' Create Error Result
#'
#' Common structure for error results in quadratic solvers
#'
#' @param error_message Error message to include
#' @return List with NA roots and error message
#' @keywords internal
create_error_result <- function(error_message) {
  list(
    roots = c(NA, NA),
    error = error_message
  )
}

#' Add Solver Metadata
#'
#' Common logic for adding metadata to solver results
#'
#' @param result Base result list
#' @param settings List of settings used
#' @param method_name Name of the solving method
#' @return Enhanced result with metadata
#' @keywords internal
add_solver_metadata <- function(result, settings, method_name) {
  result$method <- method_name
  result$settings <- settings
  result
}
