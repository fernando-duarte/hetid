#' Multi-start Optimization Runner for Theta
#'
#' Internal functions for running the multi-start optimization
#'
#' @name theta_runner
#' @keywords internal
NULL

#' Run multi-start optimization
#' @importFrom nloptr nloptr
#' @importFrom stats rnorm runif
#' @noRd
run_multistart_optimization <- function(objective_fn, constraint_fn, n_pcs,
                                        tau_bounds, weight_bounds, n_starts,
                                        algorithm, verbose) {
  # Set up optimization bounds
  lower_bounds <- c(tau_bounds[1], rep(-weight_bounds, n_pcs))
  upper_bounds <- c(tau_bounds[2], rep(weight_bounds, n_pcs))

  # Multi-start optimization
  best_result <- NULL
  best_objective <- Inf

  for (start_idx in 1:n_starts) {
    # Generate random starting point
    if (start_idx == 1) {
      # First start: equal weights
      tau_init <- mean(tau_bounds)
      weights_init <- rep(1 / sqrt(n_pcs), n_pcs)
    } else {
      # Random starts
      tau_init <- runif(1, tau_bounds[1], tau_bounds[2])
      weights_init <- rnorm(n_pcs)
      weights_init <- weights_init / sqrt(sum(weights_init^2))
    }

    x0 <- c(tau_init, weights_init)

    # Run optimization
    result <- nloptr(
      x0 = x0,
      eval_f = objective_fn,
      lb = lower_bounds,
      ub = upper_bounds,
      eval_g_eq = constraint_fn,
      opts = list(
        algorithm = algorithm,
        maxeval = 1000,
        ftol_rel = 1e-8
      )
    )

    if (result$objective < best_objective) {
      best_objective <- result$objective
      best_result <- result
    }

    if (verbose && start_idx %% 5 == 0) {
      cat("  Completed", start_idx, "of", n_starts, "starts\n")
    }
  }

  best_result
}

#' Extract and format theta optimization results
#' @noRd
extract_theta_results <- function(final_result, i, n_pcs, tau_opt,
                                  best_objective, convergence, start_time) {
  # Extract normalized weights and coefficients
  optimal_weights <- final_result$normalized_weights
  a_opt <- final_result$coefficients["a"]
  b_opt <- final_result$coefficients["b"]
  c_opt <- final_result$coefficients["c"]
  discriminant_opt <- final_result$discriminant

  # Get roots (already ordered)
  theta_lower <- final_result$roots[1]
  theta_upper <- final_result$roots[2]

  # Ensure theta_lower < theta_upper
  if (theta_lower > theta_upper) {
    temp <- theta_lower
    theta_lower <- theta_upper
    theta_upper <- temp
  }

  # End timing
  end_time <- Sys.time()
  time_elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Return results
  list(
    maturity = i,
    n_pcs = n_pcs,
    tau_opt = tau_opt,
    optimal_weights = optimal_weights,
    theta_lower = theta_lower,
    theta_upper = theta_upper,
    interval_width = theta_upper - theta_lower,
    objective_value = best_objective,
    convergence = convergence,
    time_elapsed = time_elapsed,
    A = a_opt,
    B = b_opt,
    C = c_opt,
    discriminant = discriminant_opt
  )
}
