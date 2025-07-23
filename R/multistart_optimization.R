#' Multi-start Optimization Utilities for Academic Research
#'
#' Standardized multi-start optimization functions for robust parameter estimation
#' in academic research contexts, particularly for heteroskedasticity identification.
#'
#' @name multistart_optimization
#' @keywords internal
NULL

#' Run Multi-start Optimization
#'
#' Standardizes multi-start optimization approach across different optimization
#' functions in the package, ensuring robust parameter estimation for academic research.
#'
#' @param objective_fn Function to minimize (should return scalar numeric value)
#' @param start_values Matrix or list of starting values (each row/element is one start).
#'   If NULL, random starting points will be generated.
#' @param lower_bounds Vector of lower bounds for parameters. Required if start_values is NULL.
#' @param upper_bounds Vector of upper bounds for parameters. Required if start_values is NULL.
#' @param algorithm Optimization algorithm to use (default: "NLOPT_LN_COBYLA")
#' @param n_starts Number of random starting points if start_values not provided (default: 20)
#' @param max_eval Maximum function evaluations per start (default: 1000)
#' @param verbose Logical, whether to print progress information (default: FALSE)
#' @param ... Additional arguments passed to optimization function
#'
#' @return List containing:
#'   \item{best_result}{Best optimization result across all starts}
#'   \item{all_results}{List of all optimization results}
#'   \item{n_successful}{Number of successful optimizations}
#'   \item{best_objective}{Best objective function value achieved}
#'
#' @importFrom stats runif rnorm
#' @keywords internal
run_multistart_optimization <- function(objective_fn,
                                        start_values = NULL,
                                        lower_bounds = NULL,
                                        upper_bounds = NULL,
                                        algorithm = "NLOPT_LN_COBYLA",
                                        n_starts = 20,
                                        max_eval = 1000,
                                        verbose = FALSE,
                                        ...) {
  # Validate inputs
  if (!is.function(objective_fn)) {
    stop("objective_fn must be a function")
  }

  # Generate starting values if not provided
  if (is.null(start_values)) {
    if (is.null(lower_bounds) || is.null(upper_bounds)) {
      stop("Either start_values or both lower_bounds and upper_bounds must be provided")
    }

    n_params <- length(lower_bounds)
    start_values <- matrix(
      runif(n_starts * n_params, min = lower_bounds, max = upper_bounds),
      nrow = n_starts,
      ncol = n_params
    )
  }

  # Convert to matrix if needed
  if (is.list(start_values)) {
    start_values <- do.call(rbind, start_values)
  }

  if (!is.matrix(start_values)) {
    start_values <- as.matrix(start_values)
  }

  n_starts_actual <- nrow(start_values)
  n_params <- ncol(start_values)

  if (verbose) {
    message("Running multi-start optimization with ", n_starts_actual, " starting points")
  }

  # Storage for results
  all_results <- vector("list", n_starts_actual)
  successful_results <- list()

  # Run optimization from each starting point
  for (i in seq_len(n_starts_actual)) {
    if (verbose && i %% 5 == 0) {
      message("  Starting point ", i, "/", n_starts_actual)
    }

    start_point <- start_values[i, ]

    tryCatch(
      {
        # Use nloptr for optimization
        if (requireNamespace("nloptr", quietly = TRUE)) {
          result <- nloptr::nloptr(
            x0 = start_point,
            eval_f = objective_fn,
            lb = lower_bounds,
            ub = upper_bounds,
            opts = list(
              algorithm = algorithm,
              maxeval = max_eval,
              print_level = 0
            ),
            ...
          )

          # Store result
          all_results[[i]] <- result

          # Check if optimization was successful
          if (result$status >= 0) {
            successful_results <- append(successful_results, list(result))
          }
        } else {
          stop("nloptr package is required for multi-start optimization")
        }
      },
      error = function(e) {
        if (verbose) {
          message("  Error at starting point ", i, ": ", e$message)
        }
        all_results[[i]] <- list(
          status = -1,
          message = e$message,
          objective = Inf
        )
      }
    )
  }

  # Find best result
  if (length(successful_results) == 0) {
    warning("No successful optimizations found")
    best_result <- list(
      status = -1,
      message = "No successful optimizations",
      objective = Inf
    )
    best_objective <- Inf
  } else {
    objectives <- sapply(successful_results, function(x) x$objective)
    best_idx <- which.min(objectives)
    best_result <- successful_results[[best_idx]]
    best_objective <- objectives[best_idx]
  }

  if (verbose) {
    message("Multi-start optimization completed:")
    message("  Successful runs: ", length(successful_results), "/", n_starts_actual)
    message("  Best objective: ", round(best_objective, 6))
  }

  list(
    best_result = best_result,
    all_results = all_results,
    n_successful = length(successful_results),
    best_objective = best_objective
  )
}

#' Generate Random Starting Points
#'
#' Generates random starting points for multi-start optimization with
#' appropriate distributions for academic research parameters.
#'
#' @param n_starts Number of starting points to generate
#' @param n_params Number of parameters
#' @param bounds_type Type of bounds ("uniform", "normal", "tau_weights")
#' @param lower_bounds Vector of lower bounds
#' @param upper_bounds Vector of upper bounds
#'
#' @return Matrix of starting points (n_starts x n_params)
#' @keywords internal
generate_starting_points <- function(n_starts,
                                     n_params,
                                     bounds_type = "uniform",
                                     lower_bounds = NULL,
                                     upper_bounds = NULL) {
  if (bounds_type == "uniform") {
    if (is.null(lower_bounds) || is.null(upper_bounds)) {
      stop("lower_bounds and upper_bounds required for uniform distribution")
    }

    matrix(
      runif(n_starts * n_params, min = lower_bounds, max = upper_bounds),
      nrow = n_starts,
      ncol = n_params
    )
  } else if (bounds_type == "tau_weights") {
    # Special case for tau + weights optimization
    # First parameter is tau (0, 1), rest are weights (-2, 2)
    starts <- matrix(0, nrow = n_starts, ncol = n_params)

    # Tau parameter (first column)
    starts[, 1] <- runif(n_starts, min = 0.1, max = 0.9)

    # Weight parameters (remaining columns)
    if (n_params > 1) {
      starts[, 2:n_params] <- matrix(
        runif(n_starts * (n_params - 1), min = -1, max = 1),
        nrow = n_starts,
        ncol = n_params - 1
      )
    }

    starts
  } else {
    stop("Unknown bounds_type: ", bounds_type)
  }
}
