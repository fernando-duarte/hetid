#' Optimize PC Weights to Minimize Root Distance
#'
#' Finds the optimal weights for a linear combination of principal components
#' that minimizes the distance between the two roots of the gamma_1 quadratic
#' equation, with preference for real roots.
#'
#' @param pc_matrix Matrix of principal components (n x J)
#' @param w1 Vector of reduced form residuals for Y1
#' @param w2 Vector of reduced form residuals for Y2
#' @param tau Quantile parameter between 0 and 1
#' @param maturity Maturity index (for Y2 residuals)
#' @param use_t_minus_1 Logical indicating whether to use lagged PCs
#' @param initial_weights Initial weights (default: equal weights)
#' @param method Optimization method (default: "Nelder-Mead")
#' @param maxit Maximum iterations (default: 1000)
#' @param penalty_complex Penalty for complex roots (default: 1e6)
#'
#' @return List containing:
#'   \item{optimal_weights}{Optimal normalized weights}
#'   \item{roots}{The two roots at optimal weights}
#'   \item{root_distance}{Distance between roots}
#'   \item{objective_value}{Final objective function value}
#'   \item{convergence}{Convergence indicator from optim}
#'   \item{linear_comb}{The optimal linear combination}
#'   \item{is_complex}{Whether roots are complex}
#'
#' @importFrom stats optim
#' @export
optimize_pc_weights <- function(pc_matrix, w1, w2, tau, maturity = NULL,
                                use_t_minus_1 = TRUE,
                                initial_weights = NULL,
                                method = "Nelder-Mead",
                                maxit = 1000,
                                penalty_complex = 1e6) {
  n_pcs <- ncol(pc_matrix)

  # Set initial weights if not provided
  if (is.null(initial_weights)) {
    initial_weights <- rep(1 / sqrt(n_pcs), n_pcs)
  }

  # Define objective function
  objective <- function(weights) {
    # Normalize weights to sum of squares = 1
    weight_norm <- sqrt(sum(weights^2))
    if (weight_norm < 1e-10) {
      return(penalty_complex * 2) # Invalid weights
    }

    norm_weights <- weights / weight_norm

    # Solve quadratic with these weights
    result <- solve_gamma_quadratic_lincomb(
      pc_matrix = pc_matrix,
      weights = norm_weights,
      w1 = w1,
      w2 = w2,
      tau = tau,
      use_t_minus_1 = use_t_minus_1
    )

    # Check for errors
    if (!is.null(result$error)) {
      return(penalty_complex * 3)
    }

    roots <- result$roots

    # Check if roots are complex
    if (is.complex(roots)) {
      # Return large penalty plus magnitude of imaginary parts
      return(penalty_complex + sum(abs(Im(roots))))
    }

    # For real roots, return absolute distance
    abs(roots[1] - roots[2])
  }

  # Optimize
  opt_result <- optim(
    par = initial_weights,
    fn = objective,
    method = method,
    control = list(maxit = maxit)
  )

  # Get optimal weights (normalized)
  weight_norm <- sqrt(sum(opt_result$par^2))
  optimal_weights <- opt_result$par / weight_norm

  # Compute final solution with optimal weights
  final_result <- solve_gamma_quadratic_lincomb(
    pc_matrix = pc_matrix,
    weights = optimal_weights,
    w1 = w1,
    w2 = w2,
    tau = tau,
    use_t_minus_1 = use_t_minus_1
  )

  # Prepare output
  output <- list(
    optimal_weights = optimal_weights,
    roots = final_result$roots,
    root_distance = ifelse(
      is.complex(final_result$roots),
      NA,
      abs(final_result$roots[1] - final_result$roots[2])
    ),
    objective_value = opt_result$value,
    convergence = opt_result$convergence,
    linear_comb = final_result$linear_comb,
    is_complex = is.complex(final_result$roots),
    maturity = maturity
  )

  output
}


#' Optimize PC Weights for All Maturities
#'
#' Runs the PC weight optimization for all maturities and returns
#' a summary of results.
#'
#' @param pc_matrix Matrix of principal components (n x J)
#' @param w1 Vector of reduced form residuals for Y1
#' @param w2_list List of vectors of reduced form residuals for Y2 (by maturity)
#' @param tau Quantile parameter between 0 and 1
#' @param use_t_minus_1 Logical indicating whether to use lagged PCs
#' @param parallel Logical indicating whether to use parallel processing
#' @param n_cores Number of cores for parallel processing (default: detect)
#'
#' @return Data frame with optimization results for each maturity
#'
#' @export
optimize_pc_weights_all_maturities <- function(pc_matrix, w1, w2_list, tau,
                                               use_t_minus_1 = TRUE,
                                               parallel = FALSE,
                                               n_cores = NULL) {
  n_maturities <- length(w2_list)
  n_pcs <- ncol(pc_matrix)

  # Function to optimize for one maturity
  optimize_one <- function(i) {
    result <- optimize_pc_weights(
      pc_matrix = pc_matrix,
      w1 = w1,
      w2 = w2_list[[i]],
      tau = tau,
      maturity = i,
      use_t_minus_1 = use_t_minus_1
    )

    # Create output row
    out <- data.frame(
      maturity = i,
      root1 = ifelse(result$is_complex, NA, Re(result$roots[1])),
      root2 = ifelse(result$is_complex, NA, Re(result$roots[2])),
      root_distance = result$root_distance,
      is_complex = result$is_complex,
      convergence = result$convergence,
      stringsAsFactors = FALSE
    )

    # Add weights
    for (j in 1:n_pcs) {
      out[[paste0("weight_pc", j)]] <- result$optimal_weights[j]
    }

    list(row = out, full_result = result)
  }

  # Run optimization
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))

    # Export necessary objects and functions
    parallel::clusterExport(cl, c(
      "solve_gamma_quadratic_lincomb",
      "optimize_pc_weights"
    ),
    envir = environment()
    )

    results <- parallel::parLapply(cl, 1:n_maturities, optimize_one)
  } else {
    results <- lapply(1:n_maturities, optimize_one)
  }

  # Combine results
  df_results <- do.call(rbind, lapply(results, function(x) x$row))

  # Store full results as attribute
  attr(df_results, "full_results") <- lapply(results, function(x) x$full_result)

  df_results
}
