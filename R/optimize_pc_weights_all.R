#' Optimize PC Weights for All Maturities
#'
#' Runs the PC weight optimization for all maturities and returns
#' a summary of results.
#'
#' @param pc_matrix Matrix of principal components (T x J)
#' @param w1 Vector of reduced form residuals for Y1
#' @param w2_list List of reduced form residuals for Y2, one for each maturity
#' @param tau Quantile parameter
#' @param use_t_minus_1 Whether to use lagged PCs (default TRUE)
#' @param parallel Whether to run in parallel (default FALSE)
#' @param n_cores Number of cores to use if parallel=TRUE
#'
#' @return Data frame with optimization results for each maturity
#'
#' @export
optimize_pc_weights_all <- function(pc_matrix, w1, w2_list, tau,
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
