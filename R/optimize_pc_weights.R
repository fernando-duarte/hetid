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
