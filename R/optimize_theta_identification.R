#' Optimize Theta (Heteroskedasticity Parameter) Identification
#'
#' Solves the optimization problem P(i,J) to identify the heteroskedasticity
#' parameter theta_i for a given maturity using J principal components.
#' This function finds the optimal tau and PC weights that minimize
#' the distance between the two roots of the quadratic equation.
#'
#' @param i Bond maturity in years (must be >= 2)
#' @param n_pcs Number of principal components to use (J = 1 to 6, default 4)
#' @param pc_data Matrix of principal components (T x K) where K >= n_pcs
#' @param yields Matrix of yields (T x M) where M is number of maturities
#' @param term_premia Matrix of term premia (T x M)
#' @param maturities Vector of all available maturities
#' @param tau_bounds Vector of length 2 with bounds for tau (default c(0, 0.99))
#' @param weight_bounds Scalar bound for weights (default 2, so -2 <= w <= 2)
#' @param n_starts Number of random starting points for multi-start optimization (default 20)
#' @param algorithm Optimization algorithm (default "COBYLA")
#' @param verbose Logical, whether to print progress (default FALSE)
#'
#' @return List containing:
#'   \item{maturity}{The maturity used}
#'   \item{n_pcs}{Number of PCs used (J)}
#'   \item{tau_opt}{Optimal tau value}
#'   \item{optimal_weights}{Optimal PC weights (normalized)}
#'   \item{theta_lower}{Lower bound of identified set for theta}
#'   \item{theta_upper}{Upper bound of identified set for theta}
#'   \item{interval_width}{Width of the identified interval}
#'   \item{objective_value}{Optimal objective function value}
#'   \item{convergence}{Convergence status from optimizer}
#'   \item{time_elapsed}{Computation time in seconds}
#'   \item{A}{Quadratic coefficient A}
#'   \item{B}{Quadratic coefficient B}
#'   \item{C}{Quadratic coefficient C}
#'   \item{discriminant}{B^2 - 4AC}
#'
#' @importFrom nloptr nloptr
#' @importFrom stats rnorm runif complete.cases
#' @export
optimize_theta_identification <- function(i,
                                          n_pcs = 4,
                                          pc_data,
                                          yields,
                                          term_premia,
                                          maturities,
                                          tau_bounds = c(0, 0.99),
                                          weight_bounds = 2,
                                          n_starts = 20,
                                          algorithm = "NLOPT_LN_COBYLA",
                                          verbose = FALSE) {
  # Input validation
  validate_theta_inputs(i, n_pcs, pc_data, maturities)

  # Start timing
  start_time <- Sys.time()

  # Prepare data
  data_prep <- prepare_theta_data(
    i, n_pcs, pc_data,
    yields, term_premia, verbose
  )
  w1 <- data_prep$w1
  w2 <- data_prep$w2
  pcs <- data_prep$pc_matrix

  # Define objective function
  objective_fn <- create_theta_objective(pcs, w1, w2, n_pcs)

  # Constraint function: sum of squared weights = 1
  constraint_fn <- function(params) {
    weights_raw <- params[2:(n_pcs + 1)]
    return(sum(weights_raw^2) - 1)
  }

  # Run multi-start optimization
  opt_result <- run_multistart_optimization(
    objective_fn = objective_fn,
    constraint_fn = constraint_fn,
    n_pcs = n_pcs,
    tau_bounds = tau_bounds,
    weight_bounds = weight_bounds,
    n_starts = n_starts,
    algorithm = algorithm,
    verbose = verbose
  )

  # Extract optimal parameters
  tau_opt <- opt_result$solution[1]
  weights_raw_opt <- opt_result$solution[2:(n_pcs + 1)]

  # Compute final results
  final_result <- solve_theta_quadratic_lincomb(
    pcs = pcs,
    weights = weights_raw_opt,
    w1 = w1,
    w2 = w2,
    tau = tau_opt,
    normalize_by = "norm",
    use_t_minus_1 = TRUE,
    return_df = FALSE
  )

  # Extract and format results
  results <- extract_theta_results(
    final_result = final_result,
    i = i,
    n_pcs = n_pcs,
    tau_opt = tau_opt,
    best_objective = opt_result$objective,
    convergence = opt_result$status,
    start_time = start_time
  )

  return(results)
}
