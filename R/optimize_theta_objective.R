#' Objective Function for Theta Optimization
#'
#' Internal function that creates the objective function for theta optimization
#'
#' @name theta_objective
#' @keywords internal
NULL

#' Create objective function for theta optimization
#' @noRd
create_theta_objective <- function(pcs, w1, w2, n_pcs) {
  function(params) {
    tau <- params[1]
    weights_raw <- params[2:(n_pcs + 1)]

    # Use unified function for solving
    result <- solve_theta_quadratic_lincomb(
      pcs = pcs,
      weights = weights_raw, # Let the function handle normalization
      w1 = w1,
      w2 = w2,
      tau = tau,
      normalize_by = "norm", # Use L2 norm normalization
      use_t_minus_1 = TRUE, # Use unbiased estimators
      return_df = FALSE
    )

    # Check for errors
    if (!is.null(result$error)) {
      return(HETID_CONSTANTS$OPTIMIZATION_PENALTY)
    }

    # Check if roots are complex
    if (is.complex(result$roots)) {
      return(HETID_CONSTANTS$OPTIMIZATION_PENALTY)
    }

    # Objective: minimize distance between roots
    return(abs(result$roots[1] - result$roots[2]))
  }
}
