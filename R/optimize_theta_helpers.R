#' Data Preparation Functions for Theta Optimization
#'
#' Internal functions for preparing data for theta optimization
#'
#' @name theta_data_prep
#' @keywords internal
NULL

#' Validate inputs for theta optimization
#' @noRd
validate_theta_inputs <- function(i, n_pcs, pc_data, maturities) {
  if (i < 2) {
    stop("Maturity must be >= 2")
  }

  if (!i %in% maturities) {
    stop(paste("Maturity", i, "not found in available maturities"))
  }

  if (n_pcs < 1 || n_pcs > 6) {
    stop("n_pcs must be between 1 and 6")
  }

  if (ncol(pc_data) < n_pcs) {
    stop(paste("pc_data must have at least", n_pcs, "columns"))
  }
}

#' Prepare data for theta optimization
#' @importFrom stats complete.cases
#' @noRd
prepare_theta_data <- function(i, n_pcs, pc_data,
                               yields, term_premia, verbose) {
  # Extract the first n_pcs principal components
  pc_matrix <- pc_data[, 1:n_pcs]

  # Compute residuals
  if (verbose) cat("Computing residuals...\n")

  # W1 residuals (using lagged PCs)
  res_w1 <- compute_w1_residuals(n_pcs = n_pcs)
  w1 <- res_w1$residuals

  # W2 residuals for the specified maturity
  res_w2 <- compute_w2_residuals(yields, term_premia, maturities = i)
  w2 <- res_w2$residuals[[as.character(i)]]

  # Align data (remove NAs)
  n_obs <- min(length(w1), length(w2), nrow(pc_matrix))
  w1 <- w1[1:n_obs]
  w2 <- w2[1:n_obs]
  pc_matrix <- pc_matrix[1:n_obs, ]

  # Remove any remaining NAs
  valid_idx <- complete.cases(w1, w2, pc_matrix)
  w1 <- w1[valid_idx]
  w2 <- w2[valid_idx]
  pc_matrix <- pc_matrix[valid_idx, ]

  if (verbose) {
    cat("Data dimensions after alignment:\n")
    cat("  w1:", length(w1), "\n")
    cat("  w2:", length(w2), "\n")
    cat("  pc_matrix:", nrow(pc_matrix), "x", ncol(pc_matrix), "\n")
  }

  list(w1 = w1, w2 = w2, pc_matrix = pc_matrix)
}
