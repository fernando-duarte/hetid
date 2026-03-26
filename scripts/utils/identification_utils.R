# Identification Utilities -- non-optimization plumbing

#' @return data frame mapping component IDs to bond maturities
get_identification_maturity_lookup <- function() {
  data.frame(
    component_id = 1:8,
    bond_maturity = 2:9,
    component_label = paste0("maturity_", 1:8),
    bond_label = paste0("y", 2:9),
    stringsAsFactors = FALSE
  )
}

#' Load and prepare all identification inputs
#' @param n_pcs number of principal components
#' @return list with data, variable names, and lookup
load_identification_inputs <- function(
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS
) {
  data <- readRDS(file.path(OUTPUT_DIR, "temp/data.rds"))
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  list(
    data = data,
    yield_vars = paste0("y", 1:10),
    tp_vars = paste0("tp", 1:10),
    consumption_var = "gr1.pcecc96",
    pc_vars = paste0("pc", seq_len(n_pcs)),
    lookup = get_identification_maturity_lookup()
  )
}

#' Compute W1 and W2 residuals from data
#' @param data data frame with yields, term premia, PCs,
#'   and consumption growth
#' @param maturities bond maturities for W2 (default 2:9)
#' @param n_pcs number of principal components
#' @return list with w1, w2, pcs_aligned, and n_obs
compute_identification_residuals <- function(
  data,
  maturities = 2:9,
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS
) {
  cli::cli_alert_info("Computing W1 residuals...")
  w1_result <- compute_w1_residuals(
    n_pcs = n_pcs, data = data
  )

  # Extract yields, term premia, and PCs for W2
  yield_cols <- paste0("y", 1:10)
  tp_cols <- paste0("tp", 1:10)
  pc_cols <- paste0("pc", seq_len(n_pcs))

  yields_df <- data[, yield_cols]
  tp_df <- data[, tp_cols]
  pcs_mat <- as.matrix(data[, pc_cols])

  cli::cli_alert_info("Computing W2 residuals...")
  w2_result <- compute_w2_residuals(
    yields = yields_df,
    term_premia = tp_df,
    maturities = maturities,
    n_pcs = n_pcs,
    pcs = pcs_mat
  )

  # Assemble W2 residuals into a T x I matrix
  w2_mat <- do.call(
    cbind, w2_result$residuals
  )

  # Lagged PCs aligned with residuals:
  # W1_{t+1} and W2_{i,t+1} use PC_t (rows 1:T-1)
  n_resid <- length(w1_result$residuals)
  pcs_aligned <- pcs_mat[seq_len(n_resid), , drop = FALSE]

  list(
    w1 = w1_result$residuals,
    w2 = w2_mat,
    pcs_aligned = pcs_aligned,
    w1_result = w1_result,
    w2_result = w2_result,
    n_obs = n_resid
  )
}

#' @return list with scalar, vector, and matrix statistics
compute_identification_moments <- function(
  w1, w2, pcs, maturities = 1:8
) {
  cli::cli_alert_info("Computing scalar statistics...")
  scalar <- compute_scalar_statistics(w1, w2, maturities = maturities)
  cli::cli_alert_info("Computing vector statistics...")
  vector <- compute_vector_statistics(w1, w2, pcs, maturities = maturities)
  cli::cli_alert_info("Computing matrix statistics...")
  mat <- compute_matrix_statistics(w1, w2, maturities = maturities)

  list(
    s_i_0 = scalar$s_i_0,
    sigma_i_sq = scalar$sigma_i_sq,
    r_i_0 = vector$r_i_0,
    r_i_1 = vector$r_i_1,
    p_i_0 = vector$p_i_0,
    s_i_1 = mat$s_i_1,
    s_i_2 = mat$s_i_2
  )
}

#' Get baseline gamma matrix (VFCI unit-norm loadings)
#' @param method label for the method (stored as attr)
#' @param n_pcs number of principal components
#' @param n_components number of maturity components
#' @return J x I matrix with identical columns
get_baseline_gamma <- function(
  method = "vfci",
  n_pcs = 4,
  n_components = 8
) {
  # Unit-norm VFCI PC loading vector
  # From regressing variables$vfci on pc1:pc4:
  #   vfci = 0.1095399*pc1 - 0.1692329*pc2
  #        - 0.1320361*pc3 + 0.1699299*pc4
  unit_norm <- c(
    0.3714851, -0.5739232,
    -0.4477770, 0.5762870
  )

  gamma <- matrix(
    unit_norm,
    nrow = n_pcs,
    ncol = n_components
  )
  attr(gamma, "method") <- method
  gamma
}

#' Build tau specification (point and set values)
#' @param tau_point scalar tau for point identification
#' @param tau_set scalar tau for set identification
#' @param n_components number of maturity components
#' @return list with tau_point and tau_set vectors
get_tau_spec <- function(
  tau_point = 0,
  tau_set = 0.2,
  n_components = 8
) {
  list(
    tau_point = rep(tau_point, n_components),
    tau_set = rep(tau_set, n_components)
  )
}

#' @return list with components and quadratic system
build_quadratic_system <- function(
  gamma, tau, moments,
  maturities = 1:8
) {
  cli::cli_alert_info(
    "Computing identified set components..."
  )
  components <- compute_identified_set_components(
    gamma = gamma,
    r_i_0 = moments$r_i_0,
    r_i_1 = moments$r_i_1,
    p_i_0 = moments$p_i_0,
    maturities = maturities
  )

  cli::cli_alert_info(
    "Computing quadratic form..."
  )
  quadratic <- compute_identified_set_quadratic(
    gamma = gamma,
    tau = tau,
    L_i = components$L_i,
    V_i = components$V_i,
    Q_i = components$Q_i,
    s_i_0 = moments$s_i_0,
    s_i_1 = moments$s_i_1,
    s_i_2 = moments$s_i_2,
    sigma_i_sq = moments$sigma_i_sq,
    maturities = maturities
  )

  list(
    components = components,
    quadratic = quadratic
  )
}
