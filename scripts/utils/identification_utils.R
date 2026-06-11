# Identification Utilities -- non-optimization plumbing

# Default bond maturities for identification
DEFAULT_ID_MATURITIES <- c(2, 5, 9)

#' @return data frame mapping component IDs to bond maturities
get_identification_maturity_lookup <- function(
  maturities = DEFAULT_ID_MATURITIES
) {
  n <- length(maturities)
  data.frame(
    component_id = seq_len(n),
    bond_maturity = maturities,
    component_label = paste0(HETID_CONSTANTS$MATURITY_PREFIX, seq_len(n)),
    bond_label = paste0(YIELD_PREFIX, maturities),
    stringsAsFactors = FALSE
  )
}

#' @param mode "maturities" or "factors"
#' @return list with data, variable names, and lookup
load_identification_inputs <- function(
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
  maturities = DEFAULT_ID_MATURITIES,
  mode = "maturities",
  factors = DEFAULT_ID_FACTORS
) {
  data <- readRDS(DATA_RDS_PATH)
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  lookup <- if (mode == "factors") {
    get_identification_factor_lookup(factors)
  } else {
    get_identification_maturity_lookup(maturities)
  }

  list(
    data = data,
    yield_vars = paste0(YIELD_PREFIX, seq_len(HETID_CONSTANTS$MAX_MATURITY)),
    tp_vars = paste0(TP_PREFIX, seq_len(HETID_CONSTANTS$MAX_MATURITY)),
    consumption_var = HETID_CONSTANTS$CONSUMPTION_GROWTH_COL,
    pc_vars = paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(n_pcs)),
    lookup = lookup,
    mode = mode
  )
}

# cbind-ing the per-maturity W2 residual vectors assumes row t means the SAME
# time period in every column: no maturity dropped rows that another kept.
# compute_w2_residuals() filters incomplete rows PER MATURITY (kept_idx), so
# verify complete-data alignment before flattening -- equal residual lengths,
# identical kept_idx across maturities, and kept rows forming the leading
# block of the regression sample (so residual row t is sample row t, which the
# downstream pcs_aligned subset also assumes).
assert_w2_alignment <- function(w2_result) {
  lens <- vapply(w2_result$residuals, length, integer(1))
  if (length(unique(lens)) > 1) {
    stop(
      "W2 residual vectors have unequal lengths (",
      paste(lens, collapse = ", "),
      "); per-maturity rows were dropped, so cbind would misalign dates"
    )
  }
  kept <- w2_result$kept_idx
  if (!is.null(kept) && length(kept) > 0) {
    ref <- kept[[1]]
    if (!all(vapply(kept, identical, logical(1), ref))) {
      stop(
        "W2 kept_idx differs across maturities; ",
        "cbind would glue residuals from different dates into one row"
      )
    }
    if (!identical(which(ref), seq_len(lens[[1]]))) {
      stop(
        "W2 kept_idx dropped non-trailing rows of the regression sample; ",
        "residual row t no longer corresponds to sample row t"
      )
    }
  }
  invisible(w2_result)
}

#' @param mode "maturities" or "factors"
#' @return list with w1, w2, pcs_aligned, and n_obs
compute_identification_residuals <- function(
  data,
  maturities = DEFAULT_ID_MATURITIES,
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
  mode = "maturities",
  factors = DEFAULT_ID_FACTORS
) {
  cli::cli_alert_info("Computing W1 residuals...")
  w1_result <- compute_w1_residuals(
    n_pcs = n_pcs, data = data
  )

  yield_cols <- paste0(YIELD_PREFIX, seq_len(HETID_CONSTANTS$MAX_MATURITY))
  tp_cols <- paste0(TP_PREFIX, seq_len(HETID_CONSTANTS$MAX_MATURITY))
  pc_cols <- paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(n_pcs))
  yields_df <- data[, yield_cols]
  tp_df <- data[, tp_cols]
  pcs_mat <- as.matrix(data[, pc_cols])

  if (mode == "factors") {
    w2_mat <- compute_w2_factor_residuals(
      yields_df, tp_df, pcs_mat, n_pcs, data, factors
    )
  } else {
    cli::cli_alert_info("Computing W2 residuals...")
    w2_result <- compute_w2_residuals(
      yields = yields_df, term_premia = tp_df,
      maturities = maturities,
      n_pcs = n_pcs, pcs = pcs_mat
    )
    assert_w2_alignment(w2_result)
    w2_mat <- do.call(cbind, w2_result$residuals)
  }

  n_resid <- length(w1_result$residuals)
  z_mat <- get_identification_z(data, pcs_mat)
  pcs_aligned <- z_mat[seq_len(n_resid), , drop = FALSE]

  result <- list(
    w1 = w1_result$residuals,
    w2 = w2_mat,
    pcs_aligned = pcs_aligned,
    w1_result = w1_result,
    n_obs = n_resid
  )
  if (mode == "factors") {
    result$factor_loadings <- compute_yield_factor_loadings(
      data, max(factors)
    )
    result$gamma_rf <- attr(w2_mat, "gamma_rf")
  }
  result
}

# compute_identification_moments() and build_quadratic_system() are now
# exported by the hetid package (loaded via common_settings.R) with the
# same names and compatible defaults; the moments object is a validated
# hetid_moments container carrying the maturity identity.

#' Get baseline gamma matrix (VFCI unit-norm loadings)
#' @param method label for the method (stored as attr)
#' @param n_pcs number of principal components; must equal the length of the
#'   VFCI loading vector (4) -- the loading is defined only on pc1..pc4, so
#'   any other value errors rather than silently recycling
#' @param n_components number of components (NULL = infer)
#' @return J x I matrix with identical columns
get_baseline_gamma <- function(
  method = "vfci",
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
  n_components = NULL
) {
  if (is.null(n_components)) {
    n_components <- length(DEFAULT_ID_MATURITIES)
  }
  # Unit-norm VFCI PC loading vector
  # From regressing variables$vfci on pc1:pc4:
  #   vfci = 0.1095399*pc1 - 0.1692329*pc2
  #        - 0.1320361*pc3 + 0.1699299*pc4
  unit_norm <- c(
    0.3714851, -0.5739232,
    -0.4477770, 0.5762870
  )
  if (n_pcs != length(unit_norm)) {
    stop(
      "get_baseline_gamma: the VFCI unit-norm loading is defined only for ",
      length(unit_norm), " PCs (pc1..pc4); got n_pcs = ", n_pcs,
      " -- recycling it would produce wrong loadings"
    )
  }
  gamma <- matrix(unit_norm, nrow = n_pcs, ncol = n_components)
  attr(gamma, "method") <- method
  gamma
}

#' @return list with tau_point and tau_set vectors
get_tau_spec <- function(
  tau_point = 0,
  tau_set = BASELINE_TAU,
  n_components = NULL
) {
  if (is.null(n_components)) {
    n_components <- length(DEFAULT_ID_MATURITIES)
  }
  list(
    tau_point = rep(tau_point, n_components),
    tau_set = rep(tau_set, n_components)
  )
}
