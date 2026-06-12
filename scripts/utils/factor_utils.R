# Yield Curve Factor Utilities
# PCA-based level/slope/curvature factors for identification

DEFAULT_ID_FACTORS <- c(1, 2, 3)
FACTOR_LABELS <- c("level", "slope", "curvature")

#' Compute yield curve PCA loadings
#' @param data data frame containing y1:y10 columns
#' @param n_factors number of factors to extract
#' @return list with loadings, sdev, pct_variance
compute_yield_factor_loadings <- function(
  data, n_factors = 3
) {
  yield_cols <- paste0(
    YIELD_PREFIX, HETID_CONSTANTS$DEFAULT_ACM_MATURITIES
  )
  missing <- setdiff(yield_cols, names(data))
  if (length(missing) > 0) {
    stop(
      "Missing yield columns: ",
      paste(missing, collapse = ", ")
    )
  }
  yield_mat <- as.matrix(data[, yield_cols])
  pca <- prcomp(yield_mat, center = TRUE, scale. = FALSE)
  total_var <- sum(pca$sdev^2)
  pct_var <- pca$sdev^2 / total_var * 100

  list(
    loadings = pca$rotation[, seq_len(n_factors),
      drop = FALSE
    ],
    sdev = pca$sdev[seq_len(n_factors)],
    pct_variance = pct_var[seq_len(n_factors)]
  )
}

#' Build factor lookup table
#' @param factors integer vector of factor indices
#' @return data frame mapping component IDs to factors
get_identification_factor_lookup <- function(
  factors = DEFAULT_ID_FACTORS
) {
  n <- length(factors)
  labels <- vapply(factors, function(f) {
    if (f <= length(FACTOR_LABELS)) {
      FACTOR_LABELS[f]
    } else {
      paste0("factor_", f)
    }
  }, character(1))

  data.frame(
    component_id = seq_len(n),
    factor_index = factors,
    component_label = labels,
    factor_label = paste0("factor_", factors),
    stringsAsFactors = FALSE
  )
}

#' Compute W2 projected onto yield PCA factors
#' @param yields_df yields data frame
#' @param tp_df term premia data frame
#' @param pcs_mat PC matrix
#' @param n_pcs number of PCs
#' @param data full data frame (for yield PCA)
#' @param factors which factor indices to use
#' @return T x n_factors matrix of projected W2 residuals
compute_w2_factor_residuals <- function(
  yields_df, tp_df, pcs_mat, n_pcs, data, factors
) {
  node_mats <- HETID_CONSTANTS$DEFAULT_ACM_MATURITIES
  all_mats <- node_mats[
    node_mats <= effective_max_maturity(HETID_CONSTANTS$DEFAULT_STEP)
  ]
  # Loadings rows are the annual yield nodes; index them by position,
  # not by month value
  mat_rows <- match(all_mats, node_mats)
  cli::cli_alert_info(
    "Computing W2 for all maturities ({length(all_mats)})..."
  )
  w2_all <- compute_w2_residuals(
    yields = yields_df, term_premia = tp_df,
    maturities = all_mats,
    n_pcs = n_pcs, pcs = pcs_mat
  )
  w2_all_mat <- do.call(cbind, w2_all$residuals)

  cli::cli_alert_info("Computing yield PCA loadings...")
  pca <- compute_yield_factor_loadings(data, max(factors))
  loadings_sel <- pca$loadings[mat_rows, factors,
    drop = FALSE
  ]

  cli::cli_alert_info(
    "Projecting W2 onto {length(factors)} factors..."
  )
  w2_factors <- w2_all_mat %*% loadings_sel
  # Attach the reduced-form per-factor gamma. Opportunistic: a failure must NOT
  # break the default (vfci) path, so wrap in tryCatch -> NULL; it is consumed
  # only when BASELINE_GAMMA_METHOD == "reduced_form".
  attr(w2_factors, "gamma_rf") <- tryCatch(
    get_reduced_form_gamma(w2_all$coefficients, pca$loadings, mat_rows, factors),
    error = function(e) {
      cli::cli_alert_warning(
        "Reduced-form gamma unavailable: {conditionMessage(e)}"
      )
      NULL
    }
  )
  w2_factors
}

#' Reduced-form per-factor gamma (J x I loadings)
#'
#' Each column is the levels-regression loading of a yield-curve factor on the
#' PCs: the Y2-on-PC slopes (from `compute_w2_residuals()$coefficients`, NOT the
#' residuals -- those are orthogonal to the PCs by construction and ~0) projected
#' onto the yield-factor loadings. Scale is irrelevant to the identified set
#' (each column enters its own constraint homogeneously, degree 2); `normalize`
#' is cosmetic, matching the VFCI/optimizer unit-norm convention.
#' @param coefficients n_mat x (1 + n_pcs) levels-regression coefficients
#' @param loadings yield-curve factor loading matrix (rows = maturities)
#' @param maturities maturity rows used to build `coefficients` (index `loadings`)
#' @param factors factor-column indices
#' @param normalize unit-normalize columns (default TRUE)
#' @return n_pcs x length(factors) matrix
get_reduced_form_gamma <- function(coefficients, loadings, maturities, factors,
                                   normalize = TRUE) {
  pc_slopes <- coefficients[, colnames(coefficients) != "(Intercept)",
    drop = FALSE
  ]
  load_sel <- loadings[maturities, factors, drop = FALSE]
  if (nrow(pc_slopes) != nrow(load_sel)) {
    stop("reduced-form gamma: coefficient rows != loading rows")
  }
  if (anyNA(pc_slopes) || anyNA(load_sel)) {
    stop("reduced-form gamma: NA in coefficients or loadings")
  }
  gamma <- t(pc_slopes) %*% load_sel
  # Guard against the residual-orthogonality trap: real levels slopes are
  # O(1e-3); near-machine-zero magnitude means the wrong (residual) object.
  if (max(abs(gamma)) < 1e-10) {
    stop("reduced-form gamma is ~0 (residual-orthogonality trap?)")
  }
  if (normalize) gamma <- normalize_gamma_columns(gamma)
  gamma
}
