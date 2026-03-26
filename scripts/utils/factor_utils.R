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
    "y", seq_len(HETID_CONSTANTS$MAX_MATURITY)
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
  all_mats <- seq_len(
    HETID_CONSTANTS$EFFECTIVE_MAX_MATURITY
  )
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
  loadings_sel <- pca$loadings[all_mats, factors,
    drop = FALSE
  ]

  cli::cli_alert_info(
    "Projecting W2 onto {length(factors)} factors..."
  )
  w2_all_mat %*% loadings_sel
}
