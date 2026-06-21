# Paper-spec residuals: collapse the per-maturity SDF news into ONE principal
# component (Y2, I = 1) and align the single de-meaned VFCI instrument (Z, J = 1).
#
# Reuses compute_identification_residuals() for ALL date alignment (w1, the
# per-maturity estimate-B residual matrix, the VFCI instrument, beta2R, and the
# retained dates). The single news factor is the RAW (uncentered) projection onto
# the PC1 direction -- NOT prcomp scores, which are mean-centered and would break
# the reduced-form intercept. Because each per-maturity residual is
# resid_i = news_i - X . beta2R_i (package guarantee) and the combine is linear,
#   Y2  = news_mat %*% combo
#   W2  = resid_mat %*% combo  (= residual of Y2 on X_t, EXACTLY)
#   b2R = t(beta2R) %*% combo  (intercept correct, because Y2 is uncentered)
# hold with no intercept correction. combo = w / sd_i (correlation PCA) or w
# (covariance PCA), w = PC1 loadings.

# Reconcile the per-maturity columns/rows of the three objects by NAME (never by
# position): the news levels, the estimate-B residuals, and the beta2R rows are
# each named by maturity. Returns the common maturity order; aborts on a name
# mismatch and logs any maturity dropped by a skip.
.reconcile_maturities <- function(news_full, resid_mat, beta2r_mat) {
  common <- intersect(
    intersect(colnames(news_full), colnames(resid_mat)),
    rownames(beta2r_mat)
  )
  if (length(common) == 0L) {
    stop("paper_spec_residuals: news / residual / beta2R share no maturity names")
  }
  dropped <- setdiff(
    union(union(colnames(news_full), colnames(resid_mat)), rownames(beta2r_mat)),
    common
  )
  if (length(dropped) > 0L) {
    cli::cli_alert_warning(
      "paper_spec_residuals: dropping maturities present in only some objects: {dropped}"
    )
  }
  common
}

compute_paper_spec_residuals <- function(data,
                                         n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
                                         y1_lags = if (exists("N_Y1_LAGS")) {
                                           N_Y1_LAGS
                                         } else {
                                           4L
                                         },
                                         step = if (exists("NEWS_STEP")) NEWS_STEP else 3L,
                                         news_pc_scale = "correlation") {
  news_pc_scale <- match.arg(news_pc_scale, c("correlation", "covariance"))
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  feasible <- hetid:::default_w2_maturities(step)

  # Battle-tested alignment: w1, per-maturity estimate-B residuals (resid$w2),
  # the VFCI instrument (pcs_aligned), beta2R, retained dates. HETID_Z_SOURCE
  # must point at vfci_demeaned.R (set via withr by the caller).
  resid <- compute_identification_residuals(
    data = data, maturities = feasible,
    n_pcs = n_pcs, step = step, y1_lags = y1_lags
  )
  z_mat <- resid$pcs_aligned
  if (ncol(z_mat) != 1L || !identical(colnames(z_mat), "vfci_dm")) {
    stop(
      "compute_paper_spec_residuals: expected the J=1 'vfci_dm' instrument, got ",
      ncol(z_mat), " column(s) (", paste(colnames(z_mat), collapse = ", "),
      "). Set HETID_Z_SOURCE to scripts/z_sources/vfci_demeaned.R."
    )
  }
  dates <- resid$dates
  resid_mat <- resid$w2 # T x M estimate-B residuals, date-aligned
  beta2r_mat <- resid$w2_coefficients # M x p per-maturity beta2R

  # Per-maturity SDF news LEVELS (impose_b_zero), aligned to the same dates.
  yield_cols <- grep("^y[0-9]+$", names(data), value = TRUE)
  tp_cols <- grep("^tp[0-9]+$", names(data), value = TRUE)
  pcs_mat <- as.matrix(data[, paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(n_pcs))])
  y1 <- data[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]]
  news_res <- compute_w2_residuals(
    yields = data[, yield_cols], term_premia = data[, tp_cols],
    maturities = feasible, n_pcs = n_pcs, pcs = pcs_mat, step = step,
    dates = data$date, # full-T; package shifts to t+1 realization dates
    y1 = y1, y1_lags = y1_lags, impose_b_zero = TRUE
  )
  assert_w2_alignment(news_res)
  news_full <- do.call(cbind, news_res$residuals)
  news_dates <- w2_response_dates(news_res$kept_idx, data$date)
  nsel <- match(as.character(dates), as.character(news_dates))
  if (anyNA(nsel)) {
    stop("paper_spec_residuals: SDF-news levels do not cover all aligned dates")
  }
  news_mat <- news_full[nsel, , drop = FALSE]

  # Reconcile maturities by name, then put all three in the common order. The
  # labels are "maturity_<months>"; carry the numeric months for reporting.
  common <- .reconcile_maturities(news_mat, resid_mat, beta2r_mat)
  news_mat <- news_mat[, common, drop = FALSE]
  resid_mat <- resid_mat[, common, drop = FALSE]
  beta2r_mat <- beta2r_mat[common, , drop = FALSE]
  used_maturities <- as.integer(sub("^maturity_", "", common))
  if (nrow(news_mat) != length(dates) || nrow(resid_mat) != length(dates)) {
    stop("paper_spec_residuals: row counts disagree after date alignment")
  }

  news_sd <- apply(news_mat, 2, stats::sd)
  if (!all(is.finite(news_sd) & news_sd > 0)) {
    stop("paper_spec_residuals: a maturity's SDF news has zero/non-finite variance")
  }

  pc <- stats::prcomp(news_mat, center = TRUE, scale. = (news_pc_scale == "correlation"))
  w <- pc$rotation[, 1]
  pc_var_explained <- pc$sdev[1]^2 / sum(pc$sdev^2)
  combo <- if (news_pc_scale == "correlation") w / news_sd else w

  # Deterministic sign: orient the factor to co-move with the average news.
  s <- stats::cor(as.numeric(news_mat %*% combo), rowMeans(news_mat))
  sgn <- if (is.na(s) || s == 0) sign(w[which.max(abs(w))]) else sign(s)
  w <- w * sgn
  combo <- combo * sgn

  # Raw-projection combine (exact, no intercept correction).
  y2 <- as.numeric(news_mat %*% combo)
  w2_single <- as.numeric(resid_mat %*% combo)
  beta2r_y2 <- as.numeric(crossprod(beta2r_mat, combo))
  names(beta2r_y2) <- colnames(beta2r_mat)

  beta1r <- resid$w1_result$coefficients

  # Aligned LEVELS for Table 1: Y1 (response-date consumption growth) and the
  # common design X_t (predictor-row PCs + Y1 lags) date-matched to the response.
  y1_level <- y1[match(as.character(dates), as.character(data$date))]
  x_full <- hetid:::build_common_conditioning(pcs_mat, n_pcs, y1, y1_lags)
  x_pred <- x_full[seq_len(nrow(x_full) - 1L), , drop = FALSE]
  xsel <- match(as.character(dates), as.character(data$date[-1L]))
  if (anyNA(xsel)) {
    stop("paper_spec_residuals: common design does not cover all aligned dates")
  }
  design_aligned <- x_pred[xsel, , drop = FALSE]

  # Instrument as a plain vector, de-meaned on the ALIGNED sample (moment-invariant).
  z <- as.numeric(z_mat)
  z <- z - mean(z)

  list(
    w1 = as.numeric(resid$w1),
    w2 = w2_single,
    y2 = y2,
    z = z,
    beta1r = beta1r,
    beta2r = beta2r_y2,
    dates = dates,
    n_obs = length(dates),
    r2_w1 = resid$w1_result$r_squared,
    r2_y2 = 1 - stats::var(w2_single) / stats::var(y2),
    news_loadings = stats::setNames(w, used_maturities),
    news_weights = stats::setNames(combo, used_maturities),
    feasible_maturities = feasible,
    used_maturities = used_maturities,
    news_pc_scale = news_pc_scale,
    pc_var_explained = pc_var_explained,
    y1_level = y1_level,
    design_aligned = design_aligned
  )
}
