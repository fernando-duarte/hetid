# Identification residual construction on the aligned quarterly panel.

#' @return list with w1, w2, pcs_aligned, n_obs, w1_result, and
#'   w2_coefficients = the Y2-on-PC reduced-form coefficient matrix
compute_identification_residuals <- function(
  data,
  maturities = DEFAULT_ID_MATURITIES,
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
  step = NEWS_STEP,
  y1_lags = if (exists("N_Y1_LAGS")) N_Y1_LAGS else 0L
) {
  cli::cli_alert_info("Computing W1 residuals...")
  w1_result <- compute_w1_residuals(
    n_pcs = n_pcs, data = data, y1_lags = y1_lags
  )
  assert_w1_leading_block(w1_result)

  # Carry every maturity column the data provides: the quarterly news
  # clock needs step-adjacent sub-annual maturities around each horizon
  yield_cols <- grep("^y[0-9]+$", names(data), value = TRUE)
  tp_cols <- grep("^tp[0-9]+$", names(data), value = TRUE)
  pc_cols <- paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(n_pcs))
  yields_df <- data[, yield_cols]
  tp_df <- data[, tp_cols]
  pcs_mat <- as.matrix(data[, pc_cols])

  # Condition W2 on the SAME common X_t = (1, PC_t, H lags of Y1) as W1 (or
  # impose the exact-news projection B = 0). impose_news_projection_zero() is
  # resolved by the active paper-pipeline configuration.
  cli::cli_alert_info("Computing W2 residuals...")
  y1 <- data[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]]
  w2_result <- compute_w2_residuals(
    yields = yields_df, term_premia = tp_df,
    maturities = maturities,
    n_pcs = n_pcs, pcs = pcs_mat,
    step = step,
    dates = data$date, # full-T; the package shifts to the t+1 realization dates
    y1 = y1, y1_lags = y1_lags,
    impose_b_zero = impose_news_projection_zero()
  )
  assert_w2_alignment(w2_result)
  w2_mat <- do.call(cbind, w2_result$residuals)

  # Align W1, W2, and the instruments on the actual retained CALENDAR DATES, not
  # on length offsets. With the common conditioning vector X_t both W1 and W2
  # drop the SAME H-1 leading lag rows, so an offset-based trim (which keys on
  # nrow(W2) - nrow(W1)) silently leaves the instruments anchored to panel row 1
  # while the residuals start H quarters later. Date intersection is immune to
  # that: each residual is paired with the instrument row for its OWN period.
  if (!"date" %in% names(data)) {
    cli::cli_abort("data must carry a 'date' column for calendar-date alignment")
  }
  panel_dates <- data$date
  w1_dates <- w1_result$dates
  w2_dates <- w2_response_dates(w2_result$kept_idx, panel_dates)
  # base::intersect/match/order key on the quarterly panel dates, which are
  # unique and non-NA (one row per quarter-end); pin base::intersect so the set
  # semantics remain stable if dplyr masks intersect().
  common_dates <- base::intersect(as.character(w1_dates), as.character(w2_dates))
  common_dates <- panel_dates[match(common_dates, as.character(panel_dates))]
  common_dates <- common_dates[order(common_dates)]
  if (length(common_dates) == 0L) {
    cli::cli_abort("W1 and W2 share no common calendar dates")
  }

  # Map the common date set into each ALREADY-COMPRESSED series' own positions
  # (w1/w2 residuals are post-complete.cases, so an absolute panel mask would
  # mis-index them). For the instruments, predictor row t pairs with date[t+1],
  # so select the predictor rows [1:(T-1)] whose response date is in the set.
  z_mat <- get_identification_z(data, pcs_mat)
  n_panel <- length(panel_dates)
  z_response_dates <- panel_dates[-1L]
  z_pred <- z_mat[seq_len(n_panel - 1L), , drop = FALSE]

  w1_sel <- match(as.character(common_dates), as.character(w1_dates))
  w2_sel <- match(as.character(common_dates), as.character(w2_dates))
  z_sel <- match(as.character(common_dates), as.character(z_response_dates))

  w1_aligned <- w1_result$residuals[w1_sel]
  w2_mat <- w2_mat[w2_sel, , drop = FALSE]
  pcs_aligned <- z_pred[z_sel, , drop = FALSE]
  attr(pcs_aligned, "dates") <- common_dates

  # Fail closed: the three retained-date vectors must be IDENTICAL after the
  # date-keyed subset. Anything else means an instrument is paired with the
  # wrong period (the silent shift this whole mechanism exists to prevent).
  aligned_w1_dates <- w1_dates[w1_sel]
  aligned_z_dates <- z_response_dates[z_sel]
  if (!identical(as.character(aligned_w1_dates), as.character(common_dates)) ||
    !identical(as.character(aligned_z_dates), as.character(common_dates))) {
    cli::cli_abort(
      "W1 / W2 / instrument dates disagree after date-keyed alignment"
    )
  }

  n_resid <- length(common_dates)
  result <- list(
    w1 = w1_aligned,
    w2 = w2_mat,
    pcs_aligned = pcs_aligned,
    w1_result = w1_result,
    n_obs = n_resid,
    # The common retained dates A5 fits the structural recovery on. Under
    # estimate-B, beta1R (w1_result$coefficients) and beta2R (w2_coefficients)
    # come from INDEPENDENT full fits; A5 handles the common-sample recovery
    # identity beta1(theta) = beta1R - (beta2R)' theta on these dates.
    dates = common_dates
  )
  # Retain beta2R (the Y2-on-PC reduced-form coefficient matrix, I x (1+n_pcs),
  # = the point-identified beta20) for structural-coefficient recovery; it is
  # dropped by the cbind flatten above otherwise.
  result$w2_coefficients <- w2_result$coefficients
  result
}
