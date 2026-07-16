# Identification inputs and residual-date alignment.

# Default bond maturities for identification (months)
DEFAULT_ID_MATURITIES <- c(24L, 60L, 108L)

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

#' @return list with data, variable names, and lookup
load_identification_inputs <- function(
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
  maturities = DEFAULT_ID_MATURITIES
) {
  data <- readRDS(DATA_RDS_PATH)
  if (is.list(data) && !is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  list(
    data = data,
    yield_vars = paste0(YIELD_PREFIX, HETID_CONSTANTS$DEFAULT_ACM_MATURITIES),
    tp_vars = paste0(TP_PREFIX, HETID_CONSTANTS$DEFAULT_ACM_MATURITIES),
    consumption_var = HETID_CONSTANTS$CONSUMPTION_GROWTH_COL,
    pc_vars = paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(n_pcs)),
    lookup = get_identification_maturity_lookup(maturities)
  )
}

# cbind-ing the per-maturity W2 residual vectors assumes row t means the SAME
# time period in every column: no maturity dropped rows that another kept.
# compute_w2_residuals() filters incomplete rows PER MATURITY (kept_idx), so
# verify complete-data alignment before flattening -- equal residual lengths,
# identical kept_idx across maturities, and the kept rows forming a CONTIGUOUS
# block (a leading lag-NA prefix may drop, but no interior gap that would shift
# residual row t off its calendar period). The date-based alignment downstream
# no longer assumes W2 keeps MORE rows than W1: with the common conditioning
# vector X_t, W2 drops the same H-1 leading lag rows as W1.
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
    k <- which(ref)
    if (length(k) > 0L && !identical(k, seq.int(k[1L], k[length(k)]))) {
      stop(
        "W2 kept_idx dropped interior rows of the regression sample; ",
        "the kept rows are not a contiguous block, so residual row t no ",
        "longer maps to a single calendar period"
      )
    }
  }
  invisible(w2_result)
}

# Map a W2 (or W1) per-row complete-case mask, defined over the T-1 PREDICTOR
# rows [1:(T-1)], onto the calendar RESPONSE dates of the retained rows.
#
# Both reduced forms regress a leading outcome / news term (length T-1, indexed
# by predictor row t in 1..T-1) on predictor-row regressors, so predictor row t
# pairs with the RESPONSE realized at date[t+1]. Hence the retained response
# dates are date[2:T][mask] -- exactly the convention compute_w1_residuals uses
# for w1_result$dates, which lets W1, W2, and the instruments be aligned in one
# common date space rather than by fragile length offsets.
w2_response_dates <- function(kept_idx, panel_dates) {
  if (is.null(kept_idx) || length(kept_idx) == 0L) {
    return(NULL)
  }
  mask <- kept_idx[[1]]
  response_dates <- panel_dates[-1L]
  if (length(mask) != length(response_dates)) {
    cli::cli_abort(c(
      "W2 complete-case mask length does not match the T-1 response axis",
      i = "mask = {length(mask)}, response dates = {length(response_dates)}"
    ))
  }
  response_dates[mask]
}

# W1 residuals must occupy a CONTIGUOUS calendar block so their retained
# response dates form a clean span that the date-based alignment in
# compute_identification_residuals can intersect with the W2 and instrument
# dates. Y1 own-lags legitimately drop a leading PREFIX (the first H-1 rows
# carry lag NAs), leaving a contiguous trailing block -- that is allowed. An
# INTERIOR gap (an NA in consumption or the PCs mid-sample) is the real hazard:
# it would shift residual row t off its calendar period and break the
# structural-coefficient identity beta1(theta) = beta1R - (beta2R)' theta, so it
# is still rejected.
assert_w1_leading_block <- function(w1_result) {
  kept <- w1_result$kept_idx
  if (is.null(kept)) {
    return(invisible(w1_result))
  }
  k <- which(kept)
  if (length(k) > 0L && !identical(k, seq.int(k[1L], k[length(k)]))) {
    stop(
      "W1 (consumption) complete-case filter dropped interior rows; ",
      "the kept rows are not a contiguous block, so W1 residual row t no ",
      "longer maps to a single calendar period and would misalign with the ",
      "W2 residuals and instruments"
    )
  }
  invisible(w1_result)
}
