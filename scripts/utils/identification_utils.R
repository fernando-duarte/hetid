# Identification Utilities -- non-optimization plumbing

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
    lookup = get_identification_maturity_lookup(maturities),
    mode = "maturities"
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

# W1 residuals must occupy a CONTIGUOUS calendar block so a single leading-row
# offset realigns them with the W2 residuals and instruments in
# compute_identification_residuals (which trims the first `offset` rows of
# w2_mat / z_mat). Y1 own-lags legitimately drop a leading PREFIX (the first
# H-1 rows carry lag NAs), leaving a contiguous trailing block -- that is
# allowed. An INTERIOR gap (an NA in consumption or the PCs mid-sample) is the
# real hazard: it would shift residual row t off its calendar period and break
# the structural-coefficient identity beta1(theta) = beta1R - (beta2R)' theta,
# so it is still rejected.
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

  cli::cli_alert_info("Computing W2 residuals...")
  w2_result <- compute_w2_residuals(
    yields = yields_df, term_premia = tp_df,
    maturities = maturities,
    n_pcs = n_pcs, pcs = pcs_mat,
    step = step
  )
  assert_w2_alignment(w2_result)
  w2_mat <- do.call(cbind, w2_result$residuals)

  # Y1 own-lags drop the first H-1 rows of W1 only; W2 and Z are not lagged, so
  # realign them to W1's common trailing block by trimming the leading `offset`
  # rows. A date check fails closed on misalignment.
  n_resid <- length(w1_result$residuals)
  z_mat <- get_identification_z(data, pcs_mat)
  n_w2 <- nrow(w2_mat)
  offset <- n_w2 - n_resid
  if (offset < 0L) {
    cli::cli_abort("W1 has more rows than W2; alignment impossible")
  }
  if (offset > 0L) {
    w2_mat <- w2_mat[-seq_len(offset), , drop = FALSE]
    z_mat <- z_mat[-seq_len(offset), , drop = FALSE]
  }
  pcs_aligned <- z_mat[seq_len(n_resid), , drop = FALSE]
  if (!is.null(w1_result$dates) && "date" %in% names(data)) {
    w2_dates <- utils::tail(utils::head(data$date[-1L], n_w2), n_resid)
    if (!isTRUE(all.equal(w2_dates, w1_result$dates))) {
      cli::cli_abort("W1/W2 date misalignment after Y1-lag trimming")
    }
  }

  result <- list(
    w1 = w1_result$residuals,
    w2 = w2_mat,
    pcs_aligned = pcs_aligned,
    w1_result = w1_result,
    n_obs = n_resid
  )
  # Retain beta2R (the Y2-on-PC reduced-form coefficient matrix, I x (1+n_pcs),
  # = the point-identified beta20) for structural-coefficient recovery; it is
  # dropped by the cbind flatten above otherwise.
  result$w2_coefficients <- w2_result$coefficients
  result
}

# compute_identification_moments() is exported by the hetid package (loaded via
# common_settings.R); the moments object is a validated hetid_moments container
# carrying the maturity identity.
#
# Single pipeline entry point for assembling the identified-set quadratic system.
# Delegates to the exported generalized-instrument front end
# hetid::build_general_quadratic_system(), which with a J x I matrix gamma (one
# combination per component, K_i = 1) is bit-identical on $quadratic to the
# legacy hetid::build_quadratic_system() and exposes the same L_i/V_i/Q_i values
# in $components. This is the K_i = 1 "General scheme (baseline)" case of
# docs/lewbel_multivariate_set_identification.tex (subsec:general_scheme), so the
# whole pipeline runs on one generalized assembly path.
#
# The general builder returns $components as a plain list (plus a $labels frame).
# We re-attach the hetid_components class and its maturities/n_components integer
# attributes so the persisted RDS objects and interactive print stay identical to
# the legacy shape and remain valid inputs to the hetid quadratic-stage APIs
# (which gate on inherits(., "hetid_components")). The added $labels element is an
# accepted, documented delta.
#
# Set HETID_ASSERT_EQUIV to a non-empty value to additionally assert, on every
# call, numeric-leaf identity with the legacy builder on the real pipeline inputs
# (off by default: zero overhead in normal runs). One live pipeline run with the
# flag set self-certifies every migrated call site against the legacy path.
build_pipeline_quadratic_system <- function(gamma, tau, moments) {
  out <- hetid::build_general_quadratic_system(gamma, tau, moments)
  out$components <- structure(
    out$components,
    maturities = as.integer(attr(out, "maturities")),
    n_components = as.integer(attr(out, "n_components")),
    class = "hetid_components"
  )
  if (nzchar(Sys.getenv("HETID_ASSERT_EQUIV", ""))) {
    legacy <- hetid::build_quadratic_system(gamma, tau, moments)
    assert_pipeline_quadratic_equiv(out, legacy)
  }
  out
}

# Numeric-leaf equivalence guard: the migrated generalized assembly must equal
# the legacy build_quadratic_system() output exactly on the solver-consumable
# quadratic form and on the L_i/V_i/Q_i component values. Names and the
# re-attached class are included by comparing the whole $quadratic list and each
# component leaf with identical().
assert_pipeline_quadratic_equiv <- function(general, legacy) {
  stopifnot(
    identical(general$quadratic, legacy$quadratic),
    identical(general$components$L_i, legacy$components$L_i),
    identical(general$components$V_i, legacy$components$V_i),
    identical(general$components$Q_i, legacy$components$Q_i)
  )
  invisible(TRUE)
}

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
      " -- recycling it would produce wrong loadings. For a custom-width ",
      "instrument set supply HETID_BASELINE_GAMMA=<path-to-R-file ",
      "defining build_gamma(moments)>"
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
