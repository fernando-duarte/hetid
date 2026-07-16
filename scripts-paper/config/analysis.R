# Shared scientific and computational settings for the paper pipeline.

# FRED pull window; the early start supplies the lags used downstream.
fred_from <- "1947-01-01"
fred_to <- "2026-06-19"

# Analysis sample window for the principal-component constructions.
date_begin <- "1962 Q1"
date_end <- "2025 Q4"

# SDF maturity grids in months.
all_mats <- hetid::HETID_CONSTANTS$ALL_ACM_MATURITIES
step_qtr <- hetid::HETID_CONSTANTS$MONTHS_PER_QUARTER
mats_qtr <- step_qtr:hetid::effective_max_maturity(step_qtr)
show_mats <- c(
  step_qtr,
  c(1, 5) * hetid::HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR,
  max(mats_qtr)
)

lag_qtrs <- 1L
n_pc <- 3L
n_pc_r <- 4L
impose_beta2r_null <- TRUE
news_prefix <- "sdf_news_m"
expected_prefix <- "expected_sdf_m"

# Heteroskedasticity instrument used for identification and diagnostics.
z_col <- "y60_vol"
z_source <- function() yield_vol[c("qtr", z_col)]
z_desc <- paste(
  "the de-meaned realized quarterly volatility of the five-year yield",
  "(\\texttt{y60\\_vol}, \\texttt{yield\\_vol.R})"
)

# Moving-block bootstrap controls for mean-equation set endpoints.
boot_reps <- as.integer(Sys.getenv("HETID_BOOT_REPS", unset = "200"))
boot_block <- 24L
boot_seed <- 123L

# PPML set-map and independent coverage-audit budgets.
logvar_ppml_grid_cap <- 4000L
logvar_ppml_fit_budget <- 20000L
logvar_ppml_coverage_grid_cap <- 8000L
logvar_ppml_coverage_fit_budget <- 40000L

# Reduced budgets and stability controls for each log-variance bootstrap draw.
logvar_boot_grid_cap <- 1500L
logvar_boot_fit_budget <- 8000L
logvar_boot_stability <- 0.85
logvar_boot_block_sens <- 8L
logvar_boot_m <- NULL
logvar_boot_cores <- as.integer(Sys.getenv(
  "HETID_BOOT_CORES",
  unset = as.character(max(1L, parallel::detectCores() - 1L))
))

# Search controls for date-indexed fitted-volatility envelopes.
logvar_fitted_vol_fit_budget <- 80000L
logvar_fitted_vol_starts_per_side <- 1L

# Analytic standard errors printed beneath PPML and Harvey point estimates.
logvar_ppml_se_type <- "hac"
logvar_ppml_se_hac_lags <- 4L
logvar_harvey_se_type <- "hac"
logvar_harvey_se_hac_lags <- 4L

# Restrict a qtr-keyed frame to the common analysis window.
filter_window <- function(df) {
  dplyr::filter(
    df,
    qtr >= tsibble::yearquarter(date_begin),
    qtr <= tsibble::yearquarter(date_end)
  )
}

# Return all series columns in a qtr-keyed frame.
value_cols <- function(df) setdiff(names(df), "qtr")
