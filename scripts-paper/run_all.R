# Run the scripts-paper pipeline: define the shared constants, apply the FRED
# download patch, then source each script in dependency order.
# Run from the package root: Rscript scripts-paper/run_all.R

# Shared constants ---------------------------------------------------------
# FRED pull window (calendar dates for tq_get; 1947 start so early lags exist)
fred_from <- "1947-01-01"
fred_to <- "2026-06-19"
# analysis sample window for the PCAs (adjust here)
date_begin <- "1962 Q1"
date_end <- "2025 Q4"

# SDF maturity grids, in months; a maturity i needs i - step and i + step
# available, so each grid stops one step short of the 120-month maximum
all_mats <- hetid::HETID_CONSTANTS$ALL_ACM_MATURITIES # 1..120 months
step_qtr <- hetid::HETID_CONSTANTS$MONTHS_PER_QUARTER
mats_qtr <- step_qtr:hetid::effective_max_maturity(step_qtr)
# maturities to display: 1 quarter, 1 year, 5 years, and the longest the
# quarterly step supports (117 months, standing in for 10 years)
show_mats <- c(
  step_qtr,
  c(1, 5) * hetid::HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR,
  max(mats_qtr)
)
# lag length in quarters for the lagged expected-SDF panel
lag_qtrs <- 1L
# principal components kept from each SDF panel
n_pc <- 3L
# principal components of financial asset returns kept as the log-variance
# equation's conditioning variables PC_R (bundled `variables` pc columns)
n_pc_r <- 4L
# news-block orthogonality null: TRUE imposes beta2R = 0 exactly (the news
# PCs are population-orthogonal to X_t, so they enter unresidualized and the
# design coefficients are point identified at beta1R); FALSE estimates
# beta2R by OLS on X_t from the sample as before
impose_beta2r_null <- TRUE
# column-name prefixes shared by the data scripts and their consumers
news_prefix <- "sdf_news_m"
expected_prefix <- "expected_sdf_m"
# heteroskedasticity instrument Z for set identification and the hetero tests:
# column z_col of the qtr-keyed frame z_source() returns (a function, so the
# data scripts sourced below can build the frame first); z_desc is the
# hetero-tests table note's description of Z
z_col <- "y60_vol"
z_source <- function() yield_vol[c("qtr", z_col)]
z_desc <- paste(
  "the de-meaned realized quarterly volatility of the five-year yield",
  "(\\texttt{y60\\_vol}, \\texttt{yield\\_vol.R})"
)
# moving-block bootstrap for the identified-set endpoints and tau*
# (24-quarter blocks preserve the persistent volatility episodes that give
# Z its leverage; 8- and 15-quarter blocks give nearly identical results
# except the shortest, which widens the b3N endpoint scales -- documented in
# the inference-table notes); 200 replications, deterministic seed;
# HETID_BOOT_REPS overrides the replication count for quick smoke runs
boot_reps <- as.integer(Sys.getenv("HETID_BOOT_REPS", unset = "200"))
boot_block <- 24L
boot_seed <- 123L
# PPML set-map budgets: the primary grid/fit caps and the independent
# Morton-coverage audit's larger caps (counts of inner fits, not wall clock)
logvar_ppml_grid_cap <- 4000L
logvar_ppml_fit_budget <- 20000L
logvar_ppml_coverage_grid_cap <- 8000L
logvar_ppml_coverage_fit_budget <- 40000L

# output folder for tables and figures
out_dir <- "scripts-paper/output"
dir.create(out_dir, showWarnings = FALSE)

# Shared helpers ------------------------------------------------------------
# restrict a qtr-keyed frame to the date_begin ~ date_end sample window
filter_window <- function(df) {
  dplyr::filter(
    df,
    qtr >= tsibble::yearquarter(date_begin),
    qtr <= tsibble::yearquarter(date_end)
  )
}
# names of a frame's series columns (everything but the qtr key)
value_cols <- function(df) setdiff(names(df), "qtr")

# patch quantmod's FRED download (HTTP/2 stream errors, stalls) before tq_get
source("scripts-paper/fix_fred_download.R")

# Pipeline -----------------------------------------------------------------
source("scripts-paper/sdf_series.R")
source("scripts-paper/consumption_growth.R")
source("scripts-paper/yield_vol.R")
source("scripts-paper/asset_return_pcs.R")
source("scripts-paper/sdf_pcs.R")
source("scripts-paper/ols_mean_eq_regression.R")
source("scripts-paper/set_id_mean_eq.R")
source("scripts-paper/set_id_bootstrap.R")
source("scripts-paper/structural_eq_table.R")
source("scripts-paper/var_share.R")
source("scripts-paper/var_share_table.R")
source("scripts-paper/log_var_eq.R")
source("scripts-paper/log_var_eq_table.R")
source("scripts-paper/set_id_bounds_tau.R")
# the PPML set map needs the warm-refined display-tau boxes from
# set_id_bounds_tau.R and must register its figure entry before the
# bounds-by-tau driver renders the registry
source("scripts-paper/log_var_eq_ppml_sets.R")
# the log-variance figures consume mean_eq_bounds_tau and the registry, so
# this runs after both producers
source("scripts-paper/log_var_eq_bounds_tau.R")
source("scripts-paper/set_id_region_common.R")
source("scripts-paper/set_id_projections.R")
source("scripts-paper/set_id_region_3d.R")
source("scripts-paper/hetero_tests.R")
source("scripts-paper/descriptive_stats.R")
