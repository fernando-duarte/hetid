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
source("scripts-paper/sdf_pcs.R")
source("scripts-paper/ols_mean_eq_regression.R")
source("scripts-paper/set_id_mean_eq.R")
source("scripts-paper/structural_eq_table.R")
source("scripts-paper/set_id_bounds_tau.R")
source("scripts-paper/hetero_tests.R")
source("scripts-paper/descriptive_stats.R")
