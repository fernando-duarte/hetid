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
# vol set-endpoint bootstrap: reduced caps per draw (primary pass only, no cold-
# start, no coverage/sensitivity second pass) keep B = boot_reps tractable
logvar_boot_grid_cap <- 1500L
logvar_boot_fit_budget <- 8000L
# a certified side must be bounded in >= this fraction of non-failed draws
logvar_boot_stability <- 0.85
# ell=8 block sensitivity pass (reports mandate it); NULL disables. Runs at reduced
# reps for cost; envelopes go to diagnostics, not the headline table
logvar_boot_block_sens <- 8L
# m-out-of-n subsampling size (NULL = full n). Deferred fallback for when the
# regularity gate fails widely; leaving the hook live per the reports
logvar_boot_m <- NULL
# draw-level parallel cores; serial lapply when this resolves to 1
logvar_boot_cores <- as.integer(
  Sys.getenv("HETID_BOOT_CORES", unset = as.character(max(1L, parallel::detectCores() - 1L)))
)
# search controls for each date-indexed fitted-volatility envelope. Each dated
# objective gets its grid extreme, shared seed, local polish, and cold
# replication; auxiliary polish paths are omitted because one suspect path
# correctly demotes an endpoint under the engine's fail-closed contract.
logvar_fitted_vol_fit_budget <- 80000L
logvar_fitted_vol_starts_per_side <- 1L
# PPML QMLE standard errors under the point columns (OLS, tau = 0) of the
# log-variance panels. Every non-bootstrap variant is computed and stored; this
# picks which prints, one of "naive" (Pearson-dispersion model information;
# valid only if Var(eps^2 | PC_R) is proportional to the mean), "hc0"/"hc1"
# (Eicker-White heteroskedasticity-robust), or "hac" (Newey-West Bartlett HAC).
# The default is "hac" to match the log-OLS panel's Newey-West lag-4 inference
# and to stay valid under the heteroskedastic, serially correlated scores of a
# squared-residual response. The moving-block bootstrap is deferred; the HAC lag
# count matches the log-OLS benchmark's 4 lags.
logvar_ppml_se_type <- "hac"
logvar_ppml_se_hac_lags <- 4L
# Harvey Gaussian multiplicative-variance QMLE standard errors, the same
# selection knob for the Harvey module (log_var_eq_harvey_se.R): one of
# "expected"/"observed" (Gaussian working-model Fisher / observed information),
# "opg" (outer product of gradients), "robust" (Eicker-White QMLE sandwich), or
# "hac" (Newey-West Bartlett). The default matches the PPML and log-OLS panels'
# Newey-West lag-4 inference.
logvar_harvey_se_type <- "hac"
logvar_harvey_se_hac_lags <- 4L

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
source("scripts-paper/set_id_bounds_tau.R")
# the PPML set map needs the warm-refined display-tau boxes from
# set_id_bounds_tau.R and must register its figure entry before the
# bounds-by-tau driver renders the registry
source("scripts-paper/log_var_eq_ppml_sets.R")
# shared SE scaffolding both estimators route through; before either *_se module
source("scripts-paper/log_var_eq_se_utils.R")
# analytic PPML QMLE standard errors for the point columns; must run after the
# frozen PPML object exists and before either table renders it
source("scripts-paper/log_var_eq_ppml_se.R")
# the primary table consumes the completed PPML hulls; the combined table then
# adds the mean-log robustness panel without recomputing either estimator
source("scripts-paper/log_var_eq_table.R")
# Harvey sets and dedicated table (the wrapper keeps this to one source line)
source("scripts-paper/log_var_eq_harvey_run.R")
# joint-null theta_R = 0 distance diagnostic: math, search, stability, then the
# guarded driver (inputs is sourced by log_var_eq.R; the split modules are
# self-sourced by their named parents), before the panels table appends its note
source("scripts-paper/log_var_eq_joint_null_math.R")
source("scripts-paper/log_var_eq_joint_null_search.R")
source("scripts-paper/log_var_eq_joint_null_stability.R")
source("scripts-paper/log_var_eq_joint_null.R")
source("scripts-paper/log_var_eq_ppml_table.R")
# vol set-endpoint bootstrap: reads the frozen PPML/Harvey caches and the lagged
# asset-return PCs, re-runs the whole set map per resample, and writes the outer
# confidence envelopes to output/ (mutates no upstream cache)
# L'Ecuyer-CMRG only for the forked vol bootstrap (mclapply mc.set.seed); the
# serial structural bootstrap above and the region figures below stay under the
# default RNG so their published numbers do not move
logvar_boot_rng <- RNGkind("L'Ecuyer-CMRG")
source("scripts-paper/log_var_eq_set_bootstrap.R")
RNGkind(logvar_boot_rng[1L], logvar_boot_rng[2L])
# the inference variant of the combined panels: same PPML/log-OLS/Harvey
# panels and labels as log_var_eq_ppml_table.R, with the bootstrap envelope
# threaded beneath the PPML and Harvey set cells
source("scripts-paper/log_var_eq_set_inference_table.R")
# the log-variance figures consume mean_eq_bounds_tau and the registry, so
# this runs after both producers
source("scripts-paper/log_var_eq_bounds_tau.R")
# consume the completed estimator caches only after their existing figures
source("scripts-paper/log_var_eq_fitted_vol.R")
source("scripts-paper/set_id_region_common.R")
source("scripts-paper/set_id_projections.R")
source("scripts-paper/set_id_region_3d.R")
source("scripts-paper/hetero_tests.R")
source("scripts-paper/descriptive_stats.R")
