# Shared scientific and computational settings for the paper pipeline.

paper_source_once(paper_path("config", "analysis_contract.R"))
paper_source_once(paper_path("config", "logvar_estimators.R"))
paper_source_once(paper_path("config", "reporting.R"))
analysis_contract <- PAPER_ANALYSIS_CONTRACT
reporting_contract <- PAPER_REPORTING_CONTROL

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
n_pc <- analysis_contract$model$n_mean_pc
n_pc_r <- analysis_contract$model$n_return_pc
impose_beta2r_null <- TRUE
news_prefix <- "sdf_news_m"
expected_prefix <- "expected_sdf_m"

# Heteroskedasticity instrument used for identification and diagnostics.
z_col <- analysis_contract$input$instrument$column
z_source <- function() yield_vol[c("qtr", z_col)]
z_desc <- paper_instrument_description()

# Moving-block bootstrap controls for mean-equation set endpoints.
boot_reps <- as.integer(Sys.getenv("HETID_BOOT_REPS", unset = "200"))
boot_block <- 24L
boot_seed <- 123L

# PPML set-map and independent coverage-audit budgets.
logvar_ppml_grid_cap <-
  paper_logvar_budget("ppml", "grid_cap")
logvar_ppml_fit_budget <-
  paper_logvar_budget("ppml", "fit_budget")
logvar_ppml_coverage_grid_cap <-
  PAPER_LOGVAR_BUDGETS$ppml_coverage$grid_cap
logvar_ppml_coverage_fit_budget <-
  PAPER_LOGVAR_BUDGETS$ppml_coverage$fit_budget

# Reduced budgets and stability controls for each log-variance bootstrap draw.
logvar_boot_grid_cap <-
  PAPER_LOGVAR_BUDGETS$bootstrap$grid_cap
logvar_boot_fit_budget <-
  PAPER_LOGVAR_BUDGETS$bootstrap$fit_budget
logvar_boot_block_sens <- 8L
logvar_boot_m <- NULL
logvar_boot_cores <- as.integer(Sys.getenv(
  "HETID_BOOT_CORES",
  unset = as.character(max(1L, parallel::detectCores() - 1L))
))

# Fit budget for date-indexed fitted-volatility envelopes.
logvar_fitted_vol_fit_budget <-
  PAPER_LOGVAR_BUDGETS$fitted_volatility$fit_budget

# Analytic standard errors printed beneath PPML and Harvey point estimates.
logvar_ppml_se_type <- reporting_contract$ppml$se_type
logvar_ppml_se_hac_lags <- reporting_contract$ppml$hac_lags
logvar_harvey_se_type <- reporting_contract$harvey$se_type
logvar_harvey_se_hac_lags <- reporting_contract$harvey$hac_lags

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
