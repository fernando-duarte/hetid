# Shared scientific and computational settings for the paper pipeline.

paper_source_once(paper_path("config", "analysis_contract.R"))
paper_source_once(paper_path("config", "logvar_estimators.R"))
paper_source_once(paper_path("config", "reporting.R"))
paper_source_once(paper_path(
  "support", "statistics", "bootstrap_and_stationarity.R"
))
analysis_contract <- PAPER_ANALYSIS_CONTRACT
reporting_contract <- PAPER_REPORTING_CONTROL

# FRED pull window; the early start supplies the lags used downstream.
fred_from <- "1947-01-01"
fred_to <- "2026-06-19"

# Consumption input source. "frozen" reads the committed snapshot under data/
# and never touches the network, so the vintage cannot drift under the results.
# "live" downloads the current vintage and rewrites that snapshot, which is what
# makes a vintage change land as a reviewable diff.
fred_source <- "frozen"

# ACM daily input pin. The package resolves its download through the release
# "latest" tag, so a new monthly release moves the data with no edit here:
# acm-term-premium-2026-08 published on 2026-08-01 and did exactly that.
# "frozen" requires this tag and digest and never consults "latest"; "live"
# restores the package's own auto-download. To adopt a new vintage, run once
# with "live", then copy the sha256 the download records in the cache's .meta
# sidecar into acm_daily_sha256 and bump acm_daily_release.
acm_daily_source <- "frozen"
acm_daily_release <- "acm-term-premium-2026-07"
acm_daily_sha256 <-
  "9a155e65953d4cf9f0db6131f7580459e4ccfe1d2e8e4e83ab8beb8b9dc292aa"

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
news_prefix <- "sdf_news_m"
expected_prefix <- "expected_sdf_m"

# Heteroskedasticity instrument used for identification and diagnostics.
z_col <- analysis_contract$input$instrument$column
z_source <- function() yield_vol[c("qtr", z_col)]
z_desc <- paper_instrument_description()

# Moving-block bootstrap controls shared by every bootstrap in the paper.
# Env overrides are parsed as whole numbers; a typo fails loudly instead of
# silently coercing to NA. An explicitly-empty value behaves like unset.
resolve_whole_number_env <- function(var, default) {
  raw <- Sys.getenv(var, unset = "")
  if (nzchar(raw) && !grepl("^[0-9]+$", trimws(raw))) {
    stop(sprintf("%s must be a whole number, got: %s", var, raw), call. = FALSE)
  }
  if (nzchar(raw)) as.integer(trimws(raw)) else default
}

boot_reps <- resolve_whole_number_env("HETID_BOOT_REPS", 10000L)
stopifnot(boot_reps >= 2L)
if (boot_reps != 10000L) {
  message(sprintf("boot_reps overridden to %d via HETID_BOOT_REPS", boot_reps))
}

# One seed across every bootstrap in the paper, matching macro_dynamics.
boot_seed <- 20260708L

paper_default_boot_cores <- function(
  detected_cores = parallel::detectCores(logical = TRUE),
  sysname = Sys.info()[["sysname"]]
) {
  if (is.na(detected_cores)) {
    return(1L)
  }
  reserved_cores <- if (identical(sysname, "Darwin")) 2L else 1L
  max(1L, as.integer(detected_cores) - reserved_cores)
}

detected_cores <- parallel::detectCores(logical = TRUE)
default_cores <- paper_default_boot_cores(detected_cores)
boot_cores <- resolve_whole_number_env("HETID_BOOT_CORES", default_cores)
stopifnot(boot_cores >= 1L)

# Bootstrap execution mode: "reuse" (default) loads cached per-draw estimates
# when nothing that determines them changed; "rerun" always resamples.
resolve_boot_mode_env <- function(var, default) {
  raw <- Sys.getenv(var, unset = "")
  if (!nzchar(raw)) {
    return(default)
  }
  mode <- tolower(trimws(raw))
  if (!mode %in% c("rerun", "reuse")) {
    stop(sprintf("%s must be 'rerun' or 'reuse', got: %s", var, raw), call. = FALSE)
  }
  mode
}
PAPER_BOOT_MODE <- resolve_boot_mode_env("HETID_BOOT_MODE", "reuse")
if (!identical(PAPER_BOOT_MODE, "reuse")) {
  message(sprintf("bootstrap execution mode: %s (default reuse overridden)", PAPER_BOOT_MODE))
}

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

# Which beta2R specifications the pipeline computes.
#   "B" -- beta2R estimated from the sample; W2 is the news residualized on X,
#          and b_0/b_E become set-valued through beta1(w) = beta1R - beta2R'w.
#   "A" -- beta2R forced to zero; W2 is the raw news and b_0/b_E are points.
#
# The FIRST entry of each panel is PUBLISHED. Any others are computed for
# comparison and diagnostics and reach no table, figure or paper number.
# Set mean to a single entry to skip the comparison run entirely.
#
# The volatility panel is built from the mean panel's news set and its tau = 0
# anchor, so it can only be computed under a specification the mean panel
# publishes. Running the volatility panel under both would mean two full
# estimator passes for a diagnostic, which is the expensive half of the
# pipeline; the assertion below keeps that a deliberate choice rather than an
# accident.
PAPER_SPEC_PLAN <- list(
  mean = c("B", "A"),
  volatility = "B"
)

paper_spec_impose_null <- function(spec) {
  stopifnot(is.character(spec), length(spec) == 1L, spec %in% c("A", "B"))
  identical(spec, "A")
}

paper_published_spec <- function(panel) PAPER_SPEC_PLAN[[panel]][[1L]]

stopifnot(
  "spec plan panels are mean and volatility" =
    identical(names(PAPER_SPEC_PLAN), c("mean", "volatility")),
  "every spec is A or B" =
    all(unlist(PAPER_SPEC_PLAN) %in% c("A", "B")),
  "no panel repeats a spec" =
    !any(vapply(PAPER_SPEC_PLAN, anyDuplicated, integer(1)) > 0L),
  "every panel runs at least one spec" =
    all(lengths(PAPER_SPEC_PLAN) >= 1L),
  "the volatility panel runs only the mean panel's published spec" =
    identical(PAPER_SPEC_PLAN$volatility, paper_published_spec("mean"))
)
