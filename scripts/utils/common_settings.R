# Common Settings and Configuration
# Shared parameters and paths for all scripts

# Load core packages used across most scripts
library(here)
library(cli) # For better console output
library(hetid) # Load package to access constants
library(dplyr) # Data manipulation
library(tidyr) # Data tidying
library(gt) # Table formatting
library(DT) # Interactive tables
library(nloptr) # Nonlinear optimization

# Set up output directories
SCRIPTS_DIR <- here::here("scripts")
OUTPUT_DIR <- file.path(SCRIPTS_DIR, "output")
OUTPUT_PAPER_DIR <- file.path(OUTPUT_DIR, "for_paper")
OUTPUT_TEMP_DIR <- file.path(OUTPUT_DIR, "temp")

# Merged quarterly dataset: written by stage 01 (create_data.R), read by all
# later stages
DATA_RDS_PATH <- file.path(OUTPUT_TEMP_DIR, "data.rds")

# Create directories if they don't exist
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_PAPER_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_TEMP_DIR, recursive = TRUE, showWarnings = FALSE)
# NB: for_paper holds ONLY the three paper-spec tables (enforced by
# assert_for_paper_allowlist); do NOT seed tables/figures/other subdirs under it.
dir.create(file.path(OUTPUT_TEMP_DIR, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_TEMP_DIR, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_TEMP_DIR, "other"), recursive = TRUE, showWarnings = FALSE)

# Common parameters
# News clock for the identification pipeline: one news period = one
# quarter = 3 maturity months. Stage-01 rows are quarterly, so the
# rows-equal-period contract of the SDF-news construction holds
# exactly. The quarterly clock needs maturities at 3-month spacing
# (each horizon i uses i - 3 and i + 3); the data's 3-month floor
# makes every horizon from the boundary i = 3 upward feasible.
NEWS_STEP <- 3L
PIPELINE_ACM_MATURITIES <- seq(3L, 120L, by = 3L)
SEED <- 123 # For reproducibility
BASELINE_TAU <- 0.05 # Baseline set-identification tolerance (shared across stages)
# Near-uninformative tau cap shared across the tau* oracle and the identified-set
# probes: admissible slack is tau in [0, 1) (the correlation-bound interpretation
# fails at tau >= 1 and hetid rejects it), so searches are capped strictly below 1.
OPT_TAU_CAP <- 0.99
N_Y1_LAGS <- 4L # Own-lags of Y1 (consumption growth) in the W1 reduced form (0 = none)
# FALSE = estimate B from the data (default, "let the data speak"); TRUE = impose exact-news B = 0
IMPOSE_NEWS_PROJECTION_ZERO <- FALSE
TAU_STAR_N_STARTS <- 15L # Multistart count for the tau* optimizer oracle
N_CORES <- parallel::detectCores() - 1 # Leave one core free
MAX_N_PCS <- HETID_CONSTANTS$MAX_N_PCS # Use package constant

# The for_paper directory holds ONLY the three paper-spec tables (stage 08).
# Each ships as <stem>.tex / <stem>_standalone.tex / <stem>_standalone.pdf /
# <stem>.csv; assert_for_paper_allowlist() (for_paper_guard.R) enforces this.
FOR_PAPER_TABLE_STEMS <- c(
  "table1_summary_statistics",
  "table2_structural_equation", # the structural price-of-risk equation
  "table3_estimator_properties"
)

# Column-name prefixes derived from the package's ACM schema (the single
# source of truth for the naming convention; internal, hence the :::)
YIELD_PREFIX <- hetid:::HETID_ACM_SCHEMA$yields$prefix_new
TP_PREFIX <- hetid:::HETID_ACM_SCHEMA$term_premia$prefix_new

# Plotting parameters
PLOT_WIDTH <- 8
PLOT_HEIGHT <- 6
PLOT_DPI <- 300

# Table formatting
TABLE_DIGITS <- 3

# Set options
options(
  scipen = 999, # Avoid scientific notation
  digits = 6, # Default digits
  width = 100 # Console width
)

# Function to get timestamp for file naming
get_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

# Function to ensure reproducibility
set_analysis_seed <- function(seed = SEED) {
  set.seed(seed)
  invisible(seed)
}

# Specialized package loading functions
load_visualization_packages <- function() {
  library(ggplot2)
  library(gridExtra)
  library(corrplot)
  library(svglite)
  library(plotly)
  invisible(TRUE)
}

load_timeseries_packages <- function() {
  library(urca)
  library(skedastic)
  library(lubridate)
  invisible(TRUE)
}

load_web_packages <- function() {
  library(htmltools)
  library(knitr)
  invisible(TRUE)
}

# Source utility functions if they exist
utils_dir <- file.path(SCRIPTS_DIR, "utils")
if (file.exists(file.path(utils_dir, "plotting_utils.R"))) {
  source(file.path(utils_dir, "plotting_utils.R"))
}
if (file.exists(file.path(utils_dir, "stats_utils.R"))) {
  source(file.path(utils_dir, "stats_utils.R"))
}
if (file.exists(file.path(utils_dir, "hetero_test_utils.R"))) {
  source(file.path(utils_dir, "hetero_test_utils.R"))
}
if (file.exists(file.path(utils_dir, "hetero_plot_utils.R"))) {
  source(file.path(utils_dir, "hetero_plot_utils.R"))
}
if (file.exists(file.path(utils_dir, "z_source.R"))) {
  source(file.path(utils_dir, "z_source.R"))
}
if (file.exists(file.path(utils_dir, "gamma_source.R"))) {
  source(file.path(utils_dir, "gamma_source.R"))
}
if (file.exists(file.path(utils_dir, "news_projection.R"))) {
  source(file.path(utils_dir, "news_projection.R"))
}
if (file.exists(file.path(utils_dir, "baseline_spec.R"))) {
  source(file.path(utils_dir, "baseline_spec.R"))
}
if (file.exists(file.path(utils_dir, "identification_utils.R"))) {
  source(file.path(utils_dir, "identification_utils.R"))
}
if (file.exists(file.path(utils_dir, "optimization_utils.R"))) {
  source(file.path(utils_dir, "optimization_utils.R"))
}
if (file.exists(file.path(utils_dir, "lambda_mask.R"))) {
  source(file.path(utils_dir, "lambda_mask.R"))
}
if (file.exists(file.path(utils_dir, "lambda_whitening.R"))) {
  source(file.path(utils_dir, "lambda_whitening.R"))
}
if (file.exists(file.path(utils_dir, "lambda_varnorm.R"))) {
  source(file.path(utils_dir, "lambda_varnorm.R"))
}
if (file.exists(file.path(utils_dir, "lambda_optimization.R"))) {
  source(file.path(utils_dir, "lambda_optimization.R"))
}
if (file.exists(file.path(utils_dir, "profile_bounds_core.R"))) {
  source(file.path(utils_dir, "profile_bounds_core.R"))
}
if (file.exists(file.path(utils_dir, "profile_bounds.R"))) {
  source(file.path(utils_dir, "profile_bounds.R"))
}
if (file.exists(file.path(utils_dir, "format_utils.R"))) {
  source(file.path(utils_dir, "format_utils.R"))
}
if (file.exists(file.path(utils_dir, "latex_table_utils.R"))) {
  source(file.path(utils_dir, "latex_table_utils.R"))
}
if (file.exists(file.path(utils_dir, "tau_star_utils.R"))) {
  source(file.path(utils_dir, "tau_star_utils.R"))
}
if (file.exists(file.path(utils_dir, "closure_membership.R"))) {
  source(file.path(utils_dir, "closure_membership.R"))
}
# Heteroskedasticity LM helpers + the plain-column LaTeX renderer are NOT sourced
# above (stage 02 / the consumption table source them locally today). The paper
# spec needs both, so source them here. Order: hetero_lm_tests + latex_simple
# before the paper-spec utils that consume them.
if (file.exists(file.path(utils_dir, "hetero_lm_tests.R"))) {
  source(file.path(utils_dir, "hetero_lm_tests.R"))
}
if (file.exists(file.path(utils_dir, "latex_simple_table.R"))) {
  source(file.path(utils_dir, "latex_simple_table.R"))
}
if (file.exists(file.path(utils_dir, "paper_spec_residuals.R"))) {
  source(file.path(utils_dir, "paper_spec_residuals.R"))
}
if (file.exists(file.path(utils_dir, "paper_spec_estimator.R"))) {
  source(file.path(utils_dir, "paper_spec_estimator.R"))
}
if (file.exists(file.path(utils_dir, "paper_spec_bootstrap.R"))) {
  source(file.path(utils_dir, "paper_spec_bootstrap.R"))
}
if (file.exists(file.path(utils_dir, "for_paper_guard.R"))) {
  source(file.path(utils_dir, "for_paper_guard.R"))
}
