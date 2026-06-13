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
dir.create(file.path(OUTPUT_PAPER_DIR, "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PAPER_DIR, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUTPUT_PAPER_DIR, "other"), recursive = TRUE, showWarnings = FALSE)
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
TAU_STAR_N_STARTS <- 15L # Multistart count for the tau* optimizer oracle
N_CORES <- parallel::detectCores() - 1 # Leave one core free
MAX_N_PCS <- HETID_CONSTANTS$MAX_N_PCS # Use package constant

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
if (file.exists(file.path(utils_dir, "factor_utils.R"))) {
  source(file.path(utils_dir, "factor_utils.R"))
}
if (file.exists(file.path(utils_dir, "z_source.R"))) {
  source(file.path(utils_dir, "z_source.R"))
}
if (file.exists(file.path(utils_dir, "gamma_source.R"))) {
  source(file.path(utils_dir, "gamma_source.R"))
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
