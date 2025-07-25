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

# Set up output directories
SCRIPTS_DIR <- here::here("scripts")
OUTPUT_DIR <- file.path(SCRIPTS_DIR, "output")
OUTPUT_PAPER_DIR <- file.path(OUTPUT_DIR, "for_paper")
OUTPUT_TEMP_DIR <- file.path(OUTPUT_DIR, "temp")

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
SEED <- 123 # For reproducibility
N_CORES <- parallel::detectCores() - 1 # Leave one core free
MAX_N_PCS <- HETID_CONSTANTS$MAX_N_PCS # Use package constant

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
