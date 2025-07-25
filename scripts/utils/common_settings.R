# Common Settings and Configuration
# Shared parameters and paths for all scripts

# Load required packages
library(here)
library(cli) # For better console output

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
MAX_N_PCS <- 6 # Maximum number of principal components to keep

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
