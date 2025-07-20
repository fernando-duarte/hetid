# =============================================================================
# Test: Download Functions
# =============================================================================
# This script tests the data download functions:
# - download_term_premia(): Downloads ACM data from NY Fed
# - download_yield_curve(): Downloads yield curve data from Fed

# Load package
library(hetid)

cat("Testing download functions\n")
cat("==========================\n\n")

# Note: These functions download data from the internet
# They will only run if you have an internet connection

# Test 1: Check current data availability
cat("Current data availability\n")
# The package startup message shows what's available
# Check files in extdata
extdata_path <- system.file("extdata", package = "hetid")
files <- list.files(extdata_path)
cat(sprintf("  Files in extdata: %s\n", paste(files, collapse = ", ")))

# Test 2: download_term_premia()
cat("\ndownload_term_premia() function\n")
cat("  This downloads ACM data from NY Fed\n")

# Check if file exists
acm_file <- system.file("extdata", "acm_term_premia.csv", package = "hetid")
file_exists <- file.exists(acm_file)
cat(sprintf("  ACM file exists: %s\n", file_exists))

if (file_exists) {
  # Get file info
  file_info <- file.info(acm_file)
  cat(sprintf("  File size: %.2f MB\n", file_info$size / 1024^2))
  cat(sprintf("  Last modified: %s\n", file_info$mtime))

  # Test force download (set to FALSE to avoid actual download)
  cat("\n  Testing download with force = FALSE\n")
  result <- tryCatch(
    {
      download_term_premia(force = FALSE, quiet = FALSE)
    },
    error = function(e) {
      cat(sprintf("    Error: %s\n", e$message))
      NULL
    }
  )
} else {
  cat("  File not found. Would need to download.\n")
  cat("  Run: download_term_premia()\n")
}

# Test 3: download_yield_curve()
cat("\ndownload_yield_curve() function\n")
cat("  This downloads Fed yield curve data\n")

# Two datasets available
datasets <- c("feds200533", "feds200628")
dataset_names <- c(
  "feds200533" = "Smoothed yield curve",
  "feds200628" = "Svensson model parameters"
)

for (dataset in datasets) {
  cat(sprintf("\n  Dataset: %s (%s)\n", dataset, dataset_names[dataset]))

  file_path <- system.file("extdata", paste0(dataset, ".csv"), package = "hetid")
  if (file.exists(file_path)) {
    file_info <- file.info(file_path)
    cat(sprintf("    File exists: Yes\n"))
    cat(sprintf("    Size: %.2f MB\n", file_info$size / 1024^2))
  } else {
    cat(sprintf("    File exists: No\n"))
    cat(sprintf("    Run: download_yield_curve('%s')\n", dataset))
  }
}

# Test 4: Loading functions
cat("\nLoading functions\n")

# Try loading ACM data
if (file_exists) {
  cat("  Testing load_term_premia():\n")
  acm_data <- load_term_premia()
  cat(sprintf(
    "    Loaded: %d rows × %d columns\n",
    nrow(acm_data), ncol(acm_data)
  ))
  cat(sprintf(
    "    Date range: %s to %s\n",
    min(acm_data$date), max(acm_data$date)
  ))
}

# Try loading yield curve data
fed_file <- system.file("extdata", "feds200628.csv", package = "hetid")
if (file.exists(fed_file)) {
  cat("\n  Testing load_yield_curve():\n")
  yield_data <- load_yield_curve("feds200628")
  cat(sprintf(
    "    Loaded: %d rows × %d columns\n",
    nrow(yield_data), ncol(yield_data)
  ))

  # Show some column names
  cat(sprintf(
    "    Sample columns: %s\n",
    paste(head(names(yield_data), 5), collapse = ", ")
  ))
}

# Test 5: Download URLs
cat("\nSource URLs\n")
cat("  ACM data: https://www.newyorkfed.org/research/data_indicators/term-premia-tabs\n")
cat("  Fed yield curves: https://www.federalreserve.gov/data/nominal-yield-curve.htm\n")

# Note about data usage
cat("\nNote: These functions download data from official sources:\n")
cat("- ACM term premia from NY Fed (Adrian, Crump, Moench)\n")
cat("- Yield curves from Federal Reserve Board\n")
cat("Data is stored in inst/extdata/ for package distribution.\n")

cat("\nTest complete!\n")
