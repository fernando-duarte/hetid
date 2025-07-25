# Summary Statistics
# Generate descriptive statistics for all variables

# Load required packages
library(hetid)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Set up paths
library(here)
source(here::here("scripts/utils/common_settings.R"))

# Load consolidated data
input_path <- file.path(OUTPUT_DIR, "temp/data.rds")
data <- readRDS(input_path)

# Convert list to data frame for analysis
df <- as.data.frame(data)

# Define variable groups
yield_vars <- grep("^y\\d+$", names(df), value = TRUE)
tp_vars <- grep("^tp\\d+$", names(df), value = TRUE)
pc_vars <- grep("^pc\\d+$", names(df), value = TRUE)
pc_lag_vars <- grep("^pc\\d+_lag1$", names(df), value = TRUE)
macro_vars <- HETID_CONSTANTS$CONSUMPTION_GROWTH_COL

# Function to compute summary statistics
compute_summary_stats <- function(x, var_name) {
  # Compute autocorrelations
  acf_values <- acf(x, lag.max = 2, plot = FALSE, na.action = na.pass)$acf

  data.frame(
    Variable = var_name,
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Skewness = moments::skewness(x, na.rm = TRUE),
    Kurtosis = moments::kurtosis(x, na.rm = TRUE),
    AC1 = acf_values[2], # First autocorrelation (lag 1)
    AC2 = acf_values[3], # Second autocorrelation (lag 2)
    N = sum(!is.na(x)),
    stringsAsFactors = FALSE
  )
}

# Compute statistics for each variable group
yields_stats <- do.call(rbind, lapply(yield_vars, function(v) {
  compute_summary_stats(df[[v]], v)
}))

tp_stats <- do.call(rbind, lapply(tp_vars, function(v) {
  compute_summary_stats(df[[v]], v)
}))

pc_stats <- do.call(rbind, lapply(pc_vars, function(v) {
  compute_summary_stats(df[[v]], v)
}))

pc_lag_stats <- do.call(rbind, lapply(pc_lag_vars, function(v) {
  compute_summary_stats(df[[v]], v)
}))

macro_stats <- compute_summary_stats(df[[macro_vars]], macro_vars)

# Combine all statistics
all_stats <- rbind(
  data.frame(
    Variable = "--- Yields ---", Mean = NA, SD = NA, Min = NA,
    Q1 = NA, Median = NA, Q3 = NA, Max = NA, Skewness = NA,
    Kurtosis = NA, AC1 = NA, AC2 = NA, N = NA, stringsAsFactors = FALSE
  ),
  yields_stats,
  data.frame(
    Variable = "--- Term Premia ---", Mean = NA, SD = NA, Min = NA,
    Q1 = NA, Median = NA, Q3 = NA, Max = NA, Skewness = NA,
    Kurtosis = NA, AC1 = NA, AC2 = NA, N = NA, stringsAsFactors = FALSE
  ),
  tp_stats,
  data.frame(
    Variable = "--- Principal Components ---", Mean = NA, SD = NA, Min = NA,
    Q1 = NA, Median = NA, Q3 = NA, Max = NA, Skewness = NA,
    Kurtosis = NA, AC1 = NA, AC2 = NA, N = NA, stringsAsFactors = FALSE
  ),
  pc_stats,
  data.frame(
    Variable = "--- Lagged Principal Components ---", Mean = NA, SD = NA, Min = NA,
    Q1 = NA, Median = NA, Q3 = NA, Max = NA, Skewness = NA,
    Kurtosis = NA, AC1 = NA, AC2 = NA, N = NA, stringsAsFactors = FALSE
  ),
  pc_lag_stats,
  data.frame(
    Variable = "--- Macro Variables ---", Mean = NA, SD = NA, Min = NA,
    Q1 = NA, Median = NA, Q3 = NA, Max = NA, Skewness = NA,
    Kurtosis = NA, AC1 = NA, AC2 = NA, N = NA, stringsAsFactors = FALSE
  ),
  macro_stats
)

# Format numeric columns
numeric_cols <- c("Mean", "SD", "Min", "Q1", "Median", "Q3", "Max", "Skewness", "Kurtosis", "AC1", "AC2")
all_stats[numeric_cols] <- lapply(all_stats[numeric_cols], function(x) round(x, 3))

# Display summary statistics
print(kable(all_stats, format = "simple", align = "l"))

# Compute correlation matrices for each group
cor_yields <- cor(df[yield_vars], use = "complete.obs")
cor_tp <- cor(df[tp_vars], use = "complete.obs")
cor_pc <- cor(df[pc_vars], use = "complete.obs")

# Cross-correlations between groups
cor_yields_tp <- cor(df[yield_vars], df[tp_vars], use = "complete.obs")
cor_pc_macro <- cor(df[pc_vars], df[macro_vars], use = "complete.obs")

# Time series properties
cat("\n\nTime Series Properties\n")
cat("======================\n")
cat("Date range:", format(range(df$date), "%Y-%m-%d"), "\n")
cat("Frequency: Quarterly\n")
cat("Number of observations:", nrow(df), "\n")
cat("Number of years:", round(as.numeric(diff(range(df$date))) / 365.25, 1), "\n")


# Save results
output_dir <- file.path(OUTPUT_DIR, "temp/summary_stats")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save summary statistics
write.csv(all_stats, file.path(output_dir, "summary_statistics.csv"), row.names = FALSE)

# Save correlation matrices
saveRDS(list(
  yields = cor_yields,
  term_premia = cor_tp,
  pcs = cor_pc,
  yields_tp = cor_yields_tp,
  pc_macro = cor_pc_macro
), file.path(output_dir, "correlation_matrices.rds"))

# Create a compact summary table for key variables
key_vars_summary <- all_stats %>%
  filter(Variable %in% c("y2", "y10", "tp2", "tp10", "pc1", "pc2", "gr1.pcecc96")) %>%
  select(Variable, Mean, SD, Min, Max, Skewness, N)

cat("\n\nKey Variables Summary\n")
cat("====================\n")
print(kable(key_vars_summary, format = "simple", align = "l"))

# Average statistics by maturity
cat("\n\nAverage Statistics by Maturity\n")
cat("==============================\n")

# Extract maturity numbers and compute averages
yields_stats_avg <- yields_stats %>%
  mutate(maturity = as.numeric(gsub("y", "", Variable))) %>%
  select(maturity, Mean, SD)

tp_stats_avg <- tp_stats %>%
  mutate(maturity = as.numeric(gsub("tp", "", Variable))) %>%
  select(maturity, Mean, SD)

maturity_summary <- merge(yields_stats_avg, tp_stats_avg,
  by = "maturity", suffixes = c("_yield", "_tp")
)

print(kable(maturity_summary,
  format = "simple", align = "l",
  col.names = c("Maturity", "Yield Mean", "Yield SD", "TP Mean", "TP SD")
))

cat("\nSummary statistics saved to:", file.path(output_dir, "summary_statistics.csv"))
