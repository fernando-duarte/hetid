# Create Data for Analysis
# Load ACM data, convert to quarterly, merge with variables.RData,
# consolidate into single dataset with dates

# Load required packages
library(hetid)
library(dplyr)
library(tidyr)
library(lubridate)

# Set up paths
library(here)
source(here::here("scripts/utils/common_settings.R"))

# Load ACM data with quarterly frequency
acm_data <- extract_acm_data(frequency = "quarterly")

# Define maturity range
maturity_range <- HETID_CONSTANTS$MIN_MATURITY:HETID_CONSTANTS$MAX_MATURITY

# Extract components and filter by maturity range
dates <- acm_data$date
yield_cols <- paste0("y", maturity_range)
tp_cols <- paste0("tp", maturity_range)
yields <- as.matrix(acm_data[, yield_cols])
term_premia <- as.matrix(acm_data[, tp_cols])

# Load variables data
data("variables", package = "hetid")

# Prepare variables data
# Keep only date, consumption growth, and PCs
pc_cols <- paste0("pc", 1:MAX_N_PCS)
cols_to_keep <- c("date", HETID_CONSTANTS$CONSUMPTION_GROWTH_COL, pc_cols)

# Prepare variables data
variables_df <- variables %>%
  select(all_of(cols_to_keep)) %>%
  mutate(date = as.Date(date)) %>%
  # Convert to end of quarter to match ACM dates
  mutate(date = ceiling_date(date, "quarter") - 1)

# For merging, create year-quarter identifiers
acm_yq <- paste0(year(dates), "-Q", quarter(dates))
variables_df$year_quarter <- paste0(year(variables_df$date), "-Q", quarter(variables_df$date))

# Create dataset starting with ACM dates and year-quarter
data <- data.frame(
  date = dates,
  year_quarter = acm_yq,
  stringsAsFactors = FALSE
)

# Add yields
for (i in seq_along(maturity_range)) {
  data[[yield_cols[i]]] <- yields[, i]
}

# Add term premia
for (i in seq_along(maturity_range)) {
  data[[tp_cols[i]]] <- term_premia[, i]
}

# Merge with variables data by year-quarter (only complete observations)
variables_to_merge <- variables_df %>%
  select(-date) %>%
  filter(complete.cases(.)) # Only keep complete rows before merging

data <- merge(data, variables_to_merge, by = "year_quarter", all.x = FALSE)

# Remove year_quarter column and ensure data is sorted by date
data$year_quarter <- NULL
data <- data[order(data$date), ]

# Create lagged PC variables
for (i in 1:MAX_N_PCS) {
  pc_col <- paste0("pc", i)
  lag_col <- paste0("pc", i, "_lag1")
  # Create lag with NA for first observation
  data[[lag_col]] <- c(NA, data[[pc_col]][-length(data[[pc_col]])])
}

# Remove first observation which has NA lags
data <- data[-1, ]

# Normalize lagged PCs to have mean 0 and variance 1
for (i in 1:MAX_N_PCS) {
  lag_col <- paste0("pc", i, "_lag1")
  # Calculate mean and sd
  pc_mean <- mean(data[[lag_col]], na.rm = TRUE)
  pc_sd <- sd(data[[lag_col]], na.rm = TRUE)
  # Normalize
  data[[lag_col]] <- (data[[lag_col]] - pc_mean) / pc_sd
}

# Convert to list format for consistency with package expectations
data <- as.list(data)

# Final check for any missing or NA values
na_check <- sapply(data, function(x) any(is.na(x)))
if (any(na_check)) {
  stop("ERROR: Missing values found in final dataset. This should not happen.")
}

# Create summary
summary_info <- list(
  Frequency = "quarterly",
  `Date Range` = paste(
    format(min(data$date), "%Y-%m-%d"), "to",
    format(max(data$date), "%Y-%m-%d")
  ),
  `Number of Observations` = length(data$date),
  `Yields` = paste0("y", min(maturity_range), " to y", max(maturity_range)),
  `Term Premia` = paste0("tp", min(maturity_range), " to tp", max(maturity_range)),
  `Principal Components` = paste0("pc1 to pc", MAX_N_PCS),
  `Lagged PCs` = paste0("pc1_lag1 to pc", MAX_N_PCS, "_lag1"),
  `Other Variables` = "gr1.pcecc96 (consumption growth)"
)

# Display summary
summary_df <- data.frame(
  Item = names(summary_info),
  Value = unlist(summary_info),
  stringsAsFactors = FALSE,
  row.names = NULL
)

print(summary_df, right = FALSE)

# Save data
output_path <- file.path(OUTPUT_DIR, "temp/data.rds")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(data, output_path)
