# Create Data for Analysis
# Load ACM data, convert to quarterly, merge with variables.RData,
# consolidate into single dataset with dates

source(here::here("scripts/utils/common_settings.R"))
# Core packages (hetid, dplyr, tidyr, gt, DT, here, cli) loaded via common_settings.R

maturity_range <- PIPELINE_ACM_MATURITIES

acm_data <- extract_acm_data(
  maturities = maturity_range,
  frequency = "quarterly"
)

dates <- acm_data$date
yield_cols <- paste0("y", maturity_range)
tp_cols <- paste0("tp", maturity_range)
yields <- as.matrix(acm_data[, yield_cols])
term_premia <- as.matrix(acm_data[, tp_cols])

data("variables", package = "hetid")

# Keep only date, consumption growth, and PCs
pc_cols <- paste0("pc", 1:MAX_N_PCS)
cols_to_keep <- c("date", HETID_CONSTANTS$CONSUMPTION_GROWTH_COL, pc_cols)

# Prepare variables data (already period-end dated; merged directly by date)
variables_df <- variables |>
  select(all_of(cols_to_keep)) |>
  mutate(date = as.Date(date))

# Create dataset starting with the canonical period-end ACM dates
data <- data.frame(
  date = dates,
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

# Merge with variables by calendar date (only complete observations). ACM and
# the bundled variables now share the period-end convention, so the date keys
# match exactly.
variables_to_merge <- variables_df |>
  (\(x) filter(x, complete.cases(x)))() # Only keep complete rows before merging

data <- merge(data, variables_to_merge, by = "date", all.x = FALSE)

# Ensure data is sorted by date
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

# Normalize PCs to have mean 0 and variance 1
for (i in 1:MAX_N_PCS) {
  pc_col <- paste0("pc", i)
  # Calculate mean and sd
  pc_mean <- mean(data[[pc_col]], na.rm = TRUE)
  pc_sd <- sd(data[[pc_col]], na.rm = TRUE)
  # Normalize
  data[[pc_col]] <- (data[[pc_col]] - pc_mean) / pc_sd
}

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

# Use utility function to check data completeness
check_data_completeness(as.data.frame(data), stop_on_na = TRUE)

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
  `Other Variables` = paste0(
    HETID_CONSTANTS$CONSUMPTION_GROWTH_COL, " (consumption growth)"
  )
)

summary_df <- data.frame(
  Item = names(summary_info),
  Value = unlist(summary_info),
  stringsAsFactors = FALSE,
  row.names = NULL
)

# Use utility function for formatted table
summary_table <- create_formatted_table(
  summary_df,
  title = "Data Creation Summary",
  subtitle = "Quarterly ACM and Variables Data Merge",
  highlight_rows = which(summary_df$Item == "Number of Observations"),
  highlight_color = "lightblue"
) |>
  tab_options(
    table.font.size = 12,
    table.width = pct(60)
  )

print(summary_table)

output_path <- DATA_RDS_PATH
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
saveRDS(data, output_path)
