# Summary Statistics
# Generate descriptive statistics for all variables

source(here::here("scripts/utils/common_settings.R"))
# Core packages (hetid, dplyr, tidyr, gt, DT, here, cli) loaded via common_settings.R

input_path <- file.path(OUTPUT_DIR, "temp/data.rds")
data <- readRDS(input_path)
df <- as.data.frame(data)

yield_vars <- grep("^y\\d+$", names(df), value = TRUE)
tp_vars <- grep("^tp\\d+$", names(df), value = TRUE)
pc_vars <- grep("^pc\\d+$", names(df), value = TRUE)
pc_lag_vars <- grep("^pc\\d+_lag1$", names(df), value = TRUE)
macro_vars <- HETID_CONSTANTS$CONSUMPTION_GROWTH_COL

# Compute statistics using utility function and add quartiles
compute_detailed_stats <- function(x, var_name) {
  # Use utility function for basic stats
  stats <- compute_summary_stats(x, var_name, compute_ac = TRUE, max_lags = 2)

  # Add quartiles which aren't in the utility function
  stats$Q1 <- quantile(x, 0.25, na.rm = TRUE)
  stats$Median <- median(x, na.rm = TRUE)
  stats$Q3 <- quantile(x, 0.75, na.rm = TRUE)

  # Reorder columns
  stats[, c(
    "Variable", "Mean", "SD", "Min", "Q1", "Median", "Q3", "Max",
    "Skewness", "Kurtosis", "AC1", "AC2", "N"
  )]
}

# Compute statistics for each variable group
yields_stats <- do.call(rbind, lapply(yield_vars, function(v) {
  compute_detailed_stats(df[[v]], v)
}))

tp_stats <- do.call(rbind, lapply(tp_vars, function(v) {
  compute_detailed_stats(df[[v]], v)
}))

pc_stats <- do.call(rbind, lapply(pc_vars, function(v) {
  compute_detailed_stats(df[[v]], v)
}))

pc_lag_stats <- do.call(rbind, lapply(pc_lag_vars, function(v) {
  compute_detailed_stats(df[[v]], v)
}))

macro_stats <- compute_detailed_stats(df[[macro_vars]], macro_vars)

# Helper function to create section headers
create_section_header <- function(title) {
  data.frame(
    Variable = paste0("--- ", title, " ---"), Mean = NA, SD = NA, Min = NA,
    Q1 = NA, Median = NA, Q3 = NA, Max = NA, Skewness = NA,
    Kurtosis = NA, AC1 = NA, AC2 = NA, N = NA, stringsAsFactors = FALSE
  )
}

# Combine all statistics
all_stats <- rbind(
  create_section_header("Yields"),
  yields_stats,
  create_section_header("Term Premia"),
  tp_stats,
  create_section_header("Principal Components"),
  pc_stats,
  create_section_header("Lagged Principal Components"),
  pc_lag_stats,
  create_section_header("Macro Variables"),
  macro_stats
)

# Format numeric columns
numeric_cols <- c("Mean", "SD", "Min", "Q1", "Median", "Q3", "Max", "Skewness", "Kurtosis", "AC1", "AC2")
all_stats[numeric_cols] <- lapply(all_stats[numeric_cols], function(x) round(x, 3))

cli_h2("Summary Statistics")

# Use utility function for formatted table
summary_table <- create_formatted_table(
  all_stats,
  title = "Summary Statistics for All Variables",
  subtitle = "Quarterly Data Analysis"
) %>%
  fmt_number(
    columns = c(Mean:AC2),
    decimals = 3
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f0f0f0"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = grepl("^---", Variable)
    )
  ) %>%
  tab_options(
    table.font.size = 11,
    data_row.padding = px(2)
  )

print(summary_table)

cor_yields <- cor(df[yield_vars], use = "complete.obs")
cor_tp <- cor(df[tp_vars], use = "complete.obs")
cor_pc <- cor(df[pc_vars], use = "complete.obs")
cor_yields_tp <- cor(df[yield_vars], df[tp_vars], use = "complete.obs")
cor_pc_macro <- cor(df[pc_vars], df[macro_vars], use = "complete.obs")

cli_h2("Time Series Properties")
cli_ul(c(
  paste("Date range:", paste(format(range(df$date), "%Y-%m-%d"), collapse = " to ")),
  "Frequency: Quarterly",
  paste("Number of observations:", nrow(df)),
  paste("Number of years:", round(as.numeric(diff(range(df$date))) / 365.25, 1))
))

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

key_vars_summary <- all_stats %>%
  filter(Variable %in% c("y2", "y10", "tp2", "tp10", "pc1", "pc2", "gr1.pcecc96")) %>%
  select(Variable, Mean, SD, Min, Max, Skewness, N)

cli_h2("Key Variables Summary")

# Use utility function for interactive table
key_vars_dt <- create_interactive_table(
  key_vars_summary,
  page_length = 10,
  round_digits = 3
)

print(key_vars_dt)

cli_h2("Average Statistics by Maturity")

yields_stats_avg <- yields_stats %>%
  mutate(maturity = as.numeric(gsub("y", "", Variable))) %>%
  select(maturity, Mean, SD)

tp_stats_avg <- tp_stats %>%
  mutate(maturity = as.numeric(gsub("tp", "", Variable))) %>%
  select(maturity, Mean, SD)

maturity_summary <- merge(yields_stats_avg, tp_stats_avg,
  by = "maturity", suffixes = c("_yield", "_tp")
)

# Use utility function for formatted table with custom styling
maturity_table <- create_formatted_table(
  maturity_summary,
  title = "Statistics by Maturity"
) %>%
  cols_label(
    maturity = "Maturity",
    Mean_yield = "Yield Mean",
    SD_yield = "Yield SD",
    Mean_tp = "TP Mean",
    SD_tp = "TP SD"
  ) %>%
  fmt_number(
    columns = c(Mean_yield:SD_tp),
    decimals = 3
  ) %>%
  tab_style(
    style = cell_fill(color = "lightblue", alpha = 0.3),
    locations = cells_body(columns = c(Mean_yield, SD_yield))
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgreen", alpha = 0.3),
    locations = cells_body(columns = c(Mean_tp, SD_tp))
  )

print(maturity_table)
