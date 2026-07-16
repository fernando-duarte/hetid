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
# The bundled file ships as imported (quarter-start labels); normalize to the
# package period-end convention so the merge with ACM by date keys exactly.
variables$date <- hetid::to_period_end(variables$date, "quarterly")

# Keep date, consumption growth, PCs, and the actual VFCI. The VFCI level is
# carried so the single-instrument paper spec (stage 08) can use the genuine
# de-meaned VFCI (vfci - mean(vfci)) as its lone instrument, distinct from the
# PC-projection. It is additive: every downstream stage selects columns by name.
pc_cols <- paste0("pc", 1:MAX_N_PCS)
base_cols <- c("date", HETID_CONSTANTS$CONSUMPTION_GROWTH_COL, pc_cols)
cols_to_keep <- c(base_cols, "vfci")

# vfci reaches the bundled file a quarter or so behind the other series, so the
# newest rows can be NA and the complete-case merge below ends the sample at
# vfci's last finite quarter -- for every stage, not just the paper spec that
# reads the column. That is forced, not chosen: an instrument matrix must be
# finite on every panel row (hetid::build_instrument_matrix), so a carried NA
# cannot survive to the row where it would be discarded. Missing vfci anywhere
# but the newest rows is a data defect rather than a publication lag, and a lag
# of more than a year means the upstream import broke; abort on both.
MAX_VFCI_LAG_QUARTERS <- 4L
na_rows <- which(is.na(variables$vfci))
if (length(na_rows) > 0) {
  n_var <- nrow(variables)
  if (!identical(na_rows, seq.int(n_var - length(na_rows) + 1L, n_var))) {
    stop(sprintf(
      "vfci is NA on interior rows (%s); expected missing values only at the newest quarters.",
      paste(format(variables$date[na_rows]), collapse = ", ")
    ))
  }
  if (length(na_rows) > MAX_VFCI_LAG_QUARTERS) {
    stop(sprintf(
      "vfci is NA for %d trailing quarters (max %d): the upstream variables import looks broken.",
      length(na_rows), MAX_VFCI_LAG_QUARTERS
    ))
  }
  cli_alert_warning(paste(
    "vfci is unavailable for {length(na_rows)} trailing quarter(s)",
    "({format(max(variables$date[na_rows]))}); every stage's sample ends at",
    "{format(max(variables$date[-na_rows]))}."
  ))
}

# Prepare variables data (period-end dated after the normalization above;
# merged directly by date)
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
# the normalized variables share the period-end convention, so the date keys
# match exactly.
variables_to_merge <- variables_df |>
  (\(x) filter(x, complete.cases(x)))() # Only keep complete rows before merging

data <- merge(data, variables_to_merge, by = "date", all.x = FALSE)

# Ensure data is sorted by date
data <- data[order(data$date), ]

# Create lagged PC variables
for (i in 1:MAX_N_PCS) {
  pc_col <- paste0("pc", i)
  lag_col <- paste0("l.pc", i)
  # Create lag with NA for first observation
  data[[lag_col]] <- c(NA, data[[pc_col]][-length(data[[pc_col]])])
}

# Remove first observation which has NA lags
data <- data[-1, ]

# PCs (and their lags) are kept on their native asset-return PCA scale -- they are
# NOT standardized to unit variance. The principal components already arrive
# mean-centered, and standardizing each PC by its own sd would silently rescale
# the instruments: the fixed VFCI loading holds the raw regression coefficients
# of the VFCI index on the RAW PCs, so the combined instrument PC %*% gamma_vfci
# equals VFCI - mean(VFCI) exactly; applying that loading to standardized PCs
# would no longer reconstruct VFCI. Leaving the PCs at native scale keeps the
# bundled asset data and the analysis on one scale; the optimizer's
# variance-normalization (lambda' Var(Z) lambda = 1) absorbs the instrument
# scale where it matters.

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
  `Lagged PCs` = paste0("l.pc1 to l.pc", MAX_N_PCS),
  `Other Variables` = paste0(
    HETID_CONSTANTS$CONSUMPTION_GROWTH_COL,
    " (consumption growth), vfci (instrument level)"
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
