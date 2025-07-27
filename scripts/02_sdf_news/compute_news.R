# Compute Price News and SDF Innovations
# Calculate price news (Delta_p) and SDF innovations for heteroskedasticity-based identification

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
# Core packages (hetid, dplyr, tidyr, gt, DT, here, cli) loaded via common_settings.R

# Load processed data
data <- readRDS(file.path(OUTPUT_DIR, "temp/data.rds"))

# Convert to data frame if it's a list
if (is.list(data) && !is.data.frame(data)) {
  data <- as.data.frame(data)
}

cli_h1("Computing Price News and SDF Innovations")

# Extract yields and term premia
yield_vars <- grep("^y\\d+$", names(data), value = TRUE)
tp_vars <- grep("^tp\\d+$", names(data), value = TRUE)

# Get yield maturities from variable names
maturities <- as.numeric(gsub("y", "", yield_vars))

# Ensure maturities are sorted
if (!all(maturities == sort(maturities))) {
  stop("Yield variables must be sorted by maturity")
}

cli_ul(c(
  paste("Maturities included:", paste(maturities, collapse = ", ")),
  paste("Date range:", paste(format(range(data$date), "%Y-%m-%d"), collapse = " to ")),
  paste("Number of observations:", nrow(data))
))

# Compute price news using compute_price_news function
cli_alert_info("Computing price news (Delta_p)...")

# Extract yields and term premia (keep as data frames)
yields_df <- data[, yield_vars]
tp_df <- data[, tp_vars]

# Compute price news for each maturity (starting from maturity 2)
# Delta_p = n_hat(i-1,t+1) - n_hat(i,t)
price_news_list <- list()

for (i in 2:(length(maturities) - 1)) {
  mat_idx <- maturities[i] # Actual maturity (2, 3, ..., 9)

  # Compute price news for maturity i
  w_i <- compute_price_news(
    yields = yields_df,
    term_premia = tp_df,
    i = mat_idx,
    return_yield_news = FALSE,
    return_df = FALSE,
    dates = data$date
  )

  price_news_list[[paste0("price_news_", mat_idx)]] <- w_i
}

# Combine into matrix (removing the last observation since news is forward-looking)
price_news <- do.call(cbind, price_news_list)

# Check dimensions
cli_alert_success("Price news computed")
cli_ul(c(
  paste("Price news dimensions:", paste(dim(price_news), collapse = " x ")),
  paste("Expected dimensions:", nrow(yields_df) - 1, "x", ncol(yields_df) - 2)
))

# Create variable names for price news
price_news_vars <- names(price_news_list) # price_news_2, price_news_3, ..., price_news_10

# Add price news to data frame (aligned with dates)
# price_news corresponds to t+1, so we align with future dates
data_with_news <- data[-nrow(data), ] # Remove last observation (no future data)

# Add price news columns
for (i in seq_along(price_news_vars)) {
  data_with_news[[price_news_vars[i]]] <- price_news[, i]
}

# Compute SDF innovations
cli_alert_info("Computing SDF innovations...")

# Get lagged PCs for SDF specification
pc_lag_vars <- grep("^pc\\d+_lag1$", names(data_with_news), value = TRUE)
cli_alert("Using {.val {length(pc_lag_vars)}} lagged principal components for SDF")

# Extract lagged PCs matrix
pcs_matrix <- as.matrix(data_with_news[, pc_lag_vars])

# For now, we'll compute basic statistics on price news
# The actual SDF innovations will be computed using the identification results

# Save basic statistics for analysis script
price_news_stats <- list(
  means = colMeans(price_news),
  sds = apply(price_news, 2, sd),
  mins = apply(price_news, 2, min),
  maxs = apply(price_news, 2, max)
)

cli_alert_success("Price news computation complete")
cli_ul(c(
  paste("Price news variables created:", paste(price_news_vars, collapse = ", ")),
  paste("Data saved with", nrow(data_with_news), "observations")
))

# Save output files
output_dir <- file.path(OUTPUT_DIR, "temp/sdf_news")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save price news data
price_news_df <- data.frame(
  date = data_with_news$date,
  price_news
)
colnames(price_news_df)[-1] <- price_news_vars
write.csv(price_news_df, file.path(output_dir, "price_news.csv"), row.names = FALSE)

# Save data with news variables
saveRDS(data_with_news, file.path(output_dir, "data_with_news.rds"))

# Save basic information for analysis script
compute_output <- list(
  price_news_vars = price_news_vars,
  price_news_stats = price_news_stats,
  n_obs = nrow(data_with_news),
  date_range = range(data_with_news$date)
)
saveRDS(compute_output, file.path(output_dir, "compute_output.rds"))

cli_alert_success("Output files saved to {.path {output_dir}}")
