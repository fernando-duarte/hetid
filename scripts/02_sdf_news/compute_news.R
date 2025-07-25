# Compute Price News and SDF Innovations
# Calculate price news (Delta_p) and SDF innovations for heteroskedasticity-based identification

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
library(hetid)
library(dplyr)
library(tidyr)
library(gt)

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

# Summary statistics for price news
cli_h2("Price News Summary Statistics")

# Use utility function to compute statistics for each price news series
price_news_stats <- do.call(rbind, lapply(1:ncol(price_news), function(i) {
  compute_summary_stats(price_news[, i], price_news_vars[i], compute_ac = FALSE)
}))
# Use utility function for formatted table
price_news_table <- create_formatted_table(
  price_news_stats,
  title = "Price News Summary",
  subtitle = "Statistics across maturities"
) %>%
  fmt_number(
    columns = -Variable,
    decimals = 4
  ) %>%
  tab_style(
    style = cell_fill(color = "lightcyan"),
    locations = cells_body(columns = Variable)
  )

print(price_news_table)

# Correlation structure of price news
cli_h2("Price News Correlation Matrix")
cor_price_news <- cor(price_news)
colnames(cor_price_news) <- price_news_vars
rownames(cor_price_news) <- price_news_vars

# Create heatmap-style correlation table
cor_table <- as.data.frame(cor_price_news) %>%
  tibble::rownames_to_column("Variable") %>%
  gt() %>%
  tab_header(title = "Correlation Matrix") %>%
  fmt_number(columns = -Variable, decimals = 3) %>%
  data_color(
    columns = -Variable,
    colors = scales::col_numeric(
      palette = c("blue", "white", "red"),
      domain = c(-1, 1)
    )
  )

print(cor_table)

# Compute first-order autocorrelations
cli_h2("Price News Autocorrelations (lag 1)")
ac1_price_news <- sapply(1:ncol(price_news), function(i) {
  acf(price_news[, i], lag.max = 1, plot = FALSE)$acf[2]
})
names(ac1_price_news) <- price_news_vars

# Create autocorrelation table
ac_df <- data.frame(
  Variable = price_news_vars,
  AC1 = ac1_price_news
) %>%
  gt() %>%
  fmt_number(columns = AC1, decimals = 3) %>%
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(
      columns = AC1,
      rows = AC1 > 0.08
    )
  )

print(ac_df)

# Relationship between price news and lagged PCs
cli_h2("Correlations: Price News vs Lagged PCs")
cor_news_pcs <- cor(price_news, pcs_matrix)
rownames(cor_news_pcs) <- price_news_vars
colnames(cor_news_pcs) <- pc_lag_vars

# Create formatted correlation table
cor_news_pcs_table <- as.data.frame(cor_news_pcs) %>%
  tibble::rownames_to_column("Price_News") %>%
  gt() %>%
  tab_header(title = "Price News vs Lagged PCs Correlations") %>%
  fmt_number(columns = -Price_News, decimals = 3)

print(cor_news_pcs_table)

# Relationship between price news and consumption growth
cli_h2("Correlations: Price News vs Consumption Growth")
consumption_growth <- data_with_news[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]]
cor_news_consumption <- cor(price_news, consumption_growth)
rownames(cor_news_consumption) <- price_news_vars
colnames(cor_news_consumption) <- HETID_CONSTANTS$CONSUMPTION_GROWTH_COL

# Create formatted correlation table
cor_news_consumption_table <- as.data.frame(cor_news_consumption) %>%
  tibble::rownames_to_column("Price_News") %>%
  gt() %>%
  tab_header(title = "Price News vs Consumption Growth") %>%
  fmt_number(columns = -Price_News, decimals = 3) %>%
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(
      columns = -Price_News,
      rows = abs(cor_news_consumption) > 0.1
    )
  )

print(cor_news_consumption_table)

# Heteroskedasticity Analysis of Price News
cli_h1("Heteroskedasticity Analysis of Price News")

# Select specific maturities for heteroskedasticity analysis (typically short and medium term)
# Using maturities 2 and 5 as default (can be adjusted)
price_news_2_idx <- which(price_news_vars == "price_news_2") # 2-year maturity
price_news_5_idx <- which(price_news_vars == "price_news_5") # 5-year maturity

if (length(price_news_2_idx) == 0 || length(price_news_5_idx) == 0) {
  # Fallback to first two available maturities
  price_news_2_idx <- 1
  price_news_5_idx <- min(2, length(price_news_vars))
  cat("Note: Using", price_news_vars[price_news_2_idx], "and", price_news_vars[price_news_5_idx], "for heteroskedasticity analysis\n")
}

# Extract price news for heteroskedasticity analysis (short maturity)
price_news_short <- price_news[, price_news_2_idx]
# Extract price news for heteroskedasticity analysis (medium maturity)
price_news_medium <- price_news[, price_news_5_idx]

# Regress price news on lagged PCs to get residuals
# price_news = λ0 + λ1'*PCs + residuals
lm_price_news <- lm(price_news_short ~ ., data = as.data.frame(pcs_matrix))
price_news_residuals <- residuals(lm_price_news)
lambda0 <- coef(lm_price_news)[1]
lambda1 <- coef(lm_price_news)[-1]

cli_h3("Price News Regression Results")

reg_summary <- summary(lm_price_news)
f_stat <- reg_summary$fstatistic
f_pval <- if (!is.null(f_stat)) pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE) else NA

cli_ul(c(
  paste("R-squared:", round(reg_summary$r.squared, 4)),
  paste("Adjusted R-squared:", round(reg_summary$adj.r.squared, 4)),
  paste("F-statistic p-value:", format.pval(f_pval))
))

cli_h3("Price News Residuals Summary")

resid_stats <- data.frame(
  Statistic = c("Mean", "SD", "Skewness", "Kurtosis"),
  Value = c(
    round(mean(price_news_residuals), 6),
    round(sd(price_news_residuals), 4),
    round(moments::skewness(price_news_residuals), 4),
    round(moments::kurtosis(price_news_residuals), 4)
  )
) %>%
  gt() %>%
  fmt_number(columns = Value, decimals = 4)

print(resid_stats)

cli_h2("Heteroskedasticity Test Results")

price_news_residuals_sq <- price_news_residuals^2

# Regress squared residuals on lagged PCs
hetero_lm <- lm(price_news_residuals_sq ~ ., data = as.data.frame(pcs_matrix))
hetero_summary <- summary(hetero_lm)

hetero_r2 <- round(hetero_summary$r.squared, 4)
hetero_f_pval <- format.pval(pf(hetero_summary$fstatistic[1],
  hetero_summary$fstatistic[2],
  hetero_summary$fstatistic[3],
  lower.tail = FALSE
))

cli_ul(c(
  paste("R-squared for heteroskedasticity regression:", hetero_r2),
  paste("F-statistic p-value:", hetero_f_pval)
))

output_dir <- file.path(OUTPUT_DIR, "temp/sdf_news")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

price_news_df <- data.frame(
  date = data_with_news$date,
  price_news
)
colnames(price_news_df)[-1] <- price_news_vars
write.csv(price_news_df, file.path(output_dir, "price_news.csv"), row.names = FALSE)

saveRDS(data_with_news, file.path(output_dir, "data_with_news.rds"))

residuals_output <- list(
  price_news_var_short = price_news_vars[price_news_2_idx],
  price_news_var_medium = price_news_vars[price_news_5_idx],
  price_news_residuals = price_news_residuals,
  lambda0 = lambda0,
  lambda1 = lambda1,
  price_news_regression = summary(lm_price_news),
  hetero_regression = hetero_summary,
  hetero_r_squared = hetero_summary$r.squared
)
saveRDS(residuals_output, file.path(output_dir, "price_news_residuals_analysis.rds"))

stats_output <- list(
  price_news_stats = price_news_stats,
  correlation_matrix = cor_price_news,
  autocorrelations = ac1_price_news,
  correlation_with_pcs = cor_news_pcs,
  correlation_with_consumption = cor_news_consumption
)
saveRDS(stats_output, file.path(output_dir, "price_news_statistics.rds"))

cli_h2("Heteroskedasticity Evidence Summary")

if (hetero_summary$r.squared > 0.1) {
  cli_alert_success("Strong evidence of heteroskedasticity in price news residuals")
  cli_alert_info("R-squared = {.val {round(hetero_summary$r.squared, 3)}} suggests lagged PCs explain {.val {round(hetero_summary$r.squared * 100, 1)}}% of residual variance")
} else if (hetero_summary$r.squared > 0.05) {
  cli_alert_warning("Moderate evidence of heteroskedasticity in price news residuals")
  cli_alert_info("R-squared = {.val {round(hetero_summary$r.squared, 3)}}")
} else {
  cli_alert_danger("Weak evidence of heteroskedasticity in price news residuals")
  cli_alert_info("R-squared = {.val {round(hetero_summary$r.squared, 3)}}")
  cli_alert_info("Consider using different maturities or additional conditioning variables")
}
