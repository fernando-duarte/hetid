# Compute Price News and SDF Innovations
# Calculate price news (W_i) and SDF innovations for heteroskedasticity-based identification

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
library(hetid)
library(dplyr)
library(tidyr)

# Load processed data
data <- readRDS(file.path(OUTPUT_DIR, "temp/data.rds"))

# Convert to data frame if it's a list
if (is.list(data) && !is.data.frame(data)) {
  data <- as.data.frame(data)
}

cat("Computing Price News and SDF Innovations\n")
cat("========================================\n")

# Extract yields and term premia
yield_vars <- grep("^y\\d+$", names(data), value = TRUE)
tp_vars <- grep("^tp\\d+$", names(data), value = TRUE)

# Get yield maturities from variable names
maturities <- as.numeric(gsub("y", "", yield_vars))

# Ensure maturities are sorted
if (!all(maturities == sort(maturities))) {
  stop("Yield variables must be sorted by maturity")
}

cat("\nMaturities included:", paste(maturities, collapse = ", "), "\n")
cat("Date range:", format(range(data$date), "%Y-%m-%d"), "\n")
cat("Number of observations:", nrow(data), "\n\n")

# Compute price news using compute_price_news function
cat("Computing price news (W_i)...\n")

# Extract yields and term premia (keep as data frames)
yields_df <- data[, yield_vars]
tp_df <- data[, tp_vars]

# Compute price news for each maturity (starting from maturity 2)
# W_i,t = -i * y_{i,t} + (i-1) * y_{i-1,t+1}
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

  price_news_list[[paste0("w", mat_idx)]] <- w_i
}

# Combine into matrix (removing the last observation since news is forward-looking)
price_news <- do.call(cbind, price_news_list)

# Check dimensions
cat("\nPrice news dimensions:", dim(price_news), "\n")
cat("Expected dimensions:", nrow(yields_df) - 1, "x", ncol(yields_df) - 2, "\n")

# Create variable names for price news
w_vars <- names(price_news_list) # w2, w3, ..., w10

# Add price news to data frame (aligned with dates)
# price_news corresponds to t+1, so we align with future dates
data_with_news <- data[-nrow(data), ] # Remove last observation (no future data)

# Add price news columns
for (i in seq_along(w_vars)) {
  data_with_news[[w_vars[i]]] <- price_news[, i]
}

# Compute SDF innovations
cat("\nComputing SDF innovations...\n")

# Get lagged PCs for SDF specification
pc_lag_vars <- grep("^pc\\d+_lag1$", names(data_with_news), value = TRUE)
cat("Using", length(pc_lag_vars), "lagged principal components for SDF\n")

# Extract lagged PCs matrix
pcs_matrix <- as.matrix(data_with_news[, pc_lag_vars])

# For now, we'll compute basic statistics on price news
# The actual SDF innovations will be computed using the identification results

# Summary statistics for price news
cat("\nPrice News Summary Statistics\n")
cat("=============================\n")

price_news_stats <- data.frame(
  Variable = w_vars,
  Mean = colMeans(price_news),
  SD = apply(price_news, 2, sd),
  Min = apply(price_news, 2, min),
  Max = apply(price_news, 2, max),
  Skewness = apply(price_news, 2, function(x) moments::skewness(x)),
  Kurtosis = apply(price_news, 2, function(x) moments::kurtosis(x))
)

price_news_stats[] <- lapply(price_news_stats[-1], function(x) round(x, 4))
print(knitr::kable(price_news_stats, format = "simple", align = "l"))

# Correlation structure of price news
cat("\n\nPrice News Correlation Matrix\n")
cat("=============================\n")
cor_price_news <- cor(price_news)
colnames(cor_price_news) <- w_vars
rownames(cor_price_news) <- w_vars
print(round(cor_price_news, 3))

# Compute first-order autocorrelations
cat("\n\nPrice News Autocorrelations (lag 1)\n")
cat("===================================\n")
ac1_price_news <- sapply(1:ncol(price_news), function(i) {
  acf(price_news[, i], lag.max = 1, plot = FALSE)$acf[2]
})
names(ac1_price_news) <- w_vars
print(round(ac1_price_news, 3))

# Relationship between price news and lagged PCs
cat("\n\nCorrelations: Price News vs Lagged PCs\n")
cat("======================================\n")
cor_news_pcs <- cor(price_news, pcs_matrix)
rownames(cor_news_pcs) <- w_vars
colnames(cor_news_pcs) <- pc_lag_vars
print(round(cor_news_pcs, 3))

# Relationship between price news and consumption growth
cat("\n\nCorrelations: Price News vs Consumption Growth\n")
cat("==============================================\n")
consumption_growth <- data_with_news[[HETID_CONSTANTS$CONSUMPTION_GROWTH_COL]]
cor_news_consumption <- cor(price_news, consumption_growth)
rownames(cor_news_consumption) <- w_vars
colnames(cor_news_consumption) <- HETID_CONSTANTS$CONSUMPTION_GROWTH_COL
print(round(cor_news_consumption, 3))

# Heteroskedasticity Analysis of Price News
cat("\n\nHeteroskedasticity Analysis of Price News\n")
cat("=========================================\n")

# Select specific maturities for W1 and W2 (typically short and medium term)
# Using maturities 2 and 5 as default (can be adjusted)
w1_idx <- which(w_vars == "w2") # 2-year maturity
w2_idx <- which(w_vars == "w5") # 5-year maturity

if (length(w1_idx) == 0 || length(w2_idx) == 0) {
  # Fallback to first two available maturities
  w1_idx <- 1
  w2_idx <- min(2, length(w_vars))
  cat("Note: Using", w_vars[w1_idx], "and", w_vars[w2_idx], "for W1 and W2\n")
}

w1 <- price_news[, w1_idx]
w2 <- price_news[, w2_idx]

# Regress W1 on lagged PCs to get residuals
# W1 = λ0 + λ1'*PCs + ε1
lm_w1 <- lm(w1 ~ ., data = as.data.frame(pcs_matrix))
epsilon1 <- residuals(lm_w1)
lambda0 <- coef(lm_w1)[1]
lambda1 <- coef(lm_w1)[-1]

cat("\nW1 Regression Results:\n")
cat("----------------------\n")
cat("R-squared:", round(summary(lm_w1)$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary(lm_w1)$adj.r.squared, 4), "\n")
f_stat <- summary(lm_w1)$fstatistic
if (!is.null(f_stat)) {
  f_pval <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  cat("F-statistic p-value:", format.pval(f_pval), "\n")
}

cat("\nW1 Residuals Summary:\n")
cat("Mean:", round(mean(epsilon1), 6), "\n")
cat("SD:", round(sd(epsilon1), 4), "\n")
cat("Skewness:", round(moments::skewness(epsilon1), 4), "\n")
cat("Kurtosis:", round(moments::kurtosis(epsilon1), 4), "\n")

# Test for heteroskedasticity in residuals
cat("\n\nHeteroskedasticity Analysis\n")
cat("===========================\n")

# Create squared residuals
epsilon1_sq <- epsilon1^2

# Regress squared residuals on lagged PCs
hetero_lm <- lm(epsilon1_sq ~ ., data = as.data.frame(pcs_matrix))
hetero_summary <- summary(hetero_lm)

cat("R-squared for heteroskedasticity regression:", round(hetero_summary$r.squared, 4), "\n")
cat("F-statistic p-value:", format.pval(pf(hetero_summary$fstatistic[1],
  hetero_summary$fstatistic[2],
  hetero_summary$fstatistic[3],
  lower.tail = FALSE
)), "\n")

# Save results
cat("\n\nSaving results...\n")

# Create output directory
output_dir <- file.path(OUTPUT_DIR, "temp/sdf_news")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save price news data
price_news_df <- data.frame(
  date = data_with_news$date,
  price_news
)
colnames(price_news_df)[-1] <- w_vars
write.csv(price_news_df, file.path(output_dir, "price_news.csv"), row.names = FALSE)

# Save augmented data with news
saveRDS(data_with_news, file.path(output_dir, "data_with_news.rds"))

# Save residuals and regression results
residuals_output <- list(
  w1_var = w_vars[w1_idx],
  w2_var = w_vars[w2_idx],
  epsilon1 = epsilon1,
  lambda0 = lambda0,
  lambda1 = lambda1,
  w1_regression = summary(lm_w1),
  hetero_regression = hetero_summary,
  hetero_r_squared = hetero_summary$r.squared
)
saveRDS(residuals_output, file.path(output_dir, "w1_residuals_analysis.rds"))

# Save summary statistics
stats_output <- list(
  price_news_stats = price_news_stats,
  correlation_matrix = cor_price_news,
  autocorrelations = ac1_price_news,
  correlation_with_pcs = cor_news_pcs,
  correlation_with_consumption = cor_news_consumption
)
saveRDS(stats_output, file.path(output_dir, "price_news_statistics.rds"))

cat("\nResults saved to:", output_dir, "\n")
cat("\nFiles created:\n")
cat("- price_news.csv: Time series of price news for all maturities\n")
cat("- data_with_news.rds: Full dataset including price news\n")
cat("- w1_residuals_analysis.rds: W1 residuals and heteroskedasticity analysis\n")
cat("- price_news_statistics.rds: Summary statistics and correlations\n")

# Report on heteroskedasticity evidence
cat("\n\nHeteroskedasticity Evidence Summary\n")
cat("===================================\n")
if (hetero_summary$r.squared > 0.1) {
  cat("Strong evidence of heteroskedasticity in W1 residuals\n")
  cat(
    "R-squared =", round(hetero_summary$r.squared, 3), "suggests lagged PCs explain",
    round(hetero_summary$r.squared * 100, 1), "% of residual variance\n"
  )
} else if (hetero_summary$r.squared > 0.05) {
  cat("Moderate evidence of heteroskedasticity in W1 residuals\n")
  cat("R-squared =", round(hetero_summary$r.squared, 3), "\n")
} else {
  cat("Weak evidence of heteroskedasticity in W1 residuals\n")
  cat("R-squared =", round(hetero_summary$r.squared, 3), "\n")
  cat("Consider using different maturities or additional conditioning variables\n")
}

cat("\nComputation complete.\n")
