# =============================================================================
# Heteroskedasticity Tests and Correlation Analysis
# =============================================================================
# This script focuses on testing for heteroskedasticity in the W2 residuals
# and computing correlations between PCs and squared residuals.

# -----------------------------------------------------------------------------
# USER PARAMETERS - MODIFY THESE VALUES AS NEEDED
# -----------------------------------------------------------------------------
J <- 4 # Number of principal components to use (1-6)
significance_level <- 0.05 # Significance level for tests
save_plots <- TRUE # Whether to save plots as PDF files
output_dir <- "scripts/output/02_heteroskedasticity" # Output directory

# -----------------------------------------------------------------------------
# SETUP
# -----------------------------------------------------------------------------
library(hetid)
library(ggplot2)
library(tidyr)
library(dplyr)
library(skedastic)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# DATA PREPARATION
# -----------------------------------------------------------------------------
cat("========================================\n")
cat("Heteroskedasticity Tests and Correlations\n")
cat("========================================\n")
cat(sprintf("Number of PCs: %d\n", J))
cat(sprintf("Significance level: %.2f\n\n", significance_level))

# Load data
data("variables")
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields_data <- acm_data[, grep("^y", names(acm_data))]
tp_data <- acm_data[, grep("^tp", names(acm_data))]

I <- 9 # Maximum maturity

# Compute residuals
cat("Computing residuals...\n")
res_y1 <- compute_reduced_form_residual_y1(n_pcs = J)
W1 <- res_y1$residuals

res_y2 <- compute_reduced_form_residual_y2(
  yields = yields_data,
  term_premia = tp_data,
  maturities = 1:I,
  n_pcs = J,
  variables_data = variables
)
W2_list <- res_y2$residuals

# Align data
PC_data <- as.matrix(variables[, paste0("pc", 1:J)])
n_obs <- min(length(W1), nrow(PC_data), length(W2_list[[1]]))
W1_aligned <- W1[1:n_obs]
PC_aligned <- PC_data[1:n_obs, ]

# -----------------------------------------------------------------------------
# HETEROSKEDASTICITY TESTS
# -----------------------------------------------------------------------------
cat("\n========================================\n")
cat("BREUSCH-PAGAN HETEROSKEDASTICITY TESTS\n")
cat("========================================\n\n")

# Function for Breusch-Pagan test
breusch_pagan_test <- function(residuals, regressors) {
  n <- length(residuals)
  res_squared <- residuals^2
  aux_reg <- lm(res_squared ~ regressors)
  r_squared <- summary(aux_reg)$r.squared
  lm_stat <- n * r_squared
  df <- ncol(as.matrix(regressors))
  p_value <- 1 - pchisq(lm_stat, df)
  list(
    statistic = lm_stat,
    df = df,
    p.value = p_value,
    r.squared = r_squared
  )
}

# Store test results
hetero_test_results <- matrix(NA, nrow = I, ncol = 4)
colnames(hetero_test_results) <- c("LM_statistic", "df", "p_value", "R_squared")
rownames(hetero_test_results) <- paste0("Maturity_", 1:I)

# Test each maturity
cat("Testing W2 residuals for heteroskedasticity:\n")
cat("H0: Homoskedasticity (constant variance)\n")
cat("H1: Heteroskedasticity (variance depends on PCs)\n\n")

for (i in 1:I) {
  w2_i <- W2_list[[i]][1:n_obs]
  test_result <- breusch_pagan_test(w2_i, PC_aligned)

  hetero_test_results[i, "LM_statistic"] <- test_result$statistic
  hetero_test_results[i, "df"] <- test_result$df
  hetero_test_results[i, "p_value"] <- test_result$p.value
  hetero_test_results[i, "R_squared"] <- test_result$r.squared

  sig_marker <- ""
  if (test_result$p.value < 0.01) {
    sig_marker <- "***"
  } else if (test_result$p.value < 0.05) {
    sig_marker <- "**"
  } else if (test_result$p.value < 0.10) sig_marker <- "*"

  cat(sprintf(
    "Maturity %2d: LM = %7.3f, p-value = %.4f %s\n",
    i, test_result$statistic, test_result$p.value, sig_marker
  ))
}

cat("\n*** p < 0.01, ** p < 0.05, * p < 0.10\n")

# Summary statistics
n_sig_01 <- sum(hetero_test_results[, "p_value"] < 0.01)
n_sig_05 <- sum(hetero_test_results[, "p_value"] < 0.05)
n_sig_10 <- sum(hetero_test_results[, "p_value"] < 0.10)

cat(sprintf("\nSummary:\n"))
cat(sprintf("  Significant at 1%%:  %d out of %d maturities\n", n_sig_01, I))
cat(sprintf("  Significant at 5%%:  %d out of %d maturities\n", n_sig_05, I))
cat(sprintf("  Significant at 10%%: %d out of %d maturities\n", n_sig_10, I))

# -----------------------------------------------------------------------------
# ADDITIONAL HETEROSKEDASTICITY TESTS FROM SKEDASTIC PACKAGE
# -----------------------------------------------------------------------------
cat("\n========================================\n")
cat("ADDITIONAL HETEROSKEDASTICITY TESTS\n")
cat("========================================\n\n")

# Store results from multiple tests
skedastic_results <- list()

# Select representative maturities to test (to avoid too much output)
test_maturities <- c(1, 3, 5, 7, 9)

for (i in test_maturities) {
  cat(sprintf("\n--- Maturity %d ---\n", i))

  w2_i <- W2_list[[i]][1:n_obs]

  # Create a data frame for regression
  reg_data <- data.frame(
    w2 = w2_i,
    PC_aligned
  )

  # Run regression for this maturity
  lm_model <- lm(w2 ~ ., data = reg_data)

  # 1. White's test - general test for heteroskedasticity
  cat("\nWhite's Test (general heteroskedasticity):\n")
  white_test <- white(lm_model)
  cat(sprintf(
    "  Statistic: %.3f, p-value: %.4f %s\n",
    white_test$statistic, white_test$p.value,
    ifelse(white_test$p.value < significance_level, "**", "")
  ))

  # 2. Breusch-Pagan test (studentized version from skedastic)
  cat("\nBreusch-Pagan Test (studentized):\n")
  bp_test <- breusch_pagan(lm_model)
  cat(sprintf(
    "  Statistic: %.3f, p-value: %.4f %s\n",
    bp_test$statistic, bp_test$p.value,
    ifelse(bp_test$p.value < significance_level, "**", "")
  ))

  # 3. Goldfeld-Quandt test - tests if variance changes across sorted values
  # Sort by first PC as it often captures the most variation
  cat("\nGoldfeld-Quandt Test (variance change across PC1):\n")
  gq_test <- goldfeld_quandt(lm_model, order.by = ~pc1, data = reg_data)
  cat(sprintf(
    "  Statistic: %.3f, p-value: %.4f %s\n",
    gq_test$statistic, gq_test$p.value,
    ifelse(gq_test$p.value < significance_level, "**", "")
  ))

  # 4. Harvey-Godfrey test - tests for multiplicative heteroskedasticity
  cat("\nHarvey-Godfrey Test (multiplicative heteroskedasticity):\n")
  hg_test <- harvey(lm_model)
  cat(sprintf(
    "  Statistic: %.3f, p-value: %.4f %s\n",
    hg_test$statistic, hg_test$p.value,
    ifelse(hg_test$p.value < significance_level, "**", "")
  ))

  # 5. Glejser test - tests absolute residuals against regressors
  cat("\nGlejser Test (absolute residuals):\n")
  glejser_test <- glejser(lm_model)
  cat(sprintf(
    "  Statistic: %.3f, p-value: %.4f %s\n",
    glejser_test$statistic, glejser_test$p.value,
    ifelse(glejser_test$p.value < significance_level, "**", "")
  ))

  # Store results
  skedastic_results[[paste0("maturity_", i)]] <- list(
    white = white_test,
    breusch_pagan_studentized = bp_test,
    goldfeld_quandt = gq_test,
    harvey_godfrey = hg_test,
    glejser = glejser_test
  )
}

# Summary of skedastic tests
cat("\n========================================\n")
cat("SUMMARY OF SKEDASTIC TESTS\n")
cat("========================================\n")

# Count rejections across tests
test_names <- c("White", "BP (studentized)", "Goldfeld-Quandt", "Harvey-Godfrey", "Glejser")
rejection_counts <- matrix(0, nrow = length(test_names), ncol = 3)
rownames(rejection_counts) <- test_names
colnames(rejection_counts) <- c("1%", "5%", "10%")

for (mat_name in names(skedastic_results)) {
  tests <- skedastic_results[[mat_name]]

  # Check each test
  for (j in 1:length(test_names)) {
    test_result <- tests[[j]]
    if (test_result$p.value < 0.01) rejection_counts[j, "1%"] <- rejection_counts[j, "1%"] + 1
    if (test_result$p.value < 0.05) rejection_counts[j, "5%"] <- rejection_counts[j, "5%"] + 1
    if (test_result$p.value < 0.10) rejection_counts[j, "10%"] <- rejection_counts[j, "10%"] + 1
  }
}

cat("\nNumber of rejections (out of", length(test_maturities), "tested maturities):\n")
print(rejection_counts)

# Test for ARCH effects (useful for time series context)
cat("\n========================================\n")
cat("ARCH EFFECTS TEST\n")
cat("========================================\n\n")

# Test a few maturities for ARCH effects
arch_maturities <- c(1, 5, 9)
for (i in arch_maturities) {
  w2_i <- W2_list[[i]][1:n_obs]

  # Create lagged squared residuals
  w2_i_sq <- w2_i^2
  w2_i_sq_lag1 <- c(NA, w2_i_sq[-length(w2_i_sq)])

  # Remove NA
  valid_idx <- !is.na(w2_i_sq_lag1)
  y <- w2_i_sq[valid_idx]
  x <- w2_i_sq_lag1[valid_idx]

  # ARCH(1) test: regress squared residuals on lagged squared residuals
  arch_lm <- lm(y ~ x)
  arch_r2 <- summary(arch_lm)$r.squared
  arch_stat <- (length(y) - 1) * arch_r2
  arch_pval <- 1 - pchisq(arch_stat, df = 1)

  cat(sprintf(
    "Maturity %d: LM = %.3f, p-value = %.4f %s\n",
    i, arch_stat, arch_pval,
    ifelse(arch_pval < significance_level, "**", "")
  ))
}

# -----------------------------------------------------------------------------
# CORRELATION ANALYSIS
# -----------------------------------------------------------------------------
cat("\n========================================\n")
cat("CORRELATION ANALYSIS\n")
cat("========================================\n\n")

# Create correlation matrix
corr_matrix <- matrix(NA, nrow = J, ncol = I)
rownames(corr_matrix) <- paste0("PC", 1:J)
colnames(corr_matrix) <- paste0("Mat", 1:I)

# Calculate absolute correlations between PC_j and W2_i^2
for (j in 1:J) {
  for (i in 1:I) {
    pc_j <- PC_aligned[, j]
    w2_i <- W2_list[[i]][1:n_obs]
    w2_i_squared <- w2_i^2
    correlation <- cor(pc_j, w2_i_squared, use = "complete.obs")
    corr_matrix[j, i] <- abs(correlation)
  }
}

cat("Absolute correlations |corr(PC_j, W2_i^2)|:\n\n")
print(round(corr_matrix, 4))

# Find strongest correlations
cat("\nStrongest correlations:\n")
corr_df <- expand.grid(PC = 1:J, Maturity = 1:I)
corr_df$AbsCorr <- as.vector(corr_matrix)
corr_df <- corr_df[order(corr_df$AbsCorr, decreasing = TRUE), ]

n_top <- min(10, nrow(corr_df))
for (idx in 1:n_top) {
  cat(sprintf(
    "  PC%d - Maturity %d: |corr| = %.4f\n",
    corr_df$PC[idx], corr_df$Maturity[idx], corr_df$AbsCorr[idx]
  ))
}

# Average correlations
avg_corr_by_maturity <- colMeans(corr_matrix, na.rm = TRUE)
avg_corr_by_pc <- rowMeans(corr_matrix, na.rm = TRUE)

cat("\nAverage absolute correlations:\n")
cat("  By PC:\n")
for (j in 1:J) {
  cat(sprintf("    PC%d: %.4f\n", j, avg_corr_by_pc[j]))
}

# -----------------------------------------------------------------------------
# VISUALIZATION
# -----------------------------------------------------------------------------
cat("\n\nCreating visualizations...\n")

# 1. Heteroskedasticity test p-values
hetero_df <- data.frame(
  Maturity = 1:I,
  p_value = hetero_test_results[, "p_value"]
)

p_hetero <- ggplot(hetero_df, aes(x = Maturity, y = -log10(p_value))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = -log10(significance_level),
    linetype = "dashed", color = "red"
  ) +
  geom_hline(
    yintercept = -log10(0.10),
    linetype = "dashed", color = "orange"
  ) +
  scale_x_continuous(breaks = 1:I) +
  labs(
    title = "Heteroskedasticity Tests for W2 Residuals",
    x = "Maturity (years)",
    y = "-log10(p-value)",
    caption = sprintf("Red line: p = %.2f, Orange line: p = 0.10", significance_level)
  ) +
  theme_minimal()

# 2. Correlation heatmap
corr_long <- expand.grid(PC = factor(1:J), Maturity = 1:I)
corr_long$AbsCorr <- as.vector(corr_matrix)

p_corr <- ggplot(corr_long, aes(x = Maturity, y = PC, fill = AbsCorr)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.3f", AbsCorr)), size = 3) +
  scale_fill_gradient(
    low = "white", high = "darkred",
    name = "|Correlation|",
    limits = c(0, 1)
  ) +
  scale_x_continuous(breaks = 1:I) +
  labs(
    title = expression(paste("Absolute Correlations: |corr(", PC[j], ", ", W[list(2, i)]^2, ")|")),
    x = "Maturity (years)",
    y = "Principal Component"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# 3. Skedastic test results comparison
if (length(skedastic_results) > 0) {
  # Extract p-values for visualization
  sked_pvals <- data.frame(
    Maturity = integer(),
    Test = character(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in test_maturities) {
    mat_results <- skedastic_results[[paste0("maturity_", i)]]

    sked_pvals <- rbind(
      sked_pvals,
      data.frame(
        Maturity = i,
        Test = "White",
        p_value = mat_results$white$p.value
      ),
      data.frame(
        Maturity = i,
        Test = "BP (studentized)",
        p_value = mat_results$breusch_pagan_studentized$p.value
      ),
      data.frame(
        Maturity = i,
        Test = "Goldfeld-Quandt",
        p_value = mat_results$goldfeld_quandt$p.value
      ),
      data.frame(
        Maturity = i,
        Test = "Harvey-Godfrey",
        p_value = mat_results$harvey_godfrey$p.value
      ),
      data.frame(
        Maturity = i,
        Test = "Glejser",
        p_value = mat_results$glejser$p.value
      )
    )
  }

  p_skedastic <- ggplot(sked_pvals, aes(x = Maturity, y = -log10(p_value), color = Test)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_hline(
      yintercept = -log10(significance_level),
      linetype = "dashed", color = "red"
    ) +
    scale_x_continuous(breaks = test_maturities) +
    labs(
      title = "Heteroskedasticity Tests from Skedastic Package",
      subtitle = "Higher values indicate stronger evidence of heteroskedasticity",
      x = "Maturity (years)",
      y = "-log10(p-value)",
      caption = sprintf("Red line: p = %.2f", significance_level)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p_skedastic)

  if (save_plots) {
    ggsave(file.path(output_dir, "skedastic_tests_comparison.pdf"),
      p_skedastic,
      width = 10, height = 6
    )
  }
}

# Display plots
print(p_hetero)
print(p_corr)

# Save plots if requested
if (save_plots) {
  ggsave(file.path(output_dir, "heteroskedasticity_pvalues.pdf"),
    p_hetero,
    width = 8, height = 6
  )
  ggsave(file.path(output_dir, "correlation_heatmap.pdf"),
    p_corr,
    width = 10, height = 6
  )
  cat(sprintf("\nPlots saved to %s\n", output_dir))
}

# -----------------------------------------------------------------------------
# SAVE RESULTS
# -----------------------------------------------------------------------------
test_results <- list(
  parameters = list(J = J, significance_level = significance_level),
  heteroskedasticity_tests = hetero_test_results,
  skedastic_tests = skedastic_results,
  correlation_matrix = corr_matrix,
  summary = list(
    n_sig_01 = n_sig_01,
    n_sig_05 = n_sig_05,
    n_sig_10 = n_sig_10,
    avg_corr_by_pc = avg_corr_by_pc,
    avg_corr_by_maturity = avg_corr_by_maturity,
    skedastic_rejection_counts = rejection_counts
  )
)

saveRDS(test_results, file.path(output_dir, "heteroskedasticity_test_results.rds"))
cat(sprintf(
  "\nTest results saved to %s\n",
  file.path(output_dir, "heteroskedasticity_test_results.rds")
))

cat("\nAnalysis complete!\n")
