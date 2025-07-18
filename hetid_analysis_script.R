#!/usr/bin/env Rscript

# Load the hetid package
library(hetid)

# Set user parameters
J <- 4 # Number of PCs to use (can be 1-6)
tau <- 0.5 # Parameter between 0 and 1

# Validate inputs
if (!J %in% 1:6) {
  stop("J must be between 1 and 6")
}
if (tau < 0 || tau > 1) {
  stop("tau must be between 0 and 1")
}

cat("========================================\n")
cat("Heteroskedasticity Identification Analysis\n")
cat("========================================\n")
cat(sprintf("Number of PCs (J): %d\n", J))
cat(sprintf("Tau parameter: %.3f\n", tau))
cat("\n")

# Load variables data (for PCs and consumption growth)
data("variables")

# Download and load ACM data if needed
if (!file.exists(system.file("extdata", "acm_term_premia.csv", package = "hetid"))) {
  cat("Downloading ACM data...\n")
  download_term_premia(quiet = FALSE)
}

# Extract ACM data
cat("Loading ACM data...\n")
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields_data <- acm_data[, grep("^y", names(acm_data))]
tp_data <- acm_data[, grep("^tp", names(acm_data))]

# Determine maximum maturity I (ACM data has maturities 1-10)
I <- 10
cat(sprintf("Maximum maturity (I): %d years\n\n", I))

# Step 1: Compute W1 (reduced form residual for consumption growth)
cat("Computing W1 (consumption growth residuals)...\n")
res_y1 <- compute_reduced_form_residual_y1(n_pcs = J)
W1 <- res_y1$residuals
cat(sprintf("  Number of observations: %d\n", length(W1)))
cat(sprintf("  R-squared: %.4f\n", res_y1$r_squared))
cat("\n")

# Step 2: Compute W2i for all maturities i=1,...,I
cat("Computing W2i (SDF innovation residuals) for all maturities...\n")
res_y2 <- compute_reduced_form_residual_y2(
  yields = yields_data,
  term_premia = tp_data,
  maturities = 1:I,
  n_pcs = J,
  variables_data = variables
)

# Extract all W2i
W2_list <- res_y2$residuals

# Report R-squared for each maturity
cat("\nR-squared values for W2i regressions:\n")
for (i in 1:I) {
  cat(sprintf("  Maturity %2d: R² = %.4f\n", i, res_y2$r_squared[i]))
}
cat("\n")

# Step 3: Extract PCs
PC_data <- as.matrix(variables[, paste0("pc", 1:J)])

# Ensure all data has same length (use minimum)
n_obs <- min(length(W1), nrow(PC_data), length(W2_list[[1]]))
W1_aligned <- W1[1:n_obs]
PC_aligned <- PC_data[1:n_obs, ]

# Step 4: Compute roots for all combinations of j and i
cat("Computing gamma_1 roots for all (j,i) combinations...\n")
cat("=========================================\n\n")

# Create storage for results
results_matrix_root1 <- matrix(NA, nrow = J, ncol = I)
results_matrix_root2 <- matrix(NA, nrow = J, ncol = I)
rownames(results_matrix_root1) <- paste0("PC", 1:J)
colnames(results_matrix_root1) <- paste0("Mat", 1:I)
rownames(results_matrix_root2) <- paste0("PC", 1:J)
colnames(results_matrix_root2) <- paste0("Mat", 1:I)

# Loop through all combinations
for (j in 1:J) {
  for (i in 1:I) {
    # Get PC_j and W2_i
    pc_j <- PC_aligned[, j]
    w2_i <- W2_list[[i]][1:n_obs]

    # Solve quadratic
    result <- tryCatch(
      {
        solve_gamma_quadratic(
          pc_j = pc_j,
          w1 = W1_aligned,
          w2 = w2_i,
          tau = tau,
          use_t_minus_1 = TRUE
        )
      },
      error = function(e) {
        list(roots = c(NA, NA), error = e$message)
      }
    )

    # Store results
    if (!is.null(result$error)) {
      cat(sprintf("PC%d, Maturity %d: ERROR - %s\n", j, i, result$error))
    } else {
      results_matrix_root1[j, i] <- Re(result$roots[1])
      results_matrix_root2[j, i] <- Re(result$roots[2])

      # Display results
      if (is.complex(result$roots[1])) {
        cat(sprintf(
          "PC%d, Maturity %2d: γ₁⁽¹⁾ = %8.4f %+.4fi, γ₁⁽²⁾ = %8.4f %+.4fi\n",
          j, i,
          Re(result$roots[1]), Im(result$roots[1]),
          Re(result$roots[2]), Im(result$roots[2])
        ))
      } else {
        cat(sprintf(
          "PC%d, Maturity %2d: γ₁⁽¹⁾ = %8.4f, γ₁⁽²⁾ = %8.4f\n",
          j, i, result$roots[1], result$roots[2]
        ))
      }
    }
  }
  cat("\n")
}

# Display summary tables
cat("\n========================================\n")
cat("SUMMARY: Real parts of γ₁⁽¹⁾ (first root)\n")
cat("========================================\n")
print(round(results_matrix_root1, 4))

cat("\n========================================\n")
cat("SUMMARY: Real parts of γ₁⁽²⁾ (second root)\n")
cat("========================================\n")
print(round(results_matrix_root2, 4))

# Create visualization (optional)
if (interactive() && require(ggplot2) && require(tidyr) && require(dplyr)) {
  cat("\nCreating visualization...\n")

  # Reshape data for plotting
  df_root1 <- as.data.frame(results_matrix_root1) %>%
    mutate(PC = factor(1:J)) %>%
    pivot_longer(cols = -PC, names_to = "Maturity", values_to = "Root1") %>%
    mutate(Maturity = as.numeric(gsub("Mat", "", Maturity)))

  df_root2 <- as.data.frame(results_matrix_root2) %>%
    mutate(PC = factor(1:J)) %>%
    pivot_longer(cols = -PC, names_to = "Maturity", values_to = "Root2") %>%
    mutate(Maturity = as.numeric(gsub("Mat", "", Maturity)))

  df_combined <- df_root1 %>%
    left_join(df_root2, by = c("PC", "Maturity"))

  # Plot both roots
  p1 <- ggplot(df_combined, aes(x = Maturity)) +
    geom_line(aes(y = Root1, color = PC), size = 1.2) +
    geom_point(aes(y = Root1, color = PC), size = 2) +
    scale_x_continuous(breaks = 1:10) +
    labs(
      title = sprintf("First Root of γ₁ Quadratic (tau = %.2f)", tau),
      x = "Maturity (years)",
      y = "γ₁⁽¹⁾",
      color = "PC"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  p2 <- ggplot(df_combined, aes(x = Maturity)) +
    geom_line(aes(y = Root2, color = PC), size = 1.2) +
    geom_point(aes(y = Root2, color = PC), size = 2) +
    scale_x_continuous(breaks = 1:10) +
    labs(
      title = sprintf("Second Root of γ₁ Quadratic (tau = %.2f)", tau),
      x = "Maturity (years)",
      y = "γ₁⁽²⁾",
      color = "PC"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Display plots
  print(p1)
  print(p2)

  # Save plots if desired
  ggsave("gamma1_roots_plot1.pdf", p1, width = 8, height = 6)
  ggsave("gamma1_roots_plot2.pdf", p2, width = 8, height = 6)
  cat("\nPlots saved as gamma1_roots_plot1.pdf and gamma1_roots_plot2.pdf\n")
}

cat("\nAnalysis complete!\n")
