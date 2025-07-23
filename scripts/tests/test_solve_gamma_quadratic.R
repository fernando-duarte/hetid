# =============================================================================
# Test: solve_gamma_quadratic()
# =============================================================================
# This function solves the quadratic equation for gamma_1, the key identification
# parameter in the heteroskedasticity framework. It finds the roots of the
# quadratic that arises from the moment conditions.

# Load package
library(hetid)

cat("Testing solve_gamma_quadratic() function\n")
cat("========================================\n\n")

# Prepare test data
data("variables")
# Use quarterly ACM data to match the frequency of variables (PCs)
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  frequency = "quarterly"
)
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Check dimensions and align data if needed
cat(sprintf("Variables (PCs) rows: %d\n", nrow(variables)))
cat(sprintf("ACM data rows: %d\n", nrow(acm_data)))

# Align the data by matching quarterly periods
if (nrow(variables) != nrow(acm_data)) {
  cat("\nAligning data by quarterly period...\n")

  # Create year-quarter identifiers
  variables$year_quarter <- paste0(format(variables$date, "%Y"), "-Q", quarters(variables$date))
  acm_data$year_quarter <- paste0(format(acm_data$date, "%Y"), "-Q", quarters(acm_data$date))

  # Find common quarters
  common_quarters <- intersect(variables$year_quarter, acm_data$year_quarter)
  cat(sprintf("Common quarters: %d\n", length(common_quarters)))

  # Subset both datasets to common quarters
  variables_indices <- which(variables$year_quarter %in% common_quarters)
  acm_indices <- which(acm_data$year_quarter %in% common_quarters)

  variables_aligned <- variables[variables_indices, ]
  yields <- yields[acm_indices, ]
  term_premia <- term_premia[acm_indices, ]

  # Extract aligned PCs
  pcs_aligned <- as.matrix(variables_aligned[, paste0("pc", 1:4)])

  cat(sprintf("Aligned data rows: %d\n\n", nrow(yields)))
} else {
  variables_aligned <- variables
  pcs_aligned <- as.matrix(variables[, paste0("pc", 1:4)])
}

# Compute residuals
res_y1 <- compute_w1_residuals(n_pcs = 4, data = variables_aligned)
W1 <- res_y1$residuals

res_y2 <- compute_w2_residuals(
  yields = yields,
  term_premia = term_premia,
  maturities = 5,
  n_pcs = 4,
  pcs = pcs_aligned
)
W2 <- res_y2$residuals[[1]]

# Get PC data from aligned dataset
PC <- variables_aligned$pc1

# Align all data
n_obs <- min(length(W1), length(W2), length(PC))
W1_aligned <- W1[1:n_obs]
W2_aligned <- W2[1:n_obs]
PC_aligned <- PC[1:n_obs]

# Test: Basic functionality with tau = 0.5
cat("Solving quadratic with tau = 0.5\n")
result <- solve_gamma_quadratic(
  pc_j = PC_aligned,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  use_t_minus_1 = TRUE
)

if (!is.null(result$error)) {
  cat(sprintf("  Error: %s\n", result$error))
} else {
  cat(sprintf("  First root: %.4f\n", Re(result$roots[1])))
  cat(sprintf("  Second root: %.4f\n", Re(result$roots[2])))
  cat(sprintf("  Complex roots: %s\n", ifelse(is.complex(result$roots[1]), "Yes", "No")))
}

# Test: Different tau values
cat("\nEffect of tau on roots\n")
tau_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)
for (tau in tau_values) {
  result <- solve_gamma_quadratic(
    pc_j = PC_aligned,
    w1 = W1_aligned,
    w2 = W2_aligned,
    tau = tau,
    use_t_minus_1 = TRUE
  )

  if (!is.null(result$error)) {
    cat(sprintf("  tau=%.1f: Error\n", tau))
  } else if (is.complex(result$roots[1])) {
    cat(sprintf("  tau=%.1f: Complex roots\n", tau))
  } else {
    cat(sprintf(
      "  tau=%.1f: roots = [%.3f, %.3f], distance = %.3f\n",
      tau, result$roots[1], result$roots[2],
      abs(result$roots[1] - result$roots[2])
    ))
  }
}

# Test: Different PCs
cat("\nResults for different PCs (tau = 0.5)\n")
for (j in 1:6) {
  PC_j <- variables_aligned[[paste0("pc", j)]]
  PC_j_aligned <- PC_j[1:n_obs]

  result <- solve_gamma_quadratic(
    pc_j = PC_j_aligned,
    w1 = W1_aligned,
    w2 = W2_aligned,
    tau = 0.5,
    use_t_minus_1 = TRUE
  )

  if (!is.null(result$error)) {
    cat(sprintf("  PC%d: Error\n", j))
  } else if (is.complex(result$roots[1])) {
    cat(sprintf("  PC%d: Complex roots\n", j))
  } else {
    cat(sprintf(
      "  PC%d: roots = [%.3f, %.3f]\n",
      j, result$roots[1], result$roots[2]
    ))
  }
}

# Test: Lagged vs contemporaneous PC
cat("\nEffect of use_t_minus_1\n")
result_lag <- solve_gamma_quadratic(
  pc_j = PC_aligned,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  use_t_minus_1 = TRUE
)

result_contemp <- solve_gamma_quadratic(
  pc_j = PC_aligned,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.5,
  use_t_minus_1 = FALSE
)

cat("  With lagged PC (use_t_minus_1 = TRUE):\n")
if (!is.null(result_lag$error)) {
  cat(sprintf("    Error: %s\n", result_lag$error))
} else {
  cat(sprintf(
    "    Roots: [%.4f, %.4f]\n",
    Re(result_lag$roots[1]), Re(result_lag$roots[2])
  ))
}

cat("  With contemporaneous PC (use_t_minus_1 = FALSE):\n")
if (!is.null(result_contemp$error)) {
  cat(sprintf("    Error: %s\n", result_contemp$error))
} else {
  cat(sprintf(
    "    Roots: [%.4f, %.4f]\n",
    Re(result_contemp$roots[1]), Re(result_contemp$roots[2])
  ))
}

# Test: Understanding the quadratic coefficients
cat("\nQuadratic equation structure\n")
cat("  The function solves: a*gamma_1^2 + b*gamma_1 + c = 0\n")
cat("  where:\n")
cat("    a = 1 - tau^2\n")
cat("    b and c depend on covariances between W1, W2, and PC\n")

tau <- 0.5
a <- 1 - tau^2
cat(sprintf("  For tau = %.1f: a = %.3f\n", tau, a))
cat("  This affects the curvature of the quadratic\n")

# Test: Edge cases
cat("\nEdge cases\n")

# Very small tau (approaches 0)
result_small_tau <- solve_gamma_quadratic(
  pc_j = PC_aligned,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.01,
  use_t_minus_1 = TRUE
)
cat(sprintf(
  "  tau = 0.01: %s\n",
  ifelse(is.null(result_small_tau$error), "Success", "Error")
))

# tau close to 1
result_large_tau <- solve_gamma_quadratic(
  pc_j = PC_aligned,
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = 0.99,
  use_t_minus_1 = TRUE
)
cat(sprintf(
  "  tau = 0.99: %s\n",
  ifelse(is.null(result_large_tau$error), "Success", "Error")
))

cat("\nTest complete!\n")

# =============================================================================
# Interactive Quadratic Visualization
# =============================================================================
cat("\n\nInteractive Quadratic Visualization\n")
cat("===================================\n")
cat("Modify the parameters below to explore different scenarios\n\n")

# User-configurable parameters
plot_tau <- 0.5 # Change this value between 0 and 1
plot_pc_index <- 1 # Which PC to use (1, 2, 3, 4, 5, or 6)
plot_lag_pc <- TRUE # TRUE for lagged PC (PC at t-1), FALSE for contemporaneous (PC at t)

# Store all PC data
all_pcs <- list(
  variables_aligned$pc1[1:n_obs],
  variables_aligned$pc2[1:n_obs],
  variables_aligned$pc3[1:n_obs],
  variables_aligned$pc4[1:n_obs],
  variables_aligned$pc5[1:n_obs],
  variables_aligned$pc6[1:n_obs]
)

# Function to compute quadratic coefficients
compute_quadratic_coeffs <- function(pc_j, w1, w2, tau, use_t_minus_1) {
  # Get result from solve_gamma_quadratic
  result <- solve_gamma_quadratic(
    pc_j = pc_j,
    w1 = w1,
    w2 = w2,
    tau = tau,
    use_t_minus_1 = use_t_minus_1
  )

  if (!is.null(result$error)) {
    return(list(error = result$error))
  }

  # Extract coefficients
  a <- result$coefficients["a"]
  b <- result$coefficients["b"]
  c <- result$coefficients["c"]

  list(
    a = a,
    b = b,
    c = c,
    roots = result$roots,
    discriminant = result$discriminant
  )
}

# Compute coefficients for current parameters
cat(sprintf(
  "Computing quadratic for: tau = %.2f, PC%d, %s\n",
  plot_tau, plot_pc_index,
  ifelse(plot_use_lag, "lagged PC", "contemporaneous PC")
))

coeffs <- compute_quadratic_coeffs(
  pc_j = all_pcs[[plot_pc_index]],
  w1 = W1_aligned,
  w2 = W2_aligned,
  tau = plot_tau,
  use_t_minus_1 = plot_use_lag
)

if (!is.null(coeffs$error)) {
  cat(sprintf("Error: %s\n", coeffs$error))
} else {
  # Create the plot
  cat("\nQuadratic equation: ")
  cat(sprintf(
    "%.4f * gamma_1^2 + %.4f * gamma_1 + %.4f = 0\n\n",
    coeffs$a, coeffs$b, coeffs$c
  ))

  # Define gamma_1 range for plotting
  if (is.complex(coeffs$roots[1])) {
    # For complex roots, center around -b/(2a)
    center <- -coeffs$b / (2 * coeffs$a)
    gamma_range <- seq(center - 5, center + 5, length.out = 1000)
  } else {
    # For real roots, plot around the roots
    root_min <- min(Re(coeffs$roots))
    root_max <- max(Re(coeffs$roots))
    margin <- abs(root_max - root_min) * 0.5 + 1
    gamma_range <- seq(root_min - margin, root_max + margin, length.out = 1000)
  }

  # Compute quadratic values
  quad_values <- coeffs$a * gamma_range^2 + coeffs$b * gamma_range + coeffs$c

  # Set up plot
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

  # Plot the quadratic
  plot(gamma_range, quad_values,
    type = "l", lwd = 2, col = "blue",
    xlab = expression(gamma[1]),
    ylab = expression(paste("f(", gamma[1], ")")),
    main = sprintf(
      "Quadratic Function: tau = %.2f, PC%d, %s",
      plot_tau, plot_pc_index,
      ifelse(plot_use_lag, "Lagged PC", "Contemporaneous PC")
    )
  )

  # Add horizontal line at y = 0
  abline(h = 0, col = "gray", lty = 2)

  # Add vertical lines at roots if they are real
  if (!is.complex(coeffs$roots[1])) {
    abline(v = coeffs$roots[1], col = "red", lty = 2, lwd = 2)
    abline(v = coeffs$roots[2], col = "red", lty = 2, lwd = 2)

    # Add points at roots
    points(coeffs$roots[1], 0, pch = 19, col = "red", cex = 1.5)
    points(coeffs$roots[2], 0, pch = 19, col = "red", cex = 1.5)

    # Add root labels
    text(coeffs$roots[1], max(quad_values) * 0.1,
      sprintf("γ₁⁽¹⁾ = %.3f", coeffs$roots[1]),
      col = "red", pos = 3
    )
    text(coeffs$roots[2], max(quad_values) * 0.1,
      sprintf("γ₁⁽²⁾ = %.3f", coeffs$roots[2]),
      col = "red", pos = 3
    )
  }

  # Add legend
  legend_text <- c(
    sprintf("a = %.4f", coeffs$a),
    sprintf("b = %.4f", coeffs$b),
    sprintf("c = %.4f", coeffs$c),
    sprintf("Discriminant = %.4f", coeffs$discriminant)
  )

  if (is.complex(coeffs$roots[1])) {
    legend_text <- c(legend_text, "Roots: Complex")
  } else {
    legend_text <- c(
      legend_text,
      sprintf("Root distance = %.4f", abs(coeffs$roots[1] - coeffs$roots[2]))
    )
  }

  legend("topright", legend_text, bg = "white", box.lty = 1)

  # Print roots information
  cat("Roots:\n")
  if (is.complex(coeffs$roots[1])) {
    cat(sprintf(
      "  γ₁⁽¹⁾ = %.4f + %.4fi\n",
      Re(coeffs$roots[1]), Im(coeffs$roots[1])
    ))
    cat(sprintf(
      "  γ₁⁽²⁾ = %.4f + %.4fi\n",
      Re(coeffs$roots[2]), Im(coeffs$roots[2])
    ))
  } else {
    cat(sprintf("  γ₁⁽¹⁾ = %.4f\n", coeffs$roots[1]))
    cat(sprintf("  γ₁⁽²⁾ = %.4f\n", coeffs$roots[2]))
    cat(sprintf("  Distance = %.4f\n", abs(coeffs$roots[1] - coeffs$roots[2])))
  }
}

cat("\n")
cat("To explore different scenarios, modify these parameters:\n")
cat("  plot_tau       : tau value (0 to 1)\n")
cat("  plot_pc_index  : which PC to use (1, 2, 3, 4, 5, or 6)\n")
cat("  plot_lag_pc    : TRUE for lagged PC (t-1), FALSE for contemporaneous PC (t)\n")
cat("Then re-run this section of the code.\n")

# Additional analysis: Show how roots change with tau
cat("\n\nRoot behavior across tau values:\n")
cat("================================\n")

# Create a grid of tau values
tau_grid <- seq(0.1, 0.9, by = 0.1)
root1_values <- numeric(length(tau_grid))
root2_values <- numeric(length(tau_grid))
is_complex_flags <- logical(length(tau_grid))

for (i in seq_along(tau_grid)) {
  result <- solve_gamma_quadratic(
    pc_j = all_pcs[[plot_pc_index]],
    w1 = W1_aligned,
    w2 = W2_aligned,
    tau = tau_grid[i],
    use_t_minus_1 = plot_use_lag
  )

  if (!is.null(result$error)) {
    root1_values[i] <- NA
    root2_values[i] <- NA
  } else {
    is_complex_flags[i] <- is.complex(result$roots[1])
    root1_values[i] <- Re(result$roots[1])
    root2_values[i] <- Re(result$roots[2])
  }
}

# Plot roots vs tau
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
plot(tau_grid, root1_values,
  type = "b", col = "blue", pch = 19,
  xlab = "tau", ylab = "Root values",
  main = sprintf(
    "Roots vs tau (PC%d, %s)",
    plot_pc_index,
    ifelse(plot_use_lag, "Lagged PC", "Contemporaneous PC")
  ),
  ylim = range(c(root1_values, root2_values), na.rm = TRUE)
)
lines(tau_grid, root2_values, type = "b", col = "red", pch = 19)

# Mark complex root regions
complex_idx <- which(is_complex_flags)
if (length(complex_idx) > 0) {
  for (idx in complex_idx) {
    rect(tau_grid[idx] - 0.05, par("usr")[3],
      tau_grid[idx] + 0.05, par("usr")[4],
      col = rgb(1, 0, 0, 0.2), border = NA
    )
  }
}

legend("topright",
  c("Root 1", "Root 2", "Complex region"),
  col = c("blue", "red", rgb(1, 0, 0, 0.2)),
  lty = c(1, 1, NA),
  pch = c(19, 19, NA),
  fill = c(NA, NA, rgb(1, 0, 0, 0.2))
)

cat("\nVisualization complete!\n")
