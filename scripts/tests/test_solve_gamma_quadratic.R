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
acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]

# Compute residuals
res_y1 <- compute_reduced_form_residual_y1(n_pcs = 4)
W1 <- res_y1$residuals

res_y2 <- compute_reduced_form_residual_y2(
  yields = yields,
  term_premia = term_premia,
  maturities = 5,
  n_pcs = 4,
  variables_data = variables
)
W2 <- res_y2$residuals[[1]]

# Get PC data
PC <- variables$pc1

# Align all data
n_obs <- min(length(W1), length(W2), length(PC))
W1_aligned <- W1[1:n_obs]
W2_aligned <- W2[1:n_obs]
PC_aligned <- PC[1:n_obs]

# Test 1: Basic functionality with tau = 0.5
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

# Test 2: Different tau values
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

# Test 3: Different PCs
cat("\nResults for different PCs (tau = 0.5)\n")
for (j in 1:4) {
  PC_j <- variables[[paste0("pc", j)]]
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

# Test 4: Lagged vs contemporaneous PC
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

# Test 5: Understanding the quadratic coefficients
cat("\nQuadratic equation structure\n")
cat("  The function solves: a*gamma_1^2 + b*gamma_1 + c = 0\n")
cat("  where:\n")
cat("    a = 1 - tau^2\n")
cat("    b and c depend on covariances between W1, W2, and PC\n")

tau <- 0.5
a <- 1 - tau^2
cat(sprintf("  For tau = %.1f: a = %.3f\n", tau, a))
cat("  This affects the curvature of the quadratic\n")

# Test 6: Edge cases
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
