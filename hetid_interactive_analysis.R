#!/usr/bin/env Rscript

# Interactive script for heteroskedasticity identification analysis
# Usage: Rscript hetid_interactive_analysis.R [J] [tau]
# Example: Rscript hetid_interactive_analysis.R 4 0.5

# Load the hetid package
library(hetid)

# Parse command line arguments or use defaults
args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  J <- as.integer(args[1])
} else {
  cat("Enter number of PCs to use (J, between 1-6) [default: 4]: ")
  J_input <- readline()
  J <- ifelse(J_input == "", 4, as.integer(J_input))
}

if (length(args) >= 2) {
  tau <- as.numeric(args[2])
} else {
  cat("Enter tau parameter (between 0-1) [default: 0.5]: ")
  tau_input <- readline()
  tau <- ifelse(tau_input == "", 0.5, as.numeric(tau_input))
}

# Validate inputs
if (!J %in% 1:6) {
  stop("J must be between 1 and 6")
}
if (tau < 0 || tau > 1) {
  stop("tau must be between 0 and 1")
}

# Function to display results in a compact format
display_roots_table <- function(J, I, tau) {
  # Load data
  data("variables")

  # Download ACM data if needed
  if (!file.exists(system.file("extdata", "acm_term_premia.csv", package = "hetid"))) {
    cat("Downloading ACM data...\n")
    download_term_premia(quiet = FALSE)
  }

  # Extract ACM data
  acm_data <- extract_acm_data(data_types = c("yields", "term_premia"))
  yields_data <- acm_data[, grep("^y", names(acm_data))]
  tp_data <- acm_data[, grep("^tp", names(acm_data))]

  # Compute W1
  res_y1 <- suppressWarnings(compute_reduced_form_residual_y1(n_pcs = J))
  W1 <- res_y1$residuals

  # Compute W2i for all maturities
  res_y2 <- suppressWarnings(compute_reduced_form_residual_y2(
    yields = yields_data,
    term_premia = tp_data,
    maturities = 1:I,
    n_pcs = J,
    variables_data = variables
  ))

  W2_list <- res_y2$residuals

  # Extract PCs
  PC_data <- as.matrix(variables[, paste0("pc", 1:J)])

  # Align data
  n_obs <- min(length(W1), nrow(PC_data), length(W2_list[[1]]))
  W1_aligned <- W1[1:n_obs]
  PC_aligned <- PC_data[1:n_obs, ]

  # Create results table
  cat("\n")
  cat(sprintf("Results for J=%d PCs, tau=%.3f\n", J, tau))
  cat(sprintf("Number of observations: %d\n", n_obs))
  cat("\n")

  # Header
  cat("     ")
  for (i in 1:I) {
    cat(sprintf("  Mat %2d    ", i))
  }
  cat("\n")
  cat(paste(rep("-", 13 * I + 5), collapse = ""))
  cat("\n")

  # For each PC
  for (j in 1:J) {
    cat(sprintf("PC%d: ", j))

    for (i in 1:I) {
      pc_j <- PC_aligned[, j]
      w2_i <- W2_list[[i]][1:n_obs]

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
          list(roots = c(NA, NA))
        }
      )

      # Display only the first root (smaller real part)
      if (is.na(result$roots[1])) {
        cat("     NA     ")
      } else if (is.complex(result$roots[1]) && abs(Im(result$roots[1])) > 0.001) {
        cat(sprintf("%6.1f%+.1fi ", Re(result$roots[1]), Im(result$roots[1])))
      } else {
        cat(sprintf(" %10.2f ", Re(result$roots[1])))
      }
    }
    cat("\n")
  }
  cat("\n")
}

# Main execution
cat("========================================\n")
cat("Heteroskedasticity Identification Analysis\n")
cat("========================================\n")

# Run analysis
I <- 10 # Maximum maturity for ACM data
display_roots_table(J, I, tau)

# Ask if user wants to try different parameters
repeat {
  cat("\nTry different parameters? (y/n): ")
  answer <- readline()

  if (tolower(answer) != "y") {
    break
  }

  cat("Enter number of PCs (J, 1-6): ")
  J <- as.integer(readline())

  cat("Enter tau (0-1): ")
  tau <- as.numeric(readline())

  if (J %in% 1:6 && tau >= 0 && tau <= 1) {
    display_roots_table(J, I, tau)
  } else {
    cat("Invalid parameters. J must be 1-6 and tau must be 0-1.\n")
  }
}

cat("\nAnalysis complete!\n")
