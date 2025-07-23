#' Generate Summary LaTeX Table from Optimization Results
#'
#' Creates a summary table from a data frame of optimization results,
#' typically from optimize_theta_all_maturities.
#'
#' @param results_df Data frame with optimization results
#' @param n_pcs Number of PCs to show (default 4)
#' @param format Format style: "professional" or "simple" (default "professional")
#' @param ... Additional arguments passed to generate_theta_latex_table
#'
#' @return Character string containing LaTeX code
#'
#' @export
generate_theta_summary_table <- function(results_df,
                                         n_pcs = 4,
                                         format = "professional",
                                         ...) {
  # Filter for specified n_pcs
  results_J <- results_df[results_df$J == n_pcs, ]

  if (format == "simple") {
    # Simple format - just theta intervals and tau
    latex_lines <- c(
      "\\begin{table}[!htbp]",
      "\\centering",
      "\\caption{Heteroskedasticity Parameter Identification}",
      "\\label{tab:theta_simple}",
      paste0("\\begin{tabular}{l*{", nrow(results_J), "}{c}}"),
      "\\toprule",
      paste0("& \\multicolumn{", nrow(results_J), "}{c}{Maturity (years)} \\\\"),
      paste0("\\cmidrule(lr){2-", nrow(results_J) + 1, "}"),
      paste0("$i$ & ", paste(results_J$maturity, collapse = " & "), " \\\\"),
      "\\midrule"
    )

    # Theta intervals
    theta_intervals <- sapply(1:nrow(results_J), function(i) {
      paste0(
        "$[", sprintf("%.2f", results_J$theta_lower[i]), ",\\,",
        sprintf("%.2f", results_J$theta_upper[i]), "]$"
      )
    })

    # Tau values
    tau_values <- sprintf("%.2f\\%%", results_J$tau_opt * 100)

    latex_lines <- c(
      latex_lines,
      paste0("$\\theta_i$ & ", paste(theta_intervals, collapse = " & "), " \\\\"),
      paste0("$\\tau_i$ (\\%) & ", paste(tau_values, collapse = " & "), " \\\\"),
      "\\bottomrule",
      "\\end{tabular}",
      "\\end{table}"
    )

    return(paste(latex_lines, collapse = "\n"))
  } else {
    # Use the full professional format
    # This requires converting the data frame back to the expected list format
    results_list <- list()
    for (i in 1:nrow(results_J)) {
      key <- paste0("mat_", results_J$maturity[i], "_J_", n_pcs)
      results_list[[key]] <- list(
        maturity = results_J$maturity[i],
        n_pcs = n_pcs,
        tau_opt = results_J$tau_opt[i],
        theta_lower = results_J$theta_lower[i],
        theta_upper = results_J$theta_upper[i],
        interval_width = results_J$interval_width[i],
        optimal_weights = NULL # Would need to be extracted from full results
      )
    }

    return(generate_theta_latex_table(
      results_list = results_list,
      maturities = sort(unique(results_J$maturity)),
      n_pcs = n_pcs,
      ...
    ))
  }
}
