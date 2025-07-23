#' Helper Functions for Theta LaTeX Table Generation
#'
#' Internal functions used by generate_theta_latex_table
#'
#' @name theta_latex_helpers
#' @keywords internal
NULL

#' Format numbers for LaTeX
#' @noRd
format_number <- function(x, digits = 2) {
  if (is.na(x) || is.null(x)) {
    return("--")
  }

  # For very small numbers, use scientific notation
  if (abs(x) < 0.01 && x != 0) {
    return(sprintf("%.3g", x))
  } else {
    return(sprintf(paste0("%.", digits, "f"), x))
  }
}

#' Filter results by number of PCs
#' @noRd
filter_results_by_pcs <- function(results_list, maturities, n_pcs) {
  results_filtered <- list()
  for (mat in maturities) {
    key <- paste0("mat_", mat, "_J_", n_pcs)
    if (key %in% names(results_list)) {
      results_filtered[[as.character(mat)]] <- results_list[[key]]
    }
  }
  results_filtered
}

#' Generate LaTeX document preamble
#' @noRd
generate_latex_preamble <- function(landscape) {
  c(
    "\\documentclass{article}",
    "",
    "\\usepackage[margin=1in]{geometry}",
    "\\usepackage{amsmath}",
    "\\usepackage{amssymb}",
    "\\usepackage{booktabs}",
    "\\usepackage{threeparttable}",
    "\\usepackage{array}",
    if (landscape) "\\usepackage{lscape}" else "",
    "",
    "\\usepackage{setspace}",
    "\\setstretch{1.5}",
    "\\renewcommand{\\arraystretch}{2}",
    "",
    "\\begin{document}",
    ""
  )
}

#' Generate table header
#' @noRd
generate_table_header <- function(caption, label, maturities) {
  c(
    "\\begin{table}[!h]",
    "\\centering",
    "\\begin{threeparttable}",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{l@{\\hskip 0.5in}*{", length(maturities), "}{c}}"),
    "\\toprule",
    paste0("& \\multicolumn{", length(maturities), "}{c}{Maturity (years)} \\\\"),
    paste0("\\cmidrule(lr){2-", length(maturities) + 1, "}"),
    paste0("& ", paste(paste0("$i=", maturities, "$"), collapse = " & "), " \\\\"),
    "\\midrule",
    "\\addlinespace[0.5em]",
    "\\multicolumn{9}{l}{\\textbf{A. Identified Set for $\\boldsymbol{\\theta_i}$}} \\\\",
    "\\addlinespace[0.3em]"
  )
}

#' Generate Panel A: Identified Sets
#' @noRd
generate_panel_a_identified_sets <- function(results_filtered, maturities) {
  # Extract theta bounds
  theta_lower <- sapply(maturities, function(mat) {
    if (as.character(mat) %in% names(results_filtered)) {
      paste0("$", format_number(results_filtered[[as.character(mat)]]$theta_lower), "$")
    } else {
      "$--$"
    }
  })

  theta_upper <- sapply(maturities, function(mat) {
    if (as.character(mat) %in% names(results_filtered)) {
      paste0("$", format_number(results_filtered[[as.character(mat)]]$theta_upper), "$")
    } else {
      "$--$"
    }
  })

  widths <- sapply(maturities, function(mat) {
    if (as.character(mat) %in% names(results_filtered)) {
      paste0("$", format_number(results_filtered[[as.character(mat)]]$interval_width), "$")
    } else {
      "$--$"
    }
  })

  c(
    paste0("\\quad Lower bound & ", paste(theta_lower, collapse = " & "), " \\\\"),
    paste0("\\quad Upper bound & ", paste(theta_upper, collapse = " & "), " \\\\"),
    "\\addlinespace[0.3em]",
    paste0("\\quad Width & ", paste(widths, collapse = " & "), " \\\\")
  )
}

#' Generate Panel B: PC Weights
#' @noRd
generate_panel_b_pc_weights <- function(results_filtered, maturities, n_pcs) {
  lines <- c(
    "\\addlinespace[1em]",
    "\\multicolumn{9}{l}{\\textbf{B. Optimal Principal Component Weights}} \\\\",
    "\\addlinespace[0.3em]"
  )

  for (j in 1:n_pcs) {
    weights <- sapply(maturities, function(mat) {
      if (as.character(mat) %in% names(results_filtered)) {
        res <- results_filtered[[as.character(mat)]]
        if (!is.null(res$optimal_weights) && length(res$optimal_weights) >= j) {
          paste0("$", format_number(res$optimal_weights[j]), "$")
        } else {
          "$--$"
        }
      } else {
        "$--$"
      }
    })

    lines <- c(
      lines,
      paste0("\\quad PC", j, " & ", paste(weights, collapse = " & "), " \\\\")
    )
  }

  lines
}

#' Generate Panel C: Tau Values
#' @noRd
generate_panel_c_tau_values <- function(results_filtered, maturities) {
  tau_values <- sapply(maturities, function(mat) {
    if (as.character(mat) %in% names(results_filtered)) {
      tau_pct <- results_filtered[[as.character(mat)]]$tau_opt * 100
      paste0("$", format_number(tau_pct), "\\%$")
    } else {
      "$--$"
    }
  })

  c(
    "\\addlinespace[1em]",
    "\\multicolumn{9}{l}{\\textbf{C. Optimal Lag Parameter}} \\\\",
    "\\addlinespace[0.3em]",
    paste0("\\quad $\\tau_i$ (\\%) & ", paste(tau_values, collapse = " & "), " \\\\")
  )
}

#' Generate default table notes
#' @noRd
generate_default_notes <- function(n_pcs, maturities) {
  paste(
    "\\textit{Notes:} This table presents the optimal solutions to the identification problem",
    "for the heteroskedasticity parameter $\\theta_i$ across different bond maturities.",
    "Panel A reports the identified set $[\\theta_i^{(1)}, \\theta_i^{(2)}]$ along with",
    "the interval width.",
    "Panel B shows the optimal weights for the first", n_pcs, "principal components,",
    "normalized such that $\\sum_{j=1}^{", n_pcs, "} w_j^2 = 1$.",
    "Panel C displays the optimal lag parameter $\\tau_i \\in [0, 0.99)$ as a percentage.",
    "The optimization uses the COBYLA algorithm with 20 random starting points.",
    if (min(maturities) > 1) "Maturity 1 is excluded due to data constraints." else ""
  )
}
