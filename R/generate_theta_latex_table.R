#' Generate LaTeX Table for Theta Identification Results
#'
#' Creates a professional LaTeX table showing the identified sets and optimal
#' parameters for heteroskedasticity across different maturities.
#'
#' @param results_list List of optimization results from optimize_theta_identification
#'   or the full_results component from optimize_theta_all_maturities
#' @param maturities Vector of maturities to include (default 2:9)
#' @param n_pcs Number of principal components extracted from financial asset returns
#'   to show (default 4)
#' @param table_only Logical, if TRUE returns only the table code, if FALSE
#'   returns complete LaTeX document (default TRUE)
#' @param landscape Logical, if TRUE uses landscape orientation (default TRUE)
#' @param caption Table caption (default provided)
#' @param label Table label for references (default "tab:theta_identification")
#' @param notes Table notes (default provided)
#'
#' @return Character string containing LaTeX code
#'
#' @export
generate_theta_latex_table <- function(results_list,
                                       maturities = 2:9,
                                       n_pcs = 4,
                                       table_only = TRUE,
                                       landscape = TRUE,
                                       caption = paste(
                                         "Identified Sets and Optimal",
                                         "Parameters for Heteroskedasticity"
                                       ),
                                       label = "tab:theta_identification",
                                       notes = NULL) {
  # Extract results for specified n_pcs
  results_filtered <- filter_results_by_pcs(results_list, maturities, n_pcs)

  # Build LaTeX code
  latex_lines <- character()

  # Document preamble (if not table_only)
  if (!table_only) {
    latex_lines <- c(latex_lines, generate_latex_preamble(landscape))
  }

  # Start landscape if requested
  if (landscape) {
    latex_lines <- c(latex_lines, "\\begin{landscape}", "\\vspace*{\\fill}")
  }

  # Generate table header
  latex_lines <- c(
    latex_lines,
    generate_table_header(caption, label, maturities)
  )

  # Panel A: Identified Sets
  latex_lines <- c(
    latex_lines,
    generate_panel_a_identified_sets(results_filtered, maturities)
  )

  # Panel B: PC weights
  latex_lines <- c(
    latex_lines,
    generate_panel_b_pc_weights(results_filtered, maturities, n_pcs)
  )

  # Panel C: Tau values
  latex_lines <- c(
    latex_lines,
    generate_panel_c_tau_values(results_filtered, maturities)
  )

  # Table footer
  latex_lines <- c(latex_lines, "\\bottomrule", "\\end{tabular}")

  # Table notes
  if (is.null(notes)) {
    notes <- generate_default_notes(n_pcs, maturities)
  }

  latex_lines <- c(
    latex_lines,
    "\\begin{tablenotes}[flushleft]",
    "\\small",
    paste0("\\item ", notes),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  # End landscape and document
  if (landscape) {
    latex_lines <- c(latex_lines, "\\vspace*{\\fill}", "\\end{landscape}")
  }

  if (!table_only) {
    latex_lines <- c(latex_lines, "", "\\end{document}")
  }

  return(paste(latex_lines, collapse = "\n"))
}
