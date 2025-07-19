# Global variables to avoid R CMD check notes
utils::globalVariables(c(
  # Functions defined in this package
  "download_term_premia",
  "download_yield_curve",
  "load_term_premia",
  "load_yield_curve",
  "compute_n_hat",
  "solve_gamma_quadratic_lincomb",
  "validate_gamma_inputs",
  "compute_gamma_moments",
  "build_acm_col_mapping",
  "convert_to_quarterly",
  # Data variables
  "variables"
))
