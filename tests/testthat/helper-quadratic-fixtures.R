#' Setup Minimal Valid Inputs for Quadratic Tests
#'
#' @param n_rows Number of rows for gamma (J)
#' @param n_maturities Length of statistical inputs
#' @param n_components Full column count for gamma/tau.
#'   Defaults to n_maturities (full-size case). Set larger
#'   for subset-maturity tests where gamma/tau are full-size
#'   but statistical inputs are shorter.
#' @return Named list with all required quadratic inputs
#' @keywords internal
setup_quadratic_test_inputs <- function(
  n_rows = 3,
  n_maturities = 4,
  n_components = n_maturities
) {
  list(
    gamma = matrix(
      seq_len(n_rows * n_components),
      n_rows, n_components
    ),
    tau = rep(1, n_components),
    L_i = rep(1, n_maturities),
    V_i = rep(1, n_maturities),
    Q_i = lapply(
      seq_len(n_maturities),
      function(i) rep(1, n_components)
    ),
    s_i_0 = rep(1, n_maturities),
    s_i_1 = lapply(
      seq_len(n_maturities),
      function(i) rep(1, n_components)
    ),
    s_i_2 = lapply(
      seq_len(n_maturities),
      function(i) {
        matrix(
          1, n_components, n_components
        )
      }
    ),
    sigma_i_sq = rep(1, n_maturities)
  )
}
