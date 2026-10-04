#' Setup Minimal Valid Inputs for Quadratic Tests
#'
#' Builds the (tau, components, moments) containers for
#' build_quadratic_system() from the same deterministic
#' all-ones synthetic statistics the previous raw-argument fixture used,
#' so the numeric expectations of the quadratic tests are unchanged.
#'
#' @param n_rows Instrument count J for the moment matrices
#' @param n_maturities Length of the constraint axis
#' @param n_components Theta-axis dimension. Defaults to n_maturities
#'   (full-size case). Set larger for subset-maturity tests.
#' @param maturities Constraint-axis values; defaults to the first
#'   n_maturities system columns
#' @return Named list with tau, components, and moments
#' @keywords internal
setup_quadratic_test_inputs <- function(
  n_rows = 3,
  n_maturities = 4,
  n_components = n_maturities,
  maturities = seq_len(n_maturities)
) {
  nms <- paste0("maturity_", maturities)

  ones_vec <- setNames(rep(1, n_maturities), nms)
  ones_mat <- matrix(1, n_rows, n_maturities, dimnames = list(NULL, nms))
  ones_inner_vec <- setNames(
    lapply(seq_len(n_maturities), function(k) rep(1, n_components)), nms
  )

  moments <- new_hetid_moments(
    list(
      s_i_0 = ones_vec,
      sigma_i_sq = ones_vec,
      r_i_0 = ones_mat,
      r_i_1 = setNames(
        lapply(
          seq_len(n_maturities),
          function(k) matrix(1, n_rows, n_components)
        ),
        nms
      ),
      p_i_0 = ones_mat,
      s_i_1 = ones_inner_vec,
      s_i_2 = setNames(
        lapply(
          seq_len(n_maturities),
          function(k) matrix(1, n_components, n_components)
        ),
        nms
      )
    ),
    maturities = maturities,
    n_components = n_components,
    n_obs = 50
  )

  components <- new_hetid_components(
    L_i = ones_vec,
    V_i = ones_vec,
    Q_i = ones_inner_vec,
    maturities = maturities,
    n_components = n_components
  )

  list(
    gamma = matrix(1, n_rows, n_components),
    tau = rep(0.5, n_components),
    components = components,
    moments = moments
  )
}

# Assemble d/A/b/c from raw position-indexed statistics, one kernel call per
# constrained maturity, so hand-computed expectations reach the package
# arithmetic without going through the moment or component stages
quadratic_from_components <- function(tau,
                                      L_i, V_i, Q_i, # nolint: object_name_linter.
                                      s_i_0, s_i_1, s_i_2, sigma_i_sq,
                                      maturities, n_components) {
  rows <- lapply(seq_along(maturities), function(idx) {
    assemble_constraint_quadratic(
      tau_ik = tau[maturities[idx]],
      l_val = L_i[idx], v_val = V_i[idx], q_vec = Q_i[[idx]],
      s_i_0_val = s_i_0[idx], s_i_1_vec = s_i_1[[idx]],
      s_i_2_mat = s_i_2[[idx]], sigma_i_sq_val = sigma_i_sq[idx],
      n_components = n_components,
      label = maturity_names(maturities)[idx]
    )
  })
  names(rows) <- maturity_names(maturities)
  list(
    d_i = vapply(rows, `[[`, 0, "d"), A_i = lapply(rows, `[[`, "A"),
    b_i = lapply(rows, `[[`, "b"), c_i = vapply(rows, `[[`, 0, "c")
  )
}
