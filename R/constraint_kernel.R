#' Per-Constraint Components from One Weight Column
#'
#' Internal kernel holding the L/V/Q arithmetic for a single weight
#' column applied to the moments of one constraint maturity. The legacy
#' matrix path and the general (component, combination) path share this
#' one implementation and must produce bit-identical values (enforced by
#' \code{test-constraint_kernel.R}). Operation order is load-bearing.
#'
#' @param weight_col Numeric J x 1 matrix (a single weight column)
#' @param idx Position of the maturity within the container's
#'   \code{maturities} vector
#' @param moments A \code{hetid_moments} object
#' @return List with \code{L} (scalar), \code{V} (scalar), \code{Q}
#'   (named length-I vector)
#' @noRd
constraint_components <- function(weight_col, idx, moments) {
  l_val <- as.numeric(
    crossprod(weight_col, moments$r_i_0[, idx, drop = FALSE])
  )

  p_i_0_vec <- moments$p_i_0[, idx, drop = FALSE]
  v_val <- as.numeric(
    crossprod(weight_col, p_i_0_vec)
  )^2

  r_i_1_mat <- moments$r_i_1[[idx]]
  q_vec <- as.numeric(
    crossprod(weight_col, r_i_1_mat)
  )
  names(q_vec) <- maturity_names(seq_len(ncol(r_i_1_mat)))

  list(L = l_val, V = v_val, Q = q_vec)
}

#' Assemble One Quadratic Constraint
#'
#' Internal kernel holding the d/A/b/c arithmetic for a single
#' constraint, including the exact symmetrization and both finite
#' guards. The legacy and general paths must produce bit-identical
#' values (enforced by \code{test-constraint_kernel.R}). Operation
#' order is load-bearing.
#'
#' @param tau_ik Scalar slack for this constraint
#' @param l_val,v_val Scalars from \code{constraint_components()}
#' @param q_vec Length-I vector from \code{constraint_components()}
#' @param s_i_0_val,sigma_i_sq_val Scalars for this maturity
#' @param s_i_1_vec Length-I vector for this maturity
#' @param s_i_2_mat I x I matrix for this maturity
#' @param n_components Theta-axis dimension (I)
#' @param label Constraint label used in error messages (the legacy
#'   path passes \code{paste0("maturity ", i)} so messages are
#'   unchanged)
#' @return List with \code{d}, \code{A}, \code{b}, \code{c}
#' @noRd
assemble_constraint_quadratic <- function(tau_ik, l_val, v_val, q_vec,
                                          s_i_0_val, s_i_1_vec,
                                          s_i_2_mat, sigma_i_sq_val,
                                          n_components, label) {
  d_val <- (tau_ik^2 * v_val) / sigma_i_sq_val

  if (!is.finite(d_val)) {
    stop_hetid(paste0(
      "d_i = tau_i^2 * V_i / sigma_i_sq is non-finite for ",
      label, ": tau_i = ", tau_ik, ", V_i = ", v_val,
      ", sigma_i_sq = ", sigma_i_sq_val, "."
    ))
  }

  a_mat <- tcrossprod(q_vec) - d_val * s_i_2_mat

  # Symmetrize exactly: theta' A theta is invariant under
  # (A + t(A)) / 2, and downstream consumers can rely on it
  a_mat <- (a_mat + t(a_mat)) / 2

  b_vec <- -2 * l_val * q_vec +
    2 * d_val * s_i_1_vec
  names(b_vec) <- maturity_names(seq_len(n_components))

  c_val <- l_val^2 - d_val * s_i_0_val

  # Belt-and-braces against tampered containers: validated inputs
  # guarantee finite moments and components, so a non-finite
  # assembly indicates post-construction tampering or a hetid bug
  if (!all(
    is.finite(d_val), is.finite(a_mat),
    is.finite(b_vec), is.finite(c_val)
  )) {
    stop_hetid(paste0(
      "Assembled quadratic form contains non-finite values ",
      "(d_i, A_i, b_i, or c_i) for ", label,
      "; check the moments and components for NA/NaN/Inf."
    ))
  }

  list(d = d_val, A = a_mat, b = b_vec, c = c_val)
}
