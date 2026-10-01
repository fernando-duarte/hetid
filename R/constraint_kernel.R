#' Per-Constraint Components from One Weight Column
#'
#' Internal kernel holding the L/V/Q arithmetic for a single weight
#' column applied to the moments of one constraint maturity. The
#' single-combination and general (component, combination) schemes share this
#' one implementation and must produce bit-identical values (enforced by
#' \code{test-build_general_quadratic_system.R}). Preserve operation order
#' to retain numerical equivalence.
#'
#' Inputs are validated by the callers. This kernel does not validate
#' dimensions or remove missing values.
#'
#' @param weight_col Numeric J x 1 matrix (a single weight column),
#'   where J is the number of instruments.
#' @param idx Integer position of the maturity within the container's
#'   \code{maturities} vector.
#' @param moments A validated \code{hetid_moments} object.
#' @return List with numeric scalars \code{L} and \code{V}, and a numeric
#'   length-I vector \code{Q}, where I is the theta-axis dimension.
#'   The vector is named \code{maturity_N} for system columns N = 1, ..., I.
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
#' guards. The single-combination and general schemes must produce bit-identical
#' values (enforced by \code{test-build_general_quadratic_system.R}). Operation
#' order must be preserved to retain numerical equivalence.
#'
#' Inputs have compatible dimensions and are validated by the callers.
#' Non-finite \code{d} or assembled coefficients raise a structured
#' \code{hetid_error}; missing values are not removed.
#'
#' @param tau_ik Validated numeric scalar slack in \code{[0, 1)} for this constraint.
#' @param l_val,v_val Numeric scalars from \code{constraint_components()}.
#' @param q_vec Numeric length-I vector from \code{constraint_components()}.
#' @param s_i_0_val Numeric scalar moment for this maturity.
#' @param s_i_1_vec Numeric length-I moment vector for this maturity.
#' @param s_i_2_mat Numeric I x I moment matrix for this maturity.
#' @param sigma_i_sq_val Finite, strictly positive numeric scalar moment for this maturity.
#' @param n_components Positive integer theta-axis dimension (I).
#' @param label Character scalar constraint label used in error messages.
#' @return List with numeric scalars \code{d} and \code{c}, a symmetric I x I
#'   numeric matrix \code{A}, and a numeric length-I vector \code{b}
#'   named \code{maturity_N} for system columns N = 1, ..., I.
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

  a_mat <- (a_mat + t(a_mat)) / 2

  b_vec <- -2 * l_val * q_vec +
    2 * d_val * s_i_1_vec
  names(b_vec) <- maturity_names(seq_len(n_components))

  c_val <- l_val^2 - d_val * s_i_0_val

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
