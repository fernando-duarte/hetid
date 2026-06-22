#' Build the Quadratic System for a General Instrument Scheme
#'
#' Generalizes \code{\link{build_quadratic_system}} from one linear
#' combination per component to K_i combinations per component i: one
#' quadratic constraint per (component, combination) pair, sharing the
#' same per-constraint kernel as the legacy path. With a J x I matrix
#' input (all K_i = 1) the output values are bit-identical to
#' \code{build_quadratic_system}.
#'
#' @param lambda Either a numeric J x I matrix (one combination per
#'   component; legacy-equivalent) or a list of length
#'   \code{n_components} indexed by system column with a numeric
#'   J x K_i weight matrix at every constrained column (NULL required
#'   at unconstrained columns). All-zero weight columns are rejected.
#' @param tau Scalar in [0, 1), numeric vector of length
#'   \code{n_components} (replicated across each component's
#'   combinations), or list of length \code{n_components} whose
#'   element i is a length-K_i numeric vector of slacks
#' @param moments A \code{hetid_moments} object from
#'   \code{\link{compute_identification_moments}}
#'
#' @return A list with elements
#' \describe{
#'   \item{components}{List with per-constraint \code{L_i}, \code{V_i}
#'     (named vectors) and \code{Q_i} (named list)}
#'   \item{quadratic}{List with per-constraint \code{d_i}, \code{A_i},
#'     \code{b_i}, \code{c_i}, consumable by the same profile-bound
#'     solvers as the legacy output}
#'   \item{labels}{Data frame with columns \code{constraint},
#'     \code{maturity}, \code{combo}, \code{name} mapping constraint
#'     positions to (component, combination) pairs}
#' }
#' carrying the moments' \code{maturities} and \code{n_components}
#' attributes.
#'
#' @template section-general-instruments
#' @template section-maturity-convention
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' n_obs <- 100
#' w1 <- rnorm(n_obs)
#' w2 <- matrix(rnorm(n_obs * 2), nrow = n_obs)
#' z <- matrix(rnorm(n_obs * 3), nrow = n_obs)
#' moments <- compute_identification_moments(w1, w2, z)
#'
#' # Two combinations for the first component, one for the second
#' lambda <- list(
#'   matrix(c(1, 0, 0, 0, 1, 1), nrow = 3),
#'   matrix(c(1, 1, 1), nrow = 3)
#' )
#' system <- build_general_quadratic_system(lambda, 0.2, moments)
#' system$labels
build_general_quadratic_system <- function(lambda, tau, moments) {
  assert_hetid_moments(moments)
  lambda_list <- as_lambda_list(lambda, moments)
  tau_list <- as_tau_list(tau, lambda_list, moments)

  maturities <- attr(moments, "maturities")
  n_components <- attr(moments, "n_components")
  assert_sigma_positive(moments$sigma_i_sq, maturities)
  validate_finite_by_maturity(
    list(
      s_i_0 = moments$s_i_0, s_i_1 = moments$s_i_1,
      s_i_2 = moments$s_i_2
    ),
    maturities
  )

  label_df <- general_constraint_labels(lambda_list, maturities)
  n_total <- nrow(label_df)

  L_i <- numeric(n_total) # nolint: object_name_linter.
  V_i <- numeric(n_total) # nolint: object_name_linter.
  Q_i <- vector("list", n_total) # nolint: object_name_linter.
  d_i <- numeric(n_total)
  A_i <- vector("list", n_total) # nolint: object_name_linter.
  b_i <- vector("list", n_total)
  c_i <- numeric(n_total)

  # Labels frame drives the loop; labels and constraints cannot drift apart
  nms <- label_df$name
  idx_vec <- match(label_df$maturity, maturities)
  for (pos in seq_len(n_total)) {
    i <- label_df$maturity[pos]
    k <- label_df$combo[pos]
    idx <- idx_vec[pos]
    parts <- constraint_components(
      lambda_list[[i]][, k, drop = FALSE], idx, moments
    )
    quad <- assemble_constraint_quadratic(
      tau_ik = tau_list[[i]][k],
      l_val = parts$L, v_val = parts$V, q_vec = parts$Q,
      s_i_0_val = moments$s_i_0[idx],
      s_i_1_vec = moments$s_i_1[[idx]],
      s_i_2_mat = moments$s_i_2[[idx]],
      sigma_i_sq_val = moments$sigma_i_sq[idx],
      n_components = n_components,
      label = nms[pos]
    )
    L_i[pos] <- parts$L # nolint: object_name_linter.
    V_i[pos] <- parts$V # nolint: object_name_linter.
    Q_i[[pos]] <- parts$Q # nolint: object_name_linter.
    d_i[pos] <- quad$d
    A_i[[pos]] <- quad$A # nolint: object_name_linter.
    b_i[[pos]] <- quad$b
    c_i[pos] <- quad$c
  }
  names(L_i) <- nms # nolint: object_name_linter.
  names(V_i) <- nms # nolint: object_name_linter.
  names(Q_i) <- nms # nolint: object_name_linter.
  names(d_i) <- nms
  names(A_i) <- nms # nolint: object_name_linter.
  names(b_i) <- nms
  names(c_i) <- nms

  structure(
    list(
      components = list(L_i = L_i, V_i = V_i, Q_i = Q_i),
      quadratic = list(d_i = d_i, A_i = A_i, b_i = b_i, c_i = c_i),
      labels = label_df
    ),
    maturities = maturities,
    n_components = n_components
  )
}

#' Weights That Use Every Instrument Separately
#'
#' Returns the lambda list whose every constrained component carries
#' the identity weight matrix: each of the J instrument columns is its
#' own constructed instrument (the all-instruments scheme; J x I
#' constraints when all components are constrained). Feed the result
#' to \code{\link{build_general_quadratic_system}}.
#'
#' @param moments A \code{hetid_moments} object
#' @return List of length \code{n_components}; identity J x J matrix
#'   at constrained columns, NULL elsewhere
#'
#' @template section-general-instruments
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' w1 <- rnorm(50)
#' w2 <- matrix(rnorm(100), nrow = 50)
#' z <- matrix(rnorm(150), nrow = 50)
#' moments <- compute_identification_moments(w1, w2, z)
#' lambda <- separate_instruments_lambda(moments)
#' system <- build_general_quadratic_system(lambda, 0.2, moments)
#' nrow(system$labels)
separate_instruments_lambda <- function(moments) {
  assert_hetid_moments(moments)
  j_rows <- nrow(moments$r_i_0)
  n_components <- attr(moments, "n_components")
  maturities <- attr(moments, "maturities")
  basis <- diag(j_rows)
  rownames(basis) <- rownames(moments$r_i_0)
  out <- vector("list", n_components)
  for (i in maturities) {
    out[[i]] <- basis
  }
  out
}
