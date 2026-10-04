#' Build the Quadratic System for a General Instrument Scheme
#'
#' Extends \code{\link{build_quadratic_system}} to K_i instrument combinations
#' per component i, with one quadratic constraint per combination and the same kernel.
#' A J x I matrix (all K_i = 1) gives bit-identical values to
#' \code{build_quadratic_system}.
#'
#' @param lambda Numeric J x I matrix (one combination per component), or a list of
#'   length \code{n_components}, indexed by system column: constrained entries are
#'   numeric J x K_i weight matrices; unconstrained entries must be NULL.
#'   J counts instruments; I is \code{n_components}. Constrained weights must be finite
#'   and each column nonzero. Unconstrained matrix columns are ignored.
#' @param tau Numeric scalar, numeric vector of length
#'   \code{n_components} (replicated across each component's
#'   combinations), or list of length \code{n_components} whose
#'   constrained element i contains K_i numeric slacks. List elements may
#'   have dimensions; their values are used in R's linear indexing order.
#'   All supplied slacks must be finite and in \code{[0, 1)}. In list form,
#'   unconstrained entries must be NULL or zero-length. A flat vector of
#'   slacks is interpreted by system column when its length is I and as
#'   a common slack when its length is one; other flat lengths are rejected.
#' @param moments A \code{hetid_moments} object from
#'   \code{\link{compute_identification_moments}}.
#'
#' @return A list with elements
#' \describe{
#'   \item{components}{Plain list with per-constraint \code{L_i}, \code{V_i}
#'     (named vectors) and \code{Q_i} (named list). Not a
#'     \code{hetid_components} object, so it cannot be passed to
#'     \code{\link{compute_identified_set_quadratic}}; the per-constraint
#'     axis is longer than the maturity axis whenever any component carries
#'     more than one combination. Each \code{Q_i} vector has length I.}
#'   \item{quadratic}{List with per-constraint \code{d_i}, \code{A_i},
#'     \code{b_i}, \code{c_i}, consumable by the same profile-bound
#'     solvers as \code{build_quadratic_system()}. Each \code{A_i} is I x I and each
#'     \code{b_i} has length I; \code{d_i} and \code{c_i} are numeric vectors.}
#'   \item{labels}{Data frame with columns \code{constraint},
#'     \code{maturity}, \code{combo}, \code{name} mapping constraint
#'     positions to (component, combination) pairs, in \code{maturities}
#'     order then combination order. Names are \code{maturity_N} for a
#'     single combination or \code{maturity_N_combo_K} otherwise.}
#' }
#' carrying the moments' \code{maturities} and \code{n_components}
#' attributes.
#'
#' @details Non-positive or non-finite \code{sigma_i_sq}, non-finite moment
#' values used in assembly, or non-finite coefficients cause a
#' \code{hetid_error}; missing values are not removed. No defaults are supplied.
#'
#' @template section-general-instruments
#' @template section-maturity-convention
#'
#' @export
#'
#' @examples
#' local({
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(42)
#'   n_obs <- 100
#'   w1 <- rnorm(n_obs)
#'   w2 <- matrix(rnorm(n_obs * 2), nrow = n_obs)
#'   z <- matrix(rnorm(n_obs * 3), nrow = n_obs)
#'   moments <- compute_identification_moments(w1, w2, z)
#'
#'   # Two combinations for the first component, one for the second
#'   lambda <- list(
#'     matrix(c(1, 0, 0, 0, 1, 1), nrow = 3),
#'     matrix(c(1, 1, 1), nrow = 3)
#'   )
#'   system <- build_general_quadratic_system(lambda, 0.2, moments)
#'   system$labels
#' })
build_general_quadratic_system <- function(lambda, tau, moments) {
  validate_hetid_moments(moments)
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
  # Labels frame drives the loop; labels and constraints cannot drift apart
  idx_vec <- match(label_df$maturity, maturities)
  rows <- lapply(seq_len(nrow(label_df)), function(pos) {
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
      label = label_df$name[pos]
    )
    c(parts, quad)
  })
  names(rows) <- label_df$name
  num <- function(field) vapply(rows, `[[`, 0, field)
  lst <- function(field) lapply(rows, `[[`, field)

  structure(
    list(
      components = list(L_i = num("L"), V_i = num("V"), Q_i = lst("Q")),
      quadratic = list(d_i = num("d"), A_i = lst("A"), b_i = lst("b"), c_i = num("c")),
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
#' @param moments A \code{hetid_moments} object from
#'   \code{\link{compute_identification_moments}}.
#' @return List of length \code{n_components}; identity J x J matrix
#'   at constrained columns, NULL elsewhere. Rows retain the instrument
#'   names from \code{moments}; list positions follow system column indices.
#'
#' @template section-general-instruments
#'
#' @export
#'
#' @examples
#' local({
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(42)
#'   w1 <- rnorm(50)
#'   w2 <- matrix(rnorm(100), nrow = 50)
#'   z <- matrix(rnorm(150), nrow = 50)
#'   moments <- compute_identification_moments(w1, w2, z)
#'   lambda <- separate_instruments_lambda(moments)
#'   system <- build_general_quadratic_system(lambda, 0.2, moments)
#'   nrow(system$labels)
#' })
separate_instruments_lambda <- function(moments) {
  validate_hetid_moments(moments)
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
