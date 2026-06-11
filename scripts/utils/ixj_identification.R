# I x J separate-instrument identified set.
#
# Instead of contracting the J principal-component instruments into one combined
# instrument per component (the gamma-aggregated baseline), this builds ONE
# quadratic constraint per (component i, instrument j) pair and intersects all
# I*J of them. theta stays in R^I and the output is still I profile intervals;
# only the constraint set differs.
#
# Design: thin wrapper over the package's general-instrument machinery.
# hetid::separate_instruments_lambda supplies identity weights (column j = e_j),
# so hetid::build_general_quadratic_system emits exactly the single-instrument
# (i, j) constraints through the same shared per-constraint kernel the old
# hand-rolled e_j loop reached via build_quadratic_system -- bit-identical
# values, no package (R/) changes. The general builder orders constraints
# component-major; this wrapper permutes them back to the legacy
# instrument-major order (j outer, i inner) because the downstream SLSQP solve
# order and the tolerance-0 gated CSVs depend on it.
#
# Form: centered correlation. As of the centered-moment refactor, the package
# statistics functions (compute_*_statistics) return centered 1/T covariances and
# variances (see R/statistics_utils.R::centered_cov and the spec sections on moment
# notation and centering), so the builder natively produces the literal
# correlation form |Corr(PC_j, e1 e2_i)| <= tau_ji |Corr(PC_j, e2_i^2)|.
# Centering the S-moments is algebraically identical to the previously-deferred
# centering term d_ij * (mu_i0 - mu_i1' theta)^2 with mu_i0 = mean(W1 * W2_i),
# mu_i1 = colMeans(W2 * W2_i), because
#   theta' S2^c theta - 2 S1^c' theta + S0^c = var(U_i(theta)),
# and centering L/Q/P turns them into true covariances. No script changes are
# needed here -- the identity-lambda columns are the same e_j and inherit the
# centering automatically.

# J x I gamma whose every column is the j-th canonical basis vector e_j.
make_basis_gamma <- function(j, n_pcs, n_components) {
  g <- matrix(0, nrow = n_pcs, ncol = n_components)
  g[j, ] <- 1
  g
}

# Build the intersection of I*J single-instrument quadratic constraints.
#   moments    : hetid_moments container from compute_identification_moments
#   tau_matrix : J x I matrix of tolerances tau_ji (row j = instrument j)
# Returns list(quadratic = list(A_i, b_i, c_i, d_i) each of length I*J,
#              labels   = data.frame(constraint, component, instrument)).
build_ixj_quadratic_system <- function(moments, tau_matrix) {
  n_pcs <- nrow(moments$r_i_0)
  n_components <- attr(moments, "n_components")

  # The legacy ixj contract pairs each constraint with its system column
  # (labels report component == column index), so the container's constraint
  # axis must cover the full system (maturities == 1..n_components).
  if (!identical(attr(moments, "maturities"), seq_len(n_components))) {
    stop(
      "build_ixj_quadratic_system requires a full-system moments container ",
      "(maturities 1..n_components); got maturities: ",
      paste(attr(moments, "maturities"), collapse = ", ")
    )
  }
  if (!is.matrix(tau_matrix) ||
    nrow(tau_matrix) != n_pcs || ncol(tau_matrix) != n_components) {
    stop(sprintf(
      "tau_matrix must be %d x %d (n_pcs x n_components); got %s",
      n_pcs, n_components, paste(dim(tau_matrix), collapse = " x ")
    ))
  }

  lambda <- hetid::separate_instruments_lambda(moments)
  tau_list <- lapply(
    seq_len(n_components), function(i) tau_matrix[, i]
  )
  qs <- hetid::build_general_quadratic_system(lambda, tau_list, moments)

  # The general builder emits constraints component-major (i, then
  # k = j); the legacy ixj order is instrument-major (j, then i) and
  # the downstream SLSQP solve order plus gated CSVs depend on it.
  # Permute back: pure reindexing of already-computed objects.
  gen <- qs$labels
  legacy_pos <- order(gen$combo, gen$maturity)

  list(
    quadratic = list(
      A_i = unname(qs$quadratic$A_i[legacy_pos]),
      b_i = unname(qs$quadratic$b_i[legacy_pos]),
      c_i = unname(qs$quadratic$c_i[legacy_pos]),
      d_i = unname(qs$quadratic$d_i[legacy_pos])
    ),
    labels = data.frame(
      constraint = seq_len(nrow(gen)),
      component = gen$maturity[legacy_pos],
      instrument = gen$combo[legacy_pos],
      stringsAsFactors = FALSE
    )
  )
}
