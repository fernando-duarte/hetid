# I x J separate-instrument identified set.
#
# Instead of contracting the J principal-component instruments into one combined
# instrument per component (the gamma-aggregated baseline), this builds ONE
# quadratic constraint per (component i, instrument j) pair and intersects all
# I*J of them. theta stays in R^I and the output is still I profile intervals;
# only the constraint set differs.
#
# Reuse: setting gamma_i = e_j (the j-th canonical loading) in build_quadratic_system
# reproduces exactly the single-instrument (i, j) constraint -- crossprod(e_j, .)
# selects row/element j of each instrument-indexed moment. So the whole scheme is a
# loop over j with concatenation; no package (R/) changes.
#
# Form: centered correlation. As of the centered-moment refactor, the package
# statistics functions (compute_*_statistics) return centered 1/T covariances and
# variances (see R/statistics_utils.R::centered_cov and the spec sections on moment
# notation and centering), so build_quadratic_system now natively produces the
# literal correlation form |Corr(PC_j, e1 e2_i)| <= tau_ji |Corr(PC_j, e2_i^2)|.
# Centering the S-moments is algebraically identical to the previously-deferred
# centering term d_ij * (mu_i0 - mu_i1' theta)^2 with mu_i0 = mean(W1 * W2_i),
# mu_i1 = colMeans(W2 * W2_i), because
#   theta' S2^c theta - 2 S1^c' theta + S0^c = var(U_i(theta)),
# and centering L/Q/P turns them into true covariances. No script changes are
# needed here -- the e_j reuse inherits the centering automatically.

# J x I gamma whose every column is the j-th canonical basis vector e_j.
make_basis_gamma <- function(j, n_pcs, n_components) {
  g <- matrix(0, nrow = n_pcs, ncol = n_components)
  g[j, ] <- 1
  g
}

# Build the intersection of I*J single-instrument quadratic constraints.
#   moments    : output of compute_identification_moments (position-indexed)
#   tau_matrix : J x I matrix of tolerances tau_ji (row j = instrument j)
# Returns list(quadratic = list(A_i, b_i, c_i, d_i) each of length I*J,
#              labels   = data.frame(constraint, component, instrument)).
build_ixj_quadratic_system <- function(moments, tau_matrix) {
  n_pcs <- nrow(moments$r_i_0)
  n_components <- ncol(moments$r_i_0)

  # The e_j reuse is exact only under the component-indexed (position) convention,
  # i.e. moment column k corresponds to component k. Reject maturity-value-named
  # moments (e.g. maturity_2/5/9), which would misalign gamma[, k] with the data.
  expected <- paste0("maturity_", seq_len(n_components))
  cn <- colnames(moments$r_i_0)
  if (!is.null(cn) && !identical(cn, expected)) {
    stop(
      "build_ixj_quadratic_system requires position-indexed moments ",
      "(columns maturity_1..maturity_I); got: ",
      paste(cn, collapse = ", ")
    )
  }
  if (!is.matrix(tau_matrix) ||
    nrow(tau_matrix) != n_pcs || ncol(tau_matrix) != n_components) {
    stop(sprintf(
      "tau_matrix must be %d x %d (n_pcs x n_components); got %s",
      n_pcs, n_components, paste(dim(tau_matrix), collapse = " x ")
    ))
  }

  total <- n_pcs * n_components
  a_list <- vector("list", total)
  b_list <- vector("list", total)
  c_vec <- numeric(total)
  d_vec <- numeric(total)
  comp_idx <- integer(total)
  inst_idx <- integer(total)

  pos <- 0L
  for (j in seq_len(n_pcs)) {
    gamma_j <- make_basis_gamma(j, n_pcs, n_components)
    qs_j <- suppressMessages(
      build_quadratic_system(gamma_j, tau_matrix[j, ], moments)
    )$quadratic
    for (i in seq_len(n_components)) {
      pos <- pos + 1L
      a_list[[pos]] <- qs_j$A_i[[i]]
      b_list[[pos]] <- qs_j$b_i[[i]]
      c_vec[pos] <- qs_j$c_i[i]
      d_vec[pos] <- qs_j$d_i[i]
      comp_idx[pos] <- i
      inst_idx[pos] <- j
    }
  }

  list(
    quadratic = list(A_i = a_list, b_i = b_list, c_i = c_vec, d_i = d_vec),
    labels = data.frame(
      constraint = seq_len(total),
      component = comp_idx,
      instrument = inst_idx,
      stringsAsFactors = FALSE
    )
  )
}
