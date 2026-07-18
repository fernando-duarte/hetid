# Representation-stable residualized-z basis for the joint moment-compatibility
# layer (joint-GMM, logvar-joint-gmm, Option A). Residualize z on the static regressors
# X (A = M_X z), assess numerical rank from the column-normalized A under a pinned
# singular-value gap, and grow a canonical basis from the column-space projector
# P_A = U_q U_q' alone. Because P_A depends only on col(A), the basis is invariant to
# well-conditioned right transforms of z (scale, sign, permutation, mixing); a
# numerically ambiguous rank gap is reported "unreliable" rather than a basis choice.
# Definitions only; sourced by the joint-GMM test entrypoint and replication driver.

# Grow the column-space basis from the projector alone: scan canonical axes e_i in
# increasing row order, take the projected axis P_A e_i (column i of P_A), strip its
# components along the accepted vectors by modified Gram-Schmidt, and accept the residual
# when its norm clears the pinned axis tolerance. Each accepted vector is normalized and
# oriented so its first nonvanishing element is positive, which makes the basis a
# deterministic, sign-canonical function of the projector rather than of the SVD.
logvar_project_canonical_basis <- function(
  projector,
  q,
  axis_tol = logvar_joint_gmm_constants$basis_axis_tol
) {
  n <- nrow(projector)
  basis <- matrix(0, n, q)
  axes <- integer(0)
  filled <- 0L
  for (i in seq_len(n)) {
    if (filled == q) break
    v <- projector[, i]
    for (j in seq_len(filled)) {
      u <- basis[, j]
      v <- v - sum(u * v) * u
    }
    residual_norm <- sqrt(sum(v^2))
    if (residual_norm > axis_tol) {
      v <- v / residual_norm
      lead <- which(abs(v) > axis_tol)[1L]
      if (!is.na(lead) && v[lead] < 0) v <- -v
      filled <- filled + 1L
      basis[, filled] <- v
      axes <- c(axes, i)
    }
  }
  list(basis = basis[, seq_len(filled), drop = FALSE], axes = axes)
}

# Residualize z on the static variance regressors and return the projector-derived basis
# with its rank, singular values, and reliability status. A column whose residual is a
# negligible fraction of the original column is treated as lying in span(X); when none
# survive the instrument is all-redundant (rank 0, unreliable). Column-normalizing A only
# for rank assessment preserves col(A), so the retained projector and the basis are stable
# under well-conditioned right transforms of z. A retained/discarded singular-value gap
# that is not clearly separated is reported "unreliable".
logvar_residualize_moment_basis <- function(
  z,
  x_mat,
  basis_rank_tol = logvar_joint_gmm_constants$basis_rank_tol
) {
  z <- as.matrix(z)
  n <- nrow(z)
  coef <- solve(crossprod(x_mat), crossprod(x_mat, z))
  a_mat <- z - x_mat %*% coef
  z_norms <- sqrt(colSums(z^2))
  a_norms <- sqrt(colSums(a_mat^2))
  survives <- a_norms > basis_rank_tol * z_norms
  if (!any(survives)) {
    return(list(
      basis = matrix(numeric(0), n, 0L), rank = 0L, singular_values = numeric(0),
      status = "unreliable", input_col_norms = a_norms, projector_residual = 0,
      axes = integer(0)
    ))
  }
  a_unit <- sweep(a_mat[, survives, drop = FALSE], 2L, a_norms[survives], "/")
  decomp <- svd(a_unit)
  sv <- decomp$d
  s_max <- sv[1L]
  if (!is.finite(s_max) || s_max <= 0) {
    return(list(
      basis = matrix(numeric(0), n, 0L), rank = 0L, singular_values = sv,
      status = "unreliable", input_col_norms = a_norms, projector_residual = 0,
      axes = integer(0)
    ))
  }
  cutoff <- basis_rank_tol * s_max
  q <- sum(sv > cutoff)
  first_discarded <- if (q < length(sv)) sv[q + 1L] else 0
  retained_gap <- logvar_joint_gmm_constants$basis_retained_gap_factor
  discarded_gap <- logvar_joint_gmm_constants$basis_discarded_gap_factor
  clear_gap <- sv[q] > retained_gap * cutoff &&
    (q == length(sv) || first_discarded < discarded_gap * cutoff)
  u_q <- decomp$u[, seq_len(q), drop = FALSE]
  projector <- u_q %*% t(u_q)
  built <- logvar_project_canonical_basis(projector, q)
  list(
    basis = built$basis, rank = q, singular_values = sv,
    status = if (clear_gap) "ok" else "unreliable", input_col_norms = a_norms,
    projector_residual = max(abs(projector %*% a_unit - a_unit)), axes = built$axes
  )
}
