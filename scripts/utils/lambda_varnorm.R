# Variance Normalization -- the repo's DEFAULT normalization for
# optimized instrument weights: every returned combination column of
# run_lambda_optimization satisfies lambda' Vhat lambda = 1, the
# spec's first canonical normalization F_i
# (docs/lewbel_multivariate_set_identification.tex line 479), with
# Vhat the centered 1/T instrument covariance carried by
# whiten_context (hetid:::centered_cov under whiten = list(z = .)).
# By the c^2 direction-invariance (spec lines 366-372) the
# normalization changes NO identified set: it selects the reported
# representative and excludes lambda = 0. All helpers operate on the
# whiten_context() output (wctx); quadratic forms and scalings use
# the per-component support rows, so off-support entries stay exact
# +0.0 (dividing +0.0 by a positive scalar is +0.0).

# Relative floor for the admissibility check. References the whitening
# eigenvalue gate (WHITEN_EIGEN_RTOL, sourced first) so the two stay equal by
# construction: the sub-block PD gate already guarantees
# lambda' V lambda >= min_eig * ||lambda||^2 for any nonzero lambda, so this
# check fires only for genuinely inadmissible (zero or denormal) start columns
# -- the F_i exclusion of lambda = 0 that the to-do asked to enforce.
VARNORM_ZERO_RTOL <- WHITEN_EIGEN_RTOL

# Per-column lambda' V lambda of one component's weight matrix,
# computed on its support rows. The colSums(sub * (block %*% sub))
# form is chosen so an identity V reduces BITWISE to the Euclidean
# column norms FOR ALL-FINITE weights (I %*% sub is exact, x * x is
# the same operation R uses for x^2); the non-finite guard in the
# normalizer below differs from the legacy zero-only guard, but
# finiteness holds wherever the bitwise pin applies -- the inner
# objective is everywhere finite (steering penalty), so slsqp
# terminal iterates are finite given finite starts, and start
# finiteness is enforced by the builder validators on first
# evaluation. Pinned by the identity-covariance test in
# test_lambda_whitening.R.
lambda_vcov_quadform <- function(el, rows_i, vcov) {
  block <- vcov[rows_i, rows_i, drop = FALSE]
  sub <- el[rows_i, , drop = FALSE]
  colSums(sub * (block %*% sub))
}

# Pre-optimization zero check (structured, BEFORE any RNG use): no
# start column may carry (numerically) zero instrument variance.
# Scale-free: rel = q / (||lambda_s||^2 * mean(diag(block))); a zero
# column gives 0/0 -> NaN and fails is.finite().
assert_lambda_variance_nonzero <- function(lambda_list, wctx) {
  for (i in seq_along(lambda_list)) {
    el <- lambda_list[[i]]
    if (is.null(el) || is.null(wctx$rows[[i]])) next
    rows_i <- wctx$rows[[i]]
    q <- lambda_vcov_quadform(el, rows_i, wctx$vcov)
    block_scale <- mean(diag(wctx$vcov)[rows_i])
    rel <- q / (colSums(el[rows_i, , drop = FALSE]^2) * block_scale)
    hetid:::assert_bad_argument_ok(
      all(is.finite(rel)) && all(rel > VARNORM_ZERO_RTOL),
      paste0(
        "lambda_start[[", i, "]] has a column with (numerically) ",
        "zero instrument variance lambda' V lambda; the variance ",
        "normalization excludes lambda = 0 -- drop or replace the ",
        "degenerate start column"
      ),
      arg = "lambda_start"
    )
  }
  invisible(NULL)
}

# Variance-normalize each combination column: divide by
# sqrt(lambda' V lambda) on the support rows. Columns whose quadratic
# form is not a positive finite number are returned UNSCALED (the
# vcov analog of normalize_gamma_columns' zero-column guard): they
# belong to honest-Inf runs and must not become NaN.
normalize_lambda_columns_vcov <- function(lambda_list, wctx) {
  for (i in seq_along(lambda_list)) {
    el <- lambda_list[[i]]
    if (is.null(el) || is.null(wctx$rows[[i]])) next
    scale <- sqrt(lambda_vcov_quadform(el, wctx$rows[[i]], wctx$vcov))
    scale[!is.finite(scale) | scale <= 0] <- 1
    lambda_list[[i]] <- sweep(el, 2, scale, FUN = "/")
  }
  lambda_list
}

# Identification-strength diagnostic (the to-do's "(or diagnostic for
# identification?)"): per-column lambda' V lambda of the
# unit-Euclidean representative -- the sample variance of the
# combined instrument z' lambda for a unit-norm direction. Values
# near the smallest eigenvalue of the component's V sub-block flag a
# near-degenerate weight direction.
lambda_variance_diagnostic <- function(lambda_list, wctx) {
  out <- vector("list", length(lambda_list))
  for (i in seq_along(lambda_list)) {
    el <- lambda_list[[i]]
    if (is.null(el) || is.null(wctx$rows[[i]])) next
    out[[i]] <- lambda_vcov_quadform(
      normalize_gamma_columns(el), wctx$rows[[i]], wctx$vcov
    )
  }
  out
}

# Bundle the start/optimized diagnostics for the optimizer's return
# value (kept here so lambda_optimization.R stays under the 200-line
# rule).
lambda_variance_report <- function(lambda_start, lambda_opt, wctx) {
  list(
    start = lambda_variance_diagnostic(lambda_start, wctx),
    optimized = lambda_variance_diagnostic(lambda_opt, wctx)
  )
}
