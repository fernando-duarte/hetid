# Optimization Utilities -- shared pieces of the outer weight
# optimization. The legacy single-combination Euclidean gamma
# optimizer is RETIRED: run_lambda_optimization in
# lambda_optimization.R is the single outer optimizer, and the repo
# default normalization for optimized weights is
# lambda' Var(Z) lambda = 1 (lambda_varnorm.R). What remains here is
# the total-width objective, the Euclidean column normalizer (the
# search-space representative inside the optimizer and the cosmetic
# display convention for reduced-form gammas in factor_utils.R), and
# the inner steering penalty. The inner profile-bounds solver lives
# in profile_bounds.R (sourced via common_settings.R).

compute_total_width <- function(bounds_tbl) {
  sum(bounds_tbl$width)
}

normalize_gamma_columns <- function(gamma) {
  col_norms <- sqrt(colSums(gamma^2))
  col_norms[col_norms == 0] <- 1
  sweep(gamma, 2, col_norms, FUN = "/")
}

# Steering penalty for the INNER slsqp objective only. It must be
# finite and sit far above any genuine total width, but selection and
# reporting NEVER read it: run_lambda_optimization re-evaluates every
# terminal point with honest_width_lambda, so a real bounded width
# can never lose to (or be mistaken for) the penalty.
UNBOUNDED_PENALTY <- 1e12
