# Variance shares of consumption growth for the structural equation of
# set_id_mean_eq.R: the share of Var(Delta c_{t+1}) attributable to the
# expected-SDF block PC_{E,t}' b_E and the SDF-news block PC_{N,t+1}' b_N,
# per block and per component, at the OLS coefficients, the closed-form
# Lewbel point at tau = 0, and as ranges over the joint identified set at
# each tau_display slack. Leaves the var_share result list for
# var_share_table.R (row order: E block, E components, news block, news
# components).
# Run via run_all.R after set_id_mean_eq.R.

# centered 1/T second moments on the estimation sample, the identification
# moments' centering convention
centered_cov_t <- function(mat) {
  mat <- as.matrix(mat)
  crossprod(sweep(mat, 2, colMeans(mat))) / nrow(mat)
}
s_e <- centered_cov_t(set_id_mean_eq$data[set_id_mean_eq$x_cols])
s_n <- centered_cov_t(set_id_mean_eq$y2)
var_c <- drop(centered_cov_t(set_id_mean_eq$y1))

# percent share of Var(dc) of a block; rows of coefs are candidate vectors
block_share <- function(coefs, s_block) {
  100 * rowSums((coefs %*% s_block) * coefs) / var_c
}

# each block's share is a quadratic theta' P theta + q' theta + r in theta,
# in percent of Var(dc); the news block directly, the E block through
# b_E(theta) = beta1R_E - (beta2R_E)' theta (constant under the null)
share_quad <- function(p_mat, q_vec, r_val) {
  list(
    value = function(pts) {
      rowSums((pts %*% p_mat) * pts) + drop(pts %*% q_vec) + r_val
    },
    grad = function(th) drop(2 * (p_mat %*% th) + q_vec)
  )
}
beta1r_e <- set_id_mean_eq$beta1r[set_id_mean_eq$x_cols]
beta2r_e <- set_id_mean_eq$beta2r[, set_id_mean_eq$x_cols, drop = FALSE]
b_map <- t(beta2r_e) # p x I: b_E(theta) = beta1r_e - b_map %*% theta
news_quad_share <- share_quad(100 * s_n / var_c, numeric(n_pc), 0)
e_quad_share <- share_quad(
  100 * crossprod(b_map, s_e %*% b_map) / var_c,
  drop(-200 * beta2r_e %*% (s_e %*% beta1r_e)) / var_c,
  100 * drop(beta1r_e %*% s_e %*% beta1r_e) / var_c
)

# joint quadratic constraints g_i(theta) <= 0 at each row of pts (n x I)
constraint_vals <- function(pts, quad) {
  vals <- vapply(seq_along(quad$A_i), function(i) {
    rowSums((pts %*% quad$A_i[[i]]) * pts) +
      drop(pts %*% quad$b_i[[i]]) + quad$c_i[[i]]
  }, numeric(nrow(pts)))
  matrix(vals, nrow = nrow(pts))
}
constraint_jac <- function(th, quad) {
  t(vapply(
    seq_along(quad$A_i),
    \(i) drop(2 * (quad$A_i[[i]] %*% th) + quad$b_i[[i]]),
    numeric(length(th))
  ))
}

# one SLSQP descent of sign_mult * share from a feasible x0, run in the
# shared solver's scaling (theta = delta * phi, constraints normalized by
# omega -- the profile_bounds_core.R internals already in scope via
# set_id_mean_eq.R, the same pattern as logvar_polish_bound); returns the
# share at the solution when it stays feasible, NA otherwise. A nonlinear
# extremum can sit strictly inside the set, so the certificate is
# feasibility only, not feasibility + activity.
polish_extreme <- function(x0, quad, sq, box, sign_mult, delta, omega) {
  res <- tryCatch(
    nloptr::slsqp(
      x0 = x0 / delta,
      fn = function(phi) sign_mult * sq$value(matrix(delta * phi, 1)),
      gr = function(phi) sign_mult * delta * sq$grad(delta * phi),
      lower = box$set_lower / delta, upper = box$set_upper / delta,
      hin = function(phi) {
        drop(constraint_vals(matrix(delta * phi, 1), quad)) / omega
      },
      hinjac = function(phi) {
        constraint_jac(delta * phi, quad) * (delta / omega)
      },
      control = list(xtol_rel = 1e-8, maxeval = 1000),
      deprecatedBehavior = FALSE
    ),
    error = function(e) list(par = rep(NA_real_, length(x0)))
  )
  if (!all(is.finite(res$par))) {
    return(NA_real_)
  }
  th <- pmin(pmax(delta * res$par, box$set_lower), box$set_upper)
  resid <- .feasibility_residual(quad, th, omega)
  if (is.finite(resid) && resid <= 1e-4) sq$value(matrix(th, 1)) else NA_real_
}

# min and max of a block share over the joint set. The share is convex in
# theta, so its maximum sits on the set boundary -- near tau* in a thin
# corner sliver that a fixed grid misses. A feasibility grid over the exact
# per-coefficient box seeds a multistart SLSQP polish (share extremes and
# per-coordinate extremes of the grid); the grid extremes are kept as the
# feasible fallback envelope, so a failed polish can only lose precision,
# never validity.
set_share_range <- function(box, quad, sq) {
  if (any(!is.finite(c(box$set_lower, box$set_upper)))) {
    return(c(NA_real_, NA_real_))
  }
  delta <- .derive_theta_scale(quad)
  omega <- .derive_constraint_scales(quad, delta)
  axes <- Map(
    \(l, h) seq(l, h, length.out = 101L), box$set_lower, box$set_upper
  )
  pts <- as.matrix(expand.grid(axes))
  feas <- rowSums(sweep(constraint_vals(pts, quad), 2, 1e-10 * omega, `>`)) == 0
  pts <- pts[feas, , drop = FALSE]
  stopifnot("no feasible grid point in the box" = nrow(pts) > 0)
  vals <- sq$value(pts)
  starts <- pts[unique(c(
    which.min(vals), which.max(vals),
    apply(pts, 2, which.min), apply(pts, 2, which.max)
  )), , drop = FALSE]
  polished <- function(sign_mult) {
    cand <- apply(
      starts, 1,
      \(x0) polish_extreme(x0, quad, sq, box, sign_mult, delta, omega)
    )
    cand[is.finite(cand)]
  }
  c(min(vals, polished(1)), max(vals, polished(-1)))
}

# exact share range of one component over the joint set: the image of the
# exact per-coefficient range under x -> 100 x^2 Var(PC_k)/Var(dc)
component_share_range <- function(tab, s_block) {
  scale <- 100 * diag(s_block) / var_c
  lo <- ifelse(
    tab$set_lower <= 0 & tab$set_upper >= 0, 0,
    pmin(tab$set_lower^2, tab$set_upper^2)
  )
  data.frame(
    lo = scale * lo,
    hi = scale * pmax(tab$set_lower^2, tab$set_upper^2)
  )
}

# row-order guards, as in structural_eq_table.R
stopifnot(
  identical(
    set_id_mean_eq$theta_table$coef, paste0("sdf_news_pc", seq_len(n_pc))
  ),
  identical(
    set_id_mean_eq$beta1_table$coef,
    c("(Intercept)", paste0("lag_expected_sdf_pc", seq_len(n_pc)))
  )
)
e_rows <- match(set_id_mean_eq$x_cols, set_id_mean_eq$beta1_table$coef)

# share rows (E block, E components, news block, news components) at one
# fixed coefficient vector: the OLS fit or the closed-form tau = 0 point
fixed_shares <- function(field) {
  b_n <- set_id_mean_eq$theta_table[[field]]
  b_e <- set_id_mean_eq$beta1_table[[field]][e_rows]
  c(
    block_share(matrix(b_e, 1), s_e), 100 * b_e^2 * diag(s_e) / var_c,
    block_share(matrix(b_n, 1), s_n), 100 * b_n^2 * diag(s_n) / var_c
  )
}

# share ranges over the joint identified set at each display slack
quads <- lapply(set_id_mean_eq$tau_display, function(tau) {
  hetid::build_quadratic_system(
    set_id_mean_eq$gamma, rep(tau, ncol(set_id_mean_eq$gamma)),
    set_id_mean_eq$moments
  )$quadratic
})
# under the maintained null b_E does not vary over the set, so its share is
# the constant at beta1R and needs no optimizer; otherwise it ranges over
# the same joint set as the news block
e_const <- block_share(matrix(beta1r_e, 1), s_e)
set_share_cols <- Map(function(nm, quad) {
  st <- set_id_mean_eq$set_tables[[nm]]
  e_rng <- if (impose_beta2r_null) {
    rep(e_const, 2)
  } else {
    set_share_range(st$theta, quad, e_quad_share)
  }
  news_rng <- set_share_range(st$theta, quad, news_quad_share)
  e_comp <- component_share_range(st$beta1[e_rows, ], s_e)
  n_comp <- component_share_range(st$theta, s_n)
  list(
    lo = c(e_rng[1], e_comp$lo, news_rng[1], n_comp$lo),
    hi = c(e_rng[2], e_comp$hi, news_rng[2], n_comp$hi)
  )
}, names(set_id_mean_eq$set_tables), quads)

# coherence of the polished block ranges against the exact component ranges:
# a block share dominates each of its component shares pointwise (up to the
# PCs' tiny sample cross-covariances), so the block max must reach the
# largest component max and the block min the largest component min
news_row <- n_pc + 2L
for (cc in set_share_cols) {
  for (block_row in c(1L, news_row)) {
    comps <- block_row + seq_len(n_pc)
    if (all(is.finite(c(cc$lo[c(block_row, comps)], cc$hi[c(block_row, comps)])))) {
      stopifnot(
        "block share max below a component max" =
          cc$hi[block_row] >= 0.98 * max(cc$hi[comps]),
        "block share min below a component min" =
          cc$lo[block_row] >= 0.98 * max(cc$lo[comps]) - 1e-9
      )
    }
  }
}

var_share <- list(
  ols = fixed_shares("ols"),
  point = fixed_shares("point"),
  set_cols = set_share_cols,
  news_row = news_row,
  sd_c = sqrt(var_c)
)

cat(sprintf(
  "variance shares: news block %.2f--%.2f%% of Var(dc) at tau = %.2g\n",
  var_share$set_cols[[1]]$lo[news_row], var_share$set_cols[[1]]$hi[news_row],
  set_id_mean_eq$tau_baseline
))

rm(
  centered_cov_t, s_e, s_n, var_c, block_share, share_quad, beta1r_e,
  beta2r_e, b_map, news_quad_share, e_quad_share, e_const, constraint_vals,
  constraint_jac, polish_extreme, set_share_range, component_share_range,
  e_rows, fixed_shares, quads, set_share_cols, news_row, cc, block_row, comps
)
