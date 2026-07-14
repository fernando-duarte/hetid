# Grid scan, multi-start polish, and nesting carry for the joint-null
# theta_R = 0 distance diagnostic. The objective q(b) = 0.5 ||D^-1 s(b)||^2 (the
# slope block of the log-variance map under the frozen sd-effect scales) is
# minimized over the joint Lewbel set: a vectorized feasible-grid scan seeds a
# multi-start SLSQP polish through logvar_polish_objective, and every prior-tau
# arg-min is carried in as an attained-candidate floor so the reported minimum
# is monotone across nested taus. logvar_joint_null_objective and _gradient (the
# math module) supply the map and its slope gradient; logvar_feasible_grid (the
# map module) the joint lattice; .derive_theta_scale, .derive_constraint_scales,
# and .feasibility_residual (profile_bounds_core.R) the constraint scaling.
# Definitions only; sourced by log_var_eq_joint_null.R and the joint-null tests.
# The candidate-evaluation helper and the L-infinity epigraph sensitivity live in
# the companion file, sourced at the end.

# constraint normalization omega_i, reused by every feasibility check here
.jn_omega <- function(qs) {
  .derive_constraint_scales(qs, .derive_theta_scale(qs))
}

# available feasible-grid points as tagged candidates; unavailable map rows (an
# exact-zero residual, hence NA objective) are dropped and counted separately
.jn_grid_candidates <- function(grid, gobj) {
  keep <- which(is.finite(gobj$q))
  lapply(keep, function(r) {
    list(b = grid[r, ], q = gobj$q[r], s = gobj$s[, r], type = "grid")
  })
}

# lexicographic order of a candidate pool by (q, then each b coordinate)
.jn_pool_order <- function(pool) {
  qv <- vapply(pool, function(p) p$q, numeric(1))
  bm <- do.call(rbind, lapply(pool, function(p) p$b))
  do.call(order, c(list(qv), lapply(seq_len(ncol(bm)), function(j) bm[, j])))
}

# minimum q_reported carried on the prior minima (NA when none advertises the
# attribute) -- the monotonicity reference for the nesting guard
.jn_prior_q <- function(prior_minima) {
  if (!length(prior_minima)) {
    return(NA_real_)
  }
  qr <- vapply(prior_minima, function(b) {
    a <- attr(b, "q_reported")
    if (is.null(a)) NA_real_ else as.numeric(a)
  }, numeric(1))
  if (all(is.na(qr))) NA_real_ else min(qr, na.rm = TRUE)
}

# up to n_starts lowest-q candidates separated by min_frac of the normalized box
# diagonal, each tagged with start provenance for the winning-start record
.jn_separated_starts <- function(pool, lower, upper, n_starts = 5L,
                                 min_frac = 0.05) {
  if (!length(pool)) {
    return(list())
  }
  ord <- .jn_pool_order(pool)
  bm <- do.call(rbind, lapply(pool, function(p) p$b))
  width <- upper - lower
  width[!is.finite(width) | width <= 0] <- 1
  norm <- sweep(sweep(bm, 2L, lower, "-"), 2L, width, "/")
  min_dist <- min_frac * sqrt(ncol(bm))
  sel <- integer(0)
  for (idx in ord) {
    if (length(sel)) {
      d <- sqrt(colSums((t(norm[sel, , drop = FALSE]) - norm[idx, ])^2))
      if (min(d) < min_dist) next
    }
    sel <- c(sel, idx)
    if (length(sel) >= n_starts) break
  }
  lapply(sel, function(i) structure(pool[[i]]$b, start_type = pool[[i]]$type))
}

# arg-min candidate over the pool, tagging argmin_source (carry vs grid scan)
# and the winning-start provenance for the result row
.jn_scan_best <- function(pool) {
  if (!length(pool)) {
    return(list(
      b = NA_real_, q = Inf, s = rep(NA_real_, 4L),
      source = "grid", type = "grid_arg"
    ))
  }
  top <- pool[[.jn_pool_order(pool)[1L]]]
  source <- if (identical(top$type, "carry")) "carry" else "grid"
  type <- switch(top$type,
    grid = "grid_arg",
    b_seed = "b_seed",
    carry = "carry",
    top$type
  )
  list(b = top$b, q = top$q, s = top$s, source = source, type = type)
}

# Vectorized feasible-grid scan of q over the joint Lewbel set, returning the
# attained arg-min (carry floor included), separated polish starts, and the grid
# census. A carried prior reporting a q below anything attainable now stops the
# run as implementation drift -- never a data status.
logvar_joint_null_scan <- function(qs, b_tab, w1, w2, proj, d_inv2, grid_n,
                                   prior_minima = list(), seed_points = list()) {
  lower <- as.numeric(b_tab$set_lower)
  upper <- as.numeric(b_tab$set_upper)
  grid <- logvar_feasible_grid(qs, lower, upper, grid_n)
  if (nrow(grid) < 100L) grid <- logvar_feasible_grid(qs, lower, upper, 2L * grid_n - 1L)
  sparse_grid <- nrow(grid) < 100L
  gobj <- logvar_joint_null_objective(grid, w1, w2, proj, d_inv2)
  grid_pool <- .jn_grid_candidates(grid, gobj)
  seed_pool <- .jn_extra_candidates(seed_points, "b_seed", qs, w1, w2, proj, d_inv2)
  carry_pool <- .jn_extra_candidates(prior_minima, "carry", qs, w1, w2, proj, d_inv2)
  q_of <- function(cs) if (length(cs)) min(vapply(cs, function(p) p$q, numeric(1))) else Inf
  q_current <- min(q_of(carry_pool), q_of(c(grid_pool, seed_pool)))
  q_prior <- .jn_prior_q(prior_minima)
  if (is.finite(q_current) && !is.na(q_prior) && q_current > q_prior) {
    stop(sprintf(paste0(
      "joint-null nesting/monotonicity violation: attained q_current %.17g ",
      "exceeds carried q_prior %.17g"
    ), q_current, q_prior))
  }
  pool <- c(grid_pool, seed_pool, carry_pool)
  list(
    grid_points = nrow(grid), n_unavailable = sum(gobj$unavailable),
    starts = .jn_separated_starts(pool, lower, upper),
    best = .jn_scan_best(pool), sparse_grid = sparse_grid
  )
}

# lowest-q feasible attained candidate with a fresh slope vector, lexicographic
# tie-break; NA/Inf best when nothing feasible was attained
.jn_polish_best <- function(cands, k, w1, w2, proj, d_inv2) {
  if (!length(cands)) {
    return(list(b = rep(NA_real_, k), q = Inf, s = rep(NA_real_, 4L)))
  }
  top <- cands[[.jn_pool_order(cands)[1L]]]
  o <- logvar_joint_null_objective(top$b, w1, w2, proj, d_inv2)
  list(b = top$b, q = as.numeric(o$q), s = as.numeric(o$s))
}

# Multi-start SLSQP polish of q from the separated starts through the shared
# logvar_polish_objective seam. guard_scale defaults internally to the least
# start objective; every polished point is directly recomputed and discarded
# unless it is Lewbel-feasible, so the returned best always sits in the set.
logvar_joint_null_polish <- function(qs, starts, w1, w2, proj, d_inv2) {
  k <- length(as.numeric(starts[[1L]]))
  omega <- .jn_omega(qs)
  fn <- function(b) logvar_joint_null_objective(b, w1, w2, proj, d_inv2)$q
  gr <- function(b) logvar_joint_null_gradient(b, w1, w2, proj, d_inv2)
  q_start <- vapply(starts, function(b) {
    v <- tryCatch(fn(as.numeric(b)), error = function(e) NA_real_)
    if (length(v) != 1L) NA_real_ else as.numeric(v)
  }, numeric(1))
  finite_q <- q_start[is.finite(q_start)]
  guard_scale <- if (length(finite_q)) max(1, min(finite_q)) else 1
  records <- vector("list", length(starts))
  cands <- list()
  successful <- 0L
  for (i in seq_along(starts)) {
    b0 <- as.numeric(starts[[i]])
    if (is.finite(q_start[i])) {
      r0 <- .feasibility_residual(qs, b0, omega)
      if (is.finite(r0) && r0 <= 1e-4) cands[[length(cands) + 1L]] <- list(b = b0, q = q_start[i])
    }
    pol <- tryCatch(
      logvar_polish_objective(qs, "min", b0, guard_scale, fn, gr, method = "slsqp"),
      error = function(e) NULL
    )
    records[[i]] <- pol
    if (is.null(pol) || is.null(pol$par) || any(!is.finite(pol$par))) next
    bp <- as.numeric(pol$par)
    op <- logvar_joint_null_objective(bp, w1, w2, proj, d_inv2)
    rp <- .feasibility_residual(qs, bp, omega)
    if (is.finite(op$q) && is.finite(rp) && rp <= 1e-4) {
      cands[[length(cands) + 1L]] <- list(b = bp, q = as.numeric(op$q))
      if (isFALSE(pol$suspect)) successful <- successful + 1L
    }
  }
  list(
    best = .jn_polish_best(cands, k, w1, w2, proj, d_inv2),
    records = records, successful_polishes = successful
  )
}

# conditional sup-norm epigraph sensitivity kept in its own module so this file
# stays below the repository line cap; sourced so callers see one surface
source("scripts-paper/log_var_eq_joint_null_search_epigraph.R")
