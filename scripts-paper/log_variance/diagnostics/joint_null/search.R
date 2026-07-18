# Grid scan, multi-start polish, and nesting carry for the joint-null diagnostic.
# The candidate-evaluation helper and the L-infinity epigraph sensitivity live in
# the companion file, sourced at the end.

paper_source_once(paper_path(
  "log_variance", "diagnostics", "joint_null", "search_candidates.R"
))

# Vectorized feasible-grid scan of q over the joint Lewbel set, returning the
# attained arg-min (carry floor included), separated polish starts, and the grid
# census. A carried prior reporting a q below anything attainable now stops the
# run as implementation drift -- never a data status.
logvar_joint_null_scan <- function(qs, b_tab, w1, w2, proj, d_inv2, grid_n,
                                   prior_minima = list(), seed_points = list(),
                                   control = LOGVAR_JOINT_NULL_CONTROL) {
  lower <- as.numeric(b_tab$set_lower)
  upper <- as.numeric(b_tab$set_upper)
  grid <- logvar_feasible_grid(qs, lower, upper, grid_n)
  if (nrow(grid) < control$grid_floor) {
    grid <- logvar_feasible_grid(qs, lower, upper, 2L * grid_n - 1L)
  }
  sparse_grid <- nrow(grid) < control$grid_floor
  gobj <- logvar_joint_null_objective(grid, w1, w2, proj, d_inv2)
  grid_pool <- .jn_grid_candidates(grid, gobj)
  seed_pool <- .jn_extra_candidates(
    seed_points, "b_seed", qs, w1, w2, proj, d_inv2, control
  )
  carry_pool <- .jn_extra_candidates(
    prior_minima, "carry", qs, w1, w2, proj, d_inv2, control
  )
  q_of <- function(cs) if (length(cs)) min(vapply(cs, function(p) p$q, numeric(1))) else Inf
  q_current <- min(q_of(carry_pool), q_of(c(grid_pool, seed_pool)))
  q_prior <- .jn_prior_q(prior_minima)
  if (is.finite(q_current) && !is.na(q_prior) && q_current > q_prior) {
    stop(sprintf(paste0(
      "joint-null nesting/monotonicity violation: attained q_current %s ",
      "exceeds carried q_prior %s"
    ), paper_numeric_key(q_current), paper_numeric_key(q_prior)))
  }
  pool <- c(grid_pool, seed_pool, carry_pool)
  list(
    grid_points = nrow(grid), n_unavailable = sum(gobj$unavailable),
    starts = .jn_separated_starts(pool, lower, upper, control),
    best = .jn_scan_best(pool), sparse_grid = sparse_grid
  )
}

# lowest-q feasible attained candidate with a fresh slope vector, lexicographic
# tie-break; NA/Inf best when nothing feasible was attained
.jn_polish_best <- function(cands, k, w1, w2, proj, d_inv2) {
  if (!length(cands)) {
    return(list(
      b = rep(NA_real_, k),
      q = Inf,
      s = rep(NA_real_, PAPER_ANALYSIS_CONTRACT$model$n_return_pc)
    ))
  }
  top <- cands[[.jn_pool_order(cands)[1L]]]
  o <- logvar_joint_null_objective(top$b, w1, w2, proj, d_inv2)
  list(b = top$b, q = as.numeric(o$q), s = as.numeric(o$s))
}

# Multi-start SLSQP polish of q from the separated starts through the shared
# logvar_polish_objective seam. guard_scale defaults internally to the least
# start objective; every polished point is directly recomputed and discarded
# unless it is Lewbel-feasible, so the returned best always sits in the set. The
# feasible polished minima are returned too, for the at-tau nesting carry.
logvar_joint_null_polish <- function(
  qs, starts, w1, w2, proj, d_inv2,
  control = LOGVAR_JOINT_NULL_CONTROL
) {
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
  polished <- list()
  successful <- 0L
  for (i in seq_along(starts)) {
    b0 <- as.numeric(starts[[i]])
    if (is.finite(q_start[i])) {
      r0 <- .feasibility_residual(qs, b0, omega)
      if (is.finite(r0) && r0 <= control$feasibility_tol) {
        cands[[length(cands) + 1L]] <- list(b = b0, q = q_start[i])
      }
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
    if (is.finite(op$q) && is.finite(rp) &&
      rp <= control$feasibility_tol) {
      cands[[length(cands) + 1L]] <- list(b = bp, q = as.numeric(op$q))
      polished[[length(polished) + 1L]] <- bp
      if (isFALSE(pol$suspect)) successful <- successful + 1L
    }
  }
  list(
    best = .jn_polish_best(cands, k, w1, w2, proj, d_inv2),
    records = records, successful_polishes = successful, minima = polished
  )
}

# conditional sup-norm epigraph sensitivity kept in its own module so this file
# stays below the repository line cap; sourced so callers see one surface
paper_source_once(paper_path("log_variance", "diagnostics", "joint_null", "epigraph_search.R"))
