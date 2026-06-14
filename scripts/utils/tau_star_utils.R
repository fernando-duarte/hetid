# Shared machinery for the tau* analysis. tau* is the slack at which the
# identified set transitions bounded -> unbounded; a common slack tau is
# applied across all components throughout. Provides the fixed-gamma sweep
# and bisection helpers, the re-optimizing oracle (the whitened lambda
# optimizer under the repo's variance normalization; the caller supplies
# whiten), and the curvature (recession) degeneracy diagnostic.

# Quadratic system for a fixed gamma at slack tau (A_i already symmetric).
tau_quadratic_system <- function(gamma, tau, moments) {
  build_pipeline_quadratic_system(gamma, rep(tau, ncol(gamma)), moments)$quadratic
}

# Total profile-bound width at one tau, with the house three-state status:
#   bounded    -- every side finite and feasibility-valid
#   unbounded  -- some side infinite (total width Inf)
#   unreliable -- a solve failed (width NA) or a validity certificate failed
eval_width_at_tau <- function(gamma, tau, moments) {
  b <- solve_all_profile_bounds(tau_quadratic_system(gamma, tau, moments))
  total <- sum(b$width)
  bounded <- all(b$bounded_lower & b$bounded_upper)
  valid <- all(b$valid_lower & b$valid_upper)
  status <- if (is.na(total) || (bounded && !valid)) {
    "unreliable"
  } else if (!bounded) {
    "unbounded"
  } else {
    "bounded"
  }
  list(total = total, bounded = bounded, valid = valid, status = status)
}

.sweep_row <- function(tau, w, grid_label) {
  data.frame(
    tau = tau, total_width = w$total, all_bounded = w$bounded,
    all_valid = w$valid, status = w$status, grid = grid_label,
    stringsAsFactors = FALSE
  )
}

# Sweep a fixed gamma over a tau grid. tau = 0 is the point-identified case
# (width 0 by construction) and is recorded without solving.
sweep_fixed_gamma <- function(gamma, moments, taus, grid_label) {
  rows <- lapply(taus, function(t) {
    if (t == 0) {
      w <- list(total = 0, bounded = TRUE, valid = TRUE, status = "bounded")
    } else {
      w <- eval_width_at_tau(gamma, t, moments)
    }
    .sweep_row(t, w, grid_label)
  })
  do.call(rbind, rows)
}

# Evenly spaced fine taus strictly inside the bounded region (0, first
# unbounded coarse tau), skipping values already on the coarse grid. Resolves
# the width blow-up that the coarse grid renders as only a few points.
fine_tau_grid <- function(coarse, n_fine = 20L) {
  unb <- coarse$tau[!coarse$all_bounded]
  hi <- if (length(unb)) min(unb) else max(coarse$tau)
  taus <- seq(0, hi, length.out = n_fine + 2L)
  taus <- taus[taus > 0 & taus < hi]
  taus[!round(taus, 10) %in% round(coarse$tau, 10)]
}

# tau* for a FIXED gamma: bisect the bounded -> unbounded transition bracketed
# by the coarse sweep. Returns the threshold, the (tau, width) evaluations the
# bisection performs (free fine detail at the transition), and whether the
# sweep never left the bounded region (tau* capped at the grid maximum).
tau_star_fixed <- function(gamma, moments, coarse, iters = 40L) {
  # A tau counts as inside the bounded region only when its set is CERTIFIED
  # bounded (status "bounded" = finite total width, every side bounded AND the
  # validity certificate passed). An "unreliable" finite solve (bounded but
  # validity-failed) is not a certified bounded set; counting it as bounded
  # would push tau* past the true transition, so the coarse bracket and the
  # bisection both branch on the validated status, not on the raw bounded flag.
  certified <- coarse$status == "bounded"
  unb <- coarse$tau[!certified]
  if (length(unb) == 0) {
    return(list(tau_star = max(coarse$tau), trace = NULL, capped = TRUE))
  }
  hi <- min(unb)
  below <- certified & coarse$tau < hi
  lo <- if (any(below)) max(coarse$tau[below]) else 0
  trace <- vector("list", iters)
  for (k in seq_len(iters)) {
    mid <- (lo + hi) / 2
    w <- eval_width_at_tau(gamma, mid, moments)
    trace[[k]] <- .sweep_row(mid, w, "bisection")
    if (identical(w$status, "bounded")) lo <- mid else hi <- mid
  }
  list(tau_star = (lo + hi) / 2, trace = do.call(rbind, trace), capped = FALSE)
}

# tau* for the OPTIMIZER: the largest tau at which re-optimizing the weights
# (the whitened lambda optimizer; whiten is REQUIRED -- every caller must
# choose) still yields a bounded+valid set (finite final objective). Brackets
# upward from tau_lo by doubling, with every candidate clamped to the cap so
# no tau at or above 1 is ever evaluated (hetid admits tau in [0,1) only --
# the correlation-bound interpretation fails at tau >= 1), then bisects.
# tau_star is NA when even tau_lo fails; capped = TRUE when the set stays
# bounded up to the cap, i.e. the reported tau* is a censored lower bound.
tau_star_optimized <- function(gamma_start, moments, whiten,
                               tau_lo = 0.2, cap = 0.99, iters = 25L) {
  n_comp <- ncol(gamma_start)
  oracle <- function(tau) {
    is.finite(run_lambda_optimization(
      gamma_start, moments, rep(tau, n_comp),
      whiten = whiten,
      n_starts = TAU_STAR_N_STARTS, seed = SEED
    )$objective_final)
  }
  if (!oracle(tau_lo)) {
    return(list(tau_star = NA_real_, capped = FALSE))
  }
  lo <- tau_lo
  hi <- min(tau_lo * 2, cap)
  while (oracle(hi) && hi < cap) {
    lo <- hi
    hi <- min(hi * 2, cap)
  }
  if (oracle(hi)) {
    return(list(tau_star = hi, capped = TRUE))
  }
  for (k in seq_len(iters)) {
    mid <- (lo + hi) / 2
    if (oracle(mid)) lo <- mid else hi <- mid
  }
  list(tau_star = (lo + hi) / 2, capped = FALSE)
}

# Curvature (recession) degeneracy diagnostic at one tau: the smallest, over
# unit directions d, of the largest curvature max_i d'A_i d. A clearly
# negative value certifies a common non-positive-curvature direction
# (necessary for an unbounded set). Reported raw and normalized by the mean
# Frobenius norm of the A_i, so its magnitude is comparable across taus.
# This is a DIAGNOSTIC of how close the system is to losing boundedness, not
# a tau* locator: for a near-degenerate (rank-1) gamma its margin is
# negligible on both sides of tau*.
recession_metric <- function(gamma, tau, moments, n_dir = 8000L, seed = 1L) {
  a_list <- lapply(
    tau_quadratic_system(gamma, tau, moments)$A_i,
    function(a) (a + t(a)) / 2
  )
  n_comp <- ncol(gamma)
  set.seed(seed)
  dirs <- cbind(
    diag(n_comp), -diag(n_comp),
    matrix(rnorm(n_comp * n_dir), n_comp)
  )
  dirs <- sweep(dirs, 2, sqrt(colSums(dirs^2)), "/")
  vals <- apply(dirs, 2, function(d) {
    max(vapply(a_list, function(m) drop(t(d) %*% m %*% d), numeric(1)))
  })
  fro <- mean(vapply(a_list, function(m) norm(m, "F"), numeric(1)))
  raw <- min(vals)
  list(raw = raw, normalized = raw / fro)
}
