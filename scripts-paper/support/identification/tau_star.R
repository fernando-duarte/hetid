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

mean_quadratic_system_factory <- function(
  mean_eq,
  builder = tau_quadratic_system
) {
  stopifnot(
    is.list(mean_eq),
    !is.null(mean_eq$gamma),
    !is.null(mean_eq$moments),
    is.function(builder)
  )
  gamma <- mean_eq$gamma
  moments <- mean_eq$moments
  force(builder)
  function(tau) builder(gamma, tau, moments)
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
    PAPER_ENDPOINT_STATUS[["unreliable"]]
  } else if (!bounded) {
    PAPER_ENDPOINT_STATUS[["unbounded"]]
  } else {
    PAPER_ENDPOINT_STATUS[["bounded"]]
  }
  list(total = total, bounded = bounded, valid = valid, status = status)
}

# Per-coefficient intervals from one already-built quadratic system.
coef_interval_tables_from_quadratic <- function(qs, beta1r, beta2r) {
  tb <- solve_all_profile_bounds(qs)
  theta_fail_closed <- anyNA(c(tb$lower, tb$upper))
  theta <- data.frame(
    coef = rownames(beta2r),
    set_lower = tb$lower, set_upper = tb$upper,
    status = paper_endpoint_status_from_flags(
      tb$bounded_lower & tb$bounded_upper,
      tb$valid_lower & tb$valid_upper
    ),
    row.names = NULL, stringsAsFactors = FALSE
  )
  beta1 <- do.call(rbind, lapply(names(beta1r), function(p) {
    if (all(beta2r[, p] == 0)) {
      fmin <- fmax <- list(bound = 0, bounded = TRUE, valid = !theta_fail_closed)
    } else {
      fmin <- solve_linear_functional_bound(qs, beta2r[, p], "min")
      fmax <- solve_linear_functional_bound(qs, beta2r[, p], "max")
    }
    data.frame(
      coef = p,
      set_lower = unname(beta1r[p]) - fmax$bound,
      set_upper = unname(beta1r[p]) - fmin$bound,
      status = paper_endpoint_status_from_flags(
        fmin$bounded && fmax$bounded,
        fmin$valid && fmax$valid
      ),
      row.names = NULL, stringsAsFactors = FALSE
    )
  }))
  list(beta1 = beta1, theta = theta)
}

# Compatible wrapper for callers that still provide gamma, tau, and moments.
coef_interval_tables <- function(gamma, tau, moments, beta1r, beta2r) {
  stopifnot(nrow(beta2r) == ncol(gamma), ncol(beta2r) == length(beta1r))
  stopifnot(identical(colnames(beta2r), names(beta1r)))
  coef_interval_tables_from_quadratic(
    tau_quadratic_system(gamma, tau, moments),
    beta1r,
    beta2r
  )
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
      w <- list(
        total = 0, bounded = TRUE, valid = TRUE,
        status = PAPER_ENDPOINT_STATUS[["bounded"]]
      )
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
fine_tau_grid <- function(
  coarse,
  n_fine = PAPER_INFERENCE_SEARCH_CONTROL$tau_star$fine_grid_points
) {
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
tau_star_fixed <- function(
  gamma, moments, coarse,
  iters = PAPER_INFERENCE_SEARCH_CONTROL$tau_star$bisection_iterations
) {
  # A tau counts as inside the bounded region only when its set is CERTIFIED
  # bounded (status "bounded" = finite total width, every side bounded AND the
  # validity certificate passed). An "unreliable" finite solve (bounded but
  # validity-failed) is not a certified bounded set; counting it as bounded
  # would push tau* past the true transition, so the coarse bracket and the
  # bisection both branch on the validated status, not on the raw bounded flag.
  certified <- coarse$status == PAPER_ENDPOINT_STATUS[["bounded"]]
  # "unreliable" is neither evidence of boundedness nor of unboundedness, so it
  # cannot define the transition; only a certified-unbounded tau brackets it.
  unb <- coarse$tau[coarse$status == PAPER_ENDPOINT_STATUS[["unbounded"]]]
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
    if (identical(w$status, PAPER_ENDPOINT_STATUS[["bounded"]])) {
      lo <- mid
    } else if (identical(w$status, PAPER_ENDPOINT_STATUS[["unbounded"]])) {
      hi <- mid
    } else {
      # an unreliable midpoint carries no evidence; refine no further
      break
    }
  }
  list(tau_star = (lo + hi) / 2, trace = do.call(rbind, trace), capped = FALSE)
}
