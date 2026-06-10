# Optimization Utilities -- outer Gamma optimization. The inner profile-bounds
# solver now lives in profile_bounds.R (sourced via common_settings.R).

compute_total_width <- function(bounds_tbl) {
  sum(bounds_tbl$width)
}

pack_gamma <- function(gamma) as.vector(gamma)

unpack_gamma <- function(par, n_pcs, n_components) {
  matrix(par, nrow = n_pcs, ncol = n_components)
}

normalize_gamma_columns <- function(gamma) {
  col_norms <- sqrt(colSums(gamma^2))
  col_norms[col_norms == 0] <- 1
  sweep(gamma, 2, col_norms, FUN = "/")
}

# Steering penalty for the INNER slsqp objective only. It must be finite (the
# outer optimizer is itself an slsqp call that needs finite values) and sit far
# above any genuine total width, but selection and reporting NEVER read it:
# run_gamma_optimization re-evaluates every terminal gamma with honest_width(),
# so a real bounded width can never lose to (or be mistaken for) the penalty.
UNBOUNDED_PENALTY <- 1e12

# Total identified-set width for a gamma. FAIL CLOSED: reject (steering
# penalty) any gamma whose set is unbounded in any direction OR whose any
# bounded profile fails the feasibility/active validity check.
objective_gamma_only <- function(par, moments, tau,
                                 n_pcs, n_components) {
  gamma <- unpack_gamma(par, n_pcs, n_components)
  gamma <- normalize_gamma_columns(gamma)
  tryCatch(
    {
      quad_sys <- build_quadratic_system(
        gamma, tau, moments
      )
      bounds_tbl <- solve_all_profile_bounds(
        quad_sys$quadratic
      )
      if (any(!bounds_tbl$bounded_lower) || any(!bounds_tbl$bounded_upper) ||
        any(!bounds_tbl$valid_lower) || any(!bounds_tbl$valid_upper)) {
        return(UNBOUNDED_PENALTY)
      }
      compute_total_width(bounds_tbl)
    },
    error = function(e) {
      UNBOUNDED_PENALTY
    }
  )
}

# Honest total identified-set width for a gamma: the finite total profile-bound
# width when EVERY side is bounded AND valid, else Inf (never the steering
# penalty). Columns are normalized exactly as in objective_gamma_only so this
# oracle and the optimizer evaluate the SAME system.
honest_width <- function(gamma, tau, moments) {
  gamma <- normalize_gamma_columns(gamma)
  bounds_tbl <- tryCatch(
    {
      qs <- build_quadratic_system(gamma, tau, moments)
      solve_all_profile_bounds(qs$quadratic)
    },
    error = function(e) NULL
  )
  if (is.null(bounds_tbl) ||
    any(!bounds_tbl$bounded_lower) || any(!bounds_tbl$bounded_upper) ||
    any(!bounds_tbl$valid_lower) || any(!bounds_tbl$valid_upper)) {
    return(Inf)
  }
  compute_total_width(bounds_tbl)
}

run_gamma_optimization <- function(gamma_start,
                                   moments,
                                   tau,
                                   n_starts = 10,
                                   seed = SEED,
                                   maxeval = 500L,
                                   xtol_rel = 1e-6) {
  set.seed(seed)
  n_pcs <- nrow(gamma_start)
  n_comp <- ncol(gamma_start)
  par_start <- pack_gamma(gamma_start)

  # Reported start objective is HONEST: Inf when the baseline set is unbounded
  # or invalid in any direction, else the genuine finite width -- downstream
  # width-reduction metrics never read the finite steering penalty as a
  # spurious ~100% improvement.
  objective_start <- honest_width(gamma_start, tau, moments)

  # Generate starting points: primary + perturbed
  starts <- vector("list", n_starts)
  starts[[1]] <- par_start
  for (s in seq_len(n_starts - 1) + 1) {
    perturbed <- gamma_start +
      rnorm(length(gamma_start))
    perturbed <- normalize_gamma_columns(perturbed)
    starts[[s]] <- pack_gamma(perturbed)
  }

  # Optimize from each start
  obj_fn <- function(par) {
    objective_gamma_only(
      par, moments, tau, n_pcs, n_comp
    )
  }
  all_results <- lapply(seq_len(n_starts), function(s) {
    tryCatch(nloptr::slsqp(
      x0 = starts[[s]], fn = obj_fn,
      control = list(xtol_rel = xtol_rel, maxeval = maxeval)
    ), error = function(e) {
      list(
        par = starts[[s]], value = UNBOUNDED_PENALTY,
        convergence = -99L, iter = 0L,
        message = e$message
      )
    })
  })

  # Honest selection: re-evaluate every start's terminal gamma with
  # honest_width and pick the best HONEST value (ties keep the first start).
  # The inner objective's steering penalty sits inside the genuine width range
  # in pathological systems, so r$value is never used for selection or
  # reporting.
  honest_values <- vapply(all_results, function(r) {
    honest_width(unpack_gamma(r$par, n_pcs, n_comp), tau, moments)
  }, numeric(1))
  best_idx <- which.min(honest_values)
  best <- all_results[[best_idx]]
  gamma_opt <- normalize_gamma_columns(
    unpack_gamma(best$par, n_pcs, n_comp)
  )

  # Honest final objective: Inf when no start reached a bounded+valid gamma.
  objective_final <- honest_values[[best_idx]]

  list(
    gamma_optimized = gamma_opt,
    objective_start = objective_start,
    objective_final = objective_final,
    all_results = all_results,
    best_index = best_idx
  )
}
