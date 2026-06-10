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

# Total identified-set width for a gamma. FAIL CLOSED: reject (large finite
# penalty) any gamma whose set is unbounded in any direction OR whose any bounded
# profile fails the feasibility/active validity check. The penalty must stay
# finite -- the OUTER optimizer is itself an slsqp call that needs finite values.
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
        return(1e6)
      }
      compute_total_width(bounds_tbl)
    },
    error = function(e) {
      1e6
    }
  )
}

# Is the set for this gamma unbounded OR invalid in any direction? (Either makes
# the reported start width dishonest -- both map to Inf, never the 1e6 penalty.)
.gamma_set_unbounded <- function(gamma, tau, moments) {
  # Match objective_gamma_only: it normalizes columns before building the system,
  # and the optimizer evaluates the normalized matrix. Normalizing here too keeps
  # the honest-start oracle and the objective looking at the SAME system.
  gamma <- normalize_gamma_columns(gamma)
  bt <- tryCatch(
    {
      qs <- build_quadratic_system(gamma, tau, moments)
      solve_all_profile_bounds(qs$quadratic)
    },
    error = function(e) NULL
  )
  if (is.null(bt)) {
    return(TRUE)
  }
  any(!bt$bounded_lower) || any(!bt$bounded_upper) ||
    any(!bt$valid_lower) || any(!bt$valid_upper)
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

  # Reported start objective is HONEST: Inf when the baseline set is unbounded,
  # so downstream width-reduction metrics never read the finite 1e6 penalty as a
  # spurious ~100% improvement. The optimizer internally still uses 1e6.
  objective_start <- if (
    .gamma_set_unbounded(gamma_start, tau, moments)
  ) {
    Inf
  } else {
    objective_gamma_only(par_start, moments, tau, n_pcs, n_comp)
  }

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
        par = starts[[s]], value = 1e6,
        convergence = -99L, iter = 0L,
        message = e$message
      )
    })
  })

  # Select best result
  objectives <- vapply(
    all_results, function(r) r$value, numeric(1)
  )
  best_idx <- which.min(objectives)
  best <- all_results[[best_idx]]
  gamma_opt <- normalize_gamma_columns(
    unpack_gamma(best$par, n_pcs, n_comp)
  )

  # Honest final objective: best$value == 1e6 means NO bounded+valid gamma was
  # found (all starts on the penalty plateau) -> report Inf, not the sentinel.
  objective_final <- if (is.finite(best$value) && best$value < 1e6) {
    best$value
  } else {
    Inf
  }

  list(
    gamma_optimized = gamma_opt,
    objective_start = objective_start,
    objective_final = objective_final,
    all_results = all_results,
    best_index = best_idx
  )
}
