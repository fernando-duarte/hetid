# Lambda Optimization -- outer optimization over general per-component
# weight matrices (one or more combinations per component). Mirrors
# optimization_utils.R, which stays frozen for the legacy single-
# combination gamma path; shared pieces (normalize_gamma_columns,
# UNBOUNDED_PENALTY, compute_total_width, solve_all_profile_bounds)
# are reused from the sourced utils.
#
# Objective: the SAME scalar the legacy optimizer minimizes -- the sum
# over components of profile-interval widths (compute_total_width),
# i.e. the spec's outer weight problem. Honesty notes: this is a
# seeded multistart LOCAL heuristic, not a certified global width
# minimizer; widths under data-selected weights are a width-minimizing
# computational benchmark, not a confidence statement (the spec's
# uniform-bound caveat applies). For K_i > 1 supply distinct start
# columns (collapsed directions are redundant constraints, reported
# via duplicate_directions); more than J combinations per component
# are necessarily redundant. Per-component instrument subsets: pass
# support (free-row index lists, NULL at unconstrained columns -- the
# lambda_from_support convention; helpers in lambda_mask.R).
# Off-support entries stay exactly 0.0 through the start, every
# perturbation, every slsqp iterate, the honest re-evaluations, and
# the returned optimum, because only FREE elements are packed. Masked
# runs draw fewer rnorm values and carry NO seeded-equivalence
# contract with the legacy optimizer; support = NULL packs every
# element and stays bit-identical to the pre-mask path. The canonical
# integer support is echoed in the return value.

normalize_lambda_columns <- function(lambda_list) {
  lapply(lambda_list, function(el) {
    if (is.null(el)) NULL else normalize_gamma_columns(el)
  })
}

objective_lambda_only <- function(par, dims, moments, tau,
                                  free = NULL) {
  lambda_list <- normalize_lambda_columns(
    unpack_active(par, dims, free)
  )
  tryCatch(
    {
      qs <- hetid::build_general_quadratic_system(
        lambda_list, tau, moments
      )
      bounds_tbl <- solve_all_profile_bounds(qs$quadratic)
      if (any(!bounds_tbl$bounded_lower) ||
        any(!bounds_tbl$bounded_upper) ||
        any(!bounds_tbl$valid_lower) ||
        any(!bounds_tbl$valid_upper)) {
        return(UNBOUNDED_PENALTY)
      }
      compute_total_width(bounds_tbl)
    },
    error = function(e) UNBOUNDED_PENALTY
  )
}

run_lambda_optimization <- function(lambda_start,
                                    moments,
                                    tau,
                                    n_starts = 10,
                                    seed = SEED,
                                    maxeval = 500L,
                                    xtol_rel = 1e-6,
                                    support = NULL) {
  lambda_start <- coerce_lambda_start(lambda_start, moments)
  dims <- lambda_dims(lambda_start)
  free <- NULL
  if (!is.null(support)) {
    support <- validate_support_mask(support, lambda_start, moments)
    free <- support_free_mask(dims, support)
  }
  set.seed(seed)
  par_start <- pack_active(lambda_start, free)

  objective_start <- honest_width_lambda(lambda_start, tau, moments)

  # Same multistart recipe as run_gamma_optimization: primary start
  # plus perturbed-and-normalized random starts. The K = 1 list of a
  # gamma matrix packs to as.vector(gamma) and draws the same rnorm
  # count, so the legacy and general optimizers see identical start
  # sequences under the same seed.
  starts <- vector("list", n_starts)
  starts[[1]] <- par_start
  for (s in seq_len(n_starts - 1) + 1) {
    perturbed <- unpack_active(
      par_start + rnorm(length(par_start)), dims, free
    )
    starts[[s]] <- pack_active(normalize_lambda_columns(perturbed), free)
  }

  obj_fn <- function(par) {
    objective_lambda_only(par, dims, moments, tau, free)
  }
  all_results <- lapply(seq_len(n_starts), function(s) {
    tryCatch(
      nloptr::slsqp(
        x0 = starts[[s]], fn = obj_fn,
        control = list(xtol_rel = xtol_rel, maxeval = maxeval)
      ),
      error = function(e) {
        list(
          par = starts[[s]], value = UNBOUNDED_PENALTY,
          convergence = -99L, iter = 0L, message = e$message
        )
      }
    )
  })

  honest_values <- vapply(all_results, function(r) {
    honest_width_lambda(unpack_active(r$par, dims, free), tau, moments)
  }, numeric(1))
  best_idx <- which.min(honest_values)
  lambda_opt <- normalize_lambda_columns(
    unpack_active(all_results[[best_idx]]$par, dims, free)
  )

  # Near-duplicate combination columns are redundant constraints, not
  # errors; report them so users notice collapsed directions.
  duplicate_directions <- vapply(lambda_opt, function(el) {
    if (is.null(el) || ncol(el) < 2) {
      return(FALSE)
    }
    cors <- abs(crossprod(el))
    any(cors[upper.tri(cors)] > 0.999)
  }, logical(1))

  list(
    lambda_optimized = lambda_opt,
    objective_start = objective_start,
    objective_final = honest_values[[best_idx]],
    all_results = all_results,
    best_index = best_idx,
    duplicate_directions = duplicate_directions,
    support = support
  )
}
