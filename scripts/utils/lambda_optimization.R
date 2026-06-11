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
# are necessarily redundant. Every packed element is free during
# optimization, so zero rows in a start (per-component subset
# restrictions) are NOT preserved -- subsets hold for fixed weights
# only; a support mask is future work.

lambda_dims <- function(lambda_list) {
  lapply(lambda_list, function(el) {
    if (is.null(el)) NULL else dim(el)
  })
}

pack_lambda <- function(lambda_list) {
  unlist(
    lapply(lambda_list, function(el) {
      if (is.null(el)) numeric(0) else as.vector(el)
    }),
    use.names = FALSE
  )
}

unpack_lambda <- function(par, dims) {
  out <- vector("list", length(dims))
  pos <- 0L
  for (i in seq_along(dims)) {
    if (is.null(dims[[i]])) next
    n <- prod(dims[[i]])
    out[[i]] <- matrix(par[pos + seq_len(n)], nrow = dims[[i]][1])
    pos <- pos + n
  }
  out
}

normalize_lambda_columns <- function(lambda_list) {
  lapply(lambda_list, function(el) {
    if (is.null(el)) NULL else normalize_gamma_columns(el)
  })
}

honest_width_lambda <- function(lambda_list, tau, moments) {
  lambda_list <- normalize_lambda_columns(lambda_list)
  bounds_tbl <- tryCatch(
    {
      qs <- hetid::build_general_quadratic_system(
        lambda_list, tau, moments
      )
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

objective_lambda_only <- function(par, dims, moments, tau) {
  lambda_list <- normalize_lambda_columns(unpack_lambda(par, dims))
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
                                    xtol_rel = 1e-6) {
  if (is.matrix(lambda_start)) {
    # NULL out unconstrained system columns: the strict list-form
    # validator rejects weights there, and a legacy-style full-size
    # matrix start must keep working on subset-maturity containers
    constrained <- attr(moments, "maturities")
    lambda_start <- lapply(
      seq_len(ncol(lambda_start)),
      function(i) {
        if (i %in% constrained) {
          lambda_start[, i, drop = FALSE]
        } else {
          NULL
        }
      }
    )
  }
  set.seed(seed)
  dims <- lambda_dims(lambda_start)
  par_start <- pack_lambda(lambda_start)

  objective_start <- honest_width_lambda(lambda_start, tau, moments)

  # Same multistart recipe as run_gamma_optimization: primary start
  # plus perturbed-and-normalized random starts. The K = 1 list of a
  # gamma matrix packs to as.vector(gamma) and draws the same rnorm
  # count, so the legacy and general optimizers see identical start
  # sequences under the same seed.
  starts <- vector("list", n_starts)
  starts[[1]] <- par_start
  for (s in seq_len(n_starts - 1) + 1) {
    perturbed <- unpack_lambda(
      par_start + rnorm(length(par_start)), dims
    )
    starts[[s]] <- pack_lambda(normalize_lambda_columns(perturbed))
  }

  obj_fn <- function(par) {
    objective_lambda_only(par, dims, moments, tau)
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
    honest_width_lambda(unpack_lambda(r$par, dims), tau, moments)
  }, numeric(1))
  best_idx <- which.min(honest_values)
  lambda_opt <- normalize_lambda_columns(
    unpack_lambda(all_results[[best_idx]]$par, dims)
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
    duplicate_directions = duplicate_directions
  )
}
