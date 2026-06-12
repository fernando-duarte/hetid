# Lambda Optimization -- outer optimization over general per-component
# weight matrices (one or more combinations per component). Mirrors
# optimization_utils.R, which stays frozen for the legacy single-
# combination gamma path; shared pieces (normalize_gamma_columns,
# UNBOUNDED_PENALTY, compute_total_width, solve_all_profile_bounds)
# are reused from the sourced utils. Packing, start coercion,
# masking, coordinate codecs, and honest_width_lambda live in
# lambda_mask.R; whitening validation lives in lambda_whitening.R.
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
#
# Opt-in whitening (whiten != NULL): the multistart SEARCH walks
# mu = chol(V) %*% lambda coordinates -- per-component sub-blocks
# chol(V[s_i, s_i]) of the supported instruments under a mask --
# implementing the spec's variance normalization
# lambda' V lambda = 1 as the search coordinate system. A numerical
# reparameterization within the spec's admissibility class: NOT a
# statistical improvement; NO constraint set changes (constraints
# depend on weights only through direction). Every evaluation, the
# honest oracle, the returned weights, AND every all_results par
# (decoded before return) stay in original lambda coordinates with
# unit-Euclidean columns -- mu never leaves the search. Whitened
# runs draw the SAME rnorm count but interpret draws in mu-space (a
# legitimately different draw geometry); every seeded-equivalence
# contract covers whiten = NULL only. The applied transform is
# echoed in the return value (whitening field; NULL when off).
# For a selection-honest report, select the weights on one temporal
# block and evaluate the set on the other
# (scripts/post_selection/run_split_study.R): the evaluated bound
# then uses weights fixed relative to the evaluation sample (the
# spec's fixed-Lambda inclusion), with serial dependence across the
# block boundary and the maintained bound at the selected weights
# as the remaining, stated assumptions.

normalize_lambda_columns <- function(lambda_list) {
  lapply(lambda_list, function(el) {
    if (is.null(el)) NULL else normalize_gamma_columns(el)
  })
}

objective_lambda_only <- function(par, dims, moments, tau,
                                  free = NULL, wctx = NULL) {
  lambda_list <- normalize_lambda_columns(
    decode_lambda(par, dims, free, wctx)
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
                                    support = NULL,
                                    whiten = NULL) {
  lambda_start <- coerce_lambda_start(lambda_start, moments)
  dims <- lambda_dims(lambda_start)
  free <- NULL
  if (!is.null(support)) {
    support <- validate_support_mask(support, lambda_start, moments)
    free <- support_free_mask(dims, support)
  }
  # Whitening context, validated BEFORE any RNG use; the lazy branch
  # keeps the whiten = NULL path free of any lambda_whitening.R
  # dependency (the landed test preambles stay byte-identical).
  wctx <- if (is.null(whiten)) {
    NULL
  } else {
    whiten_context(whiten, support, moments)
  }
  set.seed(seed)
  par_start <- pack_active(encode_lambda(lambda_start, wctx), free)

  objective_start <- honest_width_lambda(lambda_start, tau, moments)

  # Same multistart recipe as run_gamma_optimization: the primary
  # start packs unnormalized, perturbed starts are normalized. By the
  # spec's c^2 direction-invariance, normalization is only a choice
  # of representative (slsqp itself is unconstrained -- iterates lie
  # on no sphere). In whitened mode the packed coordinates are mu, so
  # the perturbed-start normalization is unit mu: the spec's variance
  # normalization restricted to the support (structural zeros kill
  # all cross terms). A K = 1 gamma-matrix start packs to
  # as.vector(gamma) with the same rnorm count, so the legacy and
  # general optimizers see identical start sequences under the same
  # seed (whiten = NULL only).
  starts <- vector("list", n_starts)
  starts[[1]] <- par_start
  for (s in seq_len(n_starts - 1) + 1) {
    perturbed <- unpack_active(
      par_start + rnorm(length(par_start)), dims, free
    )
    starts[[s]] <- pack_active(normalize_lambda_columns(perturbed), free)
  }

  obj_fn <- function(par) {
    objective_lambda_only(par, dims, moments, tau, free, wctx)
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
    honest_width_lambda(
      decode_lambda(r$par, dims, free, wctx), tau, moments
    )
  }, numeric(1))
  best_idx <- which.min(honest_values)
  lambda_opt <- normalize_lambda_columns(
    decode_lambda(all_results[[best_idx]]$par, dims, free, wctx)
  )

  # Legacy-field convention: all_results[[s]]$par is packed LAMBDA in
  # every mode; whitened runs decode their terminal mu here, AFTER
  # the honest re-evaluations above consumed the raw mu-space pars.
  if (!is.null(wctx)) {
    all_results <- lapply(all_results, function(r) {
      r$par <- pack_active(
        decode_lambda(r$par, dims, free, wctx), free
      )
      r
    })
  }

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
    support = support,
    whitening = if (is.null(wctx)) {
      NULL
    } else {
      wctx[c("source", "vcov", "ridge", "jitter")]
    }
  )
}
