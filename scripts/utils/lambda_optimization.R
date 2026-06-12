# Lambda Optimization -- the single outer optimizer over general
# per-component weight matrices (the legacy Euclidean gamma
# optimizer is retired). Shared pieces
# (normalize_gamma_columns, UNBOUNDED_PENALTY, compute_total_width,
# solve_all_profile_bounds) come from the sourced utils; packing,
# start coercion, masking, codecs, and honest_width_lambda live in
# lambda_mask.R; whitening validation in lambda_whitening.R;
# variance-normalization helpers in lambda_varnorm.R.
#
# Objective: the sum over components of profile-interval widths --
# the spec's outer weight problem. A seeded multistart LOCAL
# heuristic, not a certified global minimizer; widths under
# data-selected weights are a computational benchmark, not a
# confidence statement. For K_i > 1 supply distinct start columns
# (collapsed directions are reported via duplicate_directions).
# Per-component subsets: pass support (lambda_from_support
# convention); only FREE elements are packed, so off-support
# entries stay exactly 0.0 everywhere.
#
# Normalization: whiten is REQUIRED, with no default -- the caller
# must choose. whiten = list(z = .) / list(vcov = .) is the REPO
# DEFAULT (every production call site): the search walks
# mu = chol(V) %*% lambda (per-component sub-blocks under a mask)
# and returned lambda_optimized columns satisfy
# lambda' V lambda = 1, the spec's first canonical F_i (.tex line
# 479); zero-variance start columns are rejected up front, and
# lambda_variance echoes the identification-strength diagnostic.
# An EXPLICIT whiten = NULL is the Euclidean plumbing mode
# (tests/demos only -- Var(Z) is not recoverable from the moments
# container). By the c^2 direction-invariance (spec lines 366-372)
# either choice changes NO identified set. all_results[[s]]$par is
# packed LAMBDA in every mode; whitened runs draw the SAME rnorm
# count but interpret draws in mu-space; the explicit NULL path
# stays bit-identical to the pre-feature code (pinned by the frozen
# fixture in test_lambda_whitening.R).

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
                                    whiten,
                                    n_starts = 10,
                                    seed = SEED,
                                    maxeval = 500L,
                                    xtol_rel = 1e-6,
                                    support = NULL) {
  if (missing(whiten)) {
    stop(
      "whiten is required: pass list(z = .) or list(vcov = .) (the ",
      "repo default variance normalization) or an explicit NULL ",
      "(Euclidean plumbing path for tests/demos only)"
    )
  }
  lambda_start <- coerce_lambda_start(lambda_start, moments)
  dims <- lambda_dims(lambda_start)
  free <- NULL
  if (!is.null(support)) {
    support <- validate_support_mask(support, lambda_start, moments)
    free <- support_free_mask(dims, support)
  }
  # Whitening context and the zero-variance start check, both BEFORE
  # any RNG use; the lazy branch keeps the whiten = NULL path free of
  # any lambda_whitening.R / lambda_varnorm.R dependency.
  wctx <- if (is.null(whiten)) {
    NULL
  } else {
    whiten_context(whiten, support, moments)
  }
  if (!is.null(wctx)) {
    assert_lambda_variance_nonzero(lambda_start, wctx)
  }
  set.seed(seed)
  par_start <- pack_active(encode_lambda(lambda_start, wctx), free)

  objective_start <- honest_width_lambda(lambda_start, tau, moments)

  # Multistart recipe: the primary start packs unnormalized,
  # perturbed starts are normalized. In whitened mode the packed
  # coordinates are mu, so the perturbed-start normalization is unit
  # mu: the variance normalization restricted to the support
  # (structural zeros kill all cross terms).
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

  # Reported representative: variance-normalized columns
  # (lambda' V lambda = 1, the repo default) under whitening, else
  # unit-Euclidean.
  lambda_opt <- decode_lambda(
    all_results[[best_idx]]$par, dims, free, wctx
  )
  lambda_opt <- if (is.null(wctx)) {
    normalize_lambda_columns(lambda_opt)
  } else {
    normalize_lambda_columns_vcov(lambda_opt, wctx)
  }

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
  # errors; compare DIRECTION COSINES (Euclidean-unit columns) so the
  # 0.999 threshold is invariant to the reporting normalization.
  duplicate_directions <- vapply(lambda_opt, function(el) {
    if (is.null(el) || ncol(el) < 2) {
      return(FALSE)
    }
    cors <- abs(crossprod(normalize_gamma_columns(el)))
    any(cors[upper.tri(cors)] > 0.999)
  }, logical(1))

  list(
    lambda_optimized = lambda_opt,
    objective_start = objective_start,
    objective_final = honest_values[[best_idx]],
    all_results = all_results,
    best_index = best_idx,
    duplicate_directions = duplicate_directions,
    lambda_variance = if (is.null(wctx)) {
      NULL
    } else {
      lambda_variance_report(lambda_start, lambda_opt, wctx)
    },
    support = support,
    whitening = if (is.null(wctx)) {
      NULL
    } else {
      wctx[c("source", "vcov", "ridge", "jitter")]
    }
  )
}
