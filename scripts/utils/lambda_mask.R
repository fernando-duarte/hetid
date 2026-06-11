# Lambda Optimizer Shared Helpers -- base packing, the legacy-style
# matrix-start coercion, per-component support masks, coordinate
# codecs, and the honest width oracle for run_lambda_optimization
# (sourced by common_settings.R alongside lambda_optimization.R;
# helpers relocated here under the 200-line file rule). A support is
# the package convention: one integer vector of FREE instrument rows
# per constrained system column, NULL at unconstrained columns --
# the hetid::lambda_from_support() object, so a padded start and its
# mask come from one recipe. Off-support weight entries are never
# packed: they stay exactly 0.0 through the start, every
# perturbation, every slsqp iterate, the honest re-evaluations, and
# the returned optimum. Validation reuses the installed package's
# internal validators via ::: (single source of truth for the index
# rules and the structured hetid_error classes).

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

# NULL out unconstrained system columns of a legacy-style matrix
# start: the strict list-form validator rejects weights there, and
# matrix starts must keep working on subset-maturity containers.
coerce_lambda_start <- function(lambda_start, moments) {
  if (!is.matrix(lambda_start)) {
    return(lambda_start)
  }
  constrained <- attr(moments, "maturities")
  lapply(
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

# Validate a support mask against the start and the moments
# container BEFORE any RNG use or optimization: structural
# impossibilities (a fully masked column) and silent-misalignment
# hazards (nonzero start entries on masked rows) must error
# immediately, never degrade into honest-Inf runs. Returns the
# canonical integer-coerced support.
validate_support_mask <- function(support, lambda_list, moments) {
  hetid:::assert_hetid_moments(moments)
  maturities <- attr(moments, "maturities")
  n_components <- attr(moments, "n_components")
  j_rows <- nrow(moments$r_i_0)
  support <- hetid:::assert_support_list(
    support, j_rows, n_components, maturities
  )
  for (i in maturities) {
    el <- lambda_list[[i]]
    hetid:::assert_dimension_ok(
      is.matrix(el) && is.numeric(el) && nrow(el) == j_rows,
      paste0(
        "lambda_start[[", i, "]] must be a numeric matrix with J = ",
        j_rows, " rows to be masked by support"
      )
    )
    off <- setdiff(seq_len(j_rows), support[[i]])
    hetid:::assert_bad_argument_ok(
      isTRUE(all(el[off, ] == 0)),
      paste0(
        "lambda_start[[", i, "]] has nonzero (or non-finite) ",
        "entries on masked rows; off-support entries must be exact ",
        "zeros (build the start with hetid::lambda_from_support, ",
        "or fix the support)"
      ),
      arg = "lambda_start"
    )
  }
  support
}

# Packed logical mask of FREE elements in pack_lambda() order
# (column-major within each component): each of a component's K
# columns shares that component's row support.
support_free_mask <- function(dims, support) {
  unlist(
    lapply(seq_along(dims), function(i) {
      if (is.null(dims[[i]])) {
        return(logical(0))
      }
      rep(seq_len(dims[[i]][1]) %in% support[[i]], dims[[i]][2])
    }),
    use.names = FALSE
  )
}

# Masked twins of pack_lambda / unpack_lambda. free = NULL
# reproduces them exactly -- the seeded K = 1 equivalence contract
# runs through these wrappers with free = NULL, so the unmasked path
# stays bit-identical by construction. Under a mask only free
# elements are packed; unpacking scatters them into exact +0.0
# padding (numeric(length(free)) initializes to +0.0 and masked
# positions are never written).
pack_active <- function(lambda_list, free) {
  par <- pack_lambda(lambda_list)
  if (is.null(free)) par else par[free]
}

unpack_active <- function(par, dims, free) {
  if (is.null(free)) {
    return(unpack_lambda(par, dims))
  }
  full <- numeric(length(free))
  full[free] <- par
  unpack_lambda(full, dims)
}

# Coordinate codecs -- the single composition point of mask and
# opt-in whitening. wctx is the whiten_context() output or NULL;
# with wctx = NULL both codecs reduce literally to the identity /
# unpack_active (the wrapper-reduction pattern that keeps the
# unwhitened path bit-identical by construction). Under whitening
# the packed vector holds mu = R_i %*% lambda[rows_i, ], stored on
# the SAME free rows the mask packs -- zero lambda-rows never map to
# zero mu-rows (chol mixes coordinates), which is exactly why
# whitening composes on the compact free block AFTER masking, never
# on the padded matrix.
encode_lambda <- function(lambda_list, wctx) {
  if (is.null(wctx)) {
    return(lambda_list)
  }
  for (i in seq_along(lambda_list)) {
    if (is.null(lambda_list[[i]]) || is.null(wctx$rows[[i]])) next
    rows_i <- wctx$rows[[i]]
    lambda_list[[i]][rows_i, ] <-
      wctx$chol[[i]] %*% lambda_list[[i]][rows_i, , drop = FALSE]
  }
  lambda_list
}

decode_lambda <- function(par, dims, free, wctx) {
  out <- unpack_active(par, dims, free)
  if (is.null(wctx)) {
    return(out)
  }
  for (i in seq_along(out)) {
    if (is.null(out[[i]]) || is.null(wctx$rows[[i]])) next
    rows_i <- wctx$rows[[i]]
    out[[i]][rows_i, ] <-
      backsolve(wctx$chol[[i]], out[[i]][rows_i, , drop = FALSE])
  }
  out
}

# Honest total width oracle: the finite total profile-bound width
# when EVERY side is bounded AND valid, else Inf -- never the
# steering penalty.
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
