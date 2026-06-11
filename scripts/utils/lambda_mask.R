# Support-Mask Helpers -- per-component instrument subsets inside
# run_lambda_optimization (sourced by common_settings.R alongside
# lambda_optimization.R). A support is the package convention: one
# integer vector of FREE instrument rows per constrained system
# column, NULL at unconstrained columns -- the same object
# hetid::lambda_from_support() consumes, so a padded start and its
# mask come from one recipe. Off-support weight entries are never
# packed: they stay exactly 0.0 through the start, every
# perturbation, every slsqp iterate, the honest re-evaluations, and
# the returned optimum. Validation reuses the installed package's
# internal validators via ::: (single source of truth for the index
# rules and the structured hetid_error classes; precedent:
# common_settings.R reads hetid:::HETID_ACM_SCHEMA).

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
