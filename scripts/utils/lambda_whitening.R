# Whitening Validation and Context -- opt-in search-space
# reparameterization for run_lambda_optimization (sourced by
# common_settings.R; the coordinate codecs encode_lambda /
# decode_lambda live in lambda_mask.R beside the packing family).
# Whitening implements the spec's first canonical normalization
# lambda' Var(Z) lambda = 1
# (docs/lewbel_multivariate_set_identification.tex line 479) as
# search GEOMETRY only: the optimizer walks mu = chol(V) %*% lambda
# -- per-component sub-blocks chol(V[s_i, s_i]) of the supported
# instruments under a mask -- while every constraint evaluation, the
# honest oracle, and the returned weights stay in original lambda
# coordinates. Each constraint depends on its weight column only
# through direction (g(theta; c*lambda, tau) = c^2 * g(theta;
# lambda, tau), spec lines 366-372), so whitening changes NO
# constraint set and NO statistical property; the profile-bound
# solver already non-dimensionalizes the inner constraint solve, so
# the benefit is conditioning of the WEIGHT-SPACE search alone.
# Var(Z) is not recoverable from the moments container (its seven
# statistics covary Z with residual products, never Z with itself),
# hence the explicit whiten input. Any positive-definite V yields a
# valid reparameterization -- validation catches accidents
# (dimensions, names, non-finite values, singularity); it does not
# certify statistical correctness. Validation reuses the installed
# package's internal validators via ::: (single source of truth for
# the hetid_error classes; precedent: lambda_mask.R) and the
# package's centered 1/T covariance via hetid:::centered_cov.

# Relative eigenvalue floor below which a (sub-block) covariance is
# treated as numerically singular, and the relative asymmetry
# tolerance for a user-supplied vcov.
WHITEN_EIGEN_RTOL <- 1e-10
WHITEN_SYM_RTOL <- 1e-8

# Cross-check instrument names when BOTH sides are named; unnamed
# input is accepted positionally (the moments rows are positional).
check_whiten_names <- function(given, expected) {
  if (is.null(given) || is.null(expected)) {
    return(invisible(NULL))
  }
  hetid:::assert_bad_argument_ok(
    identical(given, expected),
    paste0(
      "whiten instrument names do not match the moments' instrument ",
      "rows (expected: ", paste(expected, collapse = ", "), ")"
    ),
    arg = "whiten"
  )
  invisible(NULL)
}

# Parse and validate the tagged whiten input into
# list(vcov, source, ridge). vcov is symmetrized exactly; ridge is
# validated but not yet applied (whiten_context applies it once,
# before sub-blocking).
parse_whiten_input <- function(whiten, j_rows, inst_names) {
  hetid:::assert_bad_argument_ok(
    is.list(whiten) && !is.null(names(whiten)) &&
      all(nzchar(names(whiten))) &&
      all(names(whiten) %in% c("z", "vcov", "ridge")) &&
      anyDuplicated(names(whiten)) == 0 &&
      (is.null(whiten$z) != is.null(whiten$vcov)),
    paste0(
      "whiten must be NULL or a named list carrying exactly one of ",
      "z (a T x J instrument matrix) or vcov (a J x J covariance ",
      "matrix), plus an optional scalar ridge"
    ),
    arg = "whiten"
  )
  ridge <- whiten$ridge
  if (!is.null(ridge)) {
    hetid:::assert_bad_argument_ok(
      is.numeric(ridge) && length(ridge) == 1 &&
        is.finite(ridge) && ridge >= 0,
      "whiten$ridge must be a single finite non-negative number",
      arg = "whiten"
    )
  }
  if (!is.null(whiten$z)) {
    z <- whiten$z
    hetid:::assert_bad_argument_ok(
      is.matrix(z) && is.numeric(z) && nrow(z) >= 2,
      "whiten$z must be a numeric matrix with at least two rows",
      arg = "whiten"
    )
    hetid:::assert_numeric_finite_values(z, "whiten$z")
    hetid:::assert_dimension_ok(
      ncol(z) == j_rows,
      paste0(
        "whiten$z must have J = ", j_rows,
        " columns to match the moments' instruments"
      )
    )
    check_whiten_names(colnames(z), inst_names)
    return(list(
      vcov = unname(hetid:::centered_cov(z, z)),
      source = "z", ridge = ridge
    ))
  }
  v <- whiten$vcov
  hetid:::assert_bad_argument_ok(
    is.matrix(v) && is.numeric(v),
    "whiten$vcov must be a numeric J x J matrix",
    arg = "whiten"
  )
  hetid:::assert_numeric_finite_values(v, "whiten$vcov")
  hetid:::assert_dimension_ok(
    nrow(v) == j_rows && ncol(v) == j_rows,
    paste0(
      "whiten$vcov must be ", j_rows, " x ", j_rows,
      " to match the moments' instruments"
    )
  )
  hetid:::assert_bad_argument_ok(
    max(abs(v - t(v))) <= WHITEN_SYM_RTOL * max(1, max(abs(v))),
    "whiten$vcov must be symmetric (within 1e-8 relative tolerance)",
    arg = "whiten"
  )
  check_whiten_names(rownames(v), inst_names)
  check_whiten_names(colnames(v), inst_names)
  list(vcov = unname((v + t(v)) / 2), source = "vcov", ridge = ridge)
}

# Build the whitening context BEFORE any RNG use: per constrained
# component i, the free rows (full 1..J without a mask, support[[i]]
# with one) and the upper-triangular factor chol(V[rows, rows]).
# support must already be the canonical integer-coerced support (or
# NULL). A numerically singular sub-block is a structured error, not
# a silent fix: drop a collinear instrument via support, or pass an
# explicit whiten$ridge (relative jitter ridge * mean(diag(V)),
# applied once to the full V and echoed via the jitter field).
whiten_context <- function(whiten, support, moments) {
  if (is.null(whiten)) {
    return(NULL)
  }
  hetid:::assert_hetid_moments(moments)
  j_rows <- nrow(moments$r_i_0)
  maturities <- attr(moments, "maturities")
  n_components <- attr(moments, "n_components")
  parsed <- parse_whiten_input(
    whiten, j_rows, rownames(moments$r_i_0)
  )
  vcov_used <- parsed$vcov
  ridge <- if (is.null(parsed$ridge)) 0 else parsed$ridge
  jitter <- 0
  if (ridge > 0) {
    jitter <- ridge * mean(diag(vcov_used))
    vcov_used <- vcov_used + diag(jitter, j_rows)
  }
  rows <- vector("list", n_components)
  chol_blocks <- vector("list", n_components)
  for (i in maturities) {
    rows_i <- if (is.null(support)) seq_len(j_rows) else support[[i]]
    block <- vcov_used[rows_i, rows_i, drop = FALSE]
    ev <- eigen(block, symmetric = TRUE, only.values = TRUE)$values
    hetid:::assert_bad_argument_ok(
      isTRUE(max(ev) > 0 && min(ev) > WHITEN_EIGEN_RTOL * max(ev)),
      paste0(
        "whitening covariance is numerically singular on the ",
        "instruments of component ", i, " (relative eigenvalue gap ",
        format(min(ev) / max(ev), digits = 3), "); drop a collinear ",
        "instrument via support, or pass an explicit whiten$ridge"
      ),
      arg = "whiten"
    )
    rows[[i]] <- rows_i
    chol_blocks[[i]] <- chol(block)
  }
  list(
    rows = rows, chol = chol_blocks, vcov = vcov_used,
    source = parsed$source, ridge = ridge, jitter = jitter
  )
}
