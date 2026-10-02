#' Construct and Validate hetid_tau0_fit Objects
#'
#' Container for the tau = 0 mean-equation fit built by
#' \code{\link{compute_tau0_system}}.
#'
#' @details The list contains \code{beta1r}, \code{beta2r}, \code{w1},
#' \code{w2}, \code{z}, \code{gamma}, \code{moments}, \code{point}, and
#' \code{beta1}. Attributes record the integer sample size \code{n_obs},
#' the logical \code{impose_null} flag, and the positive tolerance \code{tol}.
#' Here I = \code{ncol(w2)} is the component axis and J = \code{ncol(z)}.
#' See \code{\link{new_hetid_tau0_fit}} for element shapes and
#' \code{\link{validate_hetid_tau0_fit}} for structural checks.
#'
#' @name hetid_tau0_fit
#' @keywords internal
NULL

#' Construct a hetid_tau0_fit Object
#'
#' Checks the three identity attributes and coerces \code{n_obs} to integer,
#' trusting the data elements. \code{\link{compute_tau0_system}} always calls
#' \code{\link{validate_hetid_tau0_fit}}; known-good parts may skip that sweep.
#'
#' @param beta1r Named numeric vector, OLS coefficients of the Y1 reduced form.
#' @param beta2r Numeric matrix (I x length(beta1r)), OLS coefficients of the Y2
#'   reduced form, with named component rows and columns matching \code{names(beta1r)}.
#' @param w1 Finite numeric vector of length \code{n_obs}, Y1 reduced-form residuals.
#' @param w2 Finite numeric matrix (n_obs x I), Y2 reduced-form residuals.
#' @param z Finite numeric matrix (n_obs x J), de-meaned instruments.
#' @param gamma Numeric matrix (J x I), instrument weights.
#' @param moments A \code{hetid_moments} object with centered \eqn{1/T} moments.
#' @param point A list from \code{\link{compute_tau0_point}}, or
#'   \code{NULL} when the tau = 0 system has no unique consistent point solution.
#' @param beta1 Named numeric vector of length \code{length(beta1r)}, structural
#'   coefficients with names matching \code{beta1r}, or \code{NULL} exactly when
#'   \code{point} is \code{NULL}.
#' @param n_obs Finite integer-valued scalar in \code{[1, .Machine$integer.max]}.
#' @param impose_null A \code{TRUE} or \code{FALSE} flag, whether \eqn{\beta_2^R = 0}
#'   was imposed on the second reduced form, setting \code{beta2r} to zero.
#' @param tol Positive finite numeric scalar, the point tolerance actually used.
#' @return A \code{hetid_tau0_fit} list with the supplied elements and attributes.
#' @keywords internal
new_hetid_tau0_fit <- function(beta1r, beta2r, w1, w2, z, gamma, moments,
                               point, beta1, n_obs, impose_null, tol) {
  assert_scalar_integer_in_range(n_obs, "n_obs", 1, .Machine$integer.max)
  assert_flag(impose_null, "impose_null")
  assert_scalar_finite(tol, "tol")
  assert_bad_argument_ok(tol > 0, "tol must be positive", arg = "tol")

  structure(
    list(
      beta1r = beta1r, beta2r = beta2r, w1 = w1, w2 = w2, z = z,
      gamma = gamma, moments = moments, point = point, beta1 = beta1
    ),
    n_obs = as.integer(n_obs),
    impose_null = impose_null,
    tol = tol,
    class = "hetid_tau0_fit"
  )
}

#' Validate beta1r and beta2r Against Each Other and the Component Axis
#' @param x A classed \code{hetid_tau0_fit} object.
#' @param dims \code{list(i_dim, j_dim)} from \code{validate_tau0_fit_data_shapes()}.
#' @return Invisible TRUE.
#' @noRd
validate_tau0_fit_betas <- function(x, dims) {
  assert_bad_argument_ok(
    is.numeric(x$beta1r) && is.null(dim(x$beta1r)), "beta1r must be a vector",
    arg = "beta1r"
  )
  assert_bad_argument_ok(
    !is.null(names(x$beta1r)) && !anyNA(names(x$beta1r)) &&
      all(nzchar(names(x$beta1r))),
    "beta1r must have non-empty, non-NA names",
    arg = "beta1r"
  )
  assert_bad_argument_ok(
    is.matrix(x$beta2r) && is.numeric(x$beta2r), "beta2r must be a matrix",
    arg = "beta2r"
  )
  assert_dimension_ok(
    nrow(x$beta2r) == dims$i_dim && ncol(x$beta2r) == length(x$beta1r),
    "beta2r must be an I x length(beta1r) matrix"
  )
  assert_bad_argument_ok(
    !is.null(rownames(x$beta2r)) && !is.null(colnames(x$beta2r)),
    "beta2r must have row (y2) and column (predictor) dimnames",
    arg = "beta2r"
  )
  assert_bad_argument_ok(
    identical(colnames(x$beta2r), names(x$beta1r)),
    "colnames(beta2r) must equal names(beta1r)",
    arg = "beta2r"
  )
  invisible(TRUE)
}

#' Validate point and the point/beta1 Pairing
#' @param x A classed \code{hetid_tau0_fit} object.
#' @param dims \code{list(i_dim, j_dim)} from \code{validate_tau0_fit_data_shapes()}.
#' @return Invisible TRUE.
#' @noRd
validate_tau0_fit_point <- function(x, dims) {
  point <- x$point
  # [[ ]] because $ would partial-match beta1r when the beta1 key is absent
  beta1 <- x[["beta1"]]
  if (is.null(point)) {
    assert_bad_argument_ok(
      is.null(beta1), "beta1 must be NULL when point is NULL",
      arg = "beta1"
    )
    return(invisible(TRUE))
  }
  assert_bad_argument_ok(
    is.list(point) && all(c("theta", "cond") %in% names(point)),
    "point must be NULL or a list with elements theta and cond",
    arg = "point"
  )
  assert_bad_argument_ok(
    is.numeric(point$theta) && is.null(dim(point$theta)) &&
      all(is.finite(point$theta)),
    "point$theta must be a finite numeric vector",
    arg = "point"
  )
  assert_dimension_ok(
    length(point$theta) == dims$i_dim,
    "point$theta must have length equal to ncol(w2)"
  )
  assert_scalar_finite(point$cond, "point$cond")
  assert_bad_argument_ok(
    !is.null(beta1), "beta1 must be provided when point is non-NULL",
    arg = "beta1"
  )
  assert_bad_argument_ok(
    is.numeric(beta1) && is.null(dim(beta1)) && !is.null(names(beta1)),
    "beta1 must be a named numeric vector",
    arg = "beta1"
  )
  assert_dimension_ok(
    length(beta1) == length(x$beta1r), "beta1 must have length(beta1r)"
  )
  assert_bad_argument_ok(
    identical(names(beta1), names(x$beta1r)),
    "names(beta1) must equal names(beta1r)",
    arg = "beta1"
  )
  invisible(TRUE)
}
