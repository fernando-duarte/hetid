#' Validate numeric vector dimensions
#'
#' @noRd
is_numeric_vector_dim <- function(x, n_components) {
  is.numeric(x) && length(x) == n_components
}

#' Validate square matrix dimensions
#'
#' @noRd
is_square_matrix_dim <- function(x, n_components) {
  is.matrix(x) &&
    nrow(x) == n_components &&
    ncol(x) == n_components
}

#' Validate argument types for quadratic inputs
#'
#' @inheritParams compute_identified_set_quadratic
#' @noRd
validate_quadratic_types <- function(gamma, tau,
                                     L_i, V_i, Q_i, # nolint: object_name_linter.
                                     s_i_0, s_i_1,
                                     s_i_2,
                                     sigma_i_sq) {
  assert_bad_argument_ok(
    is.matrix(gamma),
    "gamma must be a matrix",
    arg = "gamma"
  )
  assert_bad_argument_ok(
    is.numeric(tau) && is.vector(tau),
    "tau must be a numeric vector",
    arg = "tau"
  )
  assert_bad_argument_ok(
    !any(tau < 0),
    "All elements of tau must be nonnegative",
    arg = "tau"
  )
  assert_bad_argument_ok(
    is.numeric(L_i) && is.numeric(V_i),
    "L_i and V_i must be numeric vectors"
  )
  assert_bad_argument_ok(
    is.list(Q_i),
    "Q_i must be a list",
    arg = "Q_i"
  )
  assert_bad_argument_ok(
    is.numeric(s_i_0) && is.numeric(sigma_i_sq),
    "s_i_0 and sigma_i_sq must be numeric vectors"
  )
  assert_bad_argument_ok(
    is.list(s_i_1) && is.list(s_i_2),
    "s_i_1 and s_i_2 must be lists"
  )
}

#' Validate per-element dimensions of list inputs
#'
#' @param Q_i,s_i_1,s_i_2 List inputs to check
#' @param maturities Resolved maturity vector
#' @param n_components Expected dimension (I)
#' @noRd
validate_list_element_dims <- function(Q_i, s_i_1, # nolint: object_name_linter.
                                       s_i_2,
                                       maturities,
                                       n_components) {
  for (idx in seq_along(maturities)) {
    i <- maturities[idx]
    assert_dimension_ok(
      is_numeric_vector_dim(Q_i[[idx]], n_components),
      paste(
        "Q_i[[", idx,
        "]] must be a numeric vector of length I"
      )
    )
    assert_dimension_ok(
      is_numeric_vector_dim(s_i_1[[idx]], n_components),
      paste0(
        "s_i_1 for maturity ", i,
        " (position ", idx,
        ") must be a numeric vector of length I"
      )
    )
    assert_dimension_ok(
      is_square_matrix_dim(s_i_2[[idx]], n_components),
      paste0(
        "s_i_2 for maturity ", i,
        " (position ", idx,
        ") must be an I x I matrix"
      )
    )
  }
}

#' Validate inputs for the quadratic identified set computation
#'
#' Checks types, dimensions, positivity constraints, and resolves
#' maturities for \code{\link{compute_identified_set_quadratic}}.
#'
#' @inheritParams compute_identified_set_quadratic
#'
#' @return A list with components:
#' \describe{
#'   \item{maturities}{Resolved integer vector of maturity indices}
#'   \item{n_components}{Number of columns in \code{gamma} (I)}
#'   \item{n_maturities}{Length of \code{maturities}}
#' }
#'
#' @keywords internal
validate_quadratic_inputs <- function(gamma, tau,
                                      L_i, V_i, Q_i, # nolint: object_name_linter.
                                      s_i_0, s_i_1,
                                      s_i_2, sigma_i_sq,
                                      maturities = NULL) {
  validate_quadratic_types(
    gamma, tau, L_i, V_i, Q_i,
    s_i_0, s_i_1, s_i_2, sigma_i_sq
  )

  n_components <- ncol(gamma)

  assert_dimension_ok(
    length(tau) == n_components,
    "tau must have length I (number of columns in gamma)"
  )

  # Resolve maturities from names or explicit parameter
  maturities <- resolve_maturities(
    maturities,
    list(
      L_i = L_i, V_i = V_i, Q_i = Q_i,
      s_i_0 = s_i_0, s_i_1 = s_i_1,
      s_i_2 = s_i_2, sigma_i_sq = sigma_i_sq
    ),
    n_components
  )

  # Validate maturities against gamma dimensions
  assert_bad_argument_ok(
    all(maturities >= 1) &&
      all(maturities <= n_components),
    paste0(
      "maturities must be between 1 and ncol(gamma) (",
      n_components, ")"
    ),
    arg = "maturities"
  )

  n_maturities <- length(maturities)
  assert_dimension_ok(
    all(c(
      length(L_i), length(V_i), length(Q_i),
      length(s_i_0), length(s_i_1), length(s_i_2),
      length(sigma_i_sq)
    ) == n_maturities),
    paste0(
      "All statistical inputs must have length ",
      "matching maturities (", n_maturities, ")"
    )
  )

  # Validate sigma_i_sq
  bad_sigma <- which(
    !is.finite(sigma_i_sq) | sigma_i_sq <= 0
  )
  assert_bad_argument_ok(
    length(bad_sigma) == 0,
    paste0(
      "sigma_i_sq is non-positive, non-finite, or NA ",
      "for maturity/maturities ",
      paste(maturities[bad_sigma], collapse = ", "),
      ". Cannot compute identified set -- ",
      "insufficient heteroskedasticity."
    ),
    arg = "sigma_i_sq"
  )

  validate_list_element_dims(
    Q_i, s_i_1, s_i_2, maturities, n_components
  )

  list(
    maturities = maturities,
    n_components = n_components,
    n_maturities = n_maturities
  )
}
