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

#' Validate per-element dimensions of list inputs
#'
#' @param Q_i,s_i_1,s_i_2 List inputs to check
#' @param maturities Maturity vector from the containers
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
#' Checks the container classes, that the components and moments carry
#' identical maturity identity, and the type/dimension/positivity
#' constraints for \code{\link{compute_identified_set_quadratic}}.
#'
#' @inheritParams compute_identified_set_quadratic
#'
#' @return A list with components:
#' \describe{
#'   \item{maturities}{Integer vector of maturity indices (constraint axis)}
#'   \item{n_components}{Theta-axis dimension (I)}
#'   \item{n_maturities}{Length of \code{maturities}}
#' }
#'
#' @keywords internal
validate_quadratic_inputs <- function(tau, components, moments) {
  assert_hetid_moments(moments)
  assert_bad_argument_ok(
    inherits(components, "hetid_components"),
    paste0(
      "components must be a hetid_components object created by ",
      "compute_identified_set_components()"
    ),
    arg = "components"
  )

  maturities <- attr(moments, "maturities")
  n_components <- attr(moments, "n_components")
  assert_dimension_ok(
    identical(attr(components, "maturities"), maturities),
    paste0(
      "components and moments carry different maturities: components = ",
      paste(attr(components, "maturities"), collapse = ", "),
      "; moments = ", paste(maturities, collapse = ", "),
      ". Both must come from the same system and constraint subset."
    )
  )
  assert_dimension_ok(
    identical(attr(components, "n_components"), n_components),
    paste0(
      "components and moments carry different n_components: components = ",
      attr(components, "n_components"), "; moments = ", n_components
    )
  )

  validate_numeric_inputs(
    tau = tau, L_i = components$L_i, V_i = components$V_i,
    s_i_0 = moments$s_i_0, sigma_i_sq = moments$sigma_i_sq
  )
  assert_bad_argument_ok(
    !any(tau < 0),
    "All elements of tau must be nonnegative",
    arg = "tau"
  )
  assert_dimension_ok(
    length(tau) == n_components,
    "tau must have length I (the moments' n_components)"
  )
  assert_bad_argument_ok(
    is.list(components$Q_i),
    "Q_i must be a list",
    arg = "Q_i"
  )
  assert_bad_argument_ok(
    is.list(moments$s_i_1) && is.list(moments$s_i_2),
    "s_i_1 and s_i_2 must be lists"
  )

  n_maturities <- length(maturities)
  validate_time_series_lengths(
    components$L_i, components$V_i, components$Q_i,
    moments$s_i_0, moments$s_i_1, moments$s_i_2,
    moments$sigma_i_sq,
    expected_length = n_maturities
  )

  # Validate sigma_i_sq
  sigma_i_sq <- moments$sigma_i_sq
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
    components$Q_i, moments$s_i_1, moments$s_i_2,
    maturities, n_components
  )

  list(
    maturities = maturities,
    n_components = n_components,
    n_maturities = n_maturities
  )
}
