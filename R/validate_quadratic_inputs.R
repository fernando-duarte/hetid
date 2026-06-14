#' Validate numeric vector dimensions
#'
#' @noRd
is_numeric_vector_dim <- function(x, n_components) {
  is.numeric(x) && is.null(dim(x)) && length(x) == n_components
}

#' Validate per-element dimensions of the components' Q_i list
#'
#' @param Q_i List of theta-axis vectors from the components object
#' @param maturities Maturity vector from the containers
#' @param n_components Expected dimension (I)
#' @noRd
validate_q_i_dims <- function(Q_i, maturities, # nolint: object_name_linter.
                              n_components) {
  for (idx in seq_along(maturities)) {
    assert_dimension_ok(
      is_numeric_vector_dim(Q_i[[idx]], n_components),
      paste0(
        "Q_i for maturity ", maturities[idx],
        " (position ", idx,
        ") must be a numeric vector of length I"
      )
    )
  }
}

#' Validate finiteness of per-maturity moment and component values
#'
#' Flags NA/NaN/Inf in any per-maturity value before the quadratic
#' assembly so the error names the offending object and maturity
#' instead of surfacing as a misleading downstream failure.
#'
#' @param quantities Named list of position-indexed vectors or lists
#' @param maturities Maturity vector from the containers
#' @noRd
validate_finite_by_maturity <- function(quantities, maturities) {
  for (name in names(quantities)) {
    x <- quantities[[name]]
    bad <- which(!vapply(
      seq_along(x),
      function(k) is.numeric(x[[k]]) && all(is.finite(x[[k]])),
      logical(1)
    ))
    assert_bad_argument_ok(
      length(bad) == 0,
      paste0(
        name, " contains non-finite (NA/NaN/Inf) values ",
        "for maturity/maturities ",
        paste(maturities[bad], collapse = ", ")
      ),
      arg = name
    )
  }
}

#' Validate inputs for the quadratic identified set computation
#'
#' Checks the container classes, that the components and moments carry
#' identical maturity identity, the shapes of the \code{components}
#' object (whose constructor defers shape validation here by design),
#' and the type/finiteness/positivity constraints for
#' \code{\link{compute_identified_set_quadratic}}. Moment shapes are
#' class invariants guaranteed by \code{new_hetid_moments()} and are
#' trusted rather than re-checked.
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
    tau = tau, L_i = components$L_i, V_i = components$V_i
  )
  assert_tau_values_ok(tau)
  assert_dimension_ok(
    length(tau) == n_components,
    "tau must have length I (the moments' n_components)"
  )
  assert_bad_argument_ok(
    is.list(components$Q_i),
    "Q_i must be a list",
    arg = "Q_i"
  )

  # Container shape invariants are trusted from new_hetid_moments();
  # only the components object needs shape validation here.
  n_maturities <- length(maturities)
  validate_time_series_lengths(
    components$L_i, components$V_i, components$Q_i,
    expected_length = n_maturities
  )
  validate_q_i_dims(components$Q_i, maturities, n_components)

  # Validate sigma_i_sq positivity (a quadratic-stage requirement)
  sigma_i_sq <- moments$sigma_i_sq
  assert_sigma_positive(sigma_i_sq, maturities)

  validate_finite_by_maturity(
    list(
      L_i = components$L_i, V_i = components$V_i, Q_i = components$Q_i,
      s_i_0 = moments$s_i_0, s_i_1 = moments$s_i_1, s_i_2 = moments$s_i_2
    ),
    maturities
  )

  list(
    maturities = maturities,
    n_components = n_components,
    n_maturities = n_maturities
  )
}
