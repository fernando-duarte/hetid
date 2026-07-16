#' Validate Numeric Vector Dimensions
#'
#' @param x Object to check
#' @param n_components Expected length
#' @return TRUE when \code{x} is a dimensionless numeric vector of length \code{n_components}
#' @noRd
is_numeric_vector_dim <- function(x, n_components) {
  is.numeric(x) && is.null(dim(x)) && length(x) == n_components
}

#' Validate Per-Element Dimensions of the Components' Q_i List
#'
#' @param Q_i List of theta-axis vectors from the components object
#' @param maturities Maturity vector from the containers
#' @param n_components Expected dimension (I)
#' @return Invisible TRUE
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
  invisible(TRUE)
}

#' Validate Finiteness of Per-Maturity Moment and Component Values
#'
#' Flags NA/NaN/Inf in any per-maturity value before the quadratic
#' assembly so the error names the offending object and maturity
#' instead of surfacing as a misleading downstream failure.
#'
#' @param quantities Named list of position-indexed vectors or lists
#' @param maturities Maturity vector from the containers
#' @return Invisible TRUE
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
  invisible(TRUE)
}

#' Validate a hetid_components Object
#'
#' Full shape gate for the \code{hetid_components} class, checked
#' against the object's own attributes: element lengths, per-maturity
#' names, and the theta-axis dimension of every \code{Q_i} entry. Run by
#' the public boundary \code{compute_identified_set_components()} on
#' every object it returns and by \code{validate_quadratic_inputs()} on
#' every object it receives; call it directly on containers assembled
#' via \code{new_hetid_components()} from parts that are not known-good.
#'
#' @param x A classed \code{hetid_components} object
#' @return \code{x}, invisibly
#' @keywords internal
validate_hetid_components <- function(x) {
  assert_bad_argument_ok(
    inherits(x, "hetid_components"),
    "x must be a hetid_components object",
    arg = "x"
  )
  maturities <- attr(x, "maturities")
  n_components <- attr(x, "n_components")
  expected <- maturity_names(maturities)
  assert_bad_argument_ok(
    is.list(x$Q_i),
    "Q_i must be a list of length(maturities) elements",
    arg = "Q_i"
  )
  validate_time_series_lengths(
    x$L_i, x$V_i, x$Q_i,
    expected_length = length(maturities)
  )
  for (name in c("L_i", "V_i", "Q_i")) {
    assert_bad_argument_ok(
      identical(names(x[[name]]), expected),
      paste0(name, " names must equal maturity_N for maturities"),
      arg = name
    )
  }
  validate_q_i_dims(x$Q_i, maturities, n_components)
  invisible(x)
}

#' Validate Inputs for the Quadratic Identified Set Computation
#'
#' Checks the container classes, that the components and moments carry
#' identical maturity identity, the shapes of the \code{components}
#' object (via \code{validate_hetid_components()}), and the
#' type/finiteness/positivity constraints for
#' \code{\link{compute_identified_set_quadratic}}. Moment shapes are
#' class invariants guaranteed by the
#' \code{compute_identification_moments()} boundary and are trusted
#' rather than re-checked.
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
    paste0(
      "tau must have length I (the moments' n_components): tau = ",
      length(tau), "; n_components = ", n_components
    )
  )
  validate_hetid_components(components)
  n_maturities <- length(maturities)

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
