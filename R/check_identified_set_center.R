#' Check Strict Interior at a Candidate Center
#'
#' Distinguishes numerical failure of the strict-interior criterion from an
#' invalid argument. A failed check does not establish that the set is empty.
#'
#' @param quadratic Nonempty finite symmetric quadratic system with A_i, b_i,
#'   and c_i fields, as returned by build_quadratic_system().
#' @param center Finite numeric vector in theta order, or NULL for a missing center.
#' @return A list with interior, reason, and slack. Reason is interior,
#'   missing_center, nonfinite_constraints, or not_strictly_inside. Slack retains
#'   every constraint value in input order. For a missing center it is NULL.
#'   Boundary values of zero fail the strict check. Malformed inputs raise
#'   structured hetid conditions.
#' @export
#' @examples
#' quadratic <- list(A_i = list(diag(2)), b_i = list(c(0, 0)), c_i = -1)
#' check_identified_set_center(quadratic, center = c(0, 0))
#' check_identified_set_center(quadratic, center = c(1, 0))
check_identified_set_center <- function(quadratic, center) {
  dimension <- quadratic_validate_system(quadratic)
  if (is.null(center)) {
    return(list(interior = FALSE, reason = "missing_center", slack = NULL))
  }
  assert_bad_argument_ok(quadratic_real_finite(center) && is.null(dim(center)),
    "center must be a finite numeric vector",
    arg = "center"
  )
  assert_dimension_ok(length(center) == dimension, "center has the wrong dimension")
  slack <- make_system_checker(quadratic)(center)
  reason <- if (any(!is.finite(slack))) {
    "nonfinite_constraints"
  } else if (all(slack < 0)) {
    "interior"
  } else {
    "not_strictly_inside"
  }
  list(interior = identical(reason, "interior"), reason = reason, slack = slack)
}
