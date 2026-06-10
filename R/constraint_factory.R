#' Constraint Factory for Quadratic Form Evaluation
#'
#' Factory function for precomputing quadratic form constants
#'
#' @name constraint_factory
#' @keywords internal
NULL

#' Create Constraint Checker from Quadratic Components
#'
#' Factory function that precomputes constants and returns a
#' closure for efficiently evaluating the quadratic constraint
#' at many theta values (e.g., grid search or optimisation).
#' Inputs are validated once at factory time; the returned closure
#' performs no checks.
#'
#' @param A_i Symmetric numeric matrix from quadratic computation
#' @param b_i Numeric coefficient vector from quadratic computation,
#'   with length equal to \code{nrow(A_i)}
#' @param c_i Scalar numeric constant from quadratic computation
#'
#' @return A function that takes a theta vector and returns the scalar
#'   value of \eqn{\theta' A_i \theta + b_i' \theta + c_i}. Negative
#'   values indicate theta is inside the constraint.
#'
#' @export
#'
#' @examples
#' A <- matrix(c(1, 0, 0, 1), 2, 2)
#' b <- c(-2, -2)
#' c_val <- 0.5
#' check <- make_constraint_checker(A, b, c_val)
#' check(c(0.5, 0.5)) # Evaluate constraint at theta
make_constraint_checker <- function(A_i, b_i, c_i) { # nolint: object_name_linter.
  assert_bad_argument_ok(
    is.matrix(A_i) && is.numeric(A_i) && nrow(A_i) == ncol(A_i),
    "A_i must be a square numeric matrix",
    arg = "A_i"
  )
  assert_bad_argument_ok(
    is.numeric(b_i) && is.null(dim(b_i)),
    "b_i must be a numeric vector",
    arg = "b_i"
  )
  assert_dimension_ok(
    length(b_i) == nrow(A_i),
    paste0(
      "b_i must have length nrow(A_i) = ", nrow(A_i),
      "; got length ", length(b_i)
    )
  )
  assert_bad_argument_ok(
    is.numeric(c_i) && length(c_i) == 1 && is.null(dim(c_i)),
    "c_i must be a numeric scalar",
    arg = "c_i"
  )
  force(A_i)
  force(b_i)
  force(c_i)
  function(theta) {
    as.numeric(crossprod(theta, A_i %*% theta)) +
      sum(b_i * theta) + c_i
  }
}
