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
#'
#' @param A_i Symmetric matrix from quadratic computation
#' @param b_i Coefficient vector from quadratic computation
#' @param c_i Scalar constant from quadratic computation
#'
#' @return A function that takes a theta vector and returns
#'   the scalar value of theta'A_i*theta + b_i'theta + c_i.
#'   Negative values indicate theta is inside the constraint.
#'
#' @export
#'
#' @examples
#' A <- matrix(c(1, 0, 0, 1), 2, 2)
#' b <- c(-2, -2)
#' c_val <- 0.5
#' check <- make_constraint_checker(A, b, c_val)
#' check(c(0.5, 0.5)) # Evaluate constraint at theta
make_constraint_checker <- function(A_i, b_i, c_i) {
  force(A_i)
  force(b_i)
  force(c_i)
  function(theta) {
    as.numeric(crossprod(theta, A_i %*% theta)) +
      sum(b_i * theta) + c_i
  }
}
