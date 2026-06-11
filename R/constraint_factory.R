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

#' Build a Checker Over Every Constraint of a Quadratic System
#'
#' Companion to \code{\link{make_constraint_checker}} for
#' multi-constraint systems: takes the \code{quadratic} element
#' produced by \code{\link{build_general_quadratic_system}} (or the
#' legacy \code{\link{build_quadratic_system}}) and returns a closure
#' evaluating every constraint at a candidate theta. Following the
#' package's \code{hin <= 0} convention, theta lies inside the
#' identified set exactly when every returned value is non-positive
#' (\code{max(values) <= 0}). A system where no theta satisfies all
#' constraints is an empty estimated set; this checker is the
#' intended tool for probing that case on a grid.
#'
#' @param quadratic List with parallel \code{A_i}, \code{b_i},
#'   \code{c_i} elements (the \code{quadratic} element of either
#'   builder's output)
#' @return Function of theta returning the named numeric vector of
#'   constraint values
#'
#' @template section-general-instruments
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' w1 <- rnorm(50)
#' w2 <- matrix(rnorm(100), nrow = 50)
#' z <- matrix(rnorm(150), nrow = 50)
#' moments <- compute_identification_moments(w1, w2, z)
#' qs <- build_general_quadratic_system(
#'   separate_instruments_lambda(moments), 0.2, moments
#' )
#' check_all <- make_system_checker(qs$quadratic)
#' max(check_all(c(0, 0)))
make_system_checker <- function(quadratic) {
  assert_bad_argument_ok(
    is_parallel_quadratic(quadratic),
    paste0(
      "quadratic must carry parallel A_i, b_i, c_i elements, e.g. ",
      "the quadratic element of build_general_quadratic_system()"
    ),
    arg = "quadratic"
  )
  checkers <- lapply(seq_along(quadratic$A_i), function(k) {
    make_constraint_checker(
      quadratic$A_i[[k]], quadratic$b_i[[k]], quadratic$c_i[k]
    )
  })
  names(checkers) <- names(quadratic$A_i)
  function(theta) {
    vapply(checkers, function(f) as.numeric(f(theta)), numeric(1))
  }
}

#' Quadratic List Carries Parallel A/b/c Elements
#'
#' The list guard must run first (the \code{$} accessor errors on
#' atomic input); the remaining checks are evaluated as a plain
#' vector — no short-circuiting needed, and the flat form keeps the
#' cyclomatic complexity at the project threshold.
#'
#' @param quadratic Candidate quadratic list
#' @return Logical scalar
#' @noRd
is_parallel_quadratic <- function(quadratic) {
  if (!is.list(quadratic)) {
    return(FALSE)
  }
  all(c(
    is.list(quadratic$A_i),
    is.list(quadratic$b_i),
    is.numeric(quadratic$c_i),
    length(quadratic$A_i) == length(quadratic$b_i),
    length(quadratic$A_i) == length(quadratic$c_i)
  ))
}
